use std::{
    collections::{HashMap, HashSet, VecDeque},
    error::Error,
    fmt,
    ops::Range,
    rc::Rc,
};

use bitflags::bitflags;
use indexmap::IndexSet;

use crate::js::{
    common::wtf_8::Wtf8String,
    parser::{
        ast::{self, AstPtr, LabelId, ProgramKind, ResolvedScope, TaggedResolvedScope},
        loc::Pos,
        parser::{ParseFunctionResult, ParseProgramResult},
        scope_tree::{
            AstScopeNode, Binding, BindingKind, ScopeNodeId, ScopeNodeKind, ScopeTree, VMLocation,
            VMScopeNode, ANONYMOUS_DEFAULT_EXPORT_NAME, DERIVED_CONSTRUCTOR_BINDING_NAME,
            HOME_OBJECT_BINDING_NAME, NEW_TARGET_BINDING_NAME, STATIC_HOME_OBJECT_BINDING_NAME,
        },
        source::Source,
    },
    runtime::{
        boxed_value::BoxedValue,
        bytecode::{
            function::{dump_bytecode_function, BytecodeFunction},
            instruction::DefinePropertyFlags,
        },
        class_names::{ClassNames, HomeObjectLocation, Method},
        collections::{BsVec, BsVecField},
        eval::expression::generate_template_object,
        gc::{Escapable, HandleScope},
        global_names::GlobalNames,
        interned_strings::InternedStrings,
        module::source_text_module::{
            DirectReExportEntry, ImportEntry, LocalExportEntry, NamedReExportEntry,
            SourceTextModule,
        },
        object_descriptor::ObjectKind,
        regexp::compiler::compile_regexp,
        scope::Scope,
        scope_names::{ScopeFlags, ScopeNameFlags, ScopeNames},
        source_file::SourceFile,
        string_value::FlatString,
        value::BigIntValue,
        Context, Handle, HeapPtr, Realm, Value,
    },
};

use super::{
    constant_table_builder::{ConstantTableBuilder, ConstantTableIndex},
    exception_handlers::{ExceptionHandlerBuilder, ExceptionHandlersBuilder},
    function::Closure,
    instruction::{DecodeInfo, DefinePrivatePropertyFlags, EvalFlags, OpCode},
    operand::{min_width_for_signed, ConstantIndex, Operand, Register, SInt, UInt},
    register_allocator::TemporaryRegisterAllocator,
    width::{ExtraWide, Narrow, UnsignedWidthRepr, Wide, Width, WidthEnum},
    writer::BytecodeWriter,
};

/// Bytecode generator for an entire program. Handles generating the global function as well as all
/// functions within the program.
pub struct BytecodeProgramGenerator<'a> {
    cx: Context,
    scope_tree: &'a ScopeTree,
    realm: Handle<Realm>,

    /// Source file of the functions that are being generated.
    source_file: Handle<SourceFile>,

    /// The module that is being generated, if this is a module program.
    module: Option<Handle<SourceTextModule>>,

    /// Queue of functions that still need to be generated, along with the information needed to
    /// patch their creation into their parent function.
    pending_functions_queue: VecDeque<PendingFunction>,

    /// List of all functions that have been generated in the order they were generated in.
    ///
    /// This may be used for debugging purposes, to dump all functions in the order they were
    /// generated. Functions are only added here if the option is not None.
    all_functions: Option<Handle<FunctionVec>>,
}

pub struct BytecodeScript {
    pub script_function: Handle<BytecodeFunction>,
    pub global_names: Handle<GlobalNames>,
}

impl Escapable for BytecodeScript {
    fn escape(&self, cx: Context) -> Self {
        BytecodeScript {
            script_function: self.script_function.escape(cx),
            global_names: self.global_names.escape(cx),
        }
    }
}

impl<'a> BytecodeProgramGenerator<'a> {
    pub fn new(
        cx: Context,
        scope_tree: &'a ScopeTree,
        realm: Handle<Realm>,
        source: Rc<Source>,
    ) -> Self {
        let source_file = SourceFile::new(cx, &source);

        // If we are dumping bytecode then we must collect all functions
        let all_functions = if cx.options.print_bytecode {
            Some(FunctionVecField::new_vec(cx, 4).to_handle())
        } else {
            None
        };

        Self {
            cx,
            scope_tree,
            realm,
            source_file,
            module: None,
            pending_functions_queue: VecDeque::new(),
            all_functions,
        }
    }

    /// Perform postprocessing for generated functions, such as adding to the list of all functions
    /// if necessary. Must be called on all functions after they are generated.
    fn process_generated_function(&mut self, function: Handle<BytecodeFunction>) {
        if self.all_functions.is_some() {
            FunctionVecField(&mut self.all_functions)
                .maybe_grow_for_push(self.cx)
                .push_without_growing(function.get_());
        }
    }

    /// Dump all bytecode functions if necessary. Must be called at the end of each bytecode program
    /// generation entrypoint.
    fn dump_bytecode_functions(&mut self) {
        if self.cx.options.print_bytecode {
            self.all_functions
                .unwrap()
                .as_mut_slice()
                .sort_by_key(|f| f.source_range().start);

            for bytecode_function in self.all_functions.unwrap().as_slice() {
                dump_bytecode_function(self.cx, *bytecode_function);
            }
        }
    }

    /// Generate bytecode from the result of `parse_script`. Return the toplevel script function
    /// used to execute the program.
    pub fn generate_from_parse_script_result(
        cx: Context,
        parse_result: &'a ParseProgramResult,
        realm: Handle<Realm>,
    ) -> EmitResult<BytecodeScript> {
        debug_assert!(parse_result.program.kind == ProgramKind::Script);

        HandleScope::new(cx, |_| {
            let source = parse_result.program.source.clone();
            let mut generator = Self::new(cx, &parse_result.scope_tree, realm, source);
            let script = generator.generate_script_program(&parse_result.program)?;

            generator.dump_bytecode_functions();

            Ok(script)
        })
    }

    fn generate_script_program(&mut self, program: &ast::Program) -> EmitResult<BytecodeScript> {
        let program_function = self.gen_script_function(program)?;

        while let Some(pending_function) = self.pending_functions_queue.pop_front() {
            self.gen_enqueued_function(pending_function)?;
        }

        Ok(program_function)
    }

    fn gen_script_function(&mut self, program: &ast::Program) -> EmitResult<BytecodeScript> {
        let mut emit_result = EmitFunctionResult::empty();

        let global_names = HandleScope::new(self.cx, |_| {
            let mut generator = BytecodeFunctionGenerator::new_for_program(
                self.cx,
                program,
                self.scope_tree,
                "<global>",
                self.source_file,
                self.realm,
            )?;

            let global_names = generator.gen_global_names(program.scope.as_ref())?;

            generator.gen_program_body(program)?;
            emit_result = generator.finish();

            Ok(global_names)
        })?;

        // Escape emit result into current handle scope immediately, before allocation occurs
        self.escape_emit_function_result(&mut emit_result);
        self.process_generated_function(emit_result.bytecode_function);

        self.enqueue_pending_functions(
            emit_result.bytecode_function,
            emit_result.pending_functions,
        );

        Ok(BytecodeScript { script_function: emit_result.bytecode_function, global_names })
    }

    /// Generate bytecode from the result of `parse_module`. Return the SourceTextModule which is
    /// used to execute the program.
    pub fn generate_from_parse_module_result(
        cx: Context,
        parse_result: &'a ParseProgramResult,
        realm: Handle<Realm>,
    ) -> EmitResult<Handle<SourceTextModule>> {
        debug_assert!(parse_result.program.kind == ProgramKind::Module);

        HandleScope::new(cx, |cx| {
            let source = parse_result.program.source.clone();
            let mut generator = Self::new(cx, &parse_result.scope_tree, realm, source);
            let module = generator.generate_module_program(&parse_result.program)?;

            generator.dump_bytecode_functions();

            Ok(module)
        })
    }

    fn generate_module_program(
        &mut self,
        program: &ast::Program,
    ) -> EmitResult<Handle<SourceTextModule>> {
        let program_function = self.gen_module_function(program)?;
        let source_text_module = self.gen_source_text_module(program, program_function);

        self.module = Some(source_text_module);

        while let Some(pending_function) = self.pending_functions_queue.pop_front() {
            self.gen_enqueued_function(pending_function)?;
        }

        Ok(source_text_module)
    }

    fn gen_module_function(
        &mut self,
        program: &ast::Program,
    ) -> EmitResult<Handle<BytecodeFunction>> {
        let mut emit_result = EmitFunctionResult::empty();

        HandleScope::new(self.cx, |_| {
            let mut generator = BytecodeFunctionGenerator::new_for_program(
                self.cx,
                program,
                self.scope_tree,
                "<module>",
                self.source_file,
                self.realm,
            )?;

            generator.gen_program_body(program)?;
            emit_result = generator.finish();

            Ok(())
        })?;

        // Escape emit result into current handle scope immediately, before allocation occurs
        self.escape_emit_function_result(&mut emit_result);
        self.process_generated_function(emit_result.bytecode_function);

        self.enqueue_pending_functions(
            emit_result.bytecode_function,
            emit_result.pending_functions,
        );

        Ok(emit_result.bytecode_function)
    }

    fn gen_source_text_module(
        &mut self,
        program: &ast::Program,
        program_function: Handle<BytecodeFunction>,
    ) -> Handle<SourceTextModule> {
        let mut module_specifiers = IndexSet::new();

        let mut imports = vec![];
        let mut local_exports = vec![];
        let mut named_re_exports = vec![];
        let mut direct_re_exports = vec![];

        let default = self.cx.names.default().as_string().as_flat();

        // Inlined import and export entry creation logic from:
        // ParseModule (https://tc39.es/ecma262/#sec-parsemodule)

        // First generate all imports and gather all module specifiers in source code order
        for toplevel in &program.toplevels {
            match toplevel {
                ast::Toplevel::Import(import) => {
                    let module_specifier = self.cx.alloc_wtf8_string(&import.source.value);
                    module_specifiers.insert(module_specifier);

                    // Each specifier will generate an import entry
                    for specifier in &import.specifiers {
                        let (local_id, import_name) = match specifier {
                            ast::ImportSpecifier::Default(import) => (&import.local, Some(default)),
                            ast::ImportSpecifier::Namespace(import) => (&import.local, None),
                            ast::ImportSpecifier::Named(import) => {
                                let imported = import
                                    .imported
                                    .as_ref()
                                    .map(|imported| self.alloc_export_name_string(imported))
                                    .unwrap_or_else(|| self.cx.alloc_string(&import.local.name));
                                (&import.local, Some(imported))
                            }
                        };

                        let local_name = self.cx.alloc_string(&local_id.name);
                        let slot_index = Self::id_module_slot_index(local_id);
                        let is_exported = local_id.get_binding().is_exported();

                        imports.push(ImportEntry {
                            module_request: module_specifier,
                            local_name,
                            import_name,
                            slot_index,
                            is_exported,
                        });
                    }
                }
                // Gather module specifiers from re-exports
                ast::Toplevel::ExportNamed(ast::ExportNamedDeclaration {
                    source: Some(source),
                    ..
                })
                | ast::Toplevel::ExportAll(ast::ExportAllDeclaration { source, .. }) => {
                    let module_specifier = self.cx.alloc_wtf8_string(&source.value);
                    module_specifiers.insert(module_specifier);
                }
                _ => {}
            }
        }

        // After import entries have been generated then generate all export entries. Two passes
        // are need because export entries must be able to lookup import entries.
        for toplevel in &program.toplevels {
            match toplevel {
                ast::Toplevel::ExportDefault(export) => {
                    // Default specifiers are always local exports
                    let export_name = self.cx.names.default().as_string().as_flat();
                    let local_name = BytecodeFunctionGenerator::gen_default_export_name(
                        self.cx,
                        &export.declaration,
                    );

                    let slot_index = if let Some(id) = export.id() {
                        Self::id_module_slot_index(id)
                    } else {
                        // If there is no id then this is an anonymous default export, so pull the
                        // anonymous default export binding from the program scope.
                        let scope = program.scope.as_ref();
                        if let VMLocation::ModuleScope { index, .. } = scope
                            .get_binding(ANONYMOUS_DEFAULT_EXPORT_NAME)
                            .vm_location()
                            .unwrap()
                        {
                            index
                        } else {
                            unreachable!("expected module scope location")
                        }
                    };

                    local_exports.push(LocalExportEntry { export_name, local_name, slot_index })
                }
                ast::Toplevel::ExportNamed(export) => {
                    let module_specifier = export
                        .source
                        .as_ref()
                        .map(|source| self.cx.alloc_wtf8_string(&source.value));

                    // Exporting a full named declaration adds export entries for each exported id
                    export.iter_declaration_ids(&mut |id| {
                        let local_name = self.cx.alloc_string(&id.name);
                        let slot_index = Self::id_module_slot_index(id);

                        local_exports.push(LocalExportEntry {
                            export_name: local_name,
                            local_name,
                            slot_index,
                        });
                    });

                    // Each specifier will generate an export entry of some form
                    for specifier in &export.specifiers {
                        let local_name = self.alloc_export_name_string(&specifier.local);
                        let export_name = specifier
                            .exported
                            .as_ref()
                            .map_or(local_name, |exported| self.alloc_export_name_string(exported));

                        // If there is a from source specifier this is a named re-export
                        if let Some(module_specifier) = module_specifier {
                            named_re_exports.push(NamedReExportEntry {
                                module_request: module_specifier,
                                export_name,
                                import_name: Some(local_name),
                            });

                            continue;
                        }

                        // Local name is guaranteed to be an id since there is no `from` clause
                        let local_id = specifier.local.to_id();

                        if matches!(
                            local_id.get_binding().kind(),
                            BindingKind::Import { is_namespace: false }
                        ) {
                            // If we are exporting a non-namespace import then this is actually a
                            // named re-export. Find the corresponding import entry.
                            let import_entry = imports
                                .iter()
                                .find(|entry| entry.local_name == local_name)
                                .unwrap();

                            named_re_exports.push(NamedReExportEntry {
                                module_request: import_entry.module_request,
                                export_name,
                                import_name: import_entry.import_name,
                            });
                        } else {
                            // Otherwise this is a regular local export
                            let slot_index = Self::id_module_slot_index(local_id);

                            local_exports.push(LocalExportEntry {
                                export_name,
                                local_name,
                                slot_index,
                            })
                        }
                    }
                }
                ast::Toplevel::ExportAll(export) => {
                    let module_specifier = self.cx.alloc_wtf8_string(&export.source.value);

                    if let Some(exported_name) = export.exported.as_ref() {
                        // If there is an exported name this is a namespace re-export, which counts
                        // as a named re-export.
                        let export_name = self.alloc_export_name_string(exported_name);

                        named_re_exports.push(NamedReExportEntry {
                            module_request: module_specifier,
                            export_name,
                            import_name: None,
                        });
                    } else {
                        // Otherwise this is a direct re-export
                        direct_re_exports
                            .push(DirectReExportEntry { module_request: module_specifier });
                    }
                }
                _ => {}
            }
        }

        // Flatten set of specifiers into list, preserving insertion order
        let mut module_scope = self.gen_module_scope(program);

        let module = SourceTextModule::new(
            self.cx,
            program_function,
            module_scope,
            &module_specifiers,
            &imports,
            &local_exports,
            &named_re_exports,
            &direct_re_exports,
            program.has_top_level_await,
        );

        // Place the module in the first slot of its module scope
        module_scope.set_heap_item_slot(0, module.get_().as_heap_item());

        module
    }

    /// Create the root module scope for module evaluation. Initialize
    fn gen_module_scope(&mut self, program: &ast::Program) -> Handle<Scope> {
        let ast_node = program.scope.as_ref();
        let vm_node = self.scope_tree.get_vm_node(ast_node.vm_scope_id().unwrap());

        // Create the scope itself
        let names = BytecodeFunctionGenerator::gen_scope_name_strings(self.cx, vm_node);
        let name_flags = BytecodeFunctionGenerator::gen_scope_name_flags(ast_node, self.scope_tree);
        let scope_names = ScopeNames::new(self.cx, ScopeFlags::empty(), &names, &name_flags);
        let mut module_scope = Scope::new_module(self.cx, scope_names, self.realm.global_object());

        // Initialize the exports with boxed values. Imports will be initialized during linking,
        // and all other bindings will be initialized normally during execution.
        for (_, binding) in ast_node.iter_bindings() {
            if binding.is_exported() {
                // We need to initialize with empty if TDZ checks are needed
                let init_value = if binding.needs_tdz_check() {
                    self.cx.empty()
                } else {
                    self.cx.undefined()
                };

                if let VMLocation::ModuleScope { index, .. } = binding.vm_location().unwrap() {
                    // All exported value are boxed, which will eventually be linked to import
                    let boxed_value = BoxedValue::new(self.cx, init_value);
                    module_scope.set_heap_item_slot(index, boxed_value.as_heap_item());
                } else {
                    unreachable!("expected module scope location")
                }
            }
        }

        module_scope
    }

    fn alloc_export_name_string(&mut self, module_name: &ast::ExportName) -> Handle<FlatString> {
        match module_name {
            ast::ExportName::Id(id) => self.cx.alloc_string(&id.name),
            ast::ExportName::String(lit) => self.cx.alloc_wtf8_string(&lit.value),
        }
    }

    /// Look up the slot index of the given binding in the module scope. Should only be called for
    /// import and export bindings that are known to be stored in the module scope.
    fn id_module_slot_index(id: &ast::Identifier) -> usize {
        // Look up the slot index of the import in the module scope
        if let VMLocation::ModuleScope { index, .. } | VMLocation::Scope { index, .. } =
            id.get_binding().vm_location().unwrap()
        {
            index
        } else {
            unreachable!("expected module scope location")
        }
    }

    /// Generate the contents of an eval as a function. Return the function used to execute the eval.
    pub fn generate_from_eval_parse_result(
        cx: Context,
        parse_result: &'a ParseProgramResult,
        realm: Handle<Realm>,
    ) -> EmitResult<Handle<BytecodeFunction>> {
        HandleScope::new(cx, |_| {
            let mut generator =
                Self::new(cx, &parse_result.scope_tree, realm, parse_result.program.source.clone());
            let function = generator.generate_eval(&parse_result.program);

            generator.dump_bytecode_functions();

            function
        })
    }

    fn generate_eval(
        &mut self,
        eval_program: &ast::Program,
    ) -> EmitResult<Handle<BytecodeFunction>> {
        let program_function = self.gen_eval_function(eval_program)?;

        while let Some(pending_function) = self.pending_functions_queue.pop_front() {
            self.gen_enqueued_function(pending_function)?;
        }

        Ok(program_function)
    }

    fn gen_eval_function(
        &mut self,
        eval_program: &ast::Program,
    ) -> EmitResult<Handle<BytecodeFunction>> {
        let mut emit_result = EmitFunctionResult::empty();

        HandleScope::new(self.cx, |_| {
            let mut generator = BytecodeFunctionGenerator::new_for_program(
                self.cx,
                eval_program,
                self.scope_tree,
                "<eval>",
                self.source_file,
                self.realm,
            )?;

            // Allocate statement completion which is initially undefined
            let statement_completion_dest = generator.register_allocator.allocate()?;
            generator.set_statement_completion_dest(statement_completion_dest);

            // Start the eval scope
            let program_scope = eval_program.scope.as_ref();
            generator.gen_scope_start(program_scope, None)?;

            // Store the captured `this` right away if necessary
            generator.gen_store_captured_this(program_scope)?;

            // Heuristic to ignore the use strict directive in common cases. Safe since there must
            // be a directive prologue which can be ignored if there is a use strict directive.
            let toplevels = if eval_program.has_use_strict_directive {
                &eval_program.toplevels[1..]
            } else {
                &eval_program.toplevels
            };

            // Eval function consists of toplevel statements
            for toplevel in toplevels {
                generator.gen_toplevel(eval_program, toplevel)?;
            }

            // Scope end not needed since the eval function returns

            // Return the statement completion from eval
            generator.gen_return(
                /* return_arg */ Some(statement_completion_dest),
                /* derived_constructor_scope */ None,
            )?;
            generator
                .register_allocator
                .release(statement_completion_dest);

            emit_result = generator.finish();

            Ok(())
        })?;

        // Escape emit result into current handle scope immediately, before allocation occurs
        self.escape_emit_function_result(&mut emit_result);
        self.process_generated_function(emit_result.bytecode_function);

        self.enqueue_pending_functions(
            emit_result.bytecode_function,
            emit_result.pending_functions,
        );

        Ok(emit_result.bytecode_function)
    }

    pub fn generate_from_function_constructor_parse_result(
        cx: Context,
        parse_result: &'a ParseFunctionResult,
        realm: Handle<Realm>,
    ) -> EmitResult<Handle<BytecodeFunction>> {
        HandleScope::new(cx, |_| {
            let source = parse_result.source.clone();
            let mut generator = Self::new(cx, &parse_result.scope_tree, realm, source);
            let function = generator.generate_function_constructor(&parse_result.function);

            generator.dump_bytecode_functions();

            function
        })
    }

    fn generate_function_constructor(
        &mut self,
        function: &ast::Function,
    ) -> EmitResult<Handle<BytecodeFunction>> {
        // Create a dummy global scope stack node that will not conflict with any real scope id
        let scope = Rc::new(ScopeStackNode { scope_id: usize::MAX, parent: None });

        let is_constructor = !function.is_async() && !function.is_generator();

        // Generate the dynamic function
        let generator = BytecodeFunctionGenerator::new_for_function(
            self.cx,
            function,
            self.scope_tree,
            scope,
            self.realm,
            None,
            self.source_file,
            /* source_range */ function.loc.to_range(),
            ClassFieldsInitializer::none(),
            is_constructor,
            /* is_class_constructor */ false,
            /* is_base_constructor */ is_constructor,
        )?;

        let emit_result = generator.generate(function)?;
        self.process_generated_function(emit_result.bytecode_function);

        // Generate all pending functions that were discovered while emitting the original function
        self.enqueue_pending_functions(
            emit_result.bytecode_function,
            emit_result.pending_functions,
        );

        while let Some(pending_function) = self.pending_functions_queue.pop_front() {
            self.gen_enqueued_function(pending_function)?;
        }

        Ok(emit_result.bytecode_function)
    }

    fn gen_enqueued_function(&mut self, pending_function: PendingFunction) -> EmitResult<()> {
        let PendingFunction { mut func_node, scope, patch } = pending_function;

        let mut emit_result = EmitFunctionResult::empty();

        HandleScope::new(self.cx, |_| {
            // Special handling if emitting a default constructor
            if let PendingFunctionNode::Constructor {
                node: None,
                name,
                fields,
                is_base,
                source_range,
            } = func_node
            {
                let generator = BytecodeFunctionGenerator::new_for_default_constructor(
                    self.cx,
                    self.scope_tree,
                    scope,
                    self.realm,
                    &name,
                    self.source_file,
                    source_range,
                    fields,
                    /* is_base_constructor */ is_base,
                )?;

                emit_result = generator.generate_default_constructor()?;
            } else if let PendingFunctionNode::ClassFieldsInitializer {
                scope: init_func_scope,
                fields,
                class,
            } = func_node
            {
                let init_func_scope = init_func_scope.as_ref();
                let generator = BytecodeFunctionGenerator::new_for_class_initializer(
                    self.cx,
                    class,
                    self.scope_tree,
                    scope,
                    self.realm,
                    "fieldsInitializer",
                    self.source_file,
                    init_func_scope,
                )?;

                emit_result =
                    generator.generate_class_fields_initializer(fields, init_func_scope)?;
            } else if let PendingFunctionNode::ClassStaticInitializer {
                scope: init_func_scope,
                elements,
                class,
            } = func_node
            {
                let init_func_scope = init_func_scope.as_ref();
                let generator = BytecodeFunctionGenerator::new_for_class_initializer(
                    self.cx,
                    class,
                    self.scope_tree,
                    scope,
                    self.realm,
                    "staticInitializer",
                    self.source_file,
                    init_func_scope,
                )?;

                emit_result =
                    generator.generate_class_static_initializer(elements, init_func_scope)?;
            } else {
                let func_ptr = func_node.ast_ptr();
                let func = func_ptr.as_ref();

                let is_constructor = func_node.is_constructor();
                let is_class_constructor;
                let is_base_constructor;
                let source_range;

                if let PendingFunctionNode::Constructor {
                    is_base,
                    source_range: source_range_,
                    ..
                } = &func_node
                {
                    is_class_constructor = true;
                    is_base_constructor = *is_base;
                    source_range = source_range_.clone();
                } else {
                    is_class_constructor = false;
                    is_base_constructor = is_constructor;
                    source_range = func.loc.to_range();
                }

                let class_fields = match &mut func_node {
                    PendingFunctionNode::Constructor { ref mut fields, .. } => {
                        std::mem::replace(fields, ClassFieldsInitializer::none())
                    }
                    _ => ClassFieldsInitializer::none(),
                };

                let default_name = func_node.default_name();

                // If the generated function is an exported anonymous function
                let anonymous_default_name =
                    if default_name.is_none() && matches!(patch, Patch::Export { .. }) {
                        Some(Wtf8String::from_str("default"))
                    } else {
                        None
                    };

                let default_name = default_name.or(anonymous_default_name.as_ref());

                let generator = BytecodeFunctionGenerator::new_for_function(
                    self.cx,
                    func,
                    self.scope_tree,
                    scope,
                    self.realm,
                    default_name,
                    self.source_file,
                    source_range,
                    class_fields,
                    is_constructor,
                    is_class_constructor,
                    is_base_constructor,
                )?;

                emit_result = generator.generate(func)?;
            }

            Ok(())
        })?;

        // Escape emit result into current handle scope immediately, before allocation occurs
        self.escape_emit_function_result(&mut emit_result);
        self.process_generated_function(emit_result.bytecode_function);

        self.enqueue_pending_functions(
            emit_result.bytecode_function,
            emit_result.pending_functions,
        );

        match patch {
            // Patch function into parent function's constant table
            Patch::ParentFunction { parent_function, constant_index } => {
                let mut parent_constant_table = parent_function.constant_table_ptr().unwrap();
                parent_constant_table.set_constant(
                    constant_index as usize,
                    emit_result.bytecode_function.cast::<Value>().get(),
                );
            }
            // Patch exported function into the module scope
            Patch::Export { slot_index } => {
                // Create closure for the exported function
                let module_scope = self.module.unwrap().module_scope();
                let realm = self.module.unwrap().program_function_ptr().realm();

                let closure = Closure::new_in_realm(
                    self.cx,
                    emit_result.bytecode_function,
                    module_scope,
                    realm,
                )
                .as_object();

                // And place inside boxed value in the module scope
                let mut boxed_value = module_scope.get_module_slot(slot_index);
                boxed_value.set(closure.get_().into());
            }
        }

        Ok(())
    }

    /// Escape an emit result to the parent handle scope, modifying it in place.
    ///
    /// Must be called immediately after the a child handle scope has been destroyed, before any
    /// allocations occur.
    fn escape_emit_function_result(&mut self, emit_result: &mut EmitFunctionResult) {
        // BytecodeFunction must always be escaped into parent handle scope
        emit_result.bytecode_function = emit_result.bytecode_function.escape(self.cx);
    }

    fn enqueue_pending_functions(
        &mut self,
        parent_function: Handle<BytecodeFunction>,
        pending_functions: PendingFunctionNodes,
    ) {
        for (func_node, func_gen_patch, scope) in pending_functions {
            let patch = match func_gen_patch {
                FuncGenPatch::ParentFunction(constant_index) => {
                    Patch::ParentFunction { parent_function, constant_index }
                }
                FuncGenPatch::Export(slot_index) => Patch::Export { slot_index },
            };

            self.pending_functions_queue
                .push_back(PendingFunction { func_node, scope, patch });
        }
    }
}

/// Bytecode generator for a single function.
pub struct BytecodeFunctionGenerator<'a> {
    pub writer: BytecodeWriter,
    cx: Context,
    scope_tree: &'a ScopeTree,
    realm: Handle<Realm>,

    /// A chain of VM scopes that the generator is currently inside. The first node is the innermost
    /// scope.
    scope: Rc<ScopeStackNode>,

    /// The ScopeNames for each scope in this function. Keys are VM scope ids, and values are the
    /// constant index of the ScopeNames in the constant table.
    scope_names_cache: HashMap<usize, GenConstantIndex>,

    /// Optional name of the function, used for the name property.
    name: Option<Wtf8String>,

    /// Source file of the function that is being generated.
    source_file: Handle<SourceFile>,

    /// Start and end position of the function in the source code.
    source_range: Range<Pos>,

    /// Number of blocks currently allocated in the function.
    num_blocks: usize,

    /// Map from block id to the offset of that block in the bytecode. Contains an entry for every
    /// block that has been started in the bycode. Used to calculate backwards jump offsets.
    ///
    /// Note that not every block that has been allocated has actually been started, e.g. the
    /// continue block allocated for labeled non-loop statements.
    block_offsets: HashMap<BlockId, usize>,

    /// Map from block id to all unresolved forward jumps that target that block. Only contains
    /// entries for blocks that have not yet been started.
    unresolved_forward_jumps: HashMap<BlockId, Vec<ForwardJumpInfo>>,

    /// Stack of jump statement (break and continue) targets that the generator is currently inside.
    /// The top of the stack is the innermost jump statement target.
    jump_statement_target_stack: Vec<JumpStatementTarget>,

    /// Stack of finally scopes that the generator is currently inside. The top of the stack is the
    /// innermost finally scope.
    finally_scopes: Vec<FinallyScope>,

    /// Number of toplevel parameters to this function, not counting the rest parameter.
    num_parameters: u32,

    /// Value of the `length` property of this function. Equal to the number of toplevel parameters
    /// before the first parameter with a default value or rest parameter.
    function_length: u32,

    /// Whether this function is in strict mode.
    is_strict: bool,

    /// Whether this function is a constructor.
    is_constructor: bool,

    /// Whether this function is a class constructor.
    is_class_constructor: bool,

    /// Whether this function is a base constructor.
    is_base_constructor: bool,

    /// Index of the register in which to place the new.target value for this function, if one is
    /// needed.
    new_target_index: Option<u32>,

    /// Whether the function is async (either a regular async function or an async generator).
    is_async: bool,

    /// Index of the register in which to place the generator for this function, if function is a
    /// generator function.
    generator_index: Option<u32>,

    /// Index of the register in which to place the promise for this function, if function is an
    /// async function (but not an async generator).
    promise_index: Option<u32>,

    /// Register in which to place the completion value of each statement. Only need to generate
    /// completion values when this is set.
    statement_completion_dest: Option<GenRegister>,

    /// Whether the current expression context has an assignment expression, meaning that we must
    /// emit conservatively worse code to avoid assignment hazards.
    has_assign_expr: bool,

    /// Class field initializer that must be called by this function. Only set if this is a
    /// constructor.
    class_fields: ClassFieldsInitializer,

    constant_table_builder: ConstantTableBuilder,
    register_allocator: TemporaryRegisterAllocator,
    exception_handler_builder: ExceptionHandlersBuilder,

    /// Queue of functions that still need to be generated.
    pending_functions_queue: PendingFunctionNodes,
}

impl<'a> BytecodeFunctionGenerator<'a> {
    fn new(
        cx: Context,
        scope_tree: &'a ScopeTree,
        scope: Rc<ScopeStackNode>,
        realm: Handle<Realm>,
        name: Option<Wtf8String>,
        source_file: Handle<SourceFile>,
        source_range: Range<Pos>,
        class_fields: ClassFieldsInitializer,
        num_parameters: u32,
        function_length: u32,
        num_local_registers: u32,
        is_strict: bool,
        is_constructor: bool,
        is_class_constructor: bool,
        is_base_constructor: bool,
        is_async: bool,
    ) -> Self {
        Self {
            writer: BytecodeWriter::new(),
            cx,
            scope_tree,
            realm,
            scope,
            scope_names_cache: HashMap::new(),
            name,
            source_file,
            source_range,
            num_blocks: 0,
            block_offsets: HashMap::new(),
            unresolved_forward_jumps: HashMap::new(),
            jump_statement_target_stack: vec![],
            finally_scopes: vec![],
            num_parameters,
            function_length,
            is_strict,
            is_constructor,
            is_class_constructor,
            is_base_constructor,
            is_async,
            new_target_index: None,
            generator_index: None,
            promise_index: None,
            statement_completion_dest: None,
            has_assign_expr: false,
            class_fields,
            constant_table_builder: ConstantTableBuilder::new(),
            register_allocator: TemporaryRegisterAllocator::new(num_local_registers),
            exception_handler_builder: ExceptionHandlersBuilder::new(),
            pending_functions_queue: vec![],
        }
    }

    fn new_for_function(
        cx: Context,
        func: &ast::Function,
        scope_tree: &'a ScopeTree,
        scope: Rc<ScopeStackNode>,
        realm: Handle<Realm>,
        default_name: Option<&Wtf8String>,
        source_file: Handle<SourceFile>,
        source_range: Range<Pos>,
        class_fields: ClassFieldsInitializer,
        is_constructor: bool,
        is_class_constructor: bool,
        is_base_constructor: bool,
    ) -> EmitResult<Self> {
        // Stored number of parameters does not count the rest parameter
        let num_parameters = if let Some(ast::FunctionParam::Rest { .. }) = func.params.last() {
            func.params.len() - 1
        } else {
            func.params.len()
        };

        // Function length only counts parameters before the first default value or rest parameter
        let mut function_length = 0;
        for param in &func.params {
            match param {
                ast::FunctionParam::Rest { .. }
                | ast::FunctionParam::Pattern { pattern: ast::Pattern::Assign(_), .. } => break,
                _ => {}
            }

            function_length += 1;
        }

        // Number of local registers was determined while creating the VM scope tree
        let num_local_registers = func.scope.as_ref().num_local_registers();

        // Validate that the number of arguments and local registers are within the limits of the
        // bytecode format.
        if num_parameters > GenRegister::MAX_ARGUMENT_INDEX {
            return Err(EmitError::TooManyFunctionParameters);
        }

        if num_local_registers > GenRegister::MAX_LOCAL_INDEX {
            return Err(EmitError::TooManyRegisters);
        }

        let name = func
            .id
            .as_ref()
            .map(|id| Wtf8String::from_str(&id.name))
            .or_else(|| default_name.cloned());

        Ok(Self::new(
            cx,
            scope_tree,
            scope,
            realm,
            name,
            source_file,
            source_range,
            class_fields,
            num_parameters as u32,
            function_length as u32,
            num_local_registers as u32,
            func.is_strict_mode(),
            is_constructor,
            is_class_constructor,
            is_base_constructor,
            func.is_async(),
        ))
    }

    fn new_for_program(
        cx: Context,
        program: &ast::Program,
        scope_tree: &'a ScopeTree,
        name: &str,
        source_file: Handle<SourceFile>,
        realm: Handle<Realm>,
    ) -> EmitResult<Self> {
        // Number of local registers was determined while creating the VM scope tree
        let num_local_registers = program.scope.as_ref().num_local_registers();

        // Validate that the number of local registers is within the limits of the bytecode format
        if num_local_registers > GenRegister::MAX_LOCAL_INDEX {
            return Err(EmitError::TooManyRegisters);
        }

        // Create a dummy global scope stack node that will not conflict with any real scope id
        let scope = Rc::new(ScopeStackNode { scope_id: usize::MAX, parent: None });

        Ok(Self::new(
            cx,
            scope_tree,
            scope,
            realm,
            Some(Wtf8String::from_str(name)),
            source_file,
            program.loc.to_range(),
            ClassFieldsInitializer::none(),
            /* num_parameters */ 0,
            /* function_length */ 0,
            num_local_registers as u32,
            program.is_strict_mode,
            /* is_constructor */ false,
            /* is_class_constructor */ false,
            /* is_base_constructor */ false,
            /* async */ program.has_top_level_await,
        ))
    }

    fn new_for_default_constructor(
        cx: Context,
        scope_tree: &'a ScopeTree,
        scope: Rc<ScopeStackNode>,
        realm: Handle<Realm>,
        name: &Wtf8String,
        source_file: Handle<SourceFile>,
        source_range: Range<Pos>,
        class_fields: ClassFieldsInitializer,
        is_base_constructor: bool,
    ) -> EmitResult<Self> {
        // First local register used for new target in derived constructors
        let num_local_registers = if is_base_constructor { 0 } else { 1 };

        let mut generator = Self::new(
            cx,
            scope_tree,
            scope,
            realm,
            Some(name.clone()),
            source_file,
            source_range,
            class_fields,
            /* num_parameters */ 0,
            /* function_length */ 0,
            num_local_registers,
            /* is_strict_mode */ true,
            /* is_constructor */ true,
            /* is_class_constructor */ true,
            is_base_constructor,
            /* is_async */ false,
        );

        if !is_base_constructor {
            generator.new_target_index = Some(0);
        }

        Ok(generator)
    }

    fn new_for_class_initializer(
        cx: Context,
        class: AstPtr<ast::Class>,
        scope_tree: &'a ScopeTree,
        scope: Rc<ScopeStackNode>,
        realm: Handle<Realm>,
        name: &str,
        source_file: Handle<SourceFile>,
        init_func_scope: &AstScopeNode,
    ) -> EmitResult<Self> {
        let num_local_registers = init_func_scope.num_local_registers();

        // Validate that the number of local registers is within the limits of the bytecode format.
        if num_local_registers > GenRegister::MAX_ARGUMENT_INDEX {
            return Err(EmitError::TooManyRegisters);
        }

        // Use entire class's source range for the initializer function
        let source_range = class.as_ref().loc.to_range();

        Ok(Self::new(
            cx,
            scope_tree,
            scope,
            realm,
            Some(Wtf8String::from_str(name)),
            source_file,
            source_range,
            ClassFieldsInitializer::none(),
            /* num_parameters */ 0,
            /* function_length */ 0,
            num_local_registers as u32,
            /* is_strict_mode */ true,
            /* is_constructor */ false,
            /* is_class_constructor */ false,
            /* is_base_constructor */ false,
            /* is_async */ false,
        ))
    }

    fn enter_has_assign_expr_context(&mut self, has_assign_expr: bool) {
        self.has_assign_expr = has_assign_expr;
    }

    fn exit_has_assign_expr_context(&mut self) {
        self.has_assign_expr = false;
    }

    fn set_statement_completion_dest(&mut self, dest: GenRegister) {
        self.statement_completion_dest = Some(dest);
    }

    fn is_derived_constructor(&self) -> bool {
        self.is_constructor && !self.is_base_constructor
    }

    fn is_async(&self) -> bool {
        self.is_async
    }

    fn is_generator(&self) -> bool {
        self.generator_index.is_some()
    }

    fn new_block(&mut self) -> BlockId {
        let next_block_id = self.num_blocks;
        self.num_blocks += 1;
        next_block_id
    }

    fn start_block(&mut self, block_id: BlockId) {
        let current_offset = self.writer.current_offset();
        self.block_offsets.insert(block_id, current_offset);

        if let Some(unresolved_forward_jumps) = self.unresolved_forward_jumps.remove(&block_id) {
            for jump_info in unresolved_forward_jumps {
                self.patch_forward_jump(jump_info, block_id);
            }
        }
    }

    /// Patch the jump specified by `jump_info` to jump to `target_block_id`. Only called when the
    /// target block has already been started.
    fn patch_forward_jump(&mut self, jump_info: ForwardJumpInfo, target_block_id: BlockId) {
        let target_offset = self.block_offsets[&target_block_id];

        // Offset is from the start of the jump instruction (including prefixes) to the start of the
        // target block.
        let relative_offset = (target_offset - jump_info.jump_offset) as isize;
        let min_width_needed = min_width_for_signed(relative_offset);

        let jump_instr_info = self
            .writer
            .decode_width_and_opcode_at_index(jump_info.jump_offset);

        // Determine if the relative offset can fit inline into the instruction. Be sure to check if
        // the offset can fit in an extra wide operand or if must become a constant jump.
        if min_width_needed <= jump_instr_info.width
            && GenSInt::try_from_signed(relative_offset).is_some()
        {
            // Offset fits inline, remove constant reservation and write inline into instruction
            self.constant_table_builder
                .remove_reserved(jump_info.constant_table_width);

            self.set_jump_offset_operand(&jump_instr_info, relative_offset as usize)
        } else {
            // Offset does not fit inline, so this must become a constant jump
            let constant_index = self
                .constant_table_builder
                .commit_reserved_bytecode_offset(jump_info.constant_table_width, relative_offset);

            self.set_jump_offset_operand(&jump_instr_info, constant_index as usize);

            // Convert to the equivalent constant jump instruction. Each constant jump opcode is
            // exactly one above it's corresponding immediate jump instruction.
            self.writer
                .set_u8(jump_instr_info.opcode_index, jump_instr_info.opcode as u8 + 1)
        }
    }

    fn set_jump_offset_operand(&mut self, jump_instr_info: &DecodeInfo, offset: usize) {
        // Unconditional jumps store the offset in the first operand, while all conditional jumps
        // store the offset in the second operand.
        let operand_index = if jump_instr_info.opcode == OpCode::Jump {
            0
        } else {
            1
        };

        // Determine the offset of the operand in the bytecode
        let operand_offset =
            jump_instr_info.opcode_index + 1 + (operand_index * jump_instr_info.width.num_bytes());

        match jump_instr_info.width {
            WidthEnum::Narrow => {
                self.writer.set_u8(operand_offset, offset as u8);
            }
            WidthEnum::Wide => {
                self.writer.set_u16(operand_offset, offset as u16);
            }
            WidthEnum::ExtraWide => {
                self.writer.set_u32(operand_offset, offset as u32);
            }
        }
    }

    /// Return the operand that is needed to perform a jump to the given target block. Returns an
    /// inline operand when possible, otherwise returns a constant table index. Sets up forward
    /// jumps to be patched later.
    ///
    /// Must be called when the writer is at the start of a new jump instruction.
    fn jump_target_operand(&mut self, target_block: BlockId) -> EmitResult<JumpOperand> {
        let jump_offset = self.writer.current_offset();

        if let Some(target_block_offset) = self.block_offsets.get(&target_block) {
            // Backwards jump - target block offset is already known
            let relative_offset = *target_block_offset as isize - jump_offset as isize;

            // If the offset fits into an operand use it directly, otherwise store in constant table
            if let Some(operand) = GenSInt::try_from_signed(relative_offset) {
                Ok(JumpOperand::RelativeOffset(operand))
            } else {
                let constant_index = self
                    .constant_table_builder
                    .add_bytecode_offset(relative_offset)?;
                Ok(JumpOperand::ConstantIndex(ConstantIndex::new(constant_index)))
            }
        } else {
            // Forwards jump - target block offset is not yet known so reserve an entry in the
            // constant table and patch as either an inline or constant jump later.
            let constant_table_width = self.constant_table_builder.reserve_entry()?;

            let forward_jump_info = ForwardJumpInfo { jump_offset, constant_table_width };
            self.unresolved_forward_jumps
                .entry(target_block)
                .or_default()
                .push(forward_jump_info);

            // Create a dummy operand that requires the specified width, which will be patched later
            let dummy_offset = match constant_table_width {
                WidthEnum::Narrow => Narrow::SIGNED_MIN as i32,
                WidthEnum::Wide => Wide::SIGNED_MIN as i32,
                WidthEnum::ExtraWide => ExtraWide::SIGNED_MIN as i32,
            };

            Ok(JumpOperand::RelativeOffset(SInt::new(dummy_offset)))
        }
    }

    fn write_jump_instruction(&mut self, target_block: BlockId) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => {
                self.writer.jump_instruction(relative_offset)
            }
            JumpOperand::ConstantIndex(constant_index) => {
                self.writer.jump_constant_instruction(constant_index)
            }
        }

        Ok(())
    }

    fn write_jump_true_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_true_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_true_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_to_boolean_true_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_to_boolean_true_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_to_boolean_true_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_true_for_expression(
        &mut self,
        condition_expr: &ast::Expression,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        if Self::evaluates_to_boolean(condition_expr) {
            self.write_jump_true_instruction(condition, target_block)
        } else {
            self.write_jump_to_boolean_true_instruction(condition, target_block)
        }
    }

    fn write_jump_false_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_false_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_false_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_to_boolean_false_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_to_boolean_false_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_to_boolean_false_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_not_undefined_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_not_undefined_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_not_undefined_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_nullish_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_nullish_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_nullish_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_not_nullish_instruction(
        &mut self,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        match self.jump_target_operand(target_block)? {
            JumpOperand::RelativeOffset(relative_offset) => self
                .writer
                .jump_not_nullish_instruction(condition, relative_offset),
            JumpOperand::ConstantIndex(constant_index) => self
                .writer
                .jump_not_nullish_constant_instruction(condition, constant_index),
        }

        Ok(())
    }

    fn write_jump_false_for_expression(
        &mut self,
        condition_expr: &ast::Expression,
        condition: GenRegister,
        target_block: BlockId,
    ) -> EmitResult<()> {
        if Self::evaluates_to_boolean(condition_expr) {
            self.write_jump_false_instruction(condition, target_block)
        } else {
            self.write_jump_to_boolean_false_instruction(condition, target_block)
        }
    }

    fn write_mov_instruction(&mut self, dest: GenRegister, src: GenRegister) {
        // Prevent no-op moves
        if src == dest {
            return;
        }

        self.writer.mov_instruction(dest, src);
    }

    fn add_string_constant(&mut self, str: &str) -> EmitResult<GenConstantIndex> {
        let string = InternedStrings::get_str(self.cx, str).as_flat();
        let constant_index = self.constant_table_builder.add_string(string)?;
        Ok(ConstantIndex::new(constant_index))
    }

    fn add_wtf8_string_constant(&mut self, str: &Wtf8String) -> EmitResult<GenConstantIndex> {
        let string = InternedStrings::get_wtf8_str(self.cx, str).as_flat();
        let constant_index = self.constant_table_builder.add_string(string)?;
        Ok(ConstantIndex::new(constant_index))
    }

    /// Enqueue a patchable function to be generated later, returning the constant index of the slot
    /// where the function will be patched in.
    fn enqueue_function_to_generate(
        &mut self,
        function: PendingFunctionNode,
    ) -> EmitResult<ConstantTableIndex> {
        let constant_index = self
            .constant_table_builder
            .add_heap_object(Handle::dangling())?;
        self.pending_functions_queue.push((
            function,
            FuncGenPatch::ParentFunction(constant_index),
            self.scope.clone(),
        ));

        Ok(constant_index)
    }

    /// Enqueue an exported function which will be generated later and patched into the module scope
    /// at the provided slot.
    fn enqueue_export_function_to_generate(
        &mut self,
        function: PendingFunctionNode,
        slot_index: usize,
    ) {
        self.pending_functions_queue.push((
            function,
            FuncGenPatch::Export(slot_index),
            self.scope.clone(),
        ));
    }

    /// Generate the bytecode for a function.
    fn generate(mut self, func: &ast::Function) -> EmitResult<EmitFunctionResult> {
        // Base constructors initialize class fields immediately
        if !self.is_derived_constructor() {
            self.gen_initialize_class_fields(Register::this())?;
        }

        // Async functions reserve a register for the promise object
        let mut promise_reg = None;
        if func.is_async() && !func.is_generator() {
            let register = self.register_allocator.allocate()?;
            self.promise_index = Some(register.local_index() as u32);
            promise_reg = Some(register);

            // Create the promise object that will be returned from the async function
            self.writer.new_promise_instruction(register);
        }

        // Generator functions reserve a register for the generator object
        let mut generator_reg = None;
        if func.is_generator() {
            let register = self.register_allocator.allocate()?;
            self.generator_index = Some(register.local_index() as u32);
            generator_reg = Some(register);
        }

        let func_scope = func.scope.as_ref();

        // Set up the new.target if necessary
        let new_target_to_store = if func.is_new_target_needed() {
            self.gen_new_target_to_store(func_scope)?
        } else {
            None
        };

        let mut func_scope_flags = ScopeFlags::empty();

        // Mark the function scope if it is a scope only for function parameters, separate from the
        // function body.
        if let ast::FunctionBody::Block(body) = func.body.as_ref() {
            if body.scope.is_some() {
                func_scope_flags |= ScopeFlags::IS_FUNCTION_PARAMETERS_SCOPE;
            }
        }

        // Entire function parameters and body are in their own scope.
        self.gen_push_scope(func_scope, Some(func_scope_flags))?;
        self.gen_init_tdz_for_scope(func_scope)?;

        // Store the function itself in scope if this is a named function expression
        if let Some(id) = func.id.as_ref() {
            if func_scope.kind().is_function_expression() {
                // Check that binding still has a function name kind. Otherwise this binding was
                // overwritten and the function name does not need to be stored.
                let binding = id.get_binding();
                if binding.kind().is_function_expression_name() {
                    self.gen_store_binding(
                        &id.name,
                        binding,
                        Register::closure(),
                        StoreFlags::INITIALIZATION,
                    )?;
                }
            }
        }

        // In derived constructors `this` starts unintialized, here represented by the empty value
        if self.is_derived_constructor() {
            self.writer.load_empty_instruction(Register::this());
        }

        // Store the captured `this` right away if necessary
        if !func.is_arrow() {
            self.gen_store_captured_this(func_scope)?;
        }

        // Store the captured new.target if necessary
        self.gen_store_new_target(new_target_to_store)?;

        // Set the closure itself as the derived constructor if necessary
        if func_scope.has_binding(DERIVED_CONSTRUCTOR_BINDING_NAME) {
            self.gen_store_binding(
                DERIVED_CONSTRUCTOR_BINDING_NAME,
                func_scope.get_binding(DERIVED_CONSTRUCTOR_BINDING_NAME),
                Register::closure(),
                StoreFlags::INITIALIZATION,
            )?;
        }

        // Generate the arguments object if necessary. For mapped arguments object this also places
        // the function's arguments into the function's scope.
        if func.is_arguments_object_needed() {
            let store_flags = StoreFlags::INITIALIZATION;

            let arguments_binding = func_scope.get_binding("arguments");
            let arguments_dest = self.expr_dest_for_binding(arguments_binding, store_flags);
            let arguments_object = self.allocate_destination(arguments_dest)?;

            if func.needs_mapped_arguments_object() {
                // All arguments must be placed in the function's scope
                for i in 0..func.params.len() {
                    self.gen_store_scope_binding(self.scope.scope_id, i, Register::argument(i))?;
                }

                self.writer
                    .new_mapped_arguments_instruction(arguments_object);
            } else {
                self.writer
                    .new_unmapped_arguments_instruction(arguments_object);
            }

            self.gen_store_binding("arguments", arguments_binding, arguments_object, store_flags)?;
            self.register_allocator.release(arguments_object);
        }

        // Async functions must wrap the parameters and function body in an exception handler
        let async_body_handler_start = if promise_reg.is_some() {
            Some(self.writer.current_offset())
        } else {
            None
        };

        // Generate function parameters including destructuring, default value evaluation, and
        // captured parameters.
        if !func.needs_mapped_arguments_object() {
            for (i, param) in func.params.iter().enumerate() {
                // Each parameter is in its own "has assign expression" context
                self.enter_has_assign_expr_context(param.has_assign_expr());

                match param {
                    // Emit pattern destructuring
                    ast::FunctionParam::Pattern { pattern, .. } => {
                        // Check if this is a simple id pattern which is shadowed by a later
                        // parameter. If so do not perform destructuring as this would overwrite the
                        // later parameter.
                        let mut is_shadowed = false;
                        if let ast::Pattern::Id(id) = pattern {
                            if let VMLocation::Argument(index) =
                                id.get_binding().vm_location().unwrap()
                            {
                                is_shadowed = index != i;
                            }
                        }

                        if !is_shadowed {
                            let argument = Register::argument(i);
                            self.gen_store_to_pattern(
                                pattern,
                                argument,
                                StoreFlags::INITIALIZATION,
                            )?;
                        }
                    }
                    // Create the rest parameter then destructure
                    ast::FunctionParam::Rest { rest: param, .. } => {
                        let store_flags = StoreFlags::INITIALIZATION;
                        let rest_dest = self
                            .expr_dest_for_destructuring_assignment(&param.argument, store_flags);
                        let rest = self.allocate_destination(rest_dest)?;

                        self.writer.rest_parameter_instruction(rest);
                        self.gen_store_to_pattern(&param.argument, rest, store_flags)?;
                        self.register_allocator.release(rest);
                    }
                }

                self.exit_has_assign_expr_context();
            }
        }

        // Create closures for all functions in the function body scope. This must occur after
        // creating arguments object, so that the a function named "arguments" will overwrite the
        // arguments object.
        self.gen_scope_functions(func_scope)?;

        match func.body.as_ref() {
            ast::FunctionBody::Block(block_body) => {
                // Function body may be the start of a new scope
                if let Some(body_scope) = block_body.scope {
                    self.gen_scope_start(body_scope.as_ref(), None)?;
                }

                // Heuristic to ignore the use strict directive in common cases. Safe since there
                // be a directive prologue which can be ignored if there is a use strict directive.
                let body = if func.has_use_strict_directive() {
                    &block_body.body[1..]
                } else {
                    &block_body.body
                };

                // If function is a generator then run GeneratorStart once body is ready to be
                // evaluated.
                if let Some(generator_reg) = generator_reg {
                    self.writer.generator_start_instruction(generator_reg);
                }

                // Continue to the function body
                let body_completion = self.gen_statement_list(body)?;

                // Scope end not needed since the function immediately returns

                let mut derived_constructor_scope = None;
                if self.is_derived_constructor() {
                    derived_constructor_scope = Some(func.scope);
                }

                // If the body continues, meaning there was no return statement (or throw, break,
                // etc.) then implicitly return undefined.
                if !body_completion.is_abrupt() {
                    self.gen_return(/* return_arg */ None, derived_constructor_scope)?;
                }
            }
            ast::FunctionBody::Expression(expr_body) => {
                let return_value_reg = self.gen_outer_expression(expr_body)?;

                // Scope end not needed since the function immediately returns

                // Derived constructors cannot have an expression body
                self.gen_return(
                    /* return_arg */ Some(return_value_reg),
                    /* derived_constructor_scope */ None,
                )?;
                self.register_allocator.release(return_value_reg);
            }
        }

        // Finally emit the async body exception handler if it exists
        if let Some(body_handler_start) = async_body_handler_start {
            let body_handler_end = self.writer.current_offset();
            self.gen_async_body_exception_handler(body_handler_start, body_handler_end)?;

            // Then return the rejected promise
            self.writer.ret_instruction(promise_reg.unwrap());
        }

        if let Some(generator_reg) = generator_reg {
            self.register_allocator.release(generator_reg);
        }

        if let Some(promise_reg) = promise_reg {
            self.register_allocator.release(promise_reg);
        }

        Ok(self.finish())
    }

    /// Finish generating the bytecode for a function. Returns the bytecode function and the
    /// queue of AST node functions from the body that need to be generated.
    fn finish(self) -> EmitFunctionResult {
        debug_assert!(self.block_offsets.len() == self.num_blocks);
        debug_assert!(self.unresolved_forward_jumps.is_empty());
        debug_assert!(self.register_allocator.is_empty());

        let constant_table = self.constant_table_builder.finish(self.cx);
        let exception_handlers = self.exception_handler_builder.finish(self.cx);

        let is_async = self.is_async();

        let num_registers = self.register_allocator.max_allocated();
        let name = self
            .name
            .as_ref()
            .map(|name| InternedStrings::get_wtf8_str(self.cx, name));
        let bytecode = self.writer.finish();

        let bytecode_function = BytecodeFunction::new(
            self.cx,
            bytecode,
            constant_table,
            exception_handlers,
            self.realm,
            num_registers,
            self.num_parameters,
            self.function_length,
            self.is_strict,
            self.is_constructor,
            self.is_class_constructor,
            self.is_base_constructor,
            is_async,
            self.new_target_index,
            self.generator_index,
            name,
            Some(self.source_file),
            self.source_range,
        );

        EmitFunctionResult {
            bytecode_function,
            pending_functions: self.pending_functions_queue,
        }
    }

    /// Generate the bytecode for a default constructor.
    fn generate_default_constructor(mut self) -> EmitResult<EmitFunctionResult> {
        // Call super constructor if necessary
        if self.is_derived_constructor() {
            self.writer.default_super_call_instruction();
        }

        // Initialize fields if any exist, defined onto the `this` value
        self.gen_initialize_class_fields(Register::this())?;

        // Return undefined for a base constructor and `this` for a derived constructor. No default
        // constructor scope is needed since it is only needed if `this` is captured.
        self.gen_return(/* return_arg */ None, /* default_constructor_scope */ None)?;

        Ok(self.finish())
    }

    /// Generate the bytecode for a function that initializes the instances fields of a class.
    fn generate_class_fields_initializer(
        mut self,
        class_fields: Vec<ClassField>,
        init_func_scope: &AstScopeNode,
    ) -> EmitResult<EmitFunctionResult> {
        self.gen_class_initializer_start(init_func_scope)?;

        // Private methods and accessors are initialized first
        for field in &class_fields {
            if matches!(field, ClassField::PrivateMethodOrAccessor { .. }) {
                self.gen_class_field(field, Register::this())?;
            }
        }

        // All other fields are initialized second
        for field in class_fields {
            if !matches!(field, ClassField::PrivateMethodOrAccessor { .. }) {
                self.gen_class_field(&field, Register::this())?;
            }
        }

        self.gen_class_initializer_end(init_func_scope)?;

        Ok(self.finish())
    }

    /// Generate the bytecode for a function that initializes the static elements of a class,
    /// including static fields and static block initializers.
    fn generate_class_static_initializer(
        mut self,
        static_elements: Vec<ClassStaticElement>,
        init_func_scope: &AstScopeNode,
    ) -> EmitResult<EmitFunctionResult> {
        self.gen_class_initializer_start(init_func_scope)?;

        // Evaluate the static elements in order
        for static_element in static_elements {
            match static_element {
                ClassStaticElement::Field(field) => {
                    // Static fields are defined on the constructor itself, which is placed in the
                    // `this` register.
                    self.gen_class_field(&field, Register::this())?;
                }
                ClassStaticElement::Initializer(static_initializer) => {
                    // Static initializers are each in their own block scope
                    let block_scope = static_initializer.as_ref().value.scope.as_ref();
                    self.gen_scope_start(block_scope, None)?;

                    let statements = &static_initializer.as_ref().value.body.unwrap_block().body;
                    self.gen_statement_list(statements)?;

                    self.gen_scope_end(block_scope);
                }
            }
        }

        self.gen_class_initializer_end(init_func_scope)?;

        Ok(self.finish())
    }

    fn gen_class_initializer_start(&mut self, init_func_scope: &AstScopeNode) -> EmitResult<()> {
        self.gen_scope_start(init_func_scope, None)?;

        // Store the captured `this` if necessary
        self.gen_store_captured_this(init_func_scope)?;

        // Store the captured `new.target` if necessary
        let new_target_storage = self.gen_new_target_to_store(init_func_scope)?;
        self.gen_store_new_target(new_target_storage)?;

        Ok(())
    }

    fn gen_class_initializer_end(&mut self, init_func_scope: &AstScopeNode) -> EmitResult<()> {
        self.gen_scope_end(init_func_scope);

        self.gen_return(/* return_arg */ None, /* default_constructor_scope */ None)?;

        Ok(())
    }

    fn gen_program_body(&mut self, program: &ast::Program) -> EmitResult<()> {
        let scope = program.scope.as_ref();

        // Modules with top level await create an async function. Promise is passed as the first
        // argument to the async module function, and will be stored in a register whose index is
        // marked as the promise index.
        let mut promise_reg = None;
        if program.has_top_level_await {
            let register = self.register_allocator.allocate()?;
            self.promise_index = Some(register.local_index() as u32);
            promise_reg = Some(register);

            self.write_mov_instruction(register, Register::argument(0));
        }

        // Start the program's global scope
        self.gen_start_global_scope(scope)?;

        // Modules need to initialize the TDZ for the module scope. Scripts instead have TDZ
        // initialized during GlobalDeclarationInstantiation.
        if program.kind == ast::ProgramKind::Module {
            self.gen_init_tdz_for_scope(scope)?;
        }

        // Heuristic to ignore the use strict directive in common cases. Safe since there must
        // be a directive prologue which can be ignored if there is a use strict directive.
        let toplevels = if program.has_use_strict_directive {
            &program.toplevels[1..]
        } else {
            &program.toplevels
        };

        // Async module functions must wrap execution of toplevels in an exception handler
        let async_body_handler_start = if promise_reg.is_some() {
            Some(self.writer.current_offset())
        } else {
            None
        };

        // Program function consists of toplevel statements
        for toplevel in toplevels {
            self.gen_toplevel(program, toplevel)?;
        }

        // Scope end not needed since the function returns

        // Return undefined at end of program function
        self.gen_return(/* return_arg */ None, /* derived_constructor_scope */ None)?;

        // Write the exception handler if this is an async module function
        if let Some(body_handler_start) = async_body_handler_start {
            let body_handler_end = self.writer.current_offset();
            self.gen_async_body_exception_handler(body_handler_start, body_handler_end)?;
            self.register_allocator.release(promise_reg.unwrap());

            // Then return undefined
            let return_arg = self.register_allocator.allocate()?;
            self.writer.load_undefined_instruction(return_arg);
            self.writer.ret_instruction(return_arg);
            self.register_allocator.release(return_arg);
        }

        Ok(())
    }

    fn gen_async_body_exception_handler(&mut self, start: usize, end: usize) -> EmitResult<()> {
        let mut handler = ExceptionHandlerBuilder::new(start, end);
        handler.handler = self.writer.current_offset();

        // Reject the current promise with the current error
        let error = self.register_allocator.allocate()?;
        handler.error_register = Some(error);
        self.register_allocator.release(error);

        let promise = Register::local(self.promise_index.unwrap() as usize);
        self.writer.reject_promise_instruction(promise, error);
        self.exception_handler_builder.add(handler);

        Ok(())
    }

    fn gen_toplevel(&mut self, program: &ast::Program, toplevel: &ast::Toplevel) -> EmitResult<()> {
        match toplevel {
            ast::Toplevel::Statement(stmt) => {
                // Ignore completion of toplevel statements, generate later toplevels even if abrupt
                let _ = self.gen_statement(stmt)?;
            }
            ast::Toplevel::Import(_) => {
                // No evaluation action is needed for import declarations. These are initialized
                // during module linking.
            }
            ast::Toplevel::ExportNamed(export) => {
                // Named exports should evaluate their declaration normally, which will store to
                // the exported module binding.
                if let Some(declaration) = export.declaration.as_ref() {
                    self.gen_statement(declaration)?;
                }
            }
            ast::Toplevel::ExportDefault(export) => {
                self.gen_export_default_declaration(program, export)?
            }
            ast::Toplevel::ExportAll(_) => {
                // No evaluation action is needed for export all declarations. These are resolved
                // during linking phase.
            }
        }

        Ok(())
    }

    fn gen_statement(&mut self, stmt: &ast::Statement) -> EmitResult<StmtCompletion> {
        // Every statement is generated with its own temporary register scope. Check that the number
        // of registers allocated before and after has not changed, meaning all registers in the
        // statement were released.
        let num_allocated_before = self.register_allocator.num_allocated();

        let result = match stmt {
            // Function declarations are handled when starting the scope
            ast::Statement::FuncDecl(_) => Ok(StmtCompletion::Normal),
            ast::Statement::VarDecl(var_decl) => self.gen_variable_declaration(var_decl),
            ast::Statement::ClassDecl(class_decl) => self.gen_class_declaration(class_decl),
            ast::Statement::Expr(stmt) => self.gen_expression_statement(stmt),
            ast::Statement::Block(stmt) => self.gen_block_statement(stmt),
            ast::Statement::If(stmt) => self.gen_if_statement(stmt),
            ast::Statement::Switch(stmt) => self.gen_switch_statement(stmt),
            ast::Statement::For(stmt) => self.gen_for_statement(stmt, None),
            ast::Statement::ForEach(stmt) => match stmt.kind {
                ast::ForEachKind::In => self.gen_for_in_statement(stmt, None),
                ast::ForEachKind::Of => self.gen_for_of_statement(stmt, None),
            },
            ast::Statement::While(stmt) => self.gen_while_statement(stmt, None),
            ast::Statement::DoWhile(stmt) => self.gen_do_while_statement(stmt, None),
            ast::Statement::With(stmt) => self.gen_with_statement(stmt),
            ast::Statement::Try(stmt) => self.gen_try_statement(stmt),
            ast::Statement::Throw(stmt) => self.gen_throw_statement(stmt),
            ast::Statement::Return(stmt) => self.gen_return_statement(stmt),
            ast::Statement::Break(stmt) => self.gen_break_statement(stmt),
            ast::Statement::Continue(stmt) => self.gen_continue_statement(stmt),
            ast::Statement::Labeled(stmt) => self.gen_labeled_statement(stmt),
            // Intentionally ignored as there is no runtime effect
            ast::Statement::Empty(_) | ast::Statement::Debugger(_) => Ok(StmtCompletion::Normal),
        };

        debug_assert!(num_allocated_before == self.register_allocator.num_allocated());

        result
    }

    fn gen_expression_with_dest(
        &mut self,
        expr: &ast::Expression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        match expr {
            ast::Expression::Id(expr) => self.gen_load_identifier(expr, dest),
            ast::Expression::Null(_) => self.gen_null_literal_expression(dest),
            ast::Expression::Boolean(expr) => self.gen_boolean_literal_expression(expr, dest),
            ast::Expression::Number(expr) => self.gen_number_literal(expr.value, dest),
            ast::Expression::String(expr) => self.gen_string_literal_expression(expr, dest),
            ast::Expression::BigInt(expr) => self.gen_bigint_literal_expression(expr, dest),
            ast::Expression::RegExp(expr) => self.gen_regexp_literal_expression(expr, dest),
            ast::Expression::This(expr) => self.gen_this_expression(expr, dest),
            ast::Expression::Unary(expr) => match expr.operator {
                ast::UnaryOperator::Plus => self.gen_unary_plus_expression(expr, dest),
                ast::UnaryOperator::Minus => self.gen_unary_minus_expression(expr, dest),
                ast::UnaryOperator::LogicalNot => self.gen_logical_not_expression(expr, dest),
                ast::UnaryOperator::BitwiseNot => self.gen_bitwise_not_expression(expr, dest),
                ast::UnaryOperator::TypeOf => self.gen_typeof_expression(expr, dest),
                ast::UnaryOperator::Void => self.gen_void_expression(expr, dest),
                ast::UnaryOperator::Delete => self.gen_delete_expression(expr, dest),
            },
            ast::Expression::Binary(expr) => self.gen_binary_expression(expr, dest),
            ast::Expression::Logical(expr) => self.gen_logical_expression(expr, dest),
            ast::Expression::Assign(expr) => self.gen_assignment_expression(expr, dest),
            ast::Expression::Update(expr) => self.gen_update_expression(expr, dest),
            ast::Expression::Member(expr) => self.gen_member_expression(expr, dest, None, None),
            ast::Expression::Chain(expr) => self.gen_chain_expression(expr, dest, None),
            ast::Expression::Conditional(expr) => self.gen_conditional_expression(expr, dest),
            ast::Expression::Call(expr) => self.gen_call_expression(expr, dest, None),
            ast::Expression::New(expr) => self.gen_new_expresssion(expr, dest),
            ast::Expression::Sequence(expr) => self.gen_sequence_expression(expr, dest),
            ast::Expression::Array(expr) => self.gen_array_literal(expr, dest),
            ast::Expression::Object(expr) => self.gen_object_literal(expr, dest),
            ast::Expression::Function(expr) => self.gen_function_expression(expr, None, dest),
            ast::Expression::ArrowFunction(expr) => {
                self.gen_arrow_function_expression(expr, None, dest)
            }
            ast::Expression::Class(expr) => self.gen_class_expression(expr, None, dest),
            ast::Expression::Await(expr) => self.gen_await_expression(expr, dest),
            ast::Expression::Yield(expr) => self.gen_yield_expression(expr, dest),
            ast::Expression::SuperMember(expr) => {
                self.gen_super_member_expression(expr, dest, None)
            }
            ast::Expression::SuperCall(expr) => self.gen_super_call_expression(expr, dest),
            ast::Expression::Template(expr) => self.gen_template_literal_expression(expr, dest),
            ast::Expression::TaggedTemplate(expr) => {
                self.gen_tagged_template_expression(expr, dest)
            }
            ast::Expression::MetaProperty(meta_property) => match meta_property.kind {
                ast::MetaPropertyKind::NewTarget { scope } => {
                    self.gen_new_target_expression(scope, dest)
                }
                ast::MetaPropertyKind::ImportMeta => self.gen_import_meta_expression(dest),
            },
            ast::Expression::Import(expr) => self.gen_dynamic_import_expression(expr, dest),
        }
    }

    /// Generate an expression with any destination register.
    #[inline]
    fn gen_expression(&mut self, expr: &ast::Expression) -> EmitResult<GenRegister> {
        self.gen_expression_with_dest(expr, ExprDest::Any)
    }

    #[inline]
    fn gen_outer_expression(&mut self, expr: &ast::OuterExpression) -> EmitResult<GenRegister> {
        // Starts a "has assign expression" context
        self.enter_has_assign_expr_context(expr.has_assign_expr);
        let result = self.gen_expression(&expr.expr);
        self.exit_has_assign_expr_context();

        result
    }

    #[inline]
    fn gen_outer_expression_with_dest(
        &mut self,
        expr: &ast::OuterExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // Starts a "has assign expression" context
        self.enter_has_assign_expr_context(expr.has_assign_expr);
        let result = self.gen_expression_with_dest(&expr.expr, dest);
        self.exit_has_assign_expr_context();

        result
    }

    fn gen_load_identifier(
        &mut self,
        id: &ast::Identifier,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        match id.scope.kind() {
            ResolvedScope::UnresolvedGlobal => self.gen_load_global_identifier(&id.name, dest),
            ResolvedScope::UnresolvedDynamic => self.gen_load_dynamic_identifier(&id.name, dest),
            ResolvedScope::Resolved => self.gen_load_binding(&id.name, id.get_binding(), dest),
        }
    }

    fn gen_load_binding(
        &mut self,
        name: &str,
        binding: &Binding,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // For bindings that could be accessed during their TDZ we must generate a TDZ check. Must
        // ensure that TDZ check occurs before writing to a non-temporary register.
        //
        // No explicit TDZ check is needed for exported bindings since LoadFromModule has an
        // implicit TDZ check.
        let add_tdz_check = binding.needs_tdz_check() && !binding.is_exported();

        match binding.vm_location().unwrap() {
            // Fixed registers may directly reference the register
            VMLocation::Argument(index) => {
                let arg_reg = Register::argument(index);
                self.gen_load_fixed_register_identifier(name, arg_reg, add_tdz_check, dest)
            }
            VMLocation::LocalRegister(index) => {
                let local_reg = Register::local(index);
                self.gen_load_fixed_register_identifier(name, local_reg, add_tdz_check, dest)
            }
            // Global variables must first be loaded to a register
            VMLocation::Global => {
                self.gen_load_non_fixed_identifier(name, add_tdz_check, dest, |this, dest| {
                    this.gen_load_global_identifier(name, dest)
                })
            }
            // Scope variables must be loaded to a register from the scope at the specified index
            VMLocation::Scope { scope_id, index } => {
                self.gen_load_non_fixed_identifier(name, add_tdz_check, dest, |this, dest| {
                    this.gen_load_scope_binding(scope_id, index, dest)
                })
            }
            // Module scope variables must be loaded to a register from the BoxedValue in the scope
            // at the specified index.
            VMLocation::ModuleScope { scope_id, index } => {
                self.gen_load_non_fixed_identifier(name, add_tdz_check, dest, |this, dest| {
                    this.gen_load_module_scope_binding(scope_id, index, dest)
                })
            }
            // Eval or with variables must be loaded to a register from a scope chain lookup
            VMLocation::EvalVar | VMLocation::WithVar => {
                self.gen_load_dynamic_identifier(name, dest)
            }
        }
    }

    /// Variables in arguments and registers can be encoded directly as register operands.
    /// TDZ check is performed directly on the source register itself.
    fn gen_load_fixed_register_identifier(
        &mut self,
        name: &str,
        fixed_reg: GenRegister,
        add_tdz_check: bool,
        mut dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        if add_tdz_check {
            let name_constant_index = self.add_string_constant(name)?;
            self.writer
                .check_tdz_instruction(fixed_reg, name_constant_index);
        }

        // Avoid assignment hazards in "has assignment expression" contexts by ensuring that
        // the fixed register is loaded to a temporary instead of used directly.
        if self.has_assign_expr {
            match dest {
                ExprDest::Any => {
                    dest = ExprDest::NewTemporary;
                }
                // Respect a fixed destination
                ExprDest::Fixed(_) | ExprDest::NewTemporary => {}
            }
        }

        self.gen_mov_reg_to_dest(fixed_reg, dest)
    }

    /// Variables in globals and scopes must be loaded from the global or scope, and optionally
    /// perform the TDZ check on the loaded value.
    fn gen_load_non_fixed_identifier(
        &mut self,
        name: &str,
        add_tdz_check: bool,
        dest: ExprDest,
        load_binding_fn: impl FnOnce(&mut Self, ExprDest) -> EmitResult<GenRegister>,
    ) -> EmitResult<GenRegister> {
        if add_tdz_check {
            let name_constant_index = self.add_string_constant(name)?;

            // Check if destination register is a fixed non-temporary. If so we must first
            // load to a temporary and perform the TDZ check, so that we guarantee that the
            // TDZ check occurs before the destination is written (which may be observable).
            if let ExprDest::Fixed(dest_reg) = dest {
                if !self.register_allocator.is_temporary_register(dest_reg) {
                    let temporary_reg = load_binding_fn(self, ExprDest::Any)?;
                    self.writer
                        .check_tdz_instruction(temporary_reg, name_constant_index);

                    return self.gen_mov_reg_to_dest(temporary_reg, dest);
                }
            }

            // Otherwise load the value and then perform the TDZ check directly on the
            // loaded value.
            let value = load_binding_fn(self, dest)?;
            self.writer
                .check_tdz_instruction(value, name_constant_index);

            return Ok(value);
        }

        // Without a TDZ check a simple load is performed
        load_binding_fn(self, dest)
    }

    fn gen_load_global_identifier(
        &mut self,
        name: &str,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;
        let constant_index = self.add_string_constant(name)?;

        self.writer.load_global_instruction(dest, constant_index);

        Ok(dest)
    }

    fn find_scope_depth(&self, scope_id: usize) -> EmitResult<GenUInt> {
        let mut scope_node = &self.scope;
        let mut depth = 0;

        while scope_node.scope_id != scope_id {
            scope_node = scope_node.parent.as_ref().unwrap();
            depth += 1;
        }

        GenUInt::try_from_unsigned(depth).ok_or(EmitError::TooManyScopes)
    }

    fn gen_load_scope_binding(
        &mut self,
        scope_id: usize,
        scope_index: usize,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let parent_depth = self.find_scope_depth(scope_id)?;
        let scope_index =
            GenUInt::try_from_unsigned(scope_index).ok_or(EmitError::IndexTooLarge)?;

        let dest = self.allocate_destination(dest)?;
        self.writer
            .load_from_scope_instruction(dest, scope_index, parent_depth);

        Ok(dest)
    }

    fn gen_load_module_scope_binding(
        &mut self,
        scope_id: usize,
        scope_index: usize,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let parent_depth = self.find_scope_depth(scope_id)?;
        let scope_index =
            GenUInt::try_from_unsigned(scope_index).ok_or(EmitError::IndexTooLarge)?;

        let dest = self.allocate_destination(dest)?;
        self.writer
            .load_from_module_instruction(dest, scope_index, parent_depth);

        Ok(dest)
    }

    fn gen_load_dynamic_identifier(
        &mut self,
        name: &str,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;
        let constant_index = self.add_string_constant(name)?;

        self.writer.load_dynamic_instruction(dest, constant_index);

        Ok(dest)
    }

    fn gen_store_identifier(
        &mut self,
        id: &ast::Identifier,
        value: GenRegister,
        flags: StoreFlags,
    ) -> EmitResult<()> {
        match id.scope.kind() {
            ResolvedScope::UnresolvedGlobal => self.gen_store_global_identifier(&id.name, value),
            ResolvedScope::UnresolvedDynamic => self.gen_store_dynamic_identifier(&id.name, value),
            ResolvedScope::Resolved => {
                let binding = id.get_binding();
                self.gen_store_binding(&id.name, binding, value, flags)
            }
        }
    }

    fn gen_store_binding(
        &mut self,
        name: &str,
        binding: &Binding,
        value: GenRegister,
        flags: StoreFlags,
    ) -> EmitResult<()> {
        if self.is_immutable_reassignment(binding, flags) {
            // Error if we are trying to reassign an immutable binding
            let name_constant_index = self.add_string_constant(name)?;
            self.writer.error_const_instruction(name_constant_index);
        } else if self.is_noop_reassignment(binding, flags) {
            // Ignore noop reassignments
            return Ok(());
        }

        // If this binding may be stored during its TDZ we must first load the binding and perform
        // a TDZ check. Loaded value can be ignored since it is only used for TDZ check.
        if flags.contains(StoreFlags::NEEDS_TDZ_CHECK) && binding.needs_tdz_check() {
            let value = self.gen_load_binding(name, binding, ExprDest::Any)?;
            self.register_allocator.release(value);
        }

        match binding.vm_location().unwrap() {
            // Variables in arguments and registers are be encoded directly as register operands,
            // so simply move to the correct register.
            VMLocation::Argument(index) => {
                let arg_reg = Register::argument(index);
                self.write_mov_instruction(arg_reg, value);
            }
            VMLocation::LocalRegister(index) => {
                let local_reg = Register::local(index);
                self.write_mov_instruction(local_reg, value);
            }
            // Globals must be stored with name appearing in the constant table
            VMLocation::Global => self.gen_store_global_identifier(name, value)?,
            // Scope variables must be stored in the scope at the specified index
            VMLocation::Scope { scope_id, index } => {
                self.gen_store_scope_binding(scope_id, index, value)?
            }
            // Module scope variables must be stored in the BoxedValue in the scope at the specified
            // index.
            VMLocation::ModuleScope { scope_id, index } => {
                self.gen_store_module_scope_binding(scope_id, index, value)?
            }
            // Eval or with vars are dynamically stored in parent scope at runtime, so they must be
            // dynamically stored to.
            VMLocation::EvalVar | VMLocation::WithVar => {
                self.gen_store_dynamic_identifier(name, value)?
            }
        }

        Ok(())
    }

    fn gen_store_global_identifier(&mut self, name: &str, value: GenRegister) -> EmitResult<()> {
        let constant_index = self.add_string_constant(name)?;
        self.writer.store_global_instruction(value, constant_index);

        Ok(())
    }

    fn gen_store_scope_binding(
        &mut self,
        scope_id: usize,
        scope_index: usize,
        value: GenRegister,
    ) -> EmitResult<()> {
        let parent_depth = self.find_scope_depth(scope_id)?;
        let scope_index =
            GenUInt::try_from_unsigned(scope_index).ok_or(EmitError::IndexTooLarge)?;

        self.writer
            .store_to_scope_instruction(value, scope_index, parent_depth);

        Ok(())
    }

    fn gen_store_module_scope_binding(
        &mut self,
        scope_id: usize,
        scope_index: usize,
        value: GenRegister,
    ) -> EmitResult<()> {
        let parent_depth = self.find_scope_depth(scope_id)?;
        let scope_index =
            GenUInt::try_from_unsigned(scope_index).ok_or(EmitError::IndexTooLarge)?;

        self.writer
            .store_to_module_instruction(value, scope_index, parent_depth);

        Ok(())
    }

    fn gen_store_dynamic_identifier(&mut self, name: &str, value: GenRegister) -> EmitResult<()> {
        let constant_index = self.add_string_constant(name)?;
        self.writer.store_dynamic_instruction(value, constant_index);

        Ok(())
    }

    /// Find the best expression destination in which to place the result of an expression that will
    /// be stored at the given identifier's location.
    ///
    /// This allows expressions to be stored directly into their destination register, avoiding an
    /// unnecessary mov instruction.
    fn expr_dest_for_id(&mut self, id: &ast::Identifier, store_flags: StoreFlags) -> ExprDest {
        // Unresolved variables can be stored from any register
        if id.scope.is_unresolved() {
            return ExprDest::Any;
        }

        self.expr_dest_for_binding(id.get_binding(), store_flags)
    }

    fn expr_dest_for_binding(&mut self, binding: &Binding, store_flags: StoreFlags) -> ExprDest {
        let fixed_register = match binding.vm_location().unwrap() {
            // Variables in arguments and registers can be encoded directly as register operands.
            // Make sure to load to a new temporary if necessary.
            VMLocation::Argument(index) => Register::argument(index),
            VMLocation::LocalRegister(index) => Register::local(index),
            // Other VM locations can be stored from any register
            VMLocation::Global
            | VMLocation::Scope { .. }
            | VMLocation::ModuleScope { .. }
            | VMLocation::EvalVar
            | VMLocation::WithVar => return ExprDest::Any,
        };

        // Reassigning to an immutable binding will error. Make sure to not treat the true fixed
        // register as the destination, otherwise a value may be stored directly to that fixed
        // register before we error, which is observable.
        //
        // Similarly make sure that noop reassignment is not observable.
        if self.is_immutable_reassignment(binding, store_flags)
            || self.is_noop_reassignment(binding, store_flags)
        {
            return ExprDest::Any;
        }

        ExprDest::Fixed(fixed_register)
    }

    fn is_immutable_reassignment(&self, binding: &Binding, store_flags: StoreFlags) -> bool {
        if store_flags.contains(StoreFlags::INITIALIZATION) {
            return false;
        }

        // Function expression names cannot be reassigned in strict mode
        binding.is_immutable() || (binding.kind().is_function_expression_name() && self.is_strict)
    }

    fn is_noop_reassignment(&self, binding: &Binding, store_flags: StoreFlags) -> bool {
        if store_flags.contains(StoreFlags::INITIALIZATION) {
            return false;
        }

        // Reassigning function expression name is a no-op in sloppy mode
        binding.kind().is_function_expression_name() && !self.is_strict
    }

    /// Find the best expression destination in which to place the result of a destructuring
    /// assignment.
    ///
    /// This allows destructuring assignments to store expressions directly into their destination
    /// register, avoiding unnecessary movs.
    fn expr_dest_for_destructuring_assignment(
        &mut self,
        pattern: &ast::Pattern,
        store_flags: StoreFlags,
    ) -> ExprDest {
        match pattern {
            ast::Pattern::Id(id) => self.expr_dest_for_id(id, store_flags),
            ast::Pattern::Assign(assign) => {
                // Make sure not to clobber the dest register if it is fixed, since there will be
                // multiple assignments to the dest.
                let dest = self.expr_dest_for_destructuring_assignment(&assign.left, store_flags);
                self.gen_ensure_dest_is_temporary(dest)
            }
            // Other patterns are stored via instructions, not directly into a register, so any
            // register will do.
            ast::Pattern::Object(_)
            | ast::Pattern::Array(_)
            | ast::Pattern::Member(_)
            | ast::Pattern::SuperMember(_) => ExprDest::Any,
        }
    }

    /// Allocate the destination register for an expression, which may be a new temporary or a fixed
    /// register that was specified.
    fn allocate_destination(&mut self, dest: ExprDest) -> EmitResult<GenRegister> {
        match dest {
            ExprDest::Any | ExprDest::NewTemporary => self.register_allocator.allocate(),
            ExprDest::Fixed(dest) => Ok(dest),
        }
    }

    /// Move a src register to the expression destination if necessary. Releases the source register
    /// if it was moved.
    fn gen_mov_reg_to_dest(&mut self, src: GenRegister, dest: ExprDest) -> EmitResult<GenRegister> {
        match dest {
            ExprDest::Any => Ok(src),
            ExprDest::NewTemporary => {
                self.register_allocator.release(src);
                let dest = self.register_allocator.allocate()?;
                self.write_mov_instruction(dest, src);
                Ok(dest)
            }
            ExprDest::Fixed(dest) => {
                if src == dest {
                    return Ok(dest);
                }

                self.register_allocator.release(src);
                self.write_mov_instruction(dest, src);
                Ok(dest)
            }
        }
    }

    fn gen_ensure_dest_is_temporary(&mut self, dest: ExprDest) -> ExprDest {
        match dest {
            ExprDest::Fixed(dest) if !self.register_allocator.is_temporary_register(dest) => {
                ExprDest::NewTemporary
            }
            _ => dest,
        }
    }

    fn gen_ensure_reg_is_temporary(&mut self, reg: GenRegister) -> EmitResult<GenRegister> {
        if self.register_allocator.is_temporary_register(reg) {
            Ok(reg)
        } else {
            let dest = self.register_allocator.allocate()?;
            self.write_mov_instruction(dest, reg);
            Ok(dest)
        }
    }

    fn gen_null_literal_expression(&mut self, dest: ExprDest) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;
        self.writer.load_null_instruction(dest);
        Ok(dest)
    }

    fn gen_number_literal(&mut self, value: f64, dest: ExprDest) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;

        // Smis are inlined as immediate while all other numbers are stored in the constant table
        let number = Value::number(value);
        if number.is_smi() {
            self.writer
                .load_immediate_instruction(dest, SInt::new(number.as_smi()));
        } else {
            let constant_index = self.constant_table_builder.add_double(number.as_double())?;
            self.writer
                .load_constant_instruction(dest, ConstantIndex::new(constant_index));
        }

        Ok(dest)
    }

    fn gen_boolean_literal_expression(
        &mut self,
        expr: &ast::BooleanLiteral,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;

        if expr.value {
            self.writer.load_true_instruction(dest);
        } else {
            self.writer.load_false_instruction(dest);
        }

        Ok(dest)
    }

    fn gen_string_literal_expression(
        &mut self,
        expr: &ast::StringLiteral,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;

        // All string literals are loaded from the constant table
        let constant_index = self.add_wtf8_string_constant(&expr.value)?;
        self.writer.load_constant_instruction(dest, constant_index);

        Ok(dest)
    }

    fn gen_bigint_literal_expression(
        &mut self,
        lit: &ast::BigIntLiteral,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;

        // BigInts are stored in the constant table, but not deduped
        let bigint_value = BigIntValue::new(self.cx, lit.value.clone()).to_handle();
        let constant_index = self
            .constant_table_builder
            .add_heap_object(bigint_value.cast())?;
        self.writer
            .load_constant_instruction(dest, ConstantIndex::new(constant_index));

        Ok(dest)
    }

    fn gen_regexp_literal_expression(
        &mut self,
        lit: &ast::RegExpLiteral,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // Can use source directly as "escaped" pattern source string
        let source = InternedStrings::get_wtf8_str(self.cx, &lit.pattern);

        // Compile regexp and store compiled regexp in constant table
        let compiled_regexp = compile_regexp(self.cx, &lit.regexp, source);
        let compiled_regexp_index = self
            .constant_table_builder
            .add_heap_object(compiled_regexp.cast())?;

        let dest = self.allocate_destination(dest)?;
        self.writer
            .new_regexp_instruction(dest, ConstantIndex::new(compiled_regexp_index));

        Ok(dest)
    }

    fn gen_store_captured_this(&mut self, scope: &AstScopeNode) -> EmitResult<()> {
        match scope.get_binding("this").vm_location() {
            // `this` is captured so it must be stored in the scope
            Some(VMLocation::Scope { scope_id, index }) => {
                self.gen_store_scope_binding(scope_id, index, Register::this())
            }
            // `this` is not captured, so it can be referenced directly everywhere
            None => Ok(()),
            _ => unreachable!("`this` must be captured or have no VM location"),
        }
    }

    fn gen_this_expression(
        &mut self,
        expr: &ast::ThisExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        self.gen_load_this(expr.scope, dest)
    }

    fn gen_load_this(
        &mut self,
        scope: Option<AstPtr<AstScopeNode>>,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let mut needs_init_check = false;

        // If scope has been resolved this may be a captured `this`. Load from scope directly to
        // the dest.
        let this_value = if let Some(scope) = scope.as_ref() {
            let binding = scope.as_ref().get_binding("this");

            if let BindingKind::ImplicitThis { in_derived_constructor: true } = binding.kind() {
                needs_init_check = true;
            }

            match binding.vm_location() {
                Some(VMLocation::Scope { scope_id, index }) => {
                    self.gen_load_scope_binding(scope_id, index, dest)?
                }
                _ => {
                    // A scope may be resolved but `this` is not captured in some cases, e.g. for
                    // `this` in a derived constructor.
                    self.gen_mov_reg_to_dest(Register::this(), dest)?
                }
            }
        } else {
            // Otherwise `this` is for the current function
            if self.is_derived_constructor() {
                needs_init_check = true;
            }

            self.gen_mov_reg_to_dest(Register::this(), dest)?
        };

        // Check if this was initialized
        if needs_init_check {
            self.writer.check_this_initialized_instruction(this_value);
        }

        Ok(this_value)
    }

    fn gen_unary_minus_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // A unary minus on a number literal is inlined as a negative number literal
        if let ast::Expression::Number(number_expr) = expr.argument.as_ref() {
            return self.gen_number_literal(-number_expr.value, dest);
        }

        // Otherwise generate a negate instruction
        let argument = self.gen_expression(&expr.argument)?;
        self.register_allocator.release(argument);

        let dest = self.allocate_destination(dest)?;
        self.writer.neg_instruction(dest, argument);

        Ok(dest)
    }

    fn gen_unary_plus_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // A unary plus on a number literal is inlined as that number literal
        if let ast::Expression::Number(number_expr) = expr.argument.as_ref() {
            return self.gen_number_literal(number_expr.value, dest);
        }

        // Otherwise generate a ToNumber instruction
        let argument = self.gen_expression(&expr.argument)?;
        self.register_allocator.release(argument);

        let dest = self.allocate_destination(dest)?;
        self.writer.to_number_instruction(dest, argument);

        Ok(dest)
    }

    fn gen_logical_not_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let argument = self.gen_expression(&expr.argument)?;
        self.register_allocator.release(argument);

        let dest = self.allocate_destination(dest)?;
        self.writer.log_not_instruction(dest, argument);

        Ok(dest)
    }

    fn gen_bitwise_not_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let argument = self.gen_expression(&expr.argument)?;
        self.register_allocator.release(argument);

        let dest = self.allocate_destination(dest)?;
        self.writer.bit_not_instruction(dest, argument);

        Ok(dest)
    }

    fn gen_typeof_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // Unresolved identifiers must be treated as undefined
        let argument = match expr.argument.as_ref() {
            ast::Expression::Id(id) => match id.scope.kind() {
                // Special instruction for unresolved global bindings
                ResolvedScope::UnresolvedGlobal => {
                    let argument = self.register_allocator.allocate()?;
                    let constant_index = self.add_string_constant(&id.name)?;

                    self.writer
                        .load_global_or_unresolved_instruction(argument, constant_index);

                    argument
                }
                ResolvedScope::UnresolvedDynamic => {
                    // Special instruction for unresolved dynamic bindings
                    let argument = self.register_allocator.allocate()?;
                    let constant_index = self.add_string_constant(&id.name)?;

                    self.writer
                        .load_dynamic_or_unresolved_instruction(argument, constant_index);

                    argument
                }
                // All resolved identifiers and other expressions are emitted normally
                ResolvedScope::Resolved => self.gen_expression(&expr.argument)?,
            },
            _ => self.gen_expression(&expr.argument)?,
        };

        self.register_allocator.release(argument);

        // Perform typeof on the loaded argument
        let dest = self.allocate_destination(dest)?;
        self.writer.type_of_instruction(dest, argument);

        Ok(dest)
    }

    fn gen_void_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // Void expressions are evaluated for side effects only, so simply evaluate the argument
        // and return undefined.
        let result = self.gen_expression(&expr.argument)?;
        self.register_allocator.release(result);

        let dest = self.allocate_destination(dest)?;
        self.writer.load_undefined_instruction(dest);

        Ok(dest)
    }

    fn gen_delete_expression(
        &mut self,
        expr: &ast::UnaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        match expr.argument.as_ref() {
            ast::Expression::Member(member_expr) => {
                let object = self.gen_maybe_chain_part_expression(
                    &member_expr.object,
                    ExprDest::Any,
                    None,
                    None,
                )?;

                // Generate the key
                let key = self.gen_load_property_to_value(member_expr)?;

                // Write the delete property instruction
                self.register_allocator.release(key);
                self.register_allocator.release(object);
                let dest = self.allocate_destination(dest)?;

                self.writer.delete_property_instruction(dest, object, key);

                Ok(dest)
            }
            ast::Expression::Chain(chain_expr)
                if matches!(chain_expr.expression.as_ref(), ast::Expression::Member(_)) =>
            {
                let member_expr = match chain_expr.expression.as_ref() {
                    ast::Expression::Member(member_expr) => member_expr,
                    _ => unreachable!(),
                };

                let nullish_block = self.new_block();
                let join_block = self.new_block();

                // Generate the inner chain expression that evaluates to the object
                // the object register.
                let object = self.gen_maybe_chain_part_expression(
                    &member_expr.object,
                    ExprDest::Any,
                    None,
                    Some(nullish_block),
                )?;

                // If in an optional chain, check for nullish and jump to its block if necessary
                if member_expr.is_optional {
                    self.write_jump_nullish_instruction(object, nullish_block)?;
                }

                // Generate the key
                let key = self.gen_load_property_to_value(member_expr)?;

                self.register_allocator.release(key);
                self.register_allocator.release(object);
                let dest = self.allocate_destination(dest)?;

                // Write the delete property instruction
                self.writer.delete_property_instruction(dest, object, key);
                self.write_jump_instruction(join_block)?;

                // If object is nullish (or short circuits), then entire delete will evaluate to
                // true.
                self.start_block(nullish_block);
                self.writer.load_true_instruction(dest);

                self.start_block(join_block);

                Ok(dest)
            }
            ast::Expression::Id(id) => {
                let dest = self.allocate_destination(dest)?;

                // If scope was resolved we may statically know that the binding cannot be deleted
                if id.scope.is_resolved() {
                    let binding = id.get_binding();

                    // Only var and function bindings in eval scopes can be deleted. Otherwise is a
                    // no-op which returns false.
                    let is_var_or_function =
                        matches!(binding.kind(), BindingKind::Var | BindingKind::Function { .. });
                    let in_eval_scope =
                        matches!(id.scope.unwrap_resolved().kind(), ScopeNodeKind::Eval { .. });

                    if !is_var_or_function || !in_eval_scope {
                        self.writer.load_false_instruction(dest);
                        return Ok(dest);
                    }
                }

                let name_constant_index = self.add_string_constant(&id.name)?;
                self.writer
                    .delete_binding_instruction(dest, name_constant_index);

                Ok(dest)
            }
            ast::Expression::SuperMember(super_expr) => {
                // Error when deleting a super member expression. But first be sure to evaluate the
                // computed key.
                if super_expr.is_computed {
                    let property = self.gen_expression(&super_expr.property)?;
                    self.register_allocator.release(property);
                }

                self.writer.error_delete_super_property_instruction();

                // No need to initialize dest since it will not be used
                self.allocate_destination(dest)
            }
            _ => {
                // Otherwise evaluate argument but discard value, always returning true
                let argument_value = self.gen_expression(&expr.argument)?;
                self.register_allocator.release(argument_value);

                let dest = self.allocate_destination(dest)?;
                self.writer.load_true_instruction(dest);

                Ok(dest)
            }
        }
    }

    fn gen_load_property_to_value(
        &mut self,
        member_expr: &ast::MemberExpression,
    ) -> EmitResult<GenRegister> {
        if member_expr.is_computed {
            self.gen_expression(&member_expr.property)
        } else {
            let key = self.register_allocator.allocate()?;
            let name_id = member_expr.property.to_id();
            let constant_index = self.add_string_constant(&name_id.name)?;
            self.writer.load_constant_instruction(key, constant_index);
            Ok(key)
        }
    }

    fn gen_binary_expression(
        &mut self,
        expr: &ast::BinaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // Special handling for private `in` operator
        if expr.operator == ast::BinaryOperator::InPrivate {
            return self.gen_private_in_expression(expr, dest);
        }

        let left = self.gen_expression(&expr.left)?;
        let right = self.gen_expression(&expr.right)?;

        self.register_allocator.release(right);
        self.register_allocator.release(left);
        let dest = self.allocate_destination(dest)?;

        match expr.operator {
            ast::BinaryOperator::Add => self.writer.add_instruction(dest, left, right),
            ast::BinaryOperator::Subtract => self.writer.sub_instruction(dest, left, right),
            ast::BinaryOperator::Multiply => self.writer.mul_instruction(dest, left, right),
            ast::BinaryOperator::Divide => self.writer.div_instruction(dest, left, right),
            ast::BinaryOperator::Remainder => self.writer.rem_instruction(dest, left, right),
            ast::BinaryOperator::Exponent => self.writer.exp_instruction(dest, left, right),
            ast::BinaryOperator::EqEq => self.writer.loose_equal_instruction(dest, left, right),
            ast::BinaryOperator::NotEq => {
                self.writer.loose_not_equal_instruction(dest, left, right)
            }
            ast::BinaryOperator::EqEqEq => self.writer.strict_equal_instruction(dest, left, right),
            ast::BinaryOperator::NotEqEq => {
                self.writer.strict_not_equal_instruction(dest, left, right)
            }
            ast::BinaryOperator::LessThan => self.writer.less_than_instruction(dest, left, right),
            ast::BinaryOperator::LessThanOrEqual => self
                .writer
                .less_than_or_equal_instruction(dest, left, right),
            ast::BinaryOperator::GreaterThan => {
                self.writer.greater_than_instruction(dest, left, right)
            }
            ast::BinaryOperator::GreaterThanOrEqual => self
                .writer
                .greater_than_or_equal_instruction(dest, left, right),
            ast::BinaryOperator::And => self.writer.bit_and_instruction(dest, left, right),
            ast::BinaryOperator::Or => self.writer.bit_or_instruction(dest, left, right),
            ast::BinaryOperator::Xor => self.writer.bit_xor_instruction(dest, left, right),
            ast::BinaryOperator::ShiftLeft => self.writer.shift_left_instruction(dest, left, right),
            ast::BinaryOperator::ShiftRightArithmetic => self
                .writer
                .shift_right_arithmetic_instruction(dest, left, right),
            ast::BinaryOperator::ShiftRightLogical => self
                .writer
                .shift_right_logical_instruction(dest, left, right),
            ast::BinaryOperator::In => self.writer.in_instruction(dest, right, left),
            ast::BinaryOperator::InPrivate => unreachable!(),
            ast::BinaryOperator::InstanceOf => {
                self.writer.instance_of_instruction(dest, left, right)
            }
        }

        Ok(dest)
    }

    fn gen_private_in_expression(
        &mut self,
        expr: &ast::BinaryExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let key = self.gen_load_private_symbol(expr.left.to_id())?;
        let object = self.gen_expression(&expr.right)?;

        self.register_allocator.release(object);
        self.register_allocator.release(key);
        let dest = self.allocate_destination(dest)?;

        self.writer.in_instruction(dest, object, key);

        Ok(dest)
    }

    fn gen_logical_expression(
        &mut self,
        expr: &ast::LogicalExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;
        let join_block = self.new_block();

        // If the destination is not a temporary register then we must write intermediate values
        // to a temporary register, then move that to the final destination at the end. This avoids
        // clobbering the non-temporary register if the expression throws.
        let needs_temporary_dest = !self.register_allocator.is_temporary_register(dest);
        let temporary_dest = if needs_temporary_dest {
            self.register_allocator.allocate()?
        } else {
            dest
        };

        // Generate left expression, storing in (possibly temporary) dest
        self.gen_expression_with_dest(&expr.left, ExprDest::Fixed(temporary_dest))?;

        // Write the appropriate conditional jump for each operator
        match expr.operator {
            ast::LogicalOperator::And => {
                self.write_jump_false_for_expression(&expr.left, temporary_dest, join_block)?
            }
            ast::LogicalOperator::Or => {
                self.write_jump_true_for_expression(&expr.left, temporary_dest, join_block)?;
            }
            ast::LogicalOperator::NullishCoalesce => {
                self.write_jump_not_nullish_instruction(temporary_dest, join_block)?
            }
        }

        // Generate right expression, storing in (possibly temporary) dest
        self.gen_expression_with_dest(&expr.right, ExprDest::Fixed(temporary_dest))?;

        self.start_block(join_block);

        // Move from temporary dest to final dest if necessary
        if needs_temporary_dest {
            self.write_mov_instruction(dest, temporary_dest);
            self.register_allocator.release(temporary_dest);
        }

        Ok(dest)
    }

    fn gen_conditional_expression(
        &mut self,
        expr: &ast::ConditionalExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let join_block = self.new_block();
        let altern_block = self.new_block();

        // Emit test expression and proceed to the correct block
        let test = self.gen_expression(&expr.test)?;
        self.write_jump_false_for_expression(&expr.test, test, altern_block)?;

        // Both branches place their result in the same destination register
        self.register_allocator.release(test);
        let dest = self.allocate_destination(dest)?;

        self.gen_expression_with_dest(&expr.conseq, ExprDest::Fixed(dest))?;
        self.write_jump_instruction(join_block)?;

        self.start_block(altern_block);
        self.gen_expression_with_dest(&expr.altern, ExprDest::Fixed(dest))?;

        self.start_block(join_block);

        Ok(dest)
    }

    fn gen_call_expression(
        &mut self,
        expr: &ast::CallExpression,
        dest: ExprDest,
        optional_nullish_block: Option<BlockId>,
    ) -> EmitResult<GenRegister> {
        // Check if this might be a direct eval
        let is_maybe_direct_eval = matches!(expr.callee.as_ref(), ast::Expression::Id(id) if id.name == "eval")
            && !expr.is_optional;

        // Find the callee and this value to use for the call
        let (callee, this_value) =
            self.gen_callee_and_this_value(&expr.callee, optional_nullish_block)?;

        // If in an optional chain, jump to the nullish block if the calle is nullish
        if expr.is_optional {
            self.write_jump_nullish_instruction(callee, optional_nullish_block.unwrap())?;
        }

        // Generate arguments into a contiguous argc + argv slice
        let mut args = self.gen_call_arguments(&expr.arguments, callee)?;

        // Allocate receiver register if necessary
        if let CallArgs::Varargs { args, receiver } = &mut args {
            if let Some(this_value) = this_value {
                *receiver = this_value;
            } else if !is_maybe_direct_eval {
                *receiver = self.register_allocator.allocate()?;
                self.register_allocator.release(*receiver);
            };

            self.register_allocator.release(*args);
        }

        // Release all remaining call registers
        self.register_allocator.release(callee);
        if let Some(this_value) = this_value {
            self.register_allocator.release(this_value);
        }

        // Generate the call itself, optionally with receiver
        let dest = self.allocate_destination(dest)?;

        if let Some(this_value) = this_value {
            match args {
                CallArgs::Varargs { args, receiver } => {
                    self.writer
                        .call_varargs_instruction(dest, callee, receiver, args);
                }
                CallArgs::Normal { argv, argc } => {
                    self.writer
                        .call_with_receiver_instruction(dest, callee, this_value, argv, argc);
                }
            }
        } else if is_maybe_direct_eval {
            let flags = Self::create_eval_flags(expr);
            let flags = UInt::new(flags.bits() as u32);

            match args {
                CallArgs::Varargs { args, .. } => {
                    self.writer
                        .call_maybe_eval_varargs_instruction(dest, callee, args, flags);
                }
                CallArgs::Normal { argv, argc } => {
                    self.writer
                        .call_maybe_eval_instruction(dest, callee, argv, argc, flags);
                }
            }
        } else {
            match args {
                CallArgs::Varargs { args, receiver } => {
                    // If the call is varargs without an explicit receiver, pass `undefined` as the
                    // receiver.
                    self.writer.load_undefined_instruction(receiver);
                    self.writer
                        .call_varargs_instruction(dest, callee, receiver, args);
                }
                CallArgs::Normal { argv, argc } => {
                    self.writer.call_instruction(dest, callee, argv, argc);
                }
            }
        }

        Ok(dest)
    }

    fn create_eval_flags(expr: &ast::CallExpression) -> EvalFlags {
        let mut flags = EvalFlags::empty();

        if expr.maybe_eval_in_function {
            flags |= EvalFlags::IN_FUNCTION;
        }

        if expr.maybe_eval_in_method {
            flags |= EvalFlags::IN_METHOD;
        }

        if expr.maybe_eval_in_static {
            flags |= EvalFlags::IN_STATIC;
        }

        if expr.maybe_eval_in_derived_constructor {
            flags |= EvalFlags::IN_DERIVED_CONSTRUCTOR;
        }

        if expr.maybe_eval_in_static_initializer {
            flags |= EvalFlags::IN_STATIC_INITIALIZER;
        }

        if expr.maybe_eval_in_class_field_initializer {
            flags |= EvalFlags::IN_CLASS_FIELD_INITIALIZER;
        }

        flags
    }

    fn gen_tagged_template_expression(
        &mut self,
        expr: &ast::TaggedTemplateExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // Find the callee and this value to use for the tagged template call
        let (callee, this_value) = self.gen_callee_and_this_value(&expr.tag, None)?;

        // Collect call arguments - the template object followed by all expressions
        let mut arg_regs = Vec::with_capacity(expr.quasi.expressions.len() + 1);

        // Template objects are generated eagerly and stored in constant table
        let template_object = generate_template_object(self.cx, self.realm, &expr.quasi);
        let template_object_index = self
            .constant_table_builder
            .add_heap_object(template_object.cast())?;

        // Pass the template argument as the first call argument
        let template_object_reg = self.register_allocator.allocate()?;
        self.writer.load_constant_instruction(
            template_object_reg,
            ConstantIndex::new(template_object_index),
        );
        arg_regs.push(template_object_reg);

        // Generate code for each argument, loading each into a new temporary register forming a
        // contiguous range of registers.
        for arg in &expr.quasi.expressions {
            let arg_reg = self.gen_expression_with_dest(arg, ExprDest::NewTemporary)?;
            arg_regs.push(arg_reg);
        }

        // Check that the argument registers are contiguous. This is guaranteed by the register
        // allocation strategy.
        debug_assert!(Self::are_registers_contiguous(&arg_regs));

        // Find first argument register and number of arguments - argv and argc for the call
        let argv = arg_regs[0];
        let argc =
            GenUInt::try_from_unsigned(arg_regs.len()).ok_or(EmitError::TooManyCallArguments)?;

        // Release all register allocated for the call
        for arg_reg in arg_regs.into_iter().rev() {
            self.register_allocator.release(arg_reg);
        }
        self.register_allocator.release(callee);
        if let Some(this_value) = this_value {
            self.register_allocator.release(this_value);
        }

        // Generate the call itself, optionally with receiver
        let dest = self.allocate_destination(dest)?;

        if let Some(this_value) = this_value {
            self.writer
                .call_with_receiver_instruction(dest, callee, this_value, argv, argc);
        } else {
            self.writer.call_instruction(dest, callee, argv, argc);
        }

        Ok(dest)
    }

    /// Given a callee expression, generate the callee itself and the value to use as the this value
    /// if a this value is bound (e.g. the callee is a member expression).
    fn gen_callee_and_this_value(
        &mut self,
        callee: &ast::Expression,
        optional_nullish_block: Option<BlockId>,
    ) -> EmitResult<(GenRegister, Option<GenRegister>)> {
        // Calls on a property accesses must pass the object's value as the this value
        match callee {
            // If the callee is a member expression, generate the object expression first and use
            // as the receiver.
            ast::Expression::Member(member_expr) => {
                // Dummy value that will be overwritten when generating member expression
                let mut call_receiver =
                    CallReceiver { receiver: Register::this(), is_chain: false };

                let callee = self.gen_member_expression(
                    member_expr,
                    ExprDest::Any,
                    Some(&mut call_receiver),
                    optional_nullish_block,
                )?;

                Ok((callee, Some(call_receiver.receiver)))
            }
            ast::Expression::SuperMember(super_expr) => {
                // Dummy value that will be overwritten when generating member expression
                let mut call_receiver =
                    CallReceiver { receiver: Register::this(), is_chain: false };

                let callee = self.gen_super_member_expression(
                    super_expr,
                    ExprDest::Any,
                    Some(&mut call_receiver),
                )?;

                Ok((callee, Some(call_receiver.receiver)))
            }
            // If the callee is a chain member expression there will either be a receiver or the
            // chain expression short circuits and the callee is undefined.
            ast::Expression::Chain(chain_expr)
                if matches!(chain_expr.expression.as_ref(), ast::Expression::Member(_)) =>
            {
                // Dummy receiver that will be overwritten when generating member expression
                let mut call_receiver = CallReceiver { receiver: Register::this(), is_chain: true };

                let callee =
                    self.gen_chain_expression(chain_expr, ExprDest::Any, Some(&mut call_receiver))?;

                Ok((callee, Some(call_receiver.receiver)))
            }
            // Otherwise there is no receiver
            _ => {
                let callee = self.gen_maybe_chain_part_expression(
                    callee,
                    ExprDest::Any,
                    None,
                    optional_nullish_block,
                )?;
                Ok((callee, None))
            }
        }
    }

    /// Generate the bytecode for all call arguments in a contiguous range, returning the
    /// (argv, argc) pair representing the first register and the number of registers.
    ///
    /// Also takes a default argv register to use if there are no arguments.
    ///
    /// Releases all registers in the argv + argc slice before returning.
    fn gen_stack_arguments(
        &mut self,
        arguments: &[ast::CallArgument],
        default_argv: GenRegister,
    ) -> EmitResult<(GenRegister, GenUInt)> {
        // Generate code for each argument, loading each into a new temporary register forming a
        // contiguous range of registers.
        let mut arg_regs = Vec::with_capacity(arguments.len());
        for argument in arguments {
            match argument {
                ast::CallArgument::Expression(expr) => {
                    let arg_reg = self.gen_expression_with_dest(expr, ExprDest::NewTemporary)?;
                    arg_regs.push(arg_reg);
                }
                ast::CallArgument::Spread(_) => unreachable!("spread is handled by varargs"),
            }
        }

        // Find first argument register, or use callee register as a placeholder if there are no
        // arguments (meaning the choice of argv register is arbitrary).
        let argv = if arg_regs.is_empty() {
            default_argv
        } else {
            arg_regs[0]
        };

        // Check that the argument registers are contiguous. This is guaranteed by the register
        // allocation strategy.
        debug_assert!(Self::are_registers_contiguous(&arg_regs));

        let argc =
            GenUInt::try_from_unsigned(arguments.len()).ok_or(EmitError::TooManyCallArguments)?;

        // Release all call registers
        for arg_reg in arg_regs.iter().rev() {
            self.register_allocator.release(*arg_reg);
        }

        Ok((argv, argc))
    }

    fn are_registers_contiguous(registers: &[GenRegister]) -> bool {
        if registers.is_empty() {
            return true;
        }

        let first_index = registers[0].local_index();
        for (i, reg) in registers[1..].iter().enumerate() {
            if reg.local_index() != first_index + i + 1 {
                return false;
            }
        }

        true
    }

    fn is_call_with_spread(&self, arguments: &[ast::CallArgument]) -> bool {
        arguments
            .iter()
            .any(|arg| matches!(arg, ast::CallArgument::Spread(_)))
    }

    /// If there is a spread element then we must create an array that contains all arguments.
    fn gen_call_varargs_array(
        &mut self,
        arguments: &[ast::CallArgument],
    ) -> EmitResult<GenRegister> {
        let array_elements = arguments
            .iter()
            .map(|arg| match arg {
                ast::CallArgument::Expression(expr) => ArrayElement::Expression(expr),
                ast::CallArgument::Spread(spread) => ArrayElement::Spread(spread),
            })
            .collect::<Vec<_>>();

        self.gen_array_from_elements(&array_elements, ExprDest::NewTemporary)
    }

    fn gen_call_arguments(
        &mut self,
        arguments: &[ast::CallArgument],
        default_argv: GenRegister,
    ) -> EmitResult<CallArgs> {
        if Self::is_call_with_spread(self, arguments) {
            // Any new expression with a spread generates a varargs call. This means all arguments
            // are first placed into an array.
            let args = self.gen_call_varargs_array(arguments)?;

            // Set a dummy receiver that will be overwritten by the caller if needed
            Ok(CallArgs::Varargs { args, receiver: GenRegister::this() })
        } else {
            // Generate arguments into a contiguous argc + argv slice on the VM stack
            let (argv, argc) = self.gen_stack_arguments(arguments, default_argv)?;
            Ok(CallArgs::Normal { argv, argc })
        }
    }

    fn gen_new_expresssion(
        &mut self,
        expr: &ast::NewExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let callee = self.gen_expression(&expr.callee)?;
        let args = self.gen_call_arguments(&expr.arguments, callee)?;

        // Release remaining call registers before allocating dest
        if let CallArgs::Varargs { args, .. } = &args {
            self.register_allocator.release(*args);
        }
        self.register_allocator.release(callee);
        let dest = self.allocate_destination(dest)?;

        // new.target is set to the callee
        let new_target = callee;

        match args {
            CallArgs::Normal { argv, argc } => {
                self.writer
                    .construct_instruction(dest, callee, new_target, argv, argc);
            }
            CallArgs::Varargs { args, .. } => {
                self.writer
                    .construct_varargs_instruction(dest, callee, new_target, args);
            }
        }

        Ok(dest)
    }

    fn gen_sequence_expression(
        &mut self,
        expr: &ast::SequenceExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // All expressions except the last are evaluated for side effects only
        for i in 0..expr.expressions.len() - 1 {
            let result = self.gen_expression(&expr.expressions[i])?;
            self.register_allocator.release(result);
        }

        // Value of the last expression is value of the entire sequence expression
        self.gen_expression_with_dest(expr.expressions.last().unwrap(), dest)
    }

    fn gen_array_literal(
        &mut self,
        expr: &ast::ArrayExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let elements = expr
            .elements
            .iter()
            .map(|element| match element {
                ast::ArrayElement::Expression(expr) => ArrayElement::Expression(expr),
                ast::ArrayElement::Spread(spread) => ArrayElement::Spread(spread),
                ast::ArrayElement::Hole => ArrayElement::Hole,
            })
            .collect::<Vec<_>>();

        self.gen_array_from_elements(&elements, dest)
    }

    fn gen_array_from_elements(
        &mut self,
        elements: &[ArrayElement],
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let array = self.allocate_destination(dest)?;
        self.writer.new_array_instruction(array);

        // Fast path for empty arrays
        if elements.is_empty() {
            return Ok(array);
        }

        // Keep an index register that is incremented for each element (if there is a next element)
        let num_elements = elements.len();
        let index = self.register_allocator.allocate()?;
        self.writer.load_immediate_instruction(index, SInt::new(0));

        for (i, element) in elements.iter().enumerate() {
            match element {
                ArrayElement::Expression(expr) => {
                    let value = self.gen_expression(expr)?;
                    self.register_allocator.release(value);

                    self.writer
                        .set_array_property_instruction(array, index, value);

                    if i != num_elements - 1 {
                        self.writer.inc_instruction(index);
                    }
                }
                ArrayElement::Hole => {
                    // Holes are represented as storing the empty value
                    let value = self.register_allocator.allocate()?;
                    self.writer.load_empty_instruction(value);
                    self.register_allocator.release(value);

                    self.writer
                        .set_array_property_instruction(array, index, value);

                    if i != num_elements - 1 {
                        self.writer.inc_instruction(index);
                    }
                }
                ArrayElement::Spread(spread_element) => {
                    // Evaluate the spread argument and get its iterator
                    let iterable = self.gen_expression(&spread_element.argument)?;

                    let iterator = self.register_allocator.allocate()?;
                    let next_method = self.register_allocator.allocate()?;
                    self.writer
                        .get_iterator_instruction(iterator, next_method, iterable);

                    let value = self.register_allocator.allocate()?;
                    let is_done = self.register_allocator.allocate()?;

                    let iteration_start_block = self.new_block();
                    let done_block = self.new_block();

                    // Each iteration starts by calling `next` and checking if we are done
                    self.start_block(iteration_start_block);
                    self.writer
                        .iterator_next_instruction(value, is_done, iterator, next_method);
                    self.write_jump_true_instruction(is_done, done_block)?;

                    // If we are not done then write append value to array
                    self.writer
                        .set_array_property_instruction(array, index, value);
                    self.writer.inc_instruction(index);

                    // Proceed to next iteration
                    self.write_jump_instruction(iteration_start_block)?;

                    self.start_block(done_block);

                    self.register_allocator.release(is_done);
                    self.register_allocator.release(value);
                    self.register_allocator.release(next_method);
                    self.register_allocator.release(iterator);
                    self.register_allocator.release(iterable);
                }
            }
        }

        self.register_allocator.release(index);

        Ok(array)
    }

    fn gen_object_literal(
        &mut self,
        expr: &ast::ObjectExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let body_scope = expr.scope.as_ref();
        self.gen_scope_start(body_scope, None)?;

        let object = self.allocate_destination(dest)?;
        self.writer.new_object_instruction(object);

        // If the body scope exists then the home object must have been used in a method, so store
        // the home object to the scope.
        if body_scope.vm_scope_id().is_some() {
            self.gen_store_captured_home_object(body_scope, object)?;
        }

        for property in &expr.properties {
            // Spread elements represented by a CopyDataProperties instruction with argc=0 and an
            // arbitrary argv, meaning no property keys are excluded.
            if let ast::PropertyKind::Spread(_) = property.kind {
                let source = self.gen_expression(&property.key)?;
                self.writer
                    .copy_data_properties(object, source, source, UInt::new(0));
                self.register_allocator.release(source);
                continue;
            }

            enum Property<'a> {
                Computed(GenRegister),
                Named {
                    constant_index: GenConstantIndex,
                    name: AnyStr<'a>,
                    is_proto: bool,
                },
            }

            // Evaluate property name
            let mut key = if property.is_computed {
                Property::Computed(self.gen_expression(&property.key)?)
            } else {
                match property.key.as_ref() {
                    ast::Expression::Id(id) => {
                        let constant_index = self.add_string_constant(&id.name)?;
                        let name = AnyStr::from_id(id);
                        let is_proto = id.name == "__proto__";

                        Property::Named { constant_index, name, is_proto }
                    }
                    ast::Expression::String(string) => {
                        let constant_index = self.add_wtf8_string_constant(&string.value)?;
                        let name = AnyStr::Wtf8(&string.value);
                        let is_proto = string.value == "__proto__";

                        Property::Named { constant_index, name, is_proto }
                    }
                    ast::Expression::Number(_) | ast::Expression::BigInt(_) => {
                        Property::Computed(self.gen_expression(&property.key)?)
                    }
                    _ => unreachable!("invalid property key"),
                }
            };

            let mut flags = DefinePropertyFlags::empty();

            let value = if property.value.is_none() {
                // Identifier shorthand properties
                let value_id = property.key.as_ref().to_id();
                self.gen_load_identifier(value_id, ExprDest::Any)?
            } else if property.is_method {
                // Method properties

                // Determine method name from the property
                let mut name = match &key {
                    Property::Named { name, .. } => Some(name.to_wtf8_string()),
                    Property::Computed(_) => {
                        flags |= DefinePropertyFlags::NEEDS_NAME;
                        None
                    }
                };

                // Handle getter or setter methods
                if matches!(property.kind, ast::PropertyKind::Get | ast::PropertyKind::Set) {
                    // DefineProperty instruction needs a flag noting the accessor
                    let is_getter = matches!(property.kind, ast::PropertyKind::Get);
                    if is_getter {
                        flags |= DefinePropertyFlags::GETTER;
                    } else {
                        flags |= DefinePropertyFlags::SETTER;
                    }

                    // Add accessor prefix to the name if name is known
                    if let Some(known_name) = &name {
                        let mut prefixed_name = if is_getter {
                            Wtf8String::from_str("get ")
                        } else {
                            Wtf8String::from_str("set ")
                        };
                        prefixed_name.push_wtf8_str(known_name);
                        name = Some(prefixed_name);
                    }

                    // Always use a DefineProperty instruction with flags instead of a
                    // DefineNamedProperty instruction.
                    if let Property::Named { constant_index, .. } = &key {
                        let key_reg = self.register_allocator.allocate()?;
                        self.writer
                            .load_constant_instruction(key_reg, *constant_index);
                        key = Property::Computed(key_reg);
                    }
                }

                // Function node is added to the pending functions queue
                let func_node = if let ast::Expression::Function(func_node) =
                    &property.value.as_ref().unwrap().as_ref()
                {
                    func_node
                } else {
                    unreachable!("method property value is guaranteed to be a function");
                };

                let pending_node =
                    PendingFunctionNode::Method { node: AstPtr::from_ref(func_node), name };
                let method_constant_index = self.enqueue_function_to_generate(pending_node)?;

                // Method itself is loaded from the constant table
                let method_value = self.register_allocator.allocate()?;
                self.gen_new_closure_for_function(
                    func_node,
                    method_value,
                    ConstantIndex::new(method_constant_index),
                );

                method_value
            } else if let Property::Named { is_proto: true, .. } = &key {
                // The __proto__ property corresponds to a SetPrototypeOf instruction
                let prototype = self.gen_expression(property.value.as_ref().unwrap())?;
                self.writer.set_prototype_of_instruction(object, prototype);
                self.register_allocator.release(prototype);

                continue;
            } else {
                // Regular key-value properties. Value is statically evaluated as named if the key
                // is statically known, otherwise the DefinePropertyInstruction will handle setting
                // the name of the value at runtime if necessary.
                match &key {
                    Property::Named { name, .. } => self.gen_named_expression(
                        *name,
                        property.value.as_ref().unwrap(),
                        ExprDest::Any,
                    )?,
                    Property::Computed(_) => {
                        let value_expr = property.value.as_ref().unwrap();
                        if Self::expression_needs_name(value_expr) {
                            flags |= DefinePropertyFlags::NEEDS_NAME;
                        }

                        self.gen_expression_with_dest(value_expr, ExprDest::Any)?
                    }
                }
            };

            self.register_allocator.release(value);

            // Emit the correct define property instruction depending on whether the property is
            // a statically known string.
            match key {
                Property::Computed(key) => {
                    let flags = UInt::new(flags.bits() as u32);
                    self.writer
                        .define_property_instruction(object, key, value, flags);
                    self.register_allocator.release(key);
                }
                Property::Named { constant_index, .. } => {
                    self.writer
                        .define_named_property_instruction(object, constant_index, value);
                }
            }
        }

        self.gen_scope_end(body_scope);

        Ok(object)
    }

    fn gen_store_captured_home_object(
        &mut self,
        scope: &AstScopeNode,
        home_object: GenRegister,
    ) -> EmitResult<()> {
        match scope.get_binding(HOME_OBJECT_BINDING_NAME).vm_location() {
            // Home object is captured so it must be stored in the scope
            Some(VMLocation::Scope { scope_id, index }) => {
                self.gen_store_scope_binding(scope_id, index, home_object)
            }
            // Home object is not captured so it is not used
            None => Ok(()),
            _ => unreachable!("home object must be captured or have no VM location"),
        }
    }

    fn get_home_object_location(
        &mut self,
        scope: &AstScopeNode,
        name: &str,
    ) -> EmitResult<Option<HomeObjectLocation>> {
        match scope.get_binding(name).vm_location() {
            Some(VMLocation::Scope { scope_id, index }) => {
                let parent_depth = self.find_scope_depth(scope_id)?;
                let scope_index =
                    GenUInt::try_from_unsigned(index).ok_or(EmitError::IndexTooLarge)?;

                Ok(Some(HomeObjectLocation {
                    parent_depth: parent_depth.value(),
                    scope_index: scope_index.value(),
                }))
            }
            None => Ok(None),
            _ => unreachable!("home object must be captured or have no VM location"),
        }
    }

    /// Generate a member expression. Write the object's register to the `call_receiver` register if
    /// one is provided, if evaluating a callee. Otherwise release the object's register.
    fn gen_member_expression(
        &mut self,
        expr: &ast::MemberExpression,
        dest: ExprDest,
        mut call_receiver: Option<&mut CallReceiver>,
        optional_nullish_block: Option<BlockId>,
    ) -> EmitResult<GenRegister> {
        let mut object = self.gen_maybe_chain_part_expression(
            &expr.object,
            ExprDest::Any,
            None,
            optional_nullish_block,
        )?;

        // Save the object register in the CallReceiver if one is provided
        if let Some(call_receiver) = call_receiver.as_deref_mut() {
            // A chain expression could have short circuited and will have undefined written to the
            // reciever register. Make sure that the receiver is a temporary register to avoid
            // overwriting a fixed register.
            if call_receiver.is_chain {
                object = self.gen_ensure_reg_is_temporary(object)?;
            }

            call_receiver.receiver = object;
        }

        // If in an optional chain, check for nullish and jump to the nullish block if necessary
        if expr.is_optional {
            self.write_jump_nullish_instruction(object, optional_nullish_block.unwrap())?;
        }

        let result = self.gen_member_property(
            expr,
            object,
            dest,
            /* release_object */ call_receiver.is_none(),
        )?;

        Ok(result)
    }

    fn gen_member_property(
        &mut self,
        expr: &ast::MemberExpression,
        object: GenRegister,
        dest: ExprDest,
        release_object: bool,
    ) -> EmitResult<GenRegister> {
        if expr.is_computed {
            let key = self.gen_expression(&expr.property)?;

            self.register_allocator.release(key);
            if release_object {
                self.register_allocator.release(object);
            }

            let dest = self.allocate_destination(dest)?;
            self.writer.get_property_instruction(dest, object, key);

            Ok(dest)
        } else if expr.is_private {
            let key = self.gen_load_private_symbol(expr.property.to_id())?;

            self.register_allocator.release(key);
            if release_object {
                self.register_allocator.release(object);
            }

            let dest = self.allocate_destination(dest)?;
            self.writer
                .get_private_property_instruction(dest, object, key);

            Ok(dest)
        } else {
            // Must be a named access
            let name = expr.property.to_id();
            let name_constant_index = self.add_string_constant(&name.name)?;

            if release_object {
                self.register_allocator.release(object);
            }

            let dest = self.allocate_destination(dest)?;
            self.writer
                .get_named_property_instruction(dest, object, name_constant_index);

            Ok(dest)
        }
    }

    fn gen_create_private_symbol(&mut self, private_id: &ast::Identifier) -> EmitResult<()> {
        match private_id.get_private_name_binding().vm_location().unwrap() {
            VMLocation::Scope { scope_id, index } => {
                // Add the name to the constant table (without the "#" prefix)
                let name_index = self.add_string_constant(&private_id.name)?;

                // Create a new private symbol
                let dest = self.register_allocator.allocate()?;
                self.writer.new_private_symbol_instruction(dest, name_index);

                // And store into the class scope
                self.gen_store_scope_binding(scope_id, index, dest)?;
                self.register_allocator.release(dest);

                Ok(())
            }
            _ => unreachable!("private names must be stored in scope"),
        }
    }

    fn gen_load_private_symbol(&mut self, private_id: &ast::Identifier) -> EmitResult<GenRegister> {
        match private_id.scope.kind() {
            // If resolved then directly load from the scope directly
            ResolvedScope::Resolved => {
                match private_id.get_private_name_binding().vm_location().unwrap() {
                    VMLocation::Scope { scope_id, index } => {
                        self.gen_load_scope_binding(scope_id, index, ExprDest::Any)
                    }
                    _ => unreachable!("private names must be stored in scope"),
                }
            }
            // Otherwise must be dynamic, so perform a dynamic lookup
            ResolvedScope::UnresolvedDynamic => {
                let private_name = format!("#{}", &private_id.name);
                let private_name_index = self.add_string_constant(&private_name)?;

                let dest = self.register_allocator.allocate()?;
                self.writer
                    .load_dynamic_instruction(dest, private_name_index);

                Ok(dest)
            }
            ResolvedScope::UnresolvedGlobal => {
                unreachable!("private names must be dynamic or resolved to scope")
            }
        }
    }

    /// Generate an optional expression, writing the result to dest.
    ///
    /// Takes an optional `call_receiver` which is where the outermost expression's object register
    /// will be written if it is a member expression, if evaluating a callee.
    fn gen_chain_expression(
        &mut self,
        expr: &ast::ChainExpression,
        dest: ExprDest,
        mut call_receiver: Option<&mut CallReceiver>,
    ) -> EmitResult<GenRegister> {
        let join_block = self.new_block();
        let nullish_block = self.new_block();

        // Generate the inner expression, writing to destination register if successful
        let result = self.gen_maybe_chain_part_expression(
            &expr.expression,
            dest,
            call_receiver.as_deref_mut(),
            Some(nullish_block),
        )?;

        self.write_jump_instruction(join_block)?;

        // If the inner expression ever short circuits due to a nullish value, write undefined to
        // the destination register.
        self.start_block(nullish_block);
        self.writer.load_undefined_instruction(result);

        // If there is a receiver ref, meaning this chain is followed by a call, then write to the
        // receiver register so that it has a valid value to be passed to the call.
        if let Some(call_receiver) = call_receiver {
            self.writer
                .load_undefined_instruction(call_receiver.receiver);
        }

        self.start_block(join_block);
        Ok(result)
    }

    /// Generate an expression that may be part of an optional chain, propagating the optional
    /// chaining nullish block to expressions that may short circuit.
    ///
    /// Takes an optional `call_receiver` which is where the outermost expression's object register
    /// will be written if it is a member expression, if evaluating a callee.
    fn gen_maybe_chain_part_expression(
        &mut self,
        expr: &ast::Expression,
        dest: ExprDest,
        call_receiver: Option<&mut CallReceiver>,
        optional_nullish_block: Option<BlockId>,
    ) -> EmitResult<GenRegister> {
        match expr {
            ast::Expression::Member(member_expr) => {
                self.gen_member_expression(member_expr, dest, call_receiver, optional_nullish_block)
            }
            ast::Expression::SuperMember(super_expr) => {
                self.gen_super_member_expression(super_expr, dest, call_receiver)
            }
            ast::Expression::Call(call_expr) => {
                self.gen_call_expression(call_expr, dest, optional_nullish_block)
            }
            _ => self.gen_expression_with_dest(expr, dest),
        }
    }

    fn gen_assignment_expression(
        &mut self,
        expr: &ast::AssignmentExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        match expr.left.as_ref() {
            ast::Pattern::Id(id) => {
                // Right side expression is only named (meaning name is used for anonymous functions
                // and classes) if it is a simple id assignment and id is not parenthesized.
                let is_non_parenthesized_id_predicate = || {
                    id.loc.start == expr.loc.start
                        && (expr.operator == ast::AssignmentOperator::Equals
                            || expr.operator.is_logical())
                };

                let store_flags = StoreFlags::NEEDS_TDZ_CHECK;
                let stored_value_dest = self.expr_dest_for_id(id, store_flags);
                let mut join_block = 0;

                let stored_value = if expr.operator == ast::AssignmentOperator::Equals {
                    // For simple assignments, right hand side is placed directly in the dest
                    // register.
                    self.gen_named_expression_if(
                        AnyStr::from_id(id),
                        &expr.right,
                        stored_value_dest,
                        is_non_parenthesized_id_predicate,
                    )?
                } else if !expr.operator.is_logical() {
                    // For operator assignments the old value and right value are loaded to
                    // temporary registers, with only the result placed in the dest.
                    let old_value = self.gen_load_identifier(id, ExprDest::Any)?;

                    let right_value = self.gen_named_expression_if(
                        AnyStr::from_id(id),
                        &expr.right,
                        ExprDest::Any,
                        is_non_parenthesized_id_predicate,
                    )?;

                    self.register_allocator.release(right_value);
                    self.register_allocator.release(old_value);
                    let stored_value = self.allocate_destination(stored_value_dest)?;

                    self.gen_assignment_operator(
                        expr.operator,
                        stored_value,
                        old_value,
                        right_value,
                    );

                    stored_value
                } else {
                    // Logical operator assignments
                    let old_value = self.gen_load_identifier(id, ExprDest::Any)?;

                    // If this is a logical assignment then short circuit if necessary
                    join_block = self.new_block();
                    self.gen_logical_assignment_jump(expr.operator, old_value, join_block)?;
                    self.register_allocator.release(old_value);

                    // If evaluating right side, evaluate directly into dest register
                    self.gen_named_expression_if(
                        AnyStr::from_id(id),
                        &expr.right,
                        stored_value_dest,
                        is_non_parenthesized_id_predicate,
                    )?
                };

                self.gen_store_identifier(id, stored_value, store_flags)?;

                if expr.operator.is_logical() {
                    self.start_block(join_block);
                }

                self.gen_mov_reg_to_dest(stored_value, dest)
            }
            member @ (ast::Pattern::Member(_) | ast::Pattern::SuperMember(_)) => {
                enum Property {
                    Computed(GenRegister),
                    Named(GenConstantIndex),
                    Private(GenRegister),
                    Super { key: GenRegister, this_value: GenRegister },
                }

                // Use a temporary register since intermediate values will be written, and we do
                // not want to clobber an observable dest register. If dest is temporary then use
                // instead of allocating a new temporary register.
                let temp_dest = self.gen_ensure_dest_is_temporary(dest);
                let temp = self.allocate_destination(temp_dest)?;

                // Evaluate the object expression, or find the home object if `super`
                let object = match member {
                    ast::Pattern::Member(member) => self.gen_expression(&member.object)?,
                    ast::Pattern::SuperMember(member) => {
                        self.gen_load_home_object(member, ExprDest::Any)?
                    }
                    _ => unreachable!("must be a member or super member pattern"),
                };

                // Emit the property itself, which may be a computed expression or literal name
                let property = if let ast::Pattern::SuperMember(member) = member {
                    let this_value = self.gen_load_this(member.this_scope, ExprDest::Any)?;

                    if member.is_computed {
                        let key = self.gen_expression(&member.property)?;
                        Property::Super { key, this_value }
                    } else {
                        // Must be a super named access, but we do not have a SetNamedSuperProperty
                        // instruction so load to a register.
                        let name = member.property.to_id();
                        let name_constant_index = self.add_string_constant(&name.name)?;

                        let key = self.register_allocator.allocate()?;
                        self.writer
                            .load_constant_instruction(key, name_constant_index);

                        Property::Super { key, this_value }
                    }
                } else {
                    let member = if let ast::Pattern::Member(member) = member {
                        member
                    } else {
                        unreachable!("must be a member or super member pattern")
                    };

                    if member.is_computed {
                        Property::Computed(self.gen_expression(&member.property)?)
                    } else if member.is_private {
                        Property::Private(self.gen_load_private_symbol(member.property.to_id())?)
                    } else {
                        // Must be a named access
                        let name = member.property.to_id();
                        let name_constant_index = self.add_string_constant(&name.name)?;
                        Property::Named(name_constant_index)
                    }
                };

                let mut join_block = 0;

                if expr.operator == ast::AssignmentOperator::Equals {
                    // For simple assignments, right hand side is placed directly in the dest
                    // register.
                    self.gen_expression_with_dest(&expr.right, ExprDest::Fixed(temp))?;
                } else {
                    // For operator assignments the old value is placed in the dest register, then
                    // overwritten with the result of the operator.
                    match property {
                        Property::Computed(key) => {
                            self.writer.get_property_instruction(temp, object, key)
                        }
                        Property::Named(name_constant_index) => self
                            .writer
                            .get_named_property_instruction(temp, object, name_constant_index),
                        Property::Private(key) => self
                            .writer
                            .get_private_property_instruction(temp, object, key),
                        Property::Super { key, this_value } => self
                            .writer
                            .get_super_property_instruction(temp, object, this_value, key),
                    }

                    if !expr.operator.is_logical() {
                        // If this is an operator assignment, generate right then apply operator
                        let right_value = self.gen_expression(&expr.right)?;

                        self.gen_assignment_operator(expr.operator, temp, temp, right_value);

                        self.register_allocator.release(right_value);
                    } else {
                        // If this is a logical assignment, then short circuit if necessary
                        if expr.operator.is_logical() {
                            join_block = self.new_block();
                            self.gen_logical_assignment_jump(expr.operator, temp, join_block)?;
                        }

                        // If evaluating right side, evaluate directly into dest register
                        self.gen_expression_with_dest(&expr.right, ExprDest::Fixed(temp))?;
                    }
                }

                // Write the value back to the property
                match property {
                    Property::Computed(key) => {
                        self.writer.set_property_instruction(object, key, temp);
                        self.register_allocator.release(key);
                    }
                    Property::Named(name_constant_index) => {
                        self.writer.set_named_property_instruction(
                            object,
                            name_constant_index,
                            temp,
                        );
                    }
                    Property::Private(key) => {
                        self.writer
                            .set_private_property_instruction(object, key, temp);
                        self.register_allocator.release(key);
                    }
                    Property::Super { key, this_value } => {
                        self.writer
                            .set_super_property_instruction(object, this_value, key, temp);
                        self.register_allocator.release(key);
                        self.register_allocator.release(this_value);
                    }
                }

                self.register_allocator.release(object);

                if expr.operator.is_logical() {
                    self.start_block(join_block);
                }

                self.gen_mov_reg_to_dest(temp, dest)
            }
            // Destructuring assignment
            pattern @ (ast::Pattern::Object(_) | ast::Pattern::Array(_)) => {
                let store_flags = StoreFlags::NEEDS_TDZ_CHECK;
                let stored_value_dest =
                    self.expr_dest_for_destructuring_assignment(pattern, store_flags);
                let stored_value = self.gen_expression_with_dest(&expr.right, stored_value_dest)?;

                self.gen_store_to_pattern(pattern, stored_value, store_flags)?;
                self.gen_mov_reg_to_dest(stored_value, dest)
            }
            ast::Pattern::Assign(_) => {
                unreachable!("invalid assigment left hand side")
            }
        }
    }

    fn gen_assignment_operator(
        &mut self,
        operator: ast::AssignmentOperator,
        dest: GenRegister,
        left: GenRegister,
        right: GenRegister,
    ) {
        match operator {
            ast::AssignmentOperator::Add => self.writer.add_instruction(dest, left, right),
            ast::AssignmentOperator::Subtract => self.writer.sub_instruction(dest, left, right),
            ast::AssignmentOperator::Multiply => self.writer.mul_instruction(dest, left, right),
            ast::AssignmentOperator::Divide => self.writer.div_instruction(dest, left, right),
            ast::AssignmentOperator::Remainder => self.writer.rem_instruction(dest, left, right),
            ast::AssignmentOperator::Exponent => self.writer.exp_instruction(dest, left, right),
            ast::AssignmentOperator::And => self.writer.bit_and_instruction(dest, left, right),
            ast::AssignmentOperator::Or => self.writer.bit_or_instruction(dest, left, right),
            ast::AssignmentOperator::Xor => self.writer.bit_xor_instruction(dest, left, right),
            ast::AssignmentOperator::ShiftLeft => {
                self.writer.shift_left_instruction(dest, left, right)
            }
            ast::AssignmentOperator::ShiftRightArithmetic => self
                .writer
                .shift_right_arithmetic_instruction(dest, left, right),
            ast::AssignmentOperator::ShiftRightLogical => self
                .writer
                .shift_right_logical_instruction(dest, left, right),
            ast::AssignmentOperator::Equals => unreachable!("bytecode for simple assignment"),
            ast::AssignmentOperator::LogicalAnd
            | ast::AssignmentOperator::LogicalOr
            | ast::AssignmentOperator::NullishCoalesce => {
                unreachable!("logical assignment operator")
            }
        }
    }

    fn gen_logical_assignment_jump(
        &mut self,
        operator: ast::AssignmentOperator,
        left: GenRegister,
        join_block: BlockId,
    ) -> EmitResult<()> {
        match operator {
            ast::AssignmentOperator::LogicalAnd => {
                self.write_jump_to_boolean_false_instruction(left, join_block)
            }
            ast::AssignmentOperator::LogicalOr => {
                self.write_jump_to_boolean_true_instruction(left, join_block)
            }
            ast::AssignmentOperator::NullishCoalesce => {
                self.write_jump_not_nullish_instruction(left, join_block)
            }
            _ => unreachable!("invalid logical assignment operator"),
        }
    }

    fn gen_update_expression(
        &mut self,
        expr: &ast::UpdateExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        if let member @ (ast::Expression::Member(_) | ast::Expression::SuperMember(_)) =
            expr.argument.as_ref()
        {
            enum Property {
                Computed(GenRegister),
                Named(GenConstantIndex),
                Private(GenRegister),
                Super { key: GenRegister, this_value: GenRegister },
            }

            // We will need a temporary register to hold intermediate values. This temporary
            // register will eventually hold the value to return, so use the return value directly
            // if there is no risk of observable clobbering.
            let temp_dest = self.gen_ensure_dest_is_temporary(dest);
            let temp = self.allocate_destination(temp_dest)?;

            // Evaluate the object expression, or find the home object if `super`
            let object = match member {
                ast::Expression::Member(member) => self.gen_expression(&member.object)?,
                ast::Expression::SuperMember(member) => {
                    self.gen_load_home_object(member, ExprDest::Any)?
                }
                _ => unreachable!("must be a member or super member pattern"),
            };

            let property = if let ast::Expression::SuperMember(member) = member {
                let this_value = self.gen_load_this(member.this_scope, ExprDest::Any)?;

                if member.is_computed {
                    let key = self.gen_expression(&member.property)?;
                    Property::Super { key, this_value }
                } else {
                    // Must be a super named access, but we do not have a SetNamedSuperProperty
                    // instruction so load to a register.
                    let name = member.property.to_id();
                    let name_constant_index = self.add_string_constant(&name.name)?;

                    let key = self.register_allocator.allocate()?;
                    self.writer
                        .load_constant_instruction(key, name_constant_index);

                    Property::Super { key, this_value }
                }
            } else {
                let member = if let ast::Expression::Member(member) = member {
                    member
                } else {
                    unreachable!("must be a member or super member pattern")
                };

                // Load the property to the temporary register
                if member.is_computed {
                    let key = self.gen_expression(&member.property)?;
                    self.writer.get_property_instruction(temp, object, key);

                    Property::Computed(key)
                } else if member.is_private {
                    let key = self.gen_load_private_symbol(member.property.to_id())?;
                    self.writer
                        .get_private_property_instruction(temp, object, key);

                    Property::Private(key)
                } else {
                    // Must be a named access
                    let name = member.property.to_id();
                    let name_constant_index = self.add_string_constant(&name.name)?;
                    self.writer
                        .get_named_property_instruction(temp, object, name_constant_index);

                    Property::Named(name_constant_index)
                }
            };

            // Perform the converstion and inc/dec operation in place on the temporary register
            self.writer.to_numeric_instruction(temp, temp);

            // Get a register to hold the new, modified value.
            //
            // Prefix upates return the modified value so we can perform the operation in place.
            // Postfix updates return the old value, so move it to another temporary.
            let modified_temp = if expr.is_prefix {
                temp
            } else {
                let modified_temp = self.register_allocator.allocate()?;
                self.write_mov_instruction(modified_temp, temp);
                self.register_allocator.release(modified_temp);
                modified_temp
            };

            self.write_inc_or_dec(expr.operator, modified_temp);

            // Then write modified value back to the property
            match property {
                Property::Computed(key) => {
                    self.writer
                        .set_property_instruction(object, key, modified_temp);
                    self.register_allocator.release(key);
                }
                Property::Named(name_constant_index) => {
                    self.writer.set_named_property_instruction(
                        object,
                        name_constant_index,
                        modified_temp,
                    );
                }
                Property::Private(key) => {
                    self.writer
                        .set_private_property_instruction(object, key, modified_temp);
                    self.register_allocator.release(key);
                }
                Property::Super { key, this_value } => {
                    self.writer.set_super_property_instruction(
                        object,
                        this_value,
                        key,
                        modified_temp,
                    );
                    self.register_allocator.release(key);
                    self.register_allocator.release(this_value);
                }
            }

            self.register_allocator.release(object);

            self.gen_mov_reg_to_dest(temp, dest)
        } else {
            // Otherwise must be an id assignment
            let id = expr.argument.to_id();
            let store_flags = StoreFlags::empty();

            let mut dest = dest;
            let mut old_value_dest = ExprDest::Any;

            if let ResolvedScope::Resolved = id.scope.kind() {
                let binding = id.get_binding();

                // If we are reassigning an immutable binding then we should immediately throw an
                // error. This should be done before calling ToNumeric or inc/dec since those
                // operations may be performed directly on a local register, which would be
                // observable.
                if self.is_immutable_reassignment(binding, store_flags) {
                    let name_constant_index = self.add_string_constant(&id.name)?;
                    self.writer.error_const_instruction(name_constant_index);
                }

                // Exclusively use temporary registers instead of potentially operating in place on
                // local registers to avoid accidentally assigning local registers.
                if self.is_noop_reassignment(binding, store_flags) {
                    dest = ExprDest::NewTemporary;
                    old_value_dest = ExprDest::NewTemporary;
                }
            }

            if expr.is_prefix {
                // Prefix operations return the modified value so we can perform operations in place
                let old_value = self.gen_load_identifier(id, old_value_dest)?;

                if self.register_allocator.is_temporary_register(old_value) {
                    // If the target value is in a temporary register then we can place the numeric
                    // value directly in the return register and perform operations in place. It is
                    // safe to write the return register with ToNumeric since inc/dec cannot fail.
                    self.register_allocator.release(old_value);
                    let dest = self.allocate_destination(dest)?;

                    self.writer.to_numeric_instruction(dest, old_value);
                    self.write_inc_or_dec(expr.operator, dest);
                    self.gen_store_identifier(id, dest, store_flags)?;

                    Ok(dest)
                } else {
                    // If the target value is in a param or non-temporary local register then can
                    // perform all operations in place. Safe to overwrite with ToNumeric since
                    // inc/dec cannot fail.
                    self.writer.to_numeric_instruction(old_value, old_value);
                    self.write_inc_or_dec(expr.operator, old_value);
                    self.gen_mov_reg_to_dest(old_value, dest)
                }
            } else {
                // Postfix operations return the old value, so we must make sure it is saved and
                // not clobbered. It is safe to overwrite with ToNumeric since inc/dec cannot fail.
                let dest = self.allocate_destination(dest)?;
                let old_value = self.gen_load_identifier(id, old_value_dest)?;
                self.writer.to_numeric_instruction(old_value, old_value);

                // If id is at a fixed register which matches the destination then writing the
                // modified value to the id's location would clobber the old value. But in this case
                // the desired behavior is to not actually increment/decrement the value. We just
                // need to perform the in-place numeric conversion.
                if let ExprDest::Fixed(fixed_id_reg) = self.expr_dest_for_id(id, store_flags) {
                    if fixed_id_reg == dest {
                        return Ok(dest);
                    }
                }

                // Save the old value to be returned later
                self.write_mov_instruction(dest, old_value);

                // Otherwise we are guaranteed that writing the modified value to the id's location
                // will not clobber the old value. Perform the inc/dec at the id's location.
                self.write_inc_or_dec(expr.operator, old_value);
                self.gen_store_identifier(id, old_value, store_flags)?;
                self.register_allocator.release(old_value);

                Ok(dest)
            }
        }
    }

    fn write_inc_or_dec(&mut self, operator: ast::UpdateOperator, value: GenRegister) {
        match operator {
            ast::UpdateOperator::Increment => self.writer.inc_instruction(value),
            ast::UpdateOperator::Decrement => self.writer.dec_instruction(value),
        }
    }

    fn gen_named_expression(
        &mut self,
        name: AnyStr,
        expr: &ast::Expression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        self.gen_named_expression_if(name, expr, dest, || true)
    }

    fn gen_named_expression_if(
        &mut self,
        name: AnyStr,
        expr: &ast::Expression,
        dest: ExprDest,
        if_predicate: impl Fn() -> bool,
    ) -> EmitResult<GenRegister> {
        if !if_predicate() {
            return self.gen_expression_with_dest(expr, dest);
        }

        match expr {
            ast::Expression::Function(func) if func.id.is_none() => {
                self.gen_function_expression(func, Some(name.to_wtf8_string()), dest)
            }
            ast::Expression::ArrowFunction(func) => {
                self.gen_arrow_function_expression(func, Some(name.to_wtf8_string()), dest)
            }
            ast::Expression::Class(class) if class.id.is_none() => {
                self.gen_class_expression(class, Some(name.to_wtf8_string()), dest)
            }
            _ => self.gen_expression_with_dest(expr, dest),
        }
    }

    fn gen_named_outer_expression(
        &mut self,
        name: AnyStr,
        expr: &ast::OuterExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // Starts a "has assign expression" context
        self.enter_has_assign_expr_context(expr.has_assign_expr);

        let result = if Self::expression_needs_name(&expr.expr) {
            self.gen_named_expression(name, &expr.expr, dest)
        } else {
            self.gen_expression_with_dest(&expr.expr, dest)
        };

        self.exit_has_assign_expr_context();

        result
    }

    fn expression_needs_name(expr: &ast::Expression) -> bool {
        match expr {
            ast::Expression::Function(func) if func.id.is_none() => true,
            ast::Expression::ArrowFunction(_) => true,
            ast::Expression::Class(class) if class.id.is_none() => true,
            _ => false,
        }
    }

    fn gen_function_expression(
        &mut self,
        func: &ast::Function,
        name: Option<Wtf8String>,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let pending_node = PendingFunctionNode::Expression { node: AstPtr::from_ref(func), name };
        let func_constant_index = self.enqueue_function_to_generate(pending_node)?;

        let dest = self.allocate_destination(dest)?;
        self.gen_new_closure_for_function(func, dest, ConstantIndex::new(func_constant_index));

        Ok(dest)
    }

    fn gen_arrow_function_expression(
        &mut self,
        func: &ast::Function,
        name: Option<Wtf8String>,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let pending_node = PendingFunctionNode::Arrow { node: AstPtr::from_ref(func), name };
        let func_constant_index = self.enqueue_function_to_generate(pending_node)?;

        let dest = self.allocate_destination(dest)?;
        self.gen_new_closure_for_function(func, dest, ConstantIndex::new(func_constant_index));

        Ok(dest)
    }

    fn gen_await_expression(
        &mut self,
        expr: &ast::AwaitExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let argument = self.gen_expression(&expr.argument)?;
        self.gen_await(argument, dest)
    }

    fn gen_await(&mut self, value: GenRegister, dest: ExprDest) -> EmitResult<GenRegister> {
        // Only release if destination is not the same as the source register
        if !matches!(dest, ExprDest::Fixed(dest) if dest == value) {
            self.register_allocator.release(value);
        }

        let completion_value = self.allocate_destination(dest)?;
        let completion_type = self.register_allocator.allocate()?;

        // For regular async functions return the promise stored in the promise register
        let return_promise = if let Some(promise_index) = self.promise_index {
            Register::local(promise_index as usize)
        } else {
            // Otherwise this is an async generator so pass the generator into the await.
            Register::local(self.generator_index.unwrap() as usize)
        };

        self.writer
            .await_instruction(completion_value, completion_type, return_promise, value);

        // Check the completion type, and if normal then continue execution using the completion
        // value as the value of the await expression.
        let normal_block = self.new_block();
        self.write_jump_true_instruction(completion_type, normal_block)?;
        self.register_allocator.release(completion_type);

        // Otherwise completion type must have been a throw so rethrow the error
        self.writer.throw_instruction(completion_value);

        self.start_block(normal_block);

        Ok(completion_value)
    }

    fn gen_yield_expression(
        &mut self,
        expr: &ast::YieldExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let yield_value = if let Some(argument) = expr.argument.as_ref() {
            self.gen_expression(argument)?
        } else {
            let yield_value = self.register_allocator.allocate()?;
            self.writer.load_undefined_instruction(yield_value);
            yield_value
        };

        if expr.is_delegate {
            self.gen_yield_star(yield_value, dest)
        } else if self.is_async() {
            self.gen_async_yield(yield_value, dest)
        } else {
            self.gen_yield(yield_value, dest)
        }
    }

    fn gen_yield(&mut self, value: GenRegister, dest: ExprDest) -> EmitResult<GenRegister> {
        self.register_allocator.release(value);

        let completion_value = self.allocate_destination(dest)?;
        let temp_value = self.register_allocator.allocate()?;

        // Write the value to the temp value register
        self.write_mov_instruction(temp_value, value);

        // Create an iterator result object that holds the yielded value
        let iter_result = self.register_allocator.allocate()?;
        self.writer.new_object_instruction(iter_result);

        // Iterator result object holds the yielded value
        let value_constant_index = self.add_string_constant("value")?;
        self.writer
            .set_named_property_instruction(iter_result, value_constant_index, temp_value);

        // Iterator result object is marked as not done
        let done_constant_index = self.add_string_constant("done")?;
        self.writer.load_false_instruction(temp_value);
        self.writer
            .set_named_property_instruction(iter_result, done_constant_index, temp_value);

        // Find the generator register from the stored index
        let generator = Register::local(self.generator_index.unwrap() as usize);

        // Reuse the temp value register as the completion type for the remainder of the yield
        let completion_type = temp_value;

        self.writer
            .yield_instruction(completion_value, completion_type, generator, iter_result);
        self.register_allocator.release(iter_result);

        // Check the completion type and handle accordingly
        let normal_block = self.new_block();
        let throw_block = self.new_block();

        // All abnormal completions are nullish, so jump directly to the normal block
        self.write_jump_not_nullish_instruction(completion_type, normal_block)?;

        // Otherwise check if this is a return or throw completion
        self.write_jump_not_undefined_instruction(completion_type, throw_block)?;

        self.register_allocator.release(completion_type);

        // Must be a return completion if execution falls through.
        self.gen_return(Some(completion_value), /* derived_constructor_scope */ None)?;

        // Otherwise is a throw completion
        self.start_block(throw_block);
        self.writer.throw_instruction(completion_value);

        self.start_block(normal_block);

        Ok(completion_value)
    }

    fn gen_async_yield(&mut self, value: GenRegister, dest: ExprDest) -> EmitResult<GenRegister> {
        let awaited_value = self.gen_await(value, ExprDest::Any)?;

        self.register_allocator.release(awaited_value);
        let completion_value = self.allocate_destination(dest)?;
        let completion_type = self.register_allocator.allocate()?;

        // Find the generator register from the stored index
        let generator = Register::local(self.generator_index.unwrap() as usize);

        self.writer
            .yield_instruction(completion_value, completion_type, generator, awaited_value);

        // Check the completion type and handle accordingly
        let normal_block = self.new_block();
        let throw_block = self.new_block();

        // All abnormal completions are nullish, so jump directly to the normal block
        self.write_jump_not_nullish_instruction(completion_type, normal_block)?;

        // Otherwise check if this is a return or throw completion
        self.write_jump_not_undefined_instruction(completion_type, throw_block)?;

        self.register_allocator.release(completion_type);

        // Must be a return completion if execution falls through. Must await the completion and
        // then return it.
        self.gen_await(completion_value, ExprDest::Fixed(completion_value))?;
        self.gen_return(Some(completion_value), /* derived_constructor_scope */ None)?;

        // Otherwise is a throw completion
        self.start_block(throw_block);
        self.writer.throw_instruction(completion_value);

        self.start_block(normal_block);

        Ok(completion_value)
    }

    fn gen_yield_star(&mut self, argument: GenRegister, dest: ExprDest) -> EmitResult<GenRegister> {
        self.register_allocator.release(argument);

        let dest = self.allocate_destination(dest)?;
        let iterator = self.register_allocator.allocate()?;
        let next_method = self.register_allocator.allocate()?;

        if self.is_async() {
            self.writer
                .get_async_iterator_instruction(iterator, next_method, argument);
        } else {
            self.writer
                .get_iterator_instruction(iterator, next_method, argument);
        }

        let done_constant_index = self.add_string_constant("done")?;
        let value_constant_index = self.add_string_constant("value")?;
        let throw_constant_index = self.add_string_constant("throw")?;
        let return_constant_index = self.add_string_constant("return")?;

        let completion_value = self.register_allocator.allocate()?;
        let completion_type = self.register_allocator.allocate()?;

        // Start with a normal, undefined completion
        self.writer.load_undefined_instruction(completion_value);
        self.writer.load_true_instruction(completion_type);

        let iterator_result = self.register_allocator.allocate()?;
        let is_done = self.register_allocator.allocate()?;

        let loop_start = self.new_block();
        let abnormal_block = self.new_block();
        let throw_block = self.new_block();
        let loop_yield_footer = self.new_block();
        let join_block = self.new_block();

        self.start_block(loop_start);

        // All abnormal completions are nullish so check if we can proceed to the normal block
        self.write_jump_nullish_instruction(completion_type, abnormal_block)?;

        // Normal completion block starts by calling the iterator's next method, awaiting result
        self.writer.call_with_receiver_instruction(
            iterator_result,
            next_method,
            iterator,
            completion_value,
            UInt::new(1),
        );

        if self.is_async() {
            self.gen_await(iterator_result, ExprDest::Fixed(iterator_result))?;
        }

        // Check if the iterator result is valid then check if iterator is done
        self.writer
            .check_iterator_result_object_instruction(iterator_result);
        self.writer
            .get_named_property_instruction(is_done, iterator_result, done_constant_index);

        // If iterator is not done then continue to yield
        let done_block = self.new_block();
        self.write_jump_true_instruction(is_done, done_block)?;
        self.write_jump_instruction(loop_yield_footer)?;

        // If iterator is done then yield* evaluates to the result object's current value
        self.start_block(done_block);
        self.writer
            .get_named_property_instruction(dest, iterator_result, value_constant_index);
        self.write_jump_instruction(join_block)?;

        // Yield had an abnormal completion - check if this is a return or throw
        self.start_block(abnormal_block);
        self.write_jump_not_undefined_instruction(completion_type, throw_block)?;

        // Must be a return completion if execution falls through. Extract the iterator's return
        // method if it exists.
        let return_method = self.register_allocator.allocate()?;
        self.writer
            .get_method_instruction(return_method, iterator, return_constant_index);

        // If return method does not exist then return the (awaited) completion value
        let has_return_method_block = self.new_block();
        self.write_jump_not_undefined_instruction(return_method, has_return_method_block)?;

        if self.is_async() {
            self.gen_await(completion_value, ExprDest::Fixed(completion_value))?;
        }

        self.gen_return(Some(completion_value), /* derived_constructor_scope */ None)?;

        // If return method does exist then call it
        self.start_block(has_return_method_block);
        self.writer.call_with_receiver_instruction(
            iterator_result,
            return_method,
            iterator,
            completion_value,
            UInt::new(1),
        );
        self.register_allocator.release(return_method);

        if self.is_async() {
            self.gen_await(iterator_result, ExprDest::Fixed(iterator_result))?;
        }

        // Check if the iterator result is valid then check if iterator is done
        self.writer
            .check_iterator_result_object_instruction(iterator_result);
        self.writer
            .get_named_property_instruction(is_done, iterator_result, done_constant_index);

        // If iterator is not done then continue to yield
        let done_block = self.new_block();
        self.write_jump_true_instruction(is_done, done_block)?;
        self.write_jump_instruction(loop_yield_footer)?;

        // If iterator is done then return the result object's current value
        self.start_block(done_block);
        let return_value = self.register_allocator.allocate()?;
        self.writer.get_named_property_instruction(
            return_value,
            iterator_result,
            value_constant_index,
        );
        self.gen_return(Some(return_value), /* derived_constructor_scope */ None)?;
        self.register_allocator.release(return_value);

        // Handle a throw completion from the yield
        self.start_block(throw_block);

        // Extract the throw method from the iterator if one exists
        let throw_method = self.register_allocator.allocate()?;
        self.writer
            .get_method_instruction(throw_method, iterator, throw_constant_index);

        // If throw method does not exist then close the iterator and throw an error
        let has_throw_method_block = self.new_block();
        self.write_jump_not_undefined_instruction(throw_method, has_throw_method_block)?;

        if self.is_async() {
            self.gen_async_iterator_close(iterator)?;
        } else {
            self.writer.iterator_close_instruction(iterator);
        }

        self.writer.error_iterator_no_throw_method_instruction();

        // If throw method does exist then call it
        self.start_block(has_throw_method_block);
        self.writer.call_with_receiver_instruction(
            iterator_result,
            throw_method,
            iterator,
            completion_value,
            UInt::new(1),
        );
        self.register_allocator.release(throw_method);

        if self.is_async() {
            self.gen_await(iterator_result, ExprDest::Fixed(iterator_result))?;
        }

        // Check if the iterator result is valid then check if iterator is done
        self.writer
            .check_iterator_result_object_instruction(iterator_result);
        self.writer
            .get_named_property_instruction(is_done, iterator_result, done_constant_index);

        // If iterator is not done then continue to yield
        let done_block = self.new_block();
        self.write_jump_true_instruction(is_done, done_block)?;
        self.write_jump_instruction(loop_yield_footer)?;

        // If iterator is done then yield* evaluates to the result object's current value
        self.start_block(done_block);
        self.writer
            .get_named_property_instruction(dest, iterator_result, value_constant_index);
        self.write_jump_instruction(join_block)?;

        // If the iterator is not done we end here, performing a yield then starting another
        // iteration of the loop.
        self.start_block(loop_yield_footer);

        // Find the generator register from the stored index
        let generator = Register::local(self.generator_index.unwrap() as usize);

        if !self.is_async() {
            // Yielding in a sync generator is easy - simply yield the entire iter result object
            self.writer.yield_instruction(
                completion_value,
                completion_type,
                generator,
                iterator_result,
            );
        } else {
            // Async generators must extract the value from the iter result object and yield it
            let value = self.register_allocator.allocate()?;
            self.writer.get_named_property_instruction(
                value,
                iterator_result,
                value_constant_index,
            );

            self.writer
                .yield_instruction(completion_value, completion_type, generator, value);
            self.register_allocator.release(value);

            // If yield had a non-return completion then start loop again
            self.write_jump_not_undefined_instruction(completion_type, loop_start)?;

            // Await the completion value and store as a new completion, which may be a normal or
            // throw completion.
            self.writer.await_instruction(
                completion_value,
                completion_type,
                generator,
                completion_value,
            );

            // If completion is now a throw, use it directly as the new completion for next
            // iteration of the loop.
            self.write_jump_nullish_instruction(completion_type, loop_start)?;

            // Otherwise completion was normal - convert to a return completion and continue to next
            // iteration of loop.
            self.writer.load_undefined_instruction(completion_type);
        }

        // Start the loop again once the yield completes
        self.write_jump_instruction(loop_start)?;

        self.start_block(join_block);

        self.register_allocator.release(is_done);
        self.register_allocator.release(iterator_result);
        self.register_allocator.release(completion_type);
        self.register_allocator.release(completion_value);
        self.register_allocator.release(next_method);
        self.register_allocator.release(iterator);

        Ok(dest)
    }

    /// Generate a super member expression. Write the object's register to the `call_receiver` register if
    /// one is provided, if evaluating a callee. Otherwise release the object's register.
    fn gen_super_member_expression(
        &mut self,
        expr: &ast::SuperMemberExpression,
        dest: ExprDest,
        mut call_receiver: Option<&mut CallReceiver>,
    ) -> EmitResult<GenRegister> {
        // Load `this` value and home object
        let this_value = self.gen_load_this(expr.this_scope, ExprDest::Any)?;
        let home_object = self.gen_load_home_object(expr, ExprDest::Any)?;

        // Store `this` value in the CallReceiver if one is provided
        if let Some(call_receiver) = call_receiver.as_deref_mut() {
            call_receiver.receiver = this_value;
        }

        let result = self.gen_super_member_property(
            expr,
            home_object,
            this_value,
            dest,
            /* release_receiver */ call_receiver.is_none(),
        )?;

        Ok(result)
    }

    fn gen_super_member_property(
        &mut self,
        expr: &ast::SuperMemberExpression,
        home_object: GenRegister,
        receiver: GenRegister,
        dest: ExprDest,
        release_receiver: bool,
    ) -> EmitResult<GenRegister> {
        if expr.is_computed {
            let key = self.gen_expression(&expr.property)?;

            self.register_allocator.release(key);
            self.register_allocator.release(home_object);
            if release_receiver {
                self.register_allocator.release(receiver);
            }

            let dest = self.allocate_destination(dest)?;
            self.writer
                .get_super_property_instruction(dest, home_object, receiver, key);

            Ok(dest)
        } else {
            // Must be a named access
            let name = expr.property.to_id();
            let name_constant_index = self.add_string_constant(&name.name)?;

            self.register_allocator.release(home_object);
            if release_receiver {
                self.register_allocator.release(receiver);
            }

            let dest = self.allocate_destination(dest)?;
            self.writer.get_named_super_property_instruction(
                dest,
                home_object,
                receiver,
                name_constant_index,
            );

            Ok(dest)
        }
    }

    fn gen_load_home_object(
        &mut self,
        expr: &ast::SuperMemberExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let home_object_name = expr.home_object_name();
        self.gen_resolved_or_dynamic_binding(expr.home_object_scope, home_object_name, dest)
    }

    fn gen_super_call_expression(
        &mut self,
        super_call: &ast::SuperCallExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // Super calls implicitly use the surrounding derived constructor, so load it to register
        let derived_constructor = self.gen_resolved_or_dynamic_binding(
            super_call.constructor_scope,
            DERIVED_CONSTRUCTOR_BINDING_NAME,
            ExprDest::Any,
        )?;

        self.register_allocator.release(derived_constructor);
        let super_constructor = self.register_allocator.allocate()?;

        self.writer
            .get_super_constructor_instruction(super_constructor, derived_constructor);

        // Super calls implicitly use the new.target, so load it to a register
        let new_target =
            self.gen_new_target_expression(super_call.new_target_scope, ExprDest::Any)?;

        // Generate the arguments to the super call
        let args = self.gen_call_arguments(&super_call.arguments, new_target)?;

        // Release remaining call registers before allocating dest
        if let CallArgs::Varargs { args, .. } = &args {
            self.register_allocator.release(*args);
        }
        self.register_allocator.release(new_target);
        self.register_allocator.release(super_constructor);

        // Generate the super call itself
        let call_result = self.allocate_destination(dest)?;

        match args {
            CallArgs::Varargs { args, .. } => {
                self.writer.construct_varargs_instruction(
                    call_result,
                    super_constructor,
                    new_target,
                    args,
                );
            }
            CallArgs::Normal { argv, argc } => {
                self.writer.construct_instruction(
                    call_result,
                    super_constructor,
                    new_target,
                    argv,
                    argc,
                );
            }
        }

        // Load `this` value to a register if `this` was captured
        let mut captured_this = None;
        let this_value = if let Some(VMLocation::Scope { scope_id, index }) = super_call
            .this_scope
            .as_ref()
            .get_binding("this")
            .vm_location()
        {
            captured_this = Some((scope_id, index));
            self.gen_load_scope_binding(scope_id, index, ExprDest::Any)?
        } else {
            Register::this()
        };

        // Check if super was already called and initialized `this` value, erroring if so
        self.writer
            .check_super_already_called_instruction(this_value);

        self.register_allocator.release(this_value);

        // Always store the result of the super call as the value of `this`
        if let Some((scope_id, index)) = captured_this {
            self.gen_store_scope_binding(scope_id, index, call_result)?;
        } else {
            self.write_mov_instruction(Register::this(), call_result);
        }

        // Derived constructors initialize fields after the super call
        if self.is_derived_constructor() {
            self.gen_initialize_class_fields(call_result)?;
        }

        Ok(call_result)
    }

    /// Initialize class fields in a constructor, defined onto the `this` value.
    fn gen_initialize_class_fields(&mut self, this_reg: GenRegister) -> EmitResult<()> {
        let initializer = std::mem::replace(&mut self.class_fields, ClassFieldsInitializer::none());

        // Check if a class fields initializer is needed
        if initializer.scope.is_none() {
            return Ok(());
        }

        // Fields initializer function is added to the pending functions queue
        let pending_node = PendingFunctionNode::ClassFieldsInitializer {
            scope: initializer.scope.unwrap(),
            fields: initializer.fields,
            class: initializer.class,
        };
        let func_index = self.enqueue_function_to_generate(pending_node)?;

        // Create the fields initializer closure itself
        let fields_init_value = self.register_allocator.allocate()?;
        self.writer
            .new_closure_instruction(fields_init_value, ConstantIndex::new(func_index));

        // Call the fields initializer function with the current `this` as the receiver
        self.writer.call_with_receiver_instruction(
            fields_init_value,
            fields_init_value,
            this_reg,
            /* dummy value */ fields_init_value,
            UInt::new(0),
        );

        self.register_allocator.release(fields_init_value);

        Ok(())
    }

    fn gen_template_literal_expression(
        &mut self,
        template: &ast::TemplateLiteral,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // Special case if there is only one quasi, load string directly to destination
        if template.quasis.len() == 1 {
            let dest = self.allocate_destination(dest)?;
            let constant_index =
                self.add_wtf8_string_constant(template.quasis[0].cooked.as_ref().unwrap())?;
            self.writer.load_constant_instruction(dest, constant_index);

            return Ok(dest);
        }

        // Special case if there is one expression and both quasis are empty
        if template.quasis.len() == 2
            && template.quasis[0].cooked.as_ref().unwrap().is_empty()
            && template.quasis[1].cooked.as_ref().unwrap().is_empty()
        {
            let expression = self.gen_expression(&template.expressions[0])?;
            self.register_allocator.release(expression);

            let dest = self.allocate_destination(dest)?;
            self.writer.to_string_instruction(dest, expression);

            return Ok(dest);
        }

        // Otherwise we must concatenate strings. Make sure to that accumulator is a temporary so
        // that it is not observably clobbered if ToString fails.
        let acc_dest = self.gen_ensure_dest_is_temporary(dest);
        let acc = self.allocate_destination(acc_dest)?;

        let mut is_first_part = true;

        for (i, quasi) in template.quasis.iter().enumerate() {
            // Cooked strings are guaranteed to exist for template literals
            let cooked = quasi.cooked.as_ref().unwrap();

            // Each quasi but the first is preceded by an expression
            if i > 0 {
                let expression = self.gen_expression(&template.expressions[i - 1])?;

                if is_first_part {
                    // If this is the first non-empty string, load it directly into the accumulator
                    // and mark all later elements to be added via a temporary.
                    self.writer.to_string_instruction(acc, expression);
                    self.register_allocator.release(expression);
                    is_first_part = false;
                } else {
                    // The expression value must be converted to a string and added to the
                    // accumulator. Make sure not to clobber the expression's register if it
                    // is a non-temporary.
                    let temp = if self.register_allocator.is_temporary_register(expression) {
                        expression
                    } else {
                        self.register_allocator.allocate()?
                    };

                    self.writer.to_string_instruction(temp, expression);
                    self.writer.add_instruction(acc, acc, temp);

                    self.register_allocator.release(temp);
                }
            }

            // Ignore empty cooked strings (unless there is only one quasi)
            if cooked.is_empty() && !(i == 0 && template.quasis.len() == 1) {
                continue;
            }

            let constant_index = self.add_wtf8_string_constant(cooked)?;
            if is_first_part {
                // If this is the first non-empty string, load it directly into the accumulator
                self.writer.load_constant_instruction(acc, constant_index);
                is_first_part = false;
            } else {
                // Load the cooked string to a temporary register and add to accumulator
                let temp = self.register_allocator.allocate()?;
                self.register_allocator.release(temp);

                self.writer.load_constant_instruction(temp, constant_index);
                self.writer.add_instruction(acc, acc, temp);
            }
        }

        self.gen_mov_reg_to_dest(acc, dest)
    }

    fn gen_resolved_or_dynamic_binding(
        &mut self,
        scope: TaggedResolvedScope,
        name: &str,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        match scope.kind() {
            ResolvedScope::UnresolvedGlobal => {
                unreachable!("cannot be resolved to UnresolvedGlobal")
            }
            ResolvedScope::UnresolvedDynamic => self.gen_load_dynamic_identifier(name, dest),
            ResolvedScope::Resolved => {
                let binding = scope.unwrap_resolved().get_binding(name);
                self.gen_load_binding(name, binding, dest)
            }
        }
    }

    fn gen_new_target_expression(
        &mut self,
        scope: TaggedResolvedScope,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        self.gen_resolved_or_dynamic_binding(scope, NEW_TARGET_BINDING_NAME, dest)
    }

    /// Determine the location where a new.target binding should be stored, given a function scope.
    /// Sets the new_target_index field if new.target is used.
    ///
    /// If the new.target binding should be stored in a scope, return a storage struct which must
    /// be passed to `gen_store_new_target`. Otherwise return None.
    fn gen_new_target_to_store(
        &mut self,
        scope: &AstScopeNode,
    ) -> EmitResult<Option<(usize, usize, GenRegister)>> {
        let new_target_binding = scope.get_binding_opt(NEW_TARGET_BINDING_NAME);
        if new_target_binding.is_none() {
            return Ok(None);
        }

        match new_target_binding.unwrap().vm_location().unwrap() {
            // Place new.target directly into the register
            VMLocation::LocalRegister(index) => {
                self.new_target_index = Some(index as u32);
                Ok(None)
            }
            VMLocation::Scope { scope_id, index } => {
                // Place new.target into a temporary register then store to the scope
                let new_target_register = self.register_allocator.allocate()?;
                self.new_target_index = Some(new_target_register.local_index() as u32);
                Ok(Some((scope_id, index, new_target_register)))
            }
            _ => unreachable!("new.target binding location must be a register or scope slot"),
        }
    }

    /// Store the storage struct created by `gen_new_target_to_store`.
    fn gen_store_new_target(
        &mut self,
        new_target_storage: Option<(usize, usize, GenRegister)>,
    ) -> EmitResult<()> {
        if let Some((scope_id, index, new_target_register)) = new_target_storage {
            self.gen_store_scope_binding(scope_id, index, new_target_register)?;
            self.register_allocator.release(new_target_register);
        }

        Ok(())
    }

    fn gen_import_meta_expression(&mut self, dest: ExprDest) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;
        self.writer.import_meta_instruction(dest);
        Ok(dest)
    }

    fn gen_dynamic_import_expression(
        &mut self,
        expr: &ast::ImportExpression,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        let dest = self.allocate_destination(dest)?;

        let specifier = self.gen_expression(&expr.source)?;
        self.writer.dynamic_import_instruction(dest, specifier);

        self.register_allocator.release(specifier);

        Ok(dest)
    }

    fn gen_variable_declaration(
        &mut self,
        var_decl: &ast::VariableDeclaration,
    ) -> EmitResult<StmtCompletion> {
        for decl in &var_decl.declarations {
            if let Some(init) = decl.init.as_deref() {
                let store_flags = StoreFlags::INITIALIZATION;
                let init_value_dest =
                    self.expr_dest_for_destructuring_assignment(&decl.id, store_flags);

                let init_value = if let ast::Pattern::Id(id) = decl.id.as_ref() {
                    self.gen_named_outer_expression(AnyStr::from_id(id), init, init_value_dest)?
                } else {
                    self.gen_outer_expression_with_dest(init, init_value_dest)?
                };

                // Destructuring pattern is in its own "has assign expression" context
                self.enter_has_assign_expr_context(decl.id_has_assign_expr);

                // Elide TDZ check when storing at declarations
                self.gen_store_to_pattern(&decl.id, init_value, store_flags)?;
                self.register_allocator.release(init_value);

                self.exit_has_assign_expr_context();
            } else if var_decl.kind == ast::VarKind::Let {
                // Let declarations without an initializer are initialized to undefined
                let id = decl.id.to_id();
                let store_flags = StoreFlags::INITIALIZATION;
                let init_value_dest = self.expr_dest_for_id(id, store_flags);
                let init_value = self.allocate_destination(init_value_dest)?;

                // Elide TDZ check when storing at declarations
                self.writer.load_undefined_instruction(init_value);
                self.gen_store_identifier(id, init_value, store_flags)?;
                self.register_allocator.release(init_value);
            }
        }

        Ok(StmtCompletion::Normal)
    }

    /// Store a value to a pattern, performing destructuring when necessary.
    fn gen_store_to_pattern(
        &mut self,
        pattern: &ast::Pattern,
        value: GenRegister,
        flags: StoreFlags,
    ) -> EmitResult<()> {
        let reference = self.gen_pattern_to_reference(pattern)?;
        self.gen_store_to_reference(reference, value, flags)
    }

    /// Perform the first part of storing to a pattern - evaluating the pattern to a reference
    /// that will later be actually stored to. This allows for behavior like evaluating a member
    /// expression before evaluating the RHS.
    ///
    /// Note that this differs from the spec in a few ways:
    /// - ToPropertyKey is not yet called ahead of time on member expression keys here, and is
    ///   instead called when storing to the reference.
    /// - Identifier references are not resolved ahead of time, even though evaluating the RHS may
    ///   change them.
    fn gen_pattern_to_reference<'b>(
        &mut self,
        pattern: &'b ast::Pattern,
    ) -> EmitResult<Reference<'b>> {
        match pattern {
            ast::Pattern::Id(id) => Ok(Reference::new(ReferenceKind::Id(id))),
            ast::Pattern::Member(member) => self.gen_member_expression_to_reference(member),
            ast::Pattern::SuperMember(super_member) => {
                self.gen_super_member_expression_to_reference(super_member)
            }
            ast::Pattern::Object(object) => {
                Ok(Reference::new(ReferenceKind::ObjectPattern(object)))
            }
            ast::Pattern::Array(array) => Ok(Reference::new(ReferenceKind::ArrayPattern(array))),
            ast::Pattern::Assign(assign) => {
                let mut reference = self.gen_pattern_to_reference(&assign.left)?;
                reference.init = Some(assign.right.as_ref());

                Ok(reference)
            }
        }
    }

    fn gen_member_expression_to_reference<'b>(
        &mut self,
        member: &'b ast::MemberExpression,
    ) -> EmitResult<Reference<'b>> {
        let object = self.gen_expression(&member.object)?;

        if member.is_computed {
            let property = self.gen_expression(&member.property)?;
            Ok(Reference::new(ReferenceKind::ComputedProperty { object, property }))
        } else if member.is_private {
            let property = self.gen_load_private_symbol(member.property.to_id())?;
            Ok(Reference::new(ReferenceKind::PrivateProperty { object, property }))
        } else {
            // Must be a named access
            let name = member.property.to_id();
            let property = self.add_string_constant(&name.name)?;
            Ok(Reference::new(ReferenceKind::NamedProperty { object, property }))
        }
    }

    fn gen_super_member_expression_to_reference<'b>(
        &mut self,
        member: &'b ast::SuperMemberExpression,
    ) -> EmitResult<Reference<'b>> {
        let home_object = self.gen_load_home_object(member, ExprDest::Any)?;
        let this_value = self.gen_load_this(member.this_scope, ExprDest::Any)?;

        let property = if member.is_computed {
            self.gen_expression(&member.property)?
        } else {
            // Must be a named access
            let name = member.property.to_id();
            let name_constant_index = self.add_string_constant(&name.name)?;

            let property = self.register_allocator.allocate()?;
            self.writer
                .load_constant_instruction(property, name_constant_index);

            property
        };

        Ok(Reference::new(ReferenceKind::SuperProperty {
            home_object,
            this_value,
            property,
        }))
    }

    fn gen_store_to_reference(
        &mut self,
        reference: Reference,
        value: GenRegister,
        flags: StoreFlags,
    ) -> EmitResult<()> {
        // Generate an assignment pattern, placing the right hand side in the `value` register if
        // it would otherwise be undefined.
        if let Some(init) = reference.init {
            let join_block = self.new_block();
            self.write_jump_not_undefined_instruction(value, join_block)?;

            // Named evaluation is performed if the pattern is an id
            if let ReferenceKind::Id(id) = &reference.kind {
                self.gen_named_expression(AnyStr::from_id(id), init, ExprDest::Fixed(value))?;
            } else {
                self.gen_expression_with_dest(init, ExprDest::Fixed(value))?;
            }

            self.start_block(join_block);
        }

        match &reference.kind {
            ReferenceKind::Id(id) => self.gen_store_identifier(id, value, flags),
            ReferenceKind::NamedProperty { object, property } => {
                self.writer
                    .set_named_property_instruction(*object, *property, value);

                self.register_allocator.release(*object);

                Ok(())
            }
            ReferenceKind::ComputedProperty { object, property } => {
                self.writer
                    .set_property_instruction(*object, *property, value);

                self.register_allocator.release(*property);
                self.register_allocator.release(*object);

                Ok(())
            }
            ReferenceKind::PrivateProperty { object, property } => {
                self.writer
                    .set_private_property_instruction(*object, *property, value);

                self.register_allocator.release(*property);
                self.register_allocator.release(*object);

                Ok(())
            }
            ReferenceKind::SuperProperty { home_object, this_value, property } => {
                self.writer.set_super_property_instruction(
                    *home_object,
                    *this_value,
                    *property,
                    value,
                );

                self.register_allocator.release(*property);
                self.register_allocator.release(*this_value);
                self.register_allocator.release(*home_object);

                Ok(())
            }
            ReferenceKind::ObjectPattern(object) => {
                self.gen_object_destructuring(object, value, flags)
            }
            ReferenceKind::ArrayPattern(array) => {
                self.gen_iterator_destructuring(array, value, flags)
            }
        }
    }

    fn gen_object_destructuring(
        &mut self,
        pattern: &ast::ObjectPattern,
        object_value: GenRegister,
        store_flags: StoreFlags,
    ) -> EmitResult<()> {
        enum Property<'a> {
            Named(&'a ast::Identifier),
            Computed(GenRegister),
        }

        // If there is a rest element all keys must be saved in a contiguous sequence of temporary
        // registers so they can be passed to the CopyDataProperties instruction.
        let has_rest_element = pattern.properties.last().map_or(false, |p| p.is_rest);

        let mut saved_keys = vec![];
        if has_rest_element {
            for _ in 0..pattern.properties.len() - 1 {
                saved_keys.push(self.register_allocator.allocate()?);
            }
        }

        for (i, property) in pattern.properties.iter().enumerate() {
            if property.is_rest {
                continue;
            }

            // Emit property in the best dest register for the pattern
            let property_value_dest =
                self.expr_dest_for_destructuring_assignment(&property.value, store_flags);
            let property_value = self.allocate_destination(property_value_dest)?;

            let key = match property.key.as_deref() {
                // Shorthand properties must have an id pattern, optionally with a default value
                None => match property.value.as_ref() {
                    ast::Pattern::Id(id) => Property::Named(id),
                    ast::Pattern::Assign(assign) => Property::Named(assign.left.to_id()),
                    _ => unreachable!("invalid shorthand property pattern"),
                },
                Some(ast::Expression::Id(id)) if !property.is_computed => Property::Named(id),
                Some(expr) => {
                    let key = self.gen_expression(expr)?;
                    Property::Computed(key)
                }
            };

            // Evaluate to reference before evaluating property key
            let reference = self.gen_pattern_to_reference(&property.value)?;

            match &key {
                Property::Named(id) => {
                    let name_constant_index = self.add_string_constant(&id.name)?;

                    // If there is a rest element name must be saved in the reserved registers. Can
                    // load directly to the temporary register since name is already a property key.
                    if has_rest_element {
                        self.writer
                            .load_constant_instruction(saved_keys[i], name_constant_index);
                    }

                    // Read named property from object
                    self.writer.get_named_property_instruction(
                        property_value,
                        object_value,
                        name_constant_index,
                    );
                }
                Property::Computed(key) => {
                    // If there is a rest element then we must ensure key is a property key and save
                    // it in the reserved registers.
                    if has_rest_element {
                        self.writer.to_property_key_instruction(saved_keys[i], *key);
                    }

                    // Read computed property from object
                    self.writer
                        .get_property_instruction(property_value, object_value, *key);
                }
            }

            self.gen_store_to_reference(reference, property_value, store_flags)?;

            if let Property::Computed(key) = key {
                self.register_allocator.release(key);
            }

            self.register_allocator.release(property_value);
        }

        // Emit the rest element if one was included
        if has_rest_element {
            let rest_element_pattern = pattern.properties.last().unwrap().value.as_ref();
            let rest_element_dest =
                self.expr_dest_for_destructuring_assignment(rest_element_pattern, store_flags);
            let rest_element = self.allocate_destination(rest_element_dest)?;

            // Evaluate to reference before calling CopyDataProperties
            let reference = self.gen_pattern_to_reference(rest_element_pattern)?;

            // Rest element references the saved property keys on the stack, so they can be excluded
            // Use an arbitrary value for argv
            let argv = if saved_keys.is_empty() {
                object_value
            } else {
                saved_keys[0]
            };

            let argc =
                GenUInt::try_from_unsigned(saved_keys.len()).ok_or(EmitError::TooManyRegisters)?;

            // Perform a ToObject so that we error on nullish values beforce calling
            // CopyDataProperties, which silently ignores nullish values.
            let scratch = self.register_allocator.allocate()?;
            self.writer.to_object_instruction(scratch, object_value);
            self.register_allocator.release(scratch);

            // Create a new object and copy all data properties, except for the property keys saved
            // to the stack.
            self.writer.new_object_instruction(rest_element);
            self.writer
                .copy_data_properties(rest_element, object_value, argv, argc);

            self.gen_store_to_reference(reference, rest_element, store_flags)?;
            self.register_allocator.release(rest_element);

            for saved_key in saved_keys.into_iter().rev() {
                self.register_allocator.release(saved_key);
            }
        }

        // Even if there are no properties we must still error if value is not an object. In this
        // case perform a ToObject (but discard the result) to error if necessary.
        if pattern.properties.is_empty() {
            let scratch = self.register_allocator.allocate()?;
            self.writer.to_object_instruction(scratch, object_value);
            self.register_allocator.release(scratch);
        }

        Ok(())
    }

    fn gen_iterator_destructuring(
        &mut self,
        array_pattern: &ast::ArrayPattern,
        iterable: GenRegister,
        flags: StoreFlags,
    ) -> EmitResult<()> {
        // Registers needed until end of finally block
        let iterator = self.register_allocator.allocate()?;
        let is_done = self.register_allocator.allocate()?;

        // If break or continue reaches this target then the finally scope has been left
        self.push_finally_scope_jump_target();
        let jump_target_depth = self.jump_statement_target_stack.len();

        // Enter a finally scope, setting up necessary registers
        let join_block = self.new_block();
        self.gen_start_finally_scope(join_block, jump_target_depth, jump_target_depth)?;

        // Registers only needed while iterating
        let next_method = self.register_allocator.allocate()?;
        let value = self.register_allocator.allocate()?;

        let mut exception_handlers = vec![];

        self.writer
            .get_iterator_instruction(iterator, next_method, iterable);

        // Call `next` on iterator for each element of the array pttern
        for (i, element) in array_pattern.elements.iter().enumerate() {
            // Rest element creates a new array with remaining values until iterator is done
            if let ast::ArrayPatternElement::Rest(rest) = element {
                let array = self.register_allocator.allocate()?;
                self.writer.new_array_instruction(array);

                // Evaluate pattern to reference before gathering remaining items
                let (exception_handler, reference) = self.gen_in_exception_handler(|this| {
                    this.gen_pattern_to_reference(&rest.argument)
                })?;

                if exception_handler.start != exception_handler.end {
                    exception_handlers.push(exception_handler);
                }

                let index = self.register_allocator.allocate()?;
                self.writer.load_immediate_instruction(index, SInt::new(0));

                let iteration_start_block = self.new_block();
                let is_done_block = self.new_block();

                // Skip entire loop if iterator is already done
                if i != 0 {
                    self.write_jump_true_instruction(is_done, is_done_block)?;
                }

                // Each iteration only starts if we are not done. Each iteration calls `next` and
                // then checks if iterator is done.
                self.start_block(iteration_start_block);
                self.writer
                    .iterator_next_instruction(value, is_done, iterator, next_method);
                self.write_jump_true_instruction(is_done, is_done_block)?;

                // If not done, store value to next index in array and start next iteration
                self.writer
                    .set_array_property_instruction(array, index, value);
                self.writer.inc_instruction(index);
                self.write_jump_instruction(iteration_start_block)?;

                self.register_allocator.release(index);

                // When done, store array to reference that was generated earlier
                self.start_block(is_done_block);

                let (exception_handler, _) = self.gen_in_exception_handler(|this| {
                    this.gen_store_to_reference(reference, array, flags)?;
                    this.register_allocator.release(array);
                    Ok(())
                })?;

                if exception_handler.start != exception_handler.end {
                    exception_handlers.push(exception_handler);
                }

                continue;
            }

            // Evaluate to reference before calling `next`
            let reference = if let ast::ArrayPatternElement::Pattern(pattern) = element {
                let (exception_handler, reference) =
                    self.gen_in_exception_handler(|this| this.gen_pattern_to_reference(pattern))?;

                if exception_handler.start != exception_handler.end {
                    exception_handlers.push(exception_handler);
                }

                Some(reference)
            } else {
                // Otherwise must be a hole
                None
            };

            let is_done_block = self.new_block();

            // If iterator is already done then continue to done block instead of calling `next`
            if i != 0 {
                self.write_jump_true_instruction(is_done, is_done_block)?;
            }

            // Call `next` method, and if done then continue to done block
            self.writer
                .iterator_next_instruction(value, is_done, iterator, next_method);

            // Patterns will be destructured so value from iterator can be assigned
            if let Some(reference) = reference {
                let iteration_destructure_block = self.new_block();
                self.write_jump_false_instruction(is_done, iteration_destructure_block)?;

                // If iterator is done then use undefined as value
                self.start_block(is_done_block);
                self.writer.load_undefined_instruction(value);

                // Destructure the value from the iterator, or undefined if iterator is done
                self.start_block(iteration_destructure_block);
                let (exception_handler, _) = self.gen_in_exception_handler(|this| {
                    this.gen_store_to_reference(reference, value, flags)
                })?;

                if exception_handler.start != exception_handler.end {
                    exception_handlers.push(exception_handler);
                }
            } else {
                // Holes ignore value so continue to next iteration
                self.start_block(is_done_block);
            }
        }

        // If done we can continue, otherwise we must first close the iterator
        if !array_pattern.elements.is_empty() {
            self.write_jump_true_instruction(is_done, join_block)?;
        }

        self.register_allocator.release(value);
        self.register_allocator.release(next_method);

        // We must close the iterator in a finally if it was not done
        if exception_handlers.is_empty() {
            // If there were no exception handlers needed then we can close the iterator without
            // wrapping in an exception handler, since if closing throws that exception does not
            // need to be swallowed.
            //
            // Iterator is only closed if we are not done, which is enforced by the JumpTrue above.
            self.writer.iterator_close_instruction(iterator);

            // Exit the finally scope and release all of its registers
            let finally_scope = self.pop_finally_scope();
            self.pop_jump_statement_target();

            // Clean up resources from finally scope, including registers and blocks
            self.register_allocator
                .release(finally_scope.saved_scope_register);
            self.register_allocator
                .release(finally_scope.result_register);
            self.register_allocator
                .release(finally_scope.discriminant_register);
            self.start_block(finally_scope.finally_block);
        } else {
            // Extract fields from finally scope
            let finally_scope = self.current_finally_scope().unwrap();
            let discriminant = finally_scope.discriminant_register;
            let finally_block = finally_scope.finally_block;

            // Write to the discriminant when coming from the normal completion
            let normal_branch_id = finally_scope.add_normal_branch();
            self.gen_load_finally_branch_id(normal_branch_id, discriminant)?;
            self.write_jump_instruction(finally_block)?;

            // Write a finally-aware exception handler which is shared by all handlers
            self.gen_exception_handlers_continue_to_finally(exception_handlers)?;

            // Exit the finally scope
            let finally_scope = self.pop_finally_scope();
            self.pop_jump_statement_target();

            let finally_footer_block = self.new_block();

            // Start the finally block
            self.gen_start_finally_block(&finally_scope);

            // Catch exceptions when closing the iterator. Only call close if the iterator is not
            // already done.
            self.write_jump_true_instruction(is_done, finally_footer_block)?;
            let (mut close_handler, _) = self.gen_in_exception_handler(|this| {
                this.writer.iterator_close_instruction(iterator);
                Ok(())
            })?;

            self.write_jump_instruction(finally_footer_block)?;

            // Close exception handler.
            let close_error = self.register_allocator.allocate()?;
            close_handler.handler = self.writer.current_offset();
            close_handler.error_register = Some(close_error);
            self.exception_handler_builder.add(close_handler);

            // Check if iterating threw by checking discriminant
            let cond = self.register_allocator.allocate()?;
            self.gen_load_finally_branch_id(finally_scope.throw_branch(), cond)?;
            self.writer
                .strict_not_equal_instruction(cond, cond, discriminant);

            // If the iterating did not throw then continue to throwing error from close
            let close_handler_rethrow_block = self.new_block();
            self.write_jump_true_instruction(cond, close_handler_rethrow_block)?;
            self.register_allocator.release(cond);

            // Otherwise iterating did throw, we rethrow it by overwriting the close error
            self.write_mov_instruction(close_error, finally_scope.result_register);

            // Then throw either the original close error or the error from iterating
            self.start_block(close_handler_rethrow_block);
            self.writer.throw_instruction(close_error);
            self.register_allocator.release(close_error);

            // Emit the finally footer, checking discriminant to see if we came from a normal
            // completion, throw, or return.
            self.start_block(finally_footer_block);

            // Emit the footer of the finally block, which checks the discriminant to determine the
            // incoming branch that was taken.
            self.gen_end_finally_block(
                &finally_scope,
                StmtCompletion::Normal,
                /* saved_normal_completion */ None,
            )?;
        }

        self.start_block(join_block);

        self.register_allocator.release(is_done);
        self.register_allocator.release(iterator);

        Ok(())
    }

    fn gen_function_declaration(
        &mut self,
        func_decl: &ast::Function,
        binding: &Binding,
    ) -> EmitResult<StmtCompletion> {
        // Exported functions are enqueued to be added to the module scope during initialization
        if binding.is_exported() {
            // Extract the slot in the module scope where this export is stored
            let vm_location = binding.vm_location().unwrap();
            let slot_index = if let VMLocation::ModuleScope { index, .. } = vm_location {
                index
            } else {
                unreachable!("export must be in module scope")
            };

            let func_node = PendingFunctionNode::Declaration(AstPtr::from_ref(func_decl));
            self.enqueue_export_function_to_generate(func_node, slot_index);

            return Ok(StmtCompletion::Normal);
        }

        // Otherwise function is hoisted to top of the scope and must be created at runtime
        let func_constant_index = self.enqueue_function_to_generate(
            PendingFunctionNode::Declaration(AstPtr::from_ref(func_decl)),
        )?;

        // Create a new closure, directly into binding location if possible
        let store_flags = StoreFlags::INITIALIZATION;
        let closure_dest = if let Some(id) = func_decl.id.as_ref() {
            self.expr_dest_for_id(id, store_flags)
        } else {
            ExprDest::Any
        };
        let closure_reg = self.allocate_destination(closure_dest)?;

        self.gen_new_closure_for_function(
            func_decl,
            closure_reg,
            ConstantIndex::new(func_constant_index),
        );

        // And store at the binding's location. Stores at declarations do not need a TDZ check.
        if let Some(id) = func_decl.id.as_ref() {
            self.gen_store_identifier(id, closure_reg, store_flags)?;
        }

        self.register_allocator.release(closure_reg);

        Ok(StmtCompletion::Normal)
    }

    fn gen_new_closure_for_function(
        &mut self,
        func: &ast::Function,
        dest: GenRegister,
        func_constant_index: GenConstantIndex,
    ) {
        if func.is_async() {
            if func.is_generator() {
                self.writer
                    .new_async_generator_instruction(dest, func_constant_index);
            } else {
                self.writer
                    .new_async_closure_instruction(dest, func_constant_index);
            }
        } else if func.is_generator() {
            self.writer
                .new_generator_instruction(dest, func_constant_index);
        } else {
            self.writer
                .new_closure_instruction(dest, func_constant_index);
        }
    }

    fn gen_class_declaration(&mut self, class: &ast::Class) -> EmitResult<StmtCompletion> {
        self.gen_class(class, GenClassKind::Declaration, None, ExprDest::Any)?;
        Ok(StmtCompletion::Normal)
    }

    fn gen_class_expression(
        &mut self,
        class: &ast::Class,
        name: Option<Wtf8String>,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        self.gen_class(class, GenClassKind::Expression, name, dest)
    }

    fn gen_class(
        &mut self,
        class: &ast::Class,
        kind: GenClassKind,
        name: Option<Wtf8String>,
        dest: ExprDest,
    ) -> EmitResult<GenRegister> {
        // Set up destination for the constructor, directly storing in binding location if possible
        let store_flags = StoreFlags::INITIALIZATION;
        let mut constructor_dest = dest;

        if let Some(id) = class.id.as_ref() {
            if matches!(kind, GenClassKind::Declaration) {
                constructor_dest = self.expr_dest_for_id(id, store_flags);
            }
        }
        let constructor_reg = self.allocate_destination(constructor_dest)?;

        // Start the body's scope
        let body_scope = class.scope.as_ref();
        self.gen_scope_start(body_scope, Some(ScopeFlags::IS_CLASS_SCOPE))?;

        // Evaluate super class if it exists, otherwise use empty as sentinel value
        let super_class = if let Some(super_class) = class.super_class.as_ref() {
            self.gen_outer_expression(super_class)?
        } else {
            let super_class = self.register_allocator.allocate()?;
            self.writer.load_empty_instruction(super_class);
            super_class
        };

        // Create private symbols and store to body scope
        for element in &class.body {
            match element {
                ast::ClassElement::Method(method) => {
                    // Ensure that we only create private symbol once for pair
                    if method.is_private && !method.is_private_pair_start {
                        let private_id = method.key.expr.to_id();
                        self.gen_create_private_symbol(private_id)?;
                    }
                }
                ast::ClassElement::Property(property) => {
                    if property.is_private {
                        let private_id = property.key.expr.to_id();
                        self.gen_create_private_symbol(private_id)?;
                    }
                }
            }
        }

        // Generate all methods, collecting method definitions into a ClassNames object which is
        // used in the NewClass instruction.
        let mut methods = vec![];
        let mut new_class_arguments = vec![];
        let mut static_elements = vec![];

        let mut field_scope_index = 0;
        let mut fields = vec![];

        let mut private_accessor_pairs = HashMap::new();

        for element in &class.body {
            match element {
                ast::ClassElement::Method(method) => {
                    // Constructor is already handled
                    if method.kind == ast::ClassMethodKind::Constructor {
                        continue;
                    }

                    // Collect static initializers to be run after the class is created
                    if method.kind == ast::ClassMethodKind::StaticInitializer {
                        static_elements
                            .push(ClassStaticElement::Initializer(AstPtr::from_ref(method)));
                        continue;
                    }

                    // Save the start of each private accessor pair until the second accessor is
                    // found, at which point both will be emitted.
                    if method.is_private_pair_start {
                        let name = &method.key.expr.to_id().name;
                        private_accessor_pairs.insert(name, method);
                        continue;
                    }

                    let (method_info, method_value) =
                        self.gen_class_method(method, &mut new_class_arguments)?;

                    if !method.is_private {
                        // Method is stored as an argument to the NewClass instruction
                        new_class_arguments.push(method_value);
                        methods.push(method_info);
                    } else {
                        let private_id = &method.key.expr.to_id();

                        let mut is_getter = method.kind == ast::ClassMethodKind::Get;
                        let mut is_setter = method.kind == ast::ClassMethodKind::Set;

                        // Handle private accessor pairs
                        let value = if let Some(first_method) =
                            private_accessor_pairs.remove(&private_id.name)
                        {
                            // Generate the other method in the pair that was saved from earlier
                            let (_, first_method_value) =
                                self.gen_class_method(first_method, &mut new_class_arguments)?;

                            // Determine which method is the getter and which is the setter
                            let (getter, setter) = if is_getter {
                                (method_value, first_method_value)
                            } else {
                                (first_method_value, method_value)
                            };

                            self.register_allocator.release(first_method_value);
                            self.register_allocator.release(method_value);

                            // Combine getter and setter into a single accessor
                            let accessor_value = self.register_allocator.allocate()?;
                            self.writer
                                .new_accessor_instruction(accessor_value, getter, setter);

                            is_getter = true;
                            is_setter = true;

                            accessor_value
                        } else {
                            method_value
                        };

                        // Private properties will instead be stored in the class's scope at the
                        // next available index.
                        let scope_index = field_scope_index;
                        let scope_index_uint = GenUInt::try_from_unsigned(scope_index)
                            .ok_or(EmitError::IntegerTooLarge)?;
                        field_scope_index += 1;

                        // Method or accessor is stored in the class's scope, which is guaranteed to
                        // be the current enclosing scope.
                        self.writer.store_to_scope_instruction(
                            value,
                            scope_index_uint,
                            UInt::new(0),
                        );
                        self.register_allocator.release(value);

                        let field = ClassField::PrivateMethodOrAccessor {
                            name: AstPtr::from_ref(private_id),
                            scope_id: self.scope.scope_id,
                            scope_index,
                            is_getter,
                            is_setter,
                        };

                        if method.is_static {
                            static_elements.push(ClassStaticElement::Field(field));
                        } else {
                            fields.push(field);
                        }
                    }
                }
                ast::ClassElement::Property(property) => {
                    // Determine if field has a statically known string name
                    let name = match &property.key.expr {
                        _ if property.is_computed => None,
                        ast::Expression::Id(id) => Some(Wtf8String::from_str(&id.name)),
                        ast::Expression::String(string) => Some(string.value.clone()),
                        ast::Expression::Number(_) | ast::Expression::BigInt(_) => None,
                        _ => unreachable!("invalid class property key"),
                    };

                    let field = AstPtr::from_ref(property);

                    let field = if property.is_private {
                        ClassField::PrivateField { field }
                    } else if let Some(name) = name {
                        ClassField::Named { field, name }
                    } else {
                        // Evaluate key to a property key
                        let key_reg = self.gen_outer_expression(&property.key)?;
                        self.writer.to_property_key_instruction(key_reg, key_reg);

                        // And store in scope at the next available index
                        let scope_index = field_scope_index;
                        let scope_index_uint = GenUInt::try_from_unsigned(scope_index)
                            .ok_or(EmitError::IntegerTooLarge)?;
                        field_scope_index += 1;

                        // Stored in class scope, which is guaranteed to be the enclosing scope
                        self.writer.store_to_scope_instruction(
                            key_reg,
                            scope_index_uint,
                            UInt::new(0),
                        );

                        self.register_allocator.release(key_reg);

                        ClassField::Computed { field, scope_id: self.scope.scope_id, scope_index }
                    };

                    if property.is_static {
                        static_elements.push(ClassStaticElement::Field(field));
                    } else {
                        fields.push(field);
                    }
                }
            }
        }

        // Constructor node is optional if this is a default constructor
        let node = class
            .constructor
            .as_ref()
            .map(|c| AstPtr::from_ref(c.as_ref().value.as_ref()));

        let name = if let Some(id) = class.id.as_ref() {
            // Constructor has name of class
            Wtf8String::from_str(&id.name)
        } else if let Some(name) = name {
            // Handle name passed from named evaluation
            name
        } else if let GenClassKind::Export { name, .. } = &kind {
            // Handle name passed from export, such as the anonymous default export name
            Wtf8String::from_str(name)
        } else {
            // Otherwise name is the empty string if class has no name
            Wtf8String::new()
        };

        // Create the constructor's static BytecodeFunction
        let fields = ClassFieldsInitializer {
            fields,
            scope: class.fields_initializer_scope,
            class: AstPtr::from_ref(class),
        };
        let is_base = class.super_class.is_none();
        let source_range = class.loc.to_range();
        let pending_constructor =
            PendingFunctionNode::Constructor { node, name, fields, is_base, source_range };
        let constructor_index = self.enqueue_function_to_generate(pending_constructor)?;

        // Find the scope locations for the home objects if they are needed
        let home_object = self.get_home_object_location(body_scope, HOME_OBJECT_BINDING_NAME)?;
        let static_home_object =
            self.get_home_object_location(body_scope, STATIC_HOME_OBJECT_BINDING_NAME)?;

        // ClassNames object is stored in the constant table
        let class_names = ClassNames::new(self.cx, &methods, home_object, static_home_object);
        let class_names_index = self
            .constant_table_builder
            .add_heap_object(class_names.cast())?;

        // Pass first register to NewClass instruction. If no registers are needed then pass an
        // arbitrary register as it will not be used (in this case the constructor register).
        let first_argument_reg = if new_class_arguments.is_empty() {
            constructor_reg
        } else {
            new_class_arguments[0]
        };

        // Create the class itself
        self.writer.new_class_instruction(
            constructor_reg,
            ConstantIndex::new(class_names_index),
            ConstantIndex::new(constructor_index),
            super_class,
            first_argument_reg,
        );

        // Initialize the class itself in the body scope if necessary.
        if let Some(class_id) = class.id.as_ref() {
            let class_name = &class_id.name;
            if let Some(binding) = body_scope.get_binding_opt(class_name) {
                if let Some(VMLocation::Scope { .. }) = binding.vm_location() {
                    self.gen_store_binding(
                        class_name,
                        binding,
                        constructor_reg,
                        StoreFlags::INITIALIZATION,
                    )?;
                }
            }
        }

        // Create the static initializer function if one exists
        if let Some(scope) = class.static_initializer_scope.as_ref() {
            // Static initializer function is added to the pending functions queue
            let pending_node = PendingFunctionNode::ClassStaticInitializer {
                scope: *scope,
                elements: static_elements,
                class: AstPtr::from_ref(class),
            };
            let func_index = self.enqueue_function_to_generate(pending_node)?;

            // Create the static initializer closure itself
            let static_init_value = self.register_allocator.allocate()?;
            self.writer
                .new_closure_instruction(static_init_value, ConstantIndex::new(func_index));

            // Call the static initializer function with the constructor as the receiver
            self.writer.call_with_receiver_instruction(
                static_init_value,
                static_init_value,
                constructor_reg,
                /* dummy value */ static_init_value,
                UInt::new(0),
            );

            self.register_allocator.release(static_init_value);
        }

        // End the body scope after running static initializers. According to spec this should be
        // before static initializers, but we want to keep the body's class name in scope. This is
        // the only visible binding in the body scope so this has no other effects.
        self.gen_scope_end(body_scope);

        // Store constructor at the binding's location. Stores at declarations do not need a TDZ
        // check.
        match kind {
            GenClassKind::Declaration => {
                if let Some(id) = class.id.as_ref() {
                    self.gen_store_identifier(id, constructor_reg, store_flags)?;
                }
            }
            GenClassKind::Export { name, binding } => {
                self.gen_store_binding(name, binding, constructor_reg, store_flags)?;
            }
            // Does not store constructor in parent scope
            GenClassKind::Expression => {}
        }

        for argument in new_class_arguments.iter().rev() {
            self.register_allocator.release(*argument);
        }

        self.register_allocator.release(super_class);

        if matches!(kind, GenClassKind::Expression) {
            self.gen_mov_reg_to_dest(constructor_reg, dest)
        } else {
            // Declaration result will not be used
            self.register_allocator.release(constructor_reg);
            Ok(constructor_reg)
        }
    }

    fn gen_class_method(
        &mut self,
        method: &ast::ClassMethod,
        new_class_arguments: &mut Vec<GenRegister>,
    ) -> EmitResult<(Method, GenRegister)> {
        enum Name<'a> {
            Named(AnyStr<'a>),
            Computed(GenRegister),
        }

        // Evaluate class element name
        let key = if method.is_computed {
            Name::Computed(self.gen_outer_expression(&method.key)?)
        } else {
            match &method.key.expr {
                ast::Expression::Id(id) => Name::Named(AnyStr::from_id(id)),
                ast::Expression::String(string) => Name::Named(AnyStr::Wtf8(&string.value)),
                expr @ (ast::Expression::Number(_) | ast::Expression::BigInt(_)) => {
                    Name::Computed(self.gen_expression(expr)?)
                }
                _ => unreachable!("invalid class element name"),
            }
        };

        // Find the name that should be set of the method's closure
        let closure_name = match key {
            Name::Named(name) => {
                // Prefix name with `#` if private
                let name = if method.is_private {
                    let mut prefixed_name = Wtf8String::from_str("#");
                    prefixed_name.push_wtf8_str(&name.to_wtf8_string());
                    prefixed_name
                } else {
                    name.to_wtf8_string()
                };

                // Add accessor prefix to the name if name is known
                if method.kind == ast::ClassMethodKind::Get {
                    let mut prefixed_name = Wtf8String::from_str("get ");
                    prefixed_name.push_wtf8_str(&name);
                    Some(prefixed_name)
                } else if method.kind == ast::ClassMethodKind::Set {
                    let mut prefixed_name = Wtf8String::from_str("set ");
                    prefixed_name.push_wtf8_str(&name);
                    Some(prefixed_name)
                } else {
                    Some(name)
                }
            }
            Name::Computed(name_register) => {
                // If computed then name will be passed as an argument to the NewClass instruction.
                // First convert to a property key.
                if !method.is_private {
                    self.writer
                        .to_property_key_instruction(name_register, name_register);
                }

                new_class_arguments.push(name_register);

                None
            }
        };

        // Function node is added to the pending functions queue
        let pending_node = PendingFunctionNode::Method {
            node: AstPtr::from_ref(&method.value),
            name: closure_name,
        };
        let method_constant_index = self.enqueue_function_to_generate(pending_node)?;

        // Create the method closure itself
        let method_value = self.register_allocator.allocate()?;
        self.gen_new_closure_for_function(
            &method.value,
            method_value,
            ConstantIndex::new(method_constant_index),
        );

        let method_name = if let Name::Named(name) = key {
            Some(InternedStrings::get_wtf8_str(self.cx, &name.to_wtf8_string()).as_flat())
        } else {
            None
        };

        let method_info = Method {
            name: method_name,
            is_static: method.is_static,
            is_getter: method.kind == ast::ClassMethodKind::Get,
            is_setter: method.kind == ast::ClassMethodKind::Set,
            is_private: method.is_private,
        };

        Ok((method_info, method_value))
    }

    fn gen_class_field(&mut self, field: &ClassField, target: GenRegister) -> EmitResult<()> {
        // Private methods and accessors had their closure value created and stored in the class's
        // scope when the class definition was evaluated.
        if let ClassField::PrivateMethodOrAccessor {
            name,
            scope_id,
            scope_index,
            is_getter,
            is_setter,
            ..
        } = field
        {
            // Load the private symbol from the constant table
            let name = self.gen_load_private_symbol(name.as_ref())?;

            // Load the value from the current class scope
            let value = self.gen_load_scope_binding(*scope_id, *scope_index, ExprDest::Any)?;

            // Set up flags for DefinePrivateProperty instruction
            let mut flags = DefinePrivatePropertyFlags::empty();
            if !is_getter && !is_setter {
                flags |= DefinePrivatePropertyFlags::METHOD;
            }
            if *is_getter {
                flags |= DefinePrivatePropertyFlags::GETTER;
            }
            if *is_setter {
                flags |= DefinePrivatePropertyFlags::SETTER;
            }
            let flags = UInt::new(flags.bits() as u32);

            self.writer
                .define_private_property_instruction(target, name, value, flags);

            self.register_allocator.release(value);
            self.register_allocator.release(name);

            return Ok(());
        }

        let field_node = match field {
            ClassField::Named { field, .. }
            | ClassField::Computed { field, .. }
            | ClassField::PrivateField { field, .. } => field.as_ref(),
            ClassField::PrivateMethodOrAccessor { .. } => unreachable!(),
        };

        // Evaluate the initializer, otherwise field is set to undefined
        let value = if let Some(initializer) = field_node.value.as_deref() {
            match field {
                ClassField::Named { name, .. } => {
                    self.gen_named_outer_expression(AnyStr::Wtf8(name), initializer, ExprDest::Any)?
                }
                ClassField::Computed { .. } => self.gen_outer_expression(initializer)?,
                ClassField::PrivateField { field } => {
                    let name = format!("#{}", field.as_ref().key.expr.to_id().name);
                    self.gen_named_outer_expression(AnyStr::Str(&name), initializer, ExprDest::Any)?
                }
                ClassField::PrivateMethodOrAccessor { .. } => unreachable!(),
            }
        } else {
            let value = self.register_allocator.allocate()?;
            self.writer.load_undefined_instruction(value);
            value
        };

        match field {
            ClassField::Named { name, .. } => {
                let name_constant_index = self.add_wtf8_string_constant(name)?;
                self.writer
                    .define_named_property_instruction(target, name_constant_index, value);
            }
            ClassField::Computed { scope_id, scope_index, .. } => {
                // Load the field name from the current class scope
                let name = self.gen_load_scope_binding(*scope_id, *scope_index, ExprDest::Any)?;

                // Set up flags for DefineProperty instruction
                let mut flags = DefinePropertyFlags::empty();
                if field_node.value.is_some()
                    && Self::expression_needs_name(&field_node.value.as_deref().unwrap().expr)
                {
                    flags |= DefinePropertyFlags::NEEDS_NAME;
                }
                let flags = UInt::new(flags.bits() as u32);

                self.writer
                    .define_property_instruction(target, name, value, flags);

                self.register_allocator.release(name);
            }
            ClassField::PrivateField { field, .. } => {
                let private_id = field.as_ref().key.expr.to_id();

                // Define the private field using the private symbol
                let name = self.gen_load_private_symbol(private_id)?;
                let flags = UInt::new(DefinePrivatePropertyFlags::empty().bits() as u32);

                self.writer
                    .define_private_property_instruction(target, name, value, flags);

                self.register_allocator.release(name);
            }
            ClassField::PrivateMethodOrAccessor { .. } => unreachable!(),
        }

        self.register_allocator.release(value);

        Ok(())
    }

    fn gen_expression_statement(
        &mut self,
        stmt: &ast::ExpressionStatement,
    ) -> EmitResult<StmtCompletion> {
        // If a completion destination is provided, store expression value into it. Otherwise
        // discard the expressions's value.
        if let Some(completion_dest) = self.statement_completion_dest {
            self.gen_outer_expression_with_dest(&stmt.expr, ExprDest::Fixed(completion_dest))?;
        } else {
            let result = self.gen_outer_expression(&stmt.expr)?;
            self.register_allocator.release(result);
        }

        Ok(StmtCompletion::Normal)
    }

    fn gen_block_statement(&mut self, stmt: &ast::Block) -> EmitResult<StmtCompletion> {
        // Block body forms a new scope
        let block_scope = stmt.scope.as_ref();
        self.gen_scope_start(block_scope, None)?;

        let stmt_completion = self.gen_statement_list(&stmt.body)?;

        // End of block scope
        if !stmt_completion.is_abrupt() {
            self.gen_scope_end(block_scope);
        }

        Ok(stmt_completion)
    }

    fn gen_export_default_declaration(
        &mut self,
        program: &ast::Program,
        export: &ast::ExportDefaultDeclaration,
    ) -> EmitResult<()> {
        let scope = program.scope.as_ref();

        match &export.declaration {
            // Default exported function declarations will be added to module scope and generated
            // like all other function declarations.
            ast::ExportDefaultKind::Function(_) => Ok(()),
            // Classes are generated similar to declarations, but potentially with a default name
            // if the class is anonymous.
            ast::ExportDefaultKind::Class(class) => {
                let (name, binding) = if let Some(id) = class.id.as_ref() {
                    (id.name.as_str(), id.get_binding())
                } else {
                    let anonymous_binding = scope.get_binding(ANONYMOUS_DEFAULT_EXPORT_NAME);
                    ("default", anonymous_binding)
                };

                self.gen_class(class, GenClassKind::Export { name, binding }, None, ExprDest::Any)?;

                Ok(())
            }
            // Default exported expressions are evaluated and stored as the anonymous default in
            // the current scope.
            ast::ExportDefaultKind::Expression(expr) => {
                let value =
                    self.gen_named_outer_expression(AnyStr::Str("default"), expr, ExprDest::Any)?;

                let anonymous_binding = scope.get_binding(ANONYMOUS_DEFAULT_EXPORT_NAME);
                self.gen_store_binding(
                    "default",
                    anonymous_binding,
                    value,
                    StoreFlags::INITIALIZATION,
                )?;

                self.register_allocator.release(value);

                Ok(())
            }
        }
    }

    fn gen_scope_start(
        &mut self,
        scope: &AstScopeNode,
        flags: Option<ScopeFlags>,
    ) -> EmitResult<()> {
        self.gen_push_scope(scope, flags)?;
        self.gen_init_tdz_for_scope(scope)?;
        self.gen_scope_functions(scope)?;

        Ok(())
    }

    /// Create and push a new scope onto the scope stack if necessary.
    fn gen_push_scope(
        &mut self,
        scope: &AstScopeNode,
        flags: Option<ScopeFlags>,
    ) -> EmitResult<()> {
        let mut flags = flags.unwrap_or(ScopeFlags::empty());

        match scope.kind() {
            ScopeNodeKind::Function { .. }
            | ScopeNodeKind::FunctionBody
            | ScopeNodeKind::Global
            | ScopeNodeKind::Eval { is_strict: true, .. }
            | ScopeNodeKind::StaticInitializer => {
                flags |= ScopeFlags::IS_VAR_SCOPE;
            }
            _ => {}
        }

        // Push a new scope onto the scope stack if necessary
        match scope.kind() {
            // Function scopes are considered var hoist targets
            ScopeNodeKind::Function { .. } | ScopeNodeKind::FunctionBody => {
                if let Some(vm_node_id) = scope.vm_scope_id() {
                    let scope_names_index = self.gen_scope_names(scope, flags)?;
                    self.writer
                        .push_function_scope_instruction(scope_names_index);
                    self.push_scope_stack_node(vm_node_id);
                }
            }
            // Global, module, and with scopes are handled separately
            ScopeNodeKind::Global | ScopeNodeKind::Module | ScopeNodeKind::With => {}
            // Otherwise this is a generic lexical scope
            ScopeNodeKind::Block
            | ScopeNodeKind::Switch
            | ScopeNodeKind::Class
            | ScopeNodeKind::Eval { .. }
            | ScopeNodeKind::StaticInitializer => {
                if let Some(vm_node_id) = scope.vm_scope_id() {
                    let scope_names_index = self.gen_scope_names(scope, flags)?;
                    self.writer
                        .push_lexical_scope_instruction(scope_names_index);
                    self.push_scope_stack_node(vm_node_id);
                }
            }
        }

        Ok(())
    }

    /// Initialize the TDZ for a scope, setting all bindings in that scope that need a TDZ check
    /// to empty.
    fn gen_init_tdz_for_scope(&mut self, scope: &AstScopeNode) -> EmitResult<()> {
        for (name, binding) in scope.iter_bindings() {
            if binding.needs_tdz_check() {
                match binding.vm_location().unwrap() {
                    // Bindings that are stored directly in registers can have undefined loaded
                    // directly to them.
                    VMLocation::Argument(index) => {
                        let arg_reg = Register::argument(index);
                        self.writer.load_empty_instruction(arg_reg);
                    }
                    VMLocation::LocalRegister(index) => {
                        let local_reg = Register::local(index);
                        self.writer.load_empty_instruction(local_reg);
                    }
                    // Bindings stored as globals must first load empty to a temporary register
                    VMLocation::Global => {
                        let temporary_reg = self.register_allocator.allocate()?;
                        self.writer.load_empty_instruction(temporary_reg);
                        self.gen_store_global_identifier(name, temporary_reg)?;
                        self.register_allocator.release(temporary_reg)
                    }
                    // Bindings stored in scopes must first load empty to a temporary register
                    VMLocation::Scope { scope_id, index } => {
                        let empty_reg = self.register_allocator.allocate()?;
                        self.writer.load_empty_instruction(empty_reg);
                        self.gen_store_scope_binding(scope_id, index, empty_reg)?;
                        self.register_allocator.release(empty_reg);
                    }
                    // Bindings in the module scope will already have the TDZ initialized during
                    // linking.
                    VMLocation::ModuleScope { .. } => {}
                    VMLocation::EvalVar | VMLocation::WithVar => {
                        unreachable!("vars do not need TDZ checks")
                    }
                }
            }
        }

        Ok(())
    }

    /// Generate all function declarations in this scope, hoisted to the top of the scope.
    fn gen_scope_functions(&mut self, scope: &AstScopeNode) -> EmitResult<()> {
        for (_, binding) in scope.iter_bindings() {
            if let BindingKind::Function { is_expression: false, func_node, .. } = binding.kind() {
                self.gen_function_declaration(func_node.as_ref(), binding)?;
            }
        }

        Ok(())
    }

    fn gen_scope_names(
        &mut self,
        scope: &AstScopeNode,
        flags: ScopeFlags,
    ) -> EmitResult<GenConstantIndex> {
        let vm_scope_id = scope.vm_scope_id().unwrap();

        // Check if scope was already created and cached
        if let Some(constant_index) = self.scope_names_cache.get(&vm_scope_id) {
            return Ok(*constant_index);
        }

        let vm_node = self.scope_tree.get_vm_node(vm_scope_id);
        let names = Self::gen_scope_name_strings(self.cx, vm_node);

        let name_flags = Self::gen_scope_name_flags(scope, self.scope_tree);
        let scope_names = ScopeNames::new(self.cx, flags, &names, &name_flags);

        let scope_names_index = self
            .constant_table_builder
            .add_heap_object(scope_names.cast())?;
        let constant_index = ConstantIndex::new(scope_names_index);

        // Cache the scope names for future use
        self.scope_names_cache.insert(vm_scope_id, constant_index);

        Ok(constant_index)
    }

    fn gen_scope_name_strings(cx: Context, vm_node: &VMScopeNode) -> Vec<Handle<FlatString>> {
        vm_node
            .bindings()
            .iter()
            .map(|name| InternedStrings::get_str(cx, name).as_flat())
            .collect::<Vec<_>>()
    }

    fn push_scope_stack_node(&mut self, vm_scope_id: ScopeNodeId) {
        self.scope =
            Rc::new(ScopeStackNode { scope_id: vm_scope_id, parent: Some(self.scope.clone()) });
    }

    fn gen_scope_end(&mut self, scope: &AstScopeNode) {
        self.pop_scope(scope, /* unwind */ true)
    }

    fn pop_scope(&mut self, scope: &AstScopeNode, unwind: bool) {
        if scope.vm_scope_id().is_some() {
            self.writer.pop_scope_instruction();

            if unwind {
                self.scope = self.scope.parent.clone().unwrap();
            }
        }
    }

    fn gen_start_global_scope(&mut self, scope: &AstScopeNode) -> EmitResult<()> {
        // VM scope node is guaranteed to exist, since at minimum the realm is always stored
        let vm_scope_id = scope.vm_scope_id().unwrap();
        self.push_scope_stack_node(vm_scope_id);

        self.gen_scope_functions(scope)
    }

    /// Must only be called on AST scope node with a corresponding VM scope node.
    fn gen_scope_name_flags(scope: &AstScopeNode, scope_tree: &ScopeTree) -> Vec<ScopeNameFlags> {
        let vm_scope_id = scope.vm_scope_id().unwrap();
        let vm_scope = scope_tree.get_vm_node(vm_scope_id);

        let mut all_flags = vec![];
        for name in vm_scope.bindings() {
            let mut flags = ScopeNameFlags::empty();
            let binding = scope.get_binding_opt(name);

            if binding.is_none() {
                all_flags.push(flags);
                continue;
            }

            let binding = binding.unwrap();

            if binding.is_immutable() {
                flags |= ScopeNameFlags::IS_IMMUTABLE;
            }

            if binding.kind().is_lexically_scoped() {
                flags |= ScopeNameFlags::IS_LEXICAL;
            }

            // "arguments" is treated as a function parameter so that an `eval('var arguments')`
            // within function parameters errors.
            if binding.kind().is_function_parameter() || binding.kind().is_implicit_arguments() {
                flags |= ScopeNameFlags::IS_FUNCTION_PARAMETER;
            }

            if binding.kind().is_function_expression_name() {
                flags |= ScopeNameFlags::IS_FUNCTION_EXPRESSION_NAME;
            }

            if binding.kind().is_private_name() {
                flags |= ScopeNameFlags::IS_PRIVATE_NAME;
            }

            if binding.is_module_binding() {
                flags |= ScopeNameFlags::IS_MODULE_BINDING;
            }

            all_flags.push(flags);
        }

        all_flags
    }

    fn gen_statement_list(&mut self, stmts: &[ast::Statement]) -> EmitResult<StmtCompletion> {
        for stmt in stmts {
            let completion = self.gen_statement(stmt)?;

            // An abrupt completion signals the end of the statement list
            if completion.is_abrupt() {
                return Ok(completion);
            }
        }

        Ok(StmtCompletion::Normal)
    }

    /// Many statements complete to undefined by default if no other value is found.
    fn gen_undefined_completion_if_necessary(&mut self) {
        if let Some(completion_dest) = self.statement_completion_dest {
            self.writer.load_undefined_instruction(completion_dest);
        }
    }

    fn gen_if_statement(&mut self, stmt: &ast::IfStatement) -> EmitResult<StmtCompletion> {
        self.gen_undefined_completion_if_necessary();

        let condition = self.gen_outer_expression(&stmt.test)?;
        self.register_allocator.release(condition);

        if stmt.altern.is_none() {
            let join_block = self.new_block();

            // If there is no alternative, branch between consequent and join block
            self.write_jump_false_for_expression(&stmt.test.expr, condition, join_block)?;

            self.gen_statement(&stmt.conseq)?;
            self.start_block(join_block);

            // If there is no alternative then the statement always can complete normally, with
            // execution continuing afterwards.
            Ok(StmtCompletion::Normal)
        } else {
            let altern_block = self.new_block();
            let join_block = self.new_block();

            // If there is an alternative, branch between consequent and alternative blocks before
            // joining at the join block.
            self.write_jump_false_for_expression(&stmt.test.expr, condition, altern_block)?;

            let conseq_completion = self.gen_statement(&stmt.conseq)?;
            if !conseq_completion.is_abrupt() {
                self.write_jump_instruction(join_block)?;
            }

            self.start_block(altern_block);
            let altern_completion = self.gen_statement(stmt.altern.as_ref().unwrap())?;

            self.start_block(join_block);

            Ok(conseq_completion.combine(altern_completion))
        }
    }

    /// Return whether this expression is guaranteed to evaluate to a boolean. Used to elide
    /// unnecessary ToBoolean conversions.
    fn evaluates_to_boolean(expr: &ast::Expression) -> bool {
        match expr {
            ast::Expression::Boolean(_) => true,
            ast::Expression::Unary(unary) => unary.operator == ast::UnaryOperator::LogicalNot,
            ast::Expression::Binary(binary) => matches!(
                binary.operator,
                ast::BinaryOperator::EqEq
                    | ast::BinaryOperator::NotEq
                    | ast::BinaryOperator::EqEqEq
                    | ast::BinaryOperator::NotEqEq
                    | ast::BinaryOperator::LessThan
                    | ast::BinaryOperator::LessThanOrEqual
                    | ast::BinaryOperator::GreaterThan
                    | ast::BinaryOperator::GreaterThanOrEqual
            ),
            ast::Expression::Logical(logical) => match logical.operator {
                ast::LogicalOperator::And | ast::LogicalOperator::Or => {
                    Self::evaluates_to_boolean(&logical.left)
                        && Self::evaluates_to_boolean(&logical.right)
                }
                ast::LogicalOperator::NullishCoalesce => false,
            },
            _ => false,
        }
    }

    fn gen_switch_statement(&mut self, stmt: &ast::SwitchStatement) -> EmitResult<StmtCompletion> {
        self.gen_undefined_completion_if_necessary();

        let jump_targets = self.push_jump_statement_target(None, TargetKind::Switch);
        let join_block = jump_targets.break_block;

        let discriminant = self.gen_outer_expression(&stmt.discriminant)?;

        // Start the switch body which forms a new scope
        let switch_body_scope = stmt.scope.as_ref();
        self.gen_scope_start(switch_body_scope, None)?;

        // Create and jump to the case block ids which will be generated later
        let mut case_block_ids = Vec::with_capacity(stmt.cases.len());
        let mut default_case_index = stmt.cases.len();

        for (i, case) in stmt.cases.iter().enumerate() {
            let case_block_id = self.new_block();
            case_block_ids.push(case_block_id);
            // Write the condition for all non-default blocks
            if let Some(test) = &case.test {
                let test = self.gen_outer_expression(test)?;
                self.register_allocator.release(test);

                let is_equal = self.register_allocator.allocate()?;
                self.writer
                    .strict_equal_instruction(is_equal, discriminant, test);

                // Jump to the case body if the discriminant is equal to the test value
                self.register_allocator.release(is_equal);
                self.write_jump_true_instruction(is_equal, case_block_id)?;
            } else {
                default_case_index = i;
            }
        }

        self.register_allocator.release(discriminant);

        if default_case_index == stmt.cases.len() {
            // If there is no default case then jump to the join block
            self.write_jump_instruction(join_block)?;
        } else {
            // Otherwise jump to the default block
            self.write_jump_instruction(case_block_ids[default_case_index])?;
        }

        // Write the case block bodies in order
        for (case, case_block_id) in stmt.cases.iter().zip(&case_block_ids) {
            self.start_block(*case_block_id);
            self.gen_statement_list(&case.body)?;

            // Intentionally fall through to the next case body
        }

        self.start_block(join_block);
        self.pop_jump_statement_target();

        // End of the switch body scope
        self.gen_scope_end(switch_body_scope);

        // Switch statements always complete normally since there may be a break
        Ok(StmtCompletion::Normal)
    }

    fn gen_for_statement(
        &mut self,
        stmt: &ast::ForStatement,
        jump_targets: Option<JumpStatementTarget>,
    ) -> EmitResult<StmtCompletion> {
        self.gen_undefined_completion_if_necessary();

        let jump_targets =
            jump_targets.unwrap_or_else(|| self.push_jump_statement_target(None, TargetKind::Loop));

        let loop_start_block = self.new_block();
        let loop_end_block = self.new_block();
        let update_block = jump_targets.continue_block;
        let join_block = jump_targets.break_block;

        // Entire for statement forms a new scope. Safe to call `gen_scope_start` only once on init
        // since the only part of `gen_scope_start` that could be performed for a for statement
        // is setting up TDZ checks, which only needs to happen on init.
        let for_scope = stmt.scope.as_ref();
        self.gen_scope_start(for_scope, None)?;

        // Continue statements should only unwind scope chain to for statement's scope, so it can
        // be duplicated for the next iteration.
        self.current_jump_statement_target_mut().continue_scope_id = self.scope.scope_id;

        // Evaluate the init expression once before loop starts
        match stmt.init.as_deref() {
            Some(ast::ForInit::VarDecl(var_decl)) => {
                self.gen_variable_declaration(var_decl)?;
            }
            Some(ast::ForInit::Expression(expr)) => {
                let result = self.gen_outer_expression(expr)?;
                self.register_allocator.release(result);
            }
            None => {}
        }

        // For init is performed in its own scope, separate from the scope of the first iteration.
        // This can be satisfied by duplicating the scope so that iterations cannot modify the init
        // scope (which can be observed if init scope is captured).
        if for_scope.vm_scope_id().is_some() {
            self.writer.dup_scope_instruction();
        }

        self.start_block(loop_start_block);

        // Evaluate the test expression and either continue to body or break out of loop
        if let Some(test_expr) = stmt.test.as_deref() {
            let test = self.gen_outer_expression(test_expr)?;
            self.register_allocator.release(test);

            self.write_jump_false_for_expression(&test_expr.expr, test, loop_end_block)?;
        }

        // Evaluate the loop body
        self.gen_statement(&stmt.body)?;

        // Start the update block, which is always emitted even if loop body has an abrupt
        // completion since there may be a continue.
        self.start_block(update_block);

        // If for statement has a scope (meaning the init bindings were captured), duplicate the
        // scope for the next iteration.
        if for_scope.vm_scope_id().is_some() {
            self.writer.dup_scope_instruction();
        }

        // Evaluate the update expression and return to the beginning of the loop
        if let Some(update_expr) = stmt.update.as_deref() {
            let update = self.gen_outer_expression(update_expr)?;
            self.register_allocator.release(update);
        }

        self.write_jump_instruction(loop_start_block)?;

        // Start the loop end block, which is run when the loop completes normally and unwinds the
        // for statement's scope. All other paths need to unwind the scope themselves.
        self.start_block(loop_end_block);
        self.gen_scope_end(for_scope);

        self.pop_jump_statement_target();

        // Continues to join block
        self.start_block(join_block);

        // Normal completion since there is always the test false path that skips the loop entirely
        Ok(StmtCompletion::Normal)
    }

    fn gen_for_of_statement(
        &mut self,
        stmt: &ast::ForEachStatement,
        jump_targets: Option<JumpStatementTarget>,
    ) -> EmitResult<StmtCompletion> {
        self.gen_undefined_completion_if_necessary();

        let jump_targets =
            jump_targets.unwrap_or_else(|| self.push_jump_statement_target(None, TargetKind::Loop));

        // Continues can unwind up to the loop target without leaving the finally scope
        let continue_jump_target_depth = self.jump_statement_target_stack.len();

        let loop_end_block = self.new_block();
        let iteration_end_block = self.new_block();
        let iteration_start_block = jump_targets.continue_block;
        let join_block = jump_targets.break_block;

        // Entire for-of statement forms a new scope. This scope is created around the init to
        // handle TDZ checks (since the right hand side is evaluated with left hand bindings in
        // scope but not yet initialized).
        let for_scope = stmt.scope.as_ref();
        self.gen_scope_start(for_scope, None)?;

        let iterator = self.register_allocator.allocate()?;
        let next_method = self.register_allocator.allocate()?;

        // Evaluate the right hand side and get its iterator
        let iterable = self.gen_outer_expression(&stmt.right)?;

        if stmt.is_await {
            self.writer
                .get_async_iterator_instruction(iterator, next_method, iterable);
        } else {
            self.writer
                .get_iterator_instruction(iterator, next_method, iterable);
        }

        self.register_allocator.release(iterable);

        // End of scope for evaluating the right hand side
        self.gen_scope_end(for_scope);

        // Start of a single iteration, which is in the entire for-of statement's scope again
        self.start_block(iteration_start_block);
        self.gen_scope_start(for_scope, None)?;

        // Iteration starts by calling IteratorNext to get the value and done flag
        let value = self.register_allocator.allocate()?;
        let is_done = self.register_allocator.allocate()?;

        // For-await-of must await the iterator result before unpacking it, essentially breaking up
        // the combined IteratorNext around the await.
        if stmt.is_await {
            let iterator_result = self.register_allocator.allocate()?;
            self.writer.call_with_receiver_instruction(
                iterator_result,
                next_method,
                iterator,
                // Dummy argv since no args are provided
                iterator,
                UInt::new(0),
            );

            let awaited_result = self.gen_await(iterator_result, ExprDest::Any)?;
            self.register_allocator.release(awaited_result);

            self.writer
                .iterator_unpack_result_instruction(value, is_done, awaited_result);
        } else {
            // Synchronous for-of can use a combined IteratorNext instruction
            self.writer
                .iterator_next_instruction(value, is_done, iterator, next_method);
        }

        // If we are done then exit the loop, closing the iterator is not needed
        self.write_jump_true_instruction(is_done, loop_end_block)?;
        self.register_allocator.release(is_done);

        // Body of the loop is generated inside a finally scope
        {
            // Generate the body of the for loop inside an exception handler
            let (body_handler, _) = self.gen_in_exception_handler(|this| {
                // Assigning to patterns needs a TDZ check
                let store_flags = if stmt.left.is_decl() {
                    StoreFlags::INITIALIZATION
                } else {
                    StoreFlags::NEEDS_TDZ_CHECK
                };

                // Loop body starts by storing this iteration's value to the pattern
                this.enter_has_assign_expr_context(stmt.left.has_assign_expr());
                this.gen_store_to_pattern(stmt.left.pattern(), value, store_flags)?;
                this.register_allocator.release(value);
                this.exit_has_assign_expr_context();

                // Breaks cannot reach the loop target without leaving the finally scope
                this.push_finally_scope_jump_target();
                let break_jump_target_depth = this.jump_statement_target_stack.len();

                // Set up the finally scope for the body
                this.gen_start_finally_scope(
                    iteration_end_block,
                    break_jump_target_depth,
                    continue_jump_target_depth,
                )?;

                // Then continues to the rest of the loop body
                this.gen_statement(&stmt.body)
            })?;

            // Body continues to the end of the current iteration
            self.write_jump_instruction(iteration_end_block)?;

            // Body exception handler continues to the finally block, setting the descriptor to mark
            // the incoming branch.
            self.gen_exception_handler_continue_to_finally(body_handler)?;

            // Leave the finally scope and its associated jump statement target entry
            let finally_scope = self.pop_finally_scope();
            self.pop_jump_statement_target();

            // Start the finally block
            self.gen_start_finally_block(&finally_scope);

            // Finally is only entered if there is an abrupt completion in the body, so always
            // try to close the iterator. Catch exceptions when closing the iterator.
            let (mut close_handler, _) = self.gen_in_exception_handler(|this| {
                if stmt.is_await {
                    this.gen_async_iterator_close(iterator)
                } else {
                    this.writer.iterator_close_instruction(iterator);
                    Ok(())
                }
            })?;

            // Otherwise enter the close exception handler
            if !finally_scope.has_non_throw_branch() {
                // Simple case where the body has only a throw branch. In this case we know that
                // the original body's exception will be re-thrown, so we can ignore the exception
                // from the close handler.
                //
                // We also know that the finally footer will contain a single instruction to
                // re-throw the original body's exception, so the IteratorClose exception handler
                // can jump directly to the finally footer.
                close_handler.handler = self.writer.current_offset();
                self.exception_handler_builder.add(close_handler);
            } else {
                // Otherwise we must generate a more complex IteratorClose exception handler that
                // performs a discriminant check.

                // If the close was successful, continue to the regular finally footer
                let finally_footer = self.new_block();
                self.write_jump_instruction(finally_footer)?;

                self.gen_for_of_close_handler(close_handler, &finally_scope)?;

                // The finally footer will follow the body of the finally
                self.start_block(finally_footer);
            }

            // Emit the footer of the finally block, which checks the discriminant to determine the
            // incoming branch that was taken.
            self.gen_end_finally_block(&finally_scope, StmtCompletion::Normal, None)?;
        }

        // Finally end this iteration's scope and start a new iteration
        self.start_block(iteration_end_block);
        self.gen_scope_end(for_scope);
        self.write_jump_instruction(iteration_start_block)?;

        self.register_allocator.release(next_method);
        self.register_allocator.release(iterator);

        // Start the loop end block, which is run when the loop completes normally and pops the
        // for-of statement's scope. All other paths need to unwind the scope themselves.
        self.start_block(loop_end_block);
        self.pop_scope(for_scope, /* unwind */ false);

        self.pop_jump_statement_target();

        // Continues to join block
        self.start_block(join_block);

        // Normal completion since the loop could always be avoided entirely
        Ok(StmtCompletion::Normal)
    }

    /// Generate a for-of IteratorClose exception handler when the for-of body's finally scope has a
    /// non-throw branch.
    ///
    /// Check if the original body threw, and if so rethrow that exception. Otherwise the exception
    /// from this close handler is thrown.
    fn gen_for_of_close_handler(
        &mut self,
        mut close_handler: ExceptionHandlerBuilder,
        finally_scope: &FinallyScope,
    ) -> EmitResult<()> {
        // Error is placed in a new scratch register
        let close_exception = self.register_allocator.allocate()?;
        close_handler.error_register = Some(close_exception);
        close_handler.handler = self.writer.current_offset();

        // Save the completed close handler
        self.exception_handler_builder.add(close_handler);

        let throw_block = self.new_block();

        // Check if original body threw by checking the finally discriminant
        let temp = self.register_allocator.allocate()?;
        self.gen_load_finally_branch_id(finally_scope.throw_branch, temp)?;
        self.writer
            .strict_equal_instruction(temp, finally_scope.discriminant_register, temp);
        self.write_jump_false_instruction(temp, throw_block)?;
        self.register_allocator.release(temp);

        // If so then move the exception to the register that will be thrown
        self.write_mov_instruction(close_exception, finally_scope.result_register);

        // Throw the exception, which may be the original body's or the catch handler's
        self.start_block(throw_block);
        self.writer.throw_instruction(close_exception);
        self.register_allocator.release(close_exception);

        Ok(())
    }

    fn gen_for_in_statement(
        &mut self,
        stmt: &ast::ForEachStatement,
        jump_targets: Option<JumpStatementTarget>,
    ) -> EmitResult<StmtCompletion> {
        self.gen_undefined_completion_if_necessary();

        let jump_targets =
            jump_targets.unwrap_or_else(|| self.push_jump_statement_target(None, TargetKind::Loop));

        let loop_end_block = self.new_block();
        let iteration_start_block = jump_targets.continue_block;
        let join_block = jump_targets.break_block;

        // Entire for-in statement forms a new scope. This scope is created around the init to
        // handle TDZ checks (since the right hand side is evaluated with left hand bindings in
        // scope but not yet initialized).
        let for_scope = stmt.scope.as_ref();
        self.gen_scope_start(for_scope, None)?;

        // Entire for-in statement is skipped if right hand side is nullish
        let object = self.gen_outer_expression(&stmt.right)?;
        self.write_jump_nullish_instruction(object, loop_end_block)?;

        // Otherwise create new for-in iterator object
        self.register_allocator.release(object);
        let iterator = self.register_allocator.allocate()?;
        self.writer
            .new_for_in_iterator_instruction(iterator, object);

        // End of scope for evaluating the right hand side
        self.gen_scope_end(for_scope);

        // Start of a single iteration, which is in the entire for-in statement's scope again
        self.start_block(iteration_start_block);
        self.gen_scope_start(for_scope, None)?;

        // Assigning to patterns needs a TDZ check
        let store_flags = if stmt.left.is_decl() {
            StoreFlags::INITIALIZATION
        } else {
            StoreFlags::NEEDS_TDZ_CHECK
        };

        // Iteration starts by calling the for-in iterator's `next` method
        let next_result = self.gen_for_each_next_result_dest(stmt, store_flags)?;
        self.writer.for_in_next_instruction(next_result, iterator);

        // An undefined result from `next` means there are no more keys, jump out of the loop
        self.write_jump_nullish_instruction(next_result, loop_end_block)?;

        // Otherwise `next` returned a key, so store the key to the pattern
        self.enter_has_assign_expr_context(stmt.left.has_assign_expr());
        self.gen_store_to_pattern(stmt.left.pattern(), next_result, store_flags)?;
        self.register_allocator.release(next_result);
        self.exit_has_assign_expr_context();

        // Otherwise proceed to the body of the loop
        self.gen_statement(&stmt.body)?;

        // Finally end this iteration's scope and start a new iteration
        self.gen_scope_end(for_scope);
        self.write_jump_instruction(iteration_start_block)?;

        self.register_allocator.release(iterator);

        // Start the loop end block, which is run when the loop completes normally and pops the
        // for-in statement's scope. All other paths need to unwind the scope themselves.
        self.start_block(loop_end_block);
        self.pop_scope(for_scope, /* unwind */ false);

        self.pop_jump_statement_target();

        // Continues to join block
        self.start_block(join_block);

        // Normal completion since the loop could always be avoided entirely
        Ok(StmtCompletion::Normal)
    }

    /// Determine register in which to place the iterator's `next` result
    fn gen_for_each_next_result_dest(
        &mut self,
        stmt: &ast::ForEachStatement,
        store_flags: StoreFlags,
    ) -> EmitResult<GenRegister> {
        match stmt.left.pattern() {
            // If storing to an id we attempt to store directly
            ast::Pattern::Id(id) => {
                let dest = self.expr_dest_for_id(id, store_flags);
                self.allocate_destination(dest)
            }
            // Otherwise we store to a temporary register
            _ => self.register_allocator.allocate(),
        }
    }

    fn gen_async_iterator_close(&mut self, iterator: GenRegister) -> EmitResult<()> {
        let has_return_method = self.register_allocator.allocate()?;
        let return_result = self.register_allocator.allocate()?;

        // Perform the first part of AsyncIteratorClose up until the await, storing intermediate
        // results in registers.
        self.writer.async_iterator_close_start_instruction(
            return_result,
            has_return_method,
            iterator,
        );

        // If there was no return method then there is no return result and we are done closing
        let join_block = self.new_block();
        self.write_jump_false_instruction(has_return_method, join_block)?;

        // Otherwise there was a return result so await it
        let awaited_return_result = self.gen_await(return_result, ExprDest::Any)?;

        // And then finish the AsyncIteratorClosure using the stored intermediate results
        self.writer
            .async_iterator_close_finish_instruction(awaited_return_result);

        self.start_block(join_block);

        self.register_allocator.release(awaited_return_result);
        self.register_allocator.release(has_return_method);

        Ok(())
    }

    fn gen_while_statement(
        &mut self,
        stmt: &ast::WhileStatement,
        jump_targets: Option<JumpStatementTarget>,
    ) -> EmitResult<StmtCompletion> {
        self.gen_undefined_completion_if_necessary();

        let jump_targets =
            jump_targets.unwrap_or_else(|| self.push_jump_statement_target(None, TargetKind::Loop));

        let loop_start_block = jump_targets.continue_block;
        let join_block = jump_targets.break_block;

        // Evaluate test and either continue to body or break out of loop
        self.start_block(loop_start_block);
        let test = self.gen_outer_expression(&stmt.test)?;
        self.register_allocator.release(test);

        self.write_jump_false_for_expression(&stmt.test.expr, test, join_block)?;

        let body_completion = self.gen_statement(&stmt.body)?;

        // Always jump back to the condition at the start of the loop
        if !body_completion.is_abrupt() {
            self.write_jump_instruction(loop_start_block)?;
        }

        self.start_block(join_block);
        self.pop_jump_statement_target();

        // Normal completion since there is always the test false path that skips the loop entirely
        Ok(StmtCompletion::Normal)
    }

    fn gen_do_while_statement(
        &mut self,
        stmt: &ast::DoWhileStatement,
        jump_targets: Option<JumpStatementTarget>,
    ) -> EmitResult<StmtCompletion> {
        self.gen_undefined_completion_if_necessary();

        let jump_targets =
            jump_targets.unwrap_or_else(|| self.push_jump_statement_target(None, TargetKind::Loop));

        let loop_start_block = self.new_block();
        let test_block = jump_targets.continue_block;
        let join_block = jump_targets.break_block;

        // Execute the body at the start of the loop
        self.start_block(loop_start_block);
        self.gen_statement(&stmt.body)?;

        // Then evaluate test and either break out of loop or continue to next iteration. Test is
        // always evaluated regardless of body completion since a continue could appear.
        self.start_block(test_block);
        let test = self.gen_outer_expression(&stmt.test)?;
        self.register_allocator.release(test);
        self.write_jump_true_for_expression(&stmt.test.expr, test, loop_start_block)?;

        self.start_block(join_block);
        self.pop_jump_statement_target();

        // Normal completion since even with an abnormal body there might be a continue statement
        // that proceeds past the loop.
        Ok(StmtCompletion::Normal)
    }

    fn gen_with_statement(&mut self, stmt: &ast::WithStatement) -> EmitResult<StmtCompletion> {
        self.gen_undefined_completion_if_necessary();

        let object = self.gen_outer_expression(&stmt.object)?;

        let with_scope = stmt.scope.as_ref();

        // Start with scope
        let vm_scope_id = with_scope.vm_scope_id().unwrap();
        let scope_names_index = self.gen_scope_names(with_scope, ScopeFlags::empty())?;
        self.writer
            .push_with_scope_instruction(object, scope_names_index);
        self.push_scope_stack_node(vm_scope_id);

        self.register_allocator.release(object);
        self.gen_scope_start(with_scope, None)?;

        let body_completion = self.gen_statement(stmt.body.as_ref())?;

        // End with scope
        self.gen_scope_end(with_scope);

        Ok(body_completion)
    }

    fn gen_try_statement(&mut self, stmt: &ast::TryStatement) -> EmitResult<StmtCompletion> {
        if stmt.finalizer.is_none() {
            self.gen_try_catch(stmt)
        } else {
            self.gen_try_finally(stmt)
        }
    }

    fn gen_try_catch(&mut self, stmt: &ast::TryStatement) -> EmitResult<StmtCompletion> {
        self.gen_undefined_completion_if_necessary();

        // Save the scope when entering the try statement so it can be restored when leaving
        let saved_scope = self.register_allocator.allocate()?;
        self.write_mov_instruction(saved_scope, Register::scope());

        // Generate the body of the try statement inside an exception handler
        let (body_handler, body_completion) =
            self.gen_in_exception_handler(|this| this.gen_block_statement(&stmt.block))?;

        let join_block = self.new_block();

        // Body continues to the join block
        if !body_completion.is_abrupt() {
            self.write_jump_instruction(join_block)?;
        }

        // Emit the catch clause
        let (catch_completion, _) =
            self.gen_catch_clause(stmt.handler.as_ref().unwrap(), saved_scope, body_handler)?;

        self.register_allocator.release(saved_scope);

        self.start_block(join_block);

        // Result completion is abrupt if both body and catch blocks have an abrupt completion.
        Ok(body_completion.combine(catch_completion))
    }

    fn gen_catch_clause(
        &mut self,
        catch_clause: &ast::CatchClause,
        scope_to_restore: GenRegister,
        mut body_handler: ExceptionHandlerBuilder,
    ) -> EmitResult<(StmtCompletion, ExceptionHandlerBuilder)> {
        enum Param<'a> {
            LocalRegister { index: usize },
            Scope { scope_id: ScopeNodeId, index: usize, error_reg: GenRegister },
            Destructuring { param: &'a ast::Pattern, error_reg: GenRegister },
            None,
        }

        // Determine the type of catch parameter, and if a temporary register is needed then
        // allocate it immediately so that it cannot be clobbered before use e.g. by destructuring.
        let param = if let Some(param) = catch_clause.param.as_deref() {
            if param.is_id() {
                match param.to_id().get_binding().vm_location().unwrap() {
                    VMLocation::LocalRegister(index) => Param::LocalRegister { index },
                    VMLocation::Scope { scope_id, index } => {
                        let error_reg = self.register_allocator.allocate()?;
                        Param::Scope { scope_id, index, error_reg }
                    }
                    _ => unreachable!("catch parameters must be in a register or VM scope"),
                }
            } else {
                let error_reg = self.register_allocator.allocate()?;
                Param::Destructuring { param, error_reg }
            }
        } else {
            Param::None
        };

        body_handler.handler = self.writer.current_offset();

        // Emit the catch clause in its own block, with bounds saved for a finally handler
        let catch_block = self.new_block();
        self.start_block(catch_block);
        let catch_handler_start = self.writer.current_offset();

        // Restore the scope from before the try statement
        self.write_mov_instruction(Register::scope(), scope_to_restore);

        // Catch scope starts before the parameter is evaluated and potentially destructured
        let catch_scope = catch_clause.scope.as_ref();
        self.gen_scope_start(catch_scope, None)?;

        // If there is a catch parameter, mark the register in the handler
        match param {
            // Catch parameters stored in registers tell the handler to place the error directly in
            // the register.
            Param::LocalRegister { index } => {
                body_handler.error_register = Some(Register::local(index));
            }
            // Catch parameters stored in VM scopes tell the handler to place the error in a
            // temporary register, then store it to the scope.
            Param::Scope { scope_id, index, error_reg } => {
                self.gen_store_scope_binding(scope_id, index, error_reg)?;
                self.register_allocator.release(error_reg);

                body_handler.error_register = Some(error_reg);
            }
            // Otherwise destructure catch parameter. Destructuring is in its own "has assignment
            // expression" context.
            Param::Destructuring { param, error_reg } => {
                // Storing at declaration does not need a TDZ check
                self.enter_has_assign_expr_context(catch_clause.param_has_assign_expr);
                self.gen_store_to_pattern(param, error_reg, StoreFlags::INITIALIZATION)?;
                self.register_allocator.release(error_reg);
                self.exit_has_assign_expr_context();

                body_handler.error_register = Some(error_reg);
            }
            Param::None => {}
        }

        // Save try body's exception handler now that it is complete
        self.exception_handler_builder.add(body_handler);

        // No need to write a jump from catch to next block after body, since either the finally
        // or join block will be emitted directly after the catch.
        let catch_completion = self.gen_block_statement(&catch_clause.body)?;
        let catch_handler_end = self.writer.current_offset();

        let catch_handler = ExceptionHandlerBuilder::new(catch_handler_start, catch_handler_end);

        Ok((catch_completion, catch_handler))
    }

    fn gen_in_exception_handler<R>(
        &mut self,
        f: impl FnOnce(&mut Self) -> EmitResult<R>,
    ) -> EmitResult<(ExceptionHandlerBuilder, R)> {
        // Mark the range of instructions around the callback
        let body_handler_start = self.writer.current_offset();
        let result = f(self)?;
        let body_handler_end = self.writer.current_offset();

        let body_handler = ExceptionHandlerBuilder::new(body_handler_start, body_handler_end);

        Ok((body_handler, result))
    }

    /// Enter a new finally scope, setting up necessary registers and saving old scope.
    fn gen_start_finally_scope(
        &mut self,
        normal_join_block: BlockId,
        break_jump_target_depth: usize,
        continue_jump_target_depth: usize,
    ) -> EmitResult<()> {
        // Shared discriminant and result register used to communicate the branch that was taken
        // along with the return or throw result.
        let discriminant = self.register_allocator.allocate()?;
        let result = self.register_allocator.allocate()?;

        let finally_block = self.new_block();

        // Save the scope when entering the try statement so it can be restored when leaving
        let saved_scope = self.register_allocator.allocate()?;
        self.write_mov_instruction(saved_scope, Register::scope());

        // Both the try body and catch clause are in a new finally scope, meaning any abrupt
        // completions will be tracked (i.e. returns, breaks, and continues).
        let finally_scope = FinallyScope::new(
            discriminant,
            result,
            saved_scope,
            finally_block,
            normal_join_block,
            break_jump_target_depth,
            continue_jump_target_depth,
        );
        self.push_finally_scope(finally_scope);

        Ok(())
    }

    /// Enter a dummy jump target frame to mark the finally scope. This frame is skipped during
    /// lookups, but adds to the jump target depth to distinguish break/continues that try to
    /// leave the finally scope.
    fn push_finally_scope_jump_target(&mut self) {
        self.jump_statement_target_stack.push(JumpStatementTarget {
            kind: TargetKind::FinallyScope,
            label_id: None,
            break_block: 0,
            continue_block: 0,
            break_scope_id: 0,
            continue_scope_id: 0,
        });
    }

    /// Generate the provided exception handler, completing it with the required missing fields.
    ///
    /// Exception handler continues to the current finally block, marking the incoming branch as a
    /// throw branch;
    fn gen_exception_handler_continue_to_finally(
        &mut self,
        mut handler: ExceptionHandlerBuilder,
    ) -> EmitResult<()> {
        let finally_scope = self.current_finally_scope().unwrap();

        let discriminant = finally_scope.discriminant_register;
        let throw_branch_id = finally_scope.throw_branch();

        // Error is placed in the result register
        handler.error_register = Some(finally_scope.result_register);
        handler.handler = self.writer.current_offset();

        // Mark the incoming throw branch and continue directly to finally block
        self.gen_load_finally_branch_id(throw_branch_id, discriminant)?;

        // Save the completed exception handler
        self.exception_handler_builder.add(handler);

        Ok(())
    }

    /// Generate the provided exception handlers, completing them with the required missing fields.
    ///
    /// All exception handlers continues to the current finally block, marking the incoming branch
    /// as a throw branch;
    fn gen_exception_handlers_continue_to_finally(
        &mut self,
        handlers: Vec<ExceptionHandlerBuilder>,
    ) -> EmitResult<()> {
        let finally_scope = self.current_finally_scope().unwrap();

        let discriminant = finally_scope.discriminant_register;
        let throw_branch_id = finally_scope.throw_branch();
        let result_register = finally_scope.result_register;

        for mut handler in handlers {
            // Error is placed in the result register
            handler.error_register = Some(result_register);
            handler.handler = self.writer.current_offset();

            // Save the completed exception handler
            self.exception_handler_builder.add(handler);
        }

        // Mark the incoming throw branch and continue directly to finally block
        self.gen_load_finally_branch_id(throw_branch_id, discriminant)?;

        Ok(())
    }

    /// Start the finally block for the given finally scope.
    ///
    /// Restores the scope from before the finally scope was entered.
    fn gen_start_finally_block(&mut self, finally_scope: &FinallyScope) {
        // Emit the finally block
        self.start_block(finally_scope.finally_block);

        // Restore the scope from before the try statement
        self.write_mov_instruction(Register::scope(), finally_scope.saved_scope_register);
        self.register_allocator
            .release(finally_scope.saved_scope_register);
    }

    /// End the finally block for the given finally scope.
    ///
    /// This includes writing the finally footer if necessary, and releasing all remaining registers
    /// held throughout the finally scope.
    ///
    /// Optionally takes a completion that is restored when taking the normal path.
    fn gen_end_finally_block(
        &mut self,
        finally_scope: &FinallyScope,
        finally_block_completion: StmtCompletion,
        saved_normal_completion: Option<GenRegister>,
    ) -> EmitResult<()> {
        // Emit the footer of the finally block, which checks the discriminant to determine the
        // incoming branch that was taken.
        if !finally_block_completion.is_abrupt() {
            self.gen_finally_footer(finally_scope, saved_normal_completion)?;
        }

        if let Some(reg) = saved_normal_completion {
            self.register_allocator.release(reg);
        }

        self.register_allocator
            .release(finally_scope.result_register);
        self.register_allocator
            .release(finally_scope.discriminant_register);

        Ok(())
    }

    fn gen_try_finally(&mut self, stmt: &ast::TryStatement) -> EmitResult<StmtCompletion> {
        let join_block = self.new_block();

        // If break or continue reaches this target then the finally scope has been left
        self.push_finally_scope_jump_target();
        let jump_target_depth = self.jump_statement_target_stack.len();

        // Enter a finally scope, setting up necessary registers
        self.gen_start_finally_scope(join_block, jump_target_depth, jump_target_depth)?;

        // Extract fields from finally scope
        let finally_scope = self.current_finally_scope().unwrap();
        let discriminant = finally_scope.discriminant_register;
        let saved_scope = finally_scope.saved_scope_register;
        let finally_block = finally_scope.finally_block;

        // Generate the body of the try statement inside an exception handler
        self.gen_undefined_completion_if_necessary();
        let (body_handler, body_completion) =
            self.gen_in_exception_handler(|this| this.gen_block_statement(&stmt.block))?;

        // Body continues to the finally block, setting the discriminant to mark incoming branch
        if !body_completion.is_abrupt() {
            let normal_branch_id = self.current_finally_scope().unwrap().add_normal_branch();
            self.gen_load_finally_branch_id(normal_branch_id, discriminant)?;
            self.write_jump_instruction(finally_block)?;
        }

        // If there is a catch clause, generate it then continue to the finally block.
        if let Some(catch_clause) = stmt.handler.as_ref() {
            // Generate catch clause body
            let (catch_completion, catch_handler) =
                self.gen_catch_clause(catch_clause, saved_scope, body_handler)?;

            // Catch clause continues normally to finally block
            if !catch_completion.is_abrupt() {
                let normal_branch_id = self.current_finally_scope().unwrap().add_normal_branch();
                self.gen_load_finally_branch_id(normal_branch_id, discriminant)?;
                self.write_jump_instruction(finally_block)?;
            }

            // Catch handler catches exceptions, continues to finally, then rethrows
            self.gen_exception_handler_continue_to_finally(catch_handler)?;
        } else {
            // If there is no catch, body exception handler should continue to the finally block,
            // setting the descriptor to mark the incoming branch.
            self.gen_exception_handler_continue_to_finally(body_handler)?;
        }

        let finally_scope = self.pop_finally_scope();
        self.pop_jump_statement_target();

        // Start the finally block
        self.gen_start_finally_block(&finally_scope);

        // If statement completions are being calculated we must save the completion from the body
        // and catch paths and potentially restore it after the finally block.
        let saved_body_catch_completion =
            if let Some(completion_dest) = self.statement_completion_dest {
                let reg = self.register_allocator.allocate()?;
                self.write_mov_instruction(reg, completion_dest);
                Some(reg)
            } else {
                None
            };

        // Then emit the body of the finally block
        self.gen_undefined_completion_if_necessary();
        let finalizer_completion = self.gen_block_statement(stmt.finalizer.as_ref().unwrap())?;

        // Emit the footer of the finally block, which checks the discriminant to determine the
        // incoming branch that was taken.
        self.gen_end_finally_block(
            &finally_scope,
            finalizer_completion,
            saved_body_catch_completion,
        )?;

        self.start_block(join_block);

        // Finalizer is always run, so if it has an abrupt completion then the entire try statement
        // has an abrupt completion. If normal, there always may be a normal branch.
        Ok(finalizer_completion)
    }

    fn gen_finally_footer(
        &mut self,
        finally_scope: &FinallyScope,
        saved_normal_completion: Option<GenRegister>,
    ) -> EmitResult<()> {
        let discriminant = finally_scope.discriminant_register;
        let result = finally_scope.result_register;

        let test_value = self.register_allocator.allocate()?;

        // All branches blocks but the first will be jumped to (control falls through to the first
        // block after the finally body).
        let mut finally_branch_blocks = vec![0];
        for _ in 0..finally_scope.num_branches - 1 {
            finally_branch_blocks.push(self.new_block());
        }

        let mut i = 0;

        // Add throw branch. Error was placed in the result register.
        self.gen_finally_discriminant_check(
            i,
            finally_scope.throw_branch,
            &mut finally_branch_blocks,
            discriminant,
            test_value,
        )?;
        self.writer.throw_instruction(result);
        i += 1;

        // Add normal branch if present, continues to normal join block.
        if let Some(normal_branch_id) = finally_scope.normal_branch {
            self.gen_finally_discriminant_check(
                i,
                normal_branch_id,
                &mut finally_branch_blocks,
                discriminant,
                test_value,
            )?;

            // If calculating completions, restore the completion on the normal path
            if let Some(saved_normal_completion) = saved_normal_completion {
                self.write_mov_instruction(
                    self.statement_completion_dest.unwrap(),
                    saved_normal_completion,
                );
            }

            self.write_jump_instruction(finally_scope.normal_join_block)?;
            i += 1;
        }

        // Add return branch if present. Return value was placed in the result register.
        if let Some(return_branch_id) = finally_scope.return_branch {
            self.gen_finally_discriminant_check(
                i,
                return_branch_id,
                &mut finally_branch_blocks,
                discriminant,
                test_value,
            )?;
            self.gen_return(Some(result), finally_scope.derived_constructor_scope)?;
            i += 1;
        }

        // Add break and continue blocks if present. Context to restore was placed in the result
        // register.
        for target in finally_scope.jump_target_branches.iter() {
            self.gen_finally_discriminant_check(
                i,
                target.branch_id,
                &mut finally_branch_blocks,
                discriminant,
                test_value,
            )?;
            self.gen_jump_target(
                target.block_id,
                target.scope_id,
                target.jump_target_depth,
                target.is_break,
            )?;
            i += 1;
        }

        self.register_allocator.release(test_value);

        Ok(())
    }

    fn gen_load_finally_branch_id(
        &mut self,
        finally_branch_id: FinallyBranchId,
        dest: GenRegister,
    ) -> EmitResult<()> {
        let test_imm =
            SInt::try_from_signed(finally_branch_id as isize).ok_or(EmitError::IntegerTooLarge)?;
        self.writer.load_immediate_instruction(dest, test_imm);

        Ok(())
    }

    fn gen_finally_discriminant_check(
        &mut self,
        i: usize,
        finally_branch_id: FinallyBranchId,
        finally_branch_blocks: &mut [BlockId],
        discriminant: GenRegister,
        test_value: GenRegister,
    ) -> EmitResult<()> {
        // All branches but the first are jumped to (first is fallen through to)
        if i != 0 {
            self.start_block(finally_branch_blocks[i]);
        }

        // No test needed for final branch
        if i == finally_branch_blocks.len() - 1 {
            return Ok(());
        }

        // Check if the discriminant matches the branch id
        self.gen_load_finally_branch_id(finally_branch_id, test_value)?;
        self.writer
            .strict_equal_instruction(test_value, discriminant, test_value);

        // Jump to the next branch if the discriminant does not match
        let next_branch_block = finally_branch_blocks[i + 1];
        self.write_jump_false_instruction(test_value, next_branch_block)?;

        // Continue to the case where the discriminant matches

        Ok(())
    }

    fn gen_throw_statement(&mut self, stmt: &ast::ThrowStatement) -> EmitResult<StmtCompletion> {
        let error = self.gen_outer_expression(&stmt.argument)?;
        self.writer.throw_instruction(error);
        self.register_allocator.release(error);

        Ok(StmtCompletion::Abrupt)
    }

    fn gen_return_statement(&mut self, stmt: &ast::ReturnStatement) -> EmitResult<StmtCompletion> {
        let derived_constructor_scope = stmt.this_scope;

        if stmt.argument.is_none() {
            self.gen_return(/* return_arg */ None, derived_constructor_scope)?;
            return Ok(StmtCompletion::Abrupt);
        }

        let mut return_arg = self.gen_outer_expression(stmt.argument.as_ref().unwrap())?;

        // Async generators must await the return argument before returning it
        if self.is_async() && self.is_generator() {
            return_arg = self.gen_await(return_arg, ExprDest::Any)?;
        }

        self.gen_return(Some(return_arg), derived_constructor_scope)?;
        self.register_allocator.release(return_arg);

        Ok(StmtCompletion::Abrupt)
    }

    /// Return a value from the current function, returning `undefined` if no value is provided.
    ///
    /// May not emit a return instruction directly, e.g. if return is within a finally scope.
    fn gen_return(
        &mut self,
        return_arg: Option<GenRegister>,
        derived_constructor_scope: Option<AstPtr<AstScopeNode>>,
    ) -> EmitResult<()> {
        // If not in a finally scope we can return directly
        if self.finally_scopes.is_empty() {
            // If in a derived constructor we must check if we are returning undefined, in which
            // case we return the `this` value instead.
            if self.is_derived_constructor() {
                // This value may be loaded from a scope if captured
                let gen_this_value = |this: &mut Self, dest: ExprDest| {
                    if let Some(scope) = derived_constructor_scope {
                        if let Some(VMLocation::Scope { scope_id, index }) =
                            scope.as_ref().get_binding("this").vm_location()
                        {
                            return this.gen_load_scope_binding(scope_id, index, dest);
                        }
                    }

                    Ok(Register::this())
                };

                if let Some(return_arg) = return_arg {
                    let return_value = self.register_allocator.allocate()?;
                    let return_block = self.new_block();

                    // Set the return arg as the return value
                    self.write_mov_instruction(return_value, return_arg);

                    // If the return arg is not undefined then return it directly. Otherwise overwrite
                    // the return value with `this` and return `this` instead.
                    self.write_jump_not_undefined_instruction(return_arg, return_block)?;

                    let this_value = gen_this_value(self, ExprDest::Fixed(return_value))?;
                    self.writer.check_this_initialized_instruction(this_value);
                    self.write_mov_instruction(return_value, this_value);

                    self.start_block(return_block);
                    self.writer.ret_instruction(return_value);
                    self.register_allocator.release(return_value);

                    return Ok(());
                } else {
                    // If no return argument is provided then derived constructor returns `this`.
                    // Must first assert that `this` was initialized.
                    let this_value = gen_this_value(self, ExprDest::Any)?;
                    self.writer.check_this_initialized_instruction(this_value);
                    self.writer.ret_instruction(this_value);
                    self.register_allocator.release(this_value);

                    return Ok(());
                }
            }

            if let Some(return_arg) = return_arg {
                // Return the value directly if one was provided
                self.gen_ret_or_resolve(return_arg);
            } else {
                // Otherwise load undefined then return it
                let return_arg = self.register_allocator.allocate()?;
                self.writer.load_undefined_instruction(return_arg);
                self.gen_ret_or_resolve(return_arg);
                self.register_allocator.release(return_arg);
            }
        } else {
            // If in a finally scope we must mark the finally as having a return branch
            let finally_scope = self.current_finally_scope().unwrap();
            let return_branch_id = finally_scope.add_return_branch(derived_constructor_scope);
            let discriminant_register = finally_scope.discriminant_register;
            let result_register = finally_scope.result_register;
            let finally_block = finally_scope.finally_block;

            // Then jump to the finally block with return value in the shared result register,
            // marking that we should follow the return branch after the finally completes.
            self.gen_load_finally_branch_id(return_branch_id, discriminant_register)?;

            // Save the return arg in the result register if one was provided, otherwise save
            // undefined.
            if let Some(return_arg) = return_arg {
                self.write_mov_instruction(result_register, return_arg);
            } else {
                self.writer.load_undefined_instruction(result_register);
            }

            self.write_jump_instruction(finally_block)?;
        }

        Ok(())
    }

    /// Resolve the current async function's promise with the provided value and return the promise,
    /// if the current function is async. Otherwise return the argument.
    fn gen_ret_or_resolve(&mut self, argument: GenRegister) {
        if let Some(promise_index) = self.promise_index {
            let promise = Register::local(promise_index as usize);
            self.writer.resolve_promise_instruction(promise, argument);
            self.writer.ret_instruction(promise);
        } else {
            self.writer.ret_instruction(argument)
        }
    }

    /// Break or continue to the given label (or to the innermost loop if no label is provided).
    ///
    /// May not emit a break instruction directly, e.g. if break/continue is within a finally scope.
    fn gen_jump_target(
        &mut self,
        target_block_id: BlockId,
        target_scope_id: usize,
        jump_target_depth: usize,
        is_break: bool,
    ) -> EmitResult<()> {
        // Check if we are breaking/continuing outside of the finally scope
        if let Some(finally_scope) = self.finally_scopes.last_mut() {
            let finally_scope_jump_target_depth = if is_break {
                finally_scope.break_jump_target_depth
            } else {
                finally_scope.continue_jump_target_depth
            };

            if jump_target_depth < finally_scope_jump_target_depth {
                // Jump to the finally block, marking that we should follow the break/continue
                // branch after the finally completes.
                let finally_scope = self.current_finally_scope().unwrap();
                let branch_id = finally_scope.add_jump_target_branch(
                    target_block_id,
                    target_scope_id,
                    jump_target_depth,
                    is_break,
                );
                let discriminant_register = finally_scope.discriminant_register;
                let finally_block = finally_scope.finally_block;

                self.gen_load_finally_branch_id(branch_id, discriminant_register)?;
                self.write_jump_instruction(finally_block)?;

                return Ok(());
            }
        }

        // Otherwise should perform a regular break/continue - jump to the target block, unwinding
        // the stack.
        self.unwind_scope_chain_to_scope(target_scope_id)?;
        self.write_jump_instruction(target_block_id)?;

        Ok(())
    }

    fn gen_labeled_statement(
        &mut self,
        stmt: &ast::LabeledStatement,
    ) -> EmitResult<StmtCompletion> {
        // Find the innermost labeled statement
        let mut inner_stmt = stmt;
        while let ast::Statement::Labeled(labeled_stmt) = inner_stmt.body.as_ref() {
            inner_stmt = labeled_stmt;
        }

        // All nested labels share the same id
        let label_id = inner_stmt.label.id;

        match inner_stmt.body.as_ref() {
            ast::Statement::For(stmt) => {
                let jump_targets =
                    self.push_jump_statement_target(Some(label_id), TargetKind::Loop);
                self.gen_for_statement(stmt, Some(jump_targets))?;
            }
            ast::Statement::ForEach(stmt) => {
                let jump_targets =
                    self.push_jump_statement_target(Some(label_id), TargetKind::Loop);
                match stmt.kind {
                    ast::ForEachKind::In => {
                        self.gen_for_in_statement(stmt, Some(jump_targets))?;
                    }
                    ast::ForEachKind::Of => {
                        self.gen_for_of_statement(stmt, Some(jump_targets))?;
                    }
                }
            }
            ast::Statement::While(stmt) => {
                let jump_targets =
                    self.push_jump_statement_target(Some(label_id), TargetKind::Loop);
                self.gen_while_statement(stmt, Some(jump_targets))?;
            }
            ast::Statement::DoWhile(stmt) => {
                let jump_targets =
                    self.push_jump_statement_target(Some(label_id), TargetKind::Loop);
                self.gen_do_while_statement(stmt, Some(jump_targets))?;
            }
            stmt => {
                // Do not generate a continue block for labeled non-loop statements
                let jump_targets =
                    self.push_jump_statement_target(Some(label_id), TargetKind::Other);
                self.gen_statement(stmt)?;

                // The break target is after the labeled statement
                self.start_block(jump_targets.break_block);
                self.pop_jump_statement_target();
            }
        }

        // Always completes normally as there could have been a break at label statement
        Ok(StmtCompletion::Normal)
    }

    fn push_jump_statement_target(
        &mut self,
        label_id: Option<ast::LabelId>,
        kind: TargetKind,
    ) -> JumpStatementTarget {
        let break_block = self.new_block();
        let continue_block = if kind == TargetKind::Loop {
            self.new_block()
        } else {
            0
        };
        let scope_id = self.scope.scope_id;

        self.jump_statement_target_stack.push(JumpStatementTarget {
            label_id,
            kind,
            break_block,
            continue_block,
            // Default to breaking and continuing to the current scope. Note that the continue id
            // may be overwritten to a lower scope in for loops.
            break_scope_id: scope_id,
            continue_scope_id: scope_id,
        });

        self.jump_statement_target_stack.last().unwrap().clone()
    }

    fn pop_jump_statement_target(&mut self) {
        self.jump_statement_target_stack.pop();
    }

    fn current_jump_statement_target_mut(&mut self) -> &mut JumpStatementTarget {
        self.jump_statement_target_stack.last_mut().unwrap()
    }

    /// Lookup the innermost loop jump statement target, or the target that matches the label id if
    /// one is provided.
    ///
    /// Also returns the "jump target depth" of the matching jump target, aka one more than its
    /// index in the jump statement target stack.
    fn lookup_jump_statement_target(
        &self,
        label_id: Option<LabelId>,
        lookup_break: bool,
    ) -> (usize, &JumpStatementTarget) {
        let (index, target) = if let Some(label_id) = label_id {
            self.jump_statement_target_stack
                .iter()
                .enumerate()
                .rev()
                .find(|(_, target)| target.label_id == Some(label_id))
                .unwrap()
        } else if lookup_break {
            // An unlabeled break ignores non-loop labeled statements
            self.jump_statement_target_stack
                .iter()
                .enumerate()
                .rev()
                .find(|(_, target)| {
                    target.kind == TargetKind::Loop || target.kind == TargetKind::Switch
                })
                .unwrap()
        } else {
            // Find the most recent loop, since continues are only allowed for loops
            self.jump_statement_target_stack
                .iter()
                .enumerate()
                .rev()
                .find(|(_, target)| target.kind == TargetKind::Loop)
                .unwrap()

            // let last_jump_target = self.jump_statement_target_stack.last().unwrap();
            // let last_depth = self.jump_statement_target_stack.len();
            // (last_depth, last_jump_target)
        };

        (index + 1, target)
    }

    fn push_finally_scope(&mut self, finally_scope: FinallyScope) {
        self.finally_scopes.push(finally_scope);
    }

    fn pop_finally_scope(&mut self) -> FinallyScope {
        self.finally_scopes.pop().unwrap()
    }

    fn current_finally_scope(&mut self) -> Option<&mut FinallyScope> {
        self.finally_scopes.last_mut()
    }

    /// Pop scopes until the current scope equals the provided scope.
    fn unwind_scope_chain_to_scope(&mut self, scope_id: usize) -> EmitResult<()> {
        let scope_depth = self.find_scope_depth(scope_id)?.unsigned().to_usize();

        for _ in 0..scope_depth {
            self.writer.pop_scope_instruction();
        }

        Ok(())
    }

    fn gen_break_statement(&mut self, stmt: &ast::BreakStatement) -> EmitResult<StmtCompletion> {
        let label_id = stmt.label.as_ref().map(|l| l.id);
        let (jump_target_depth, target) =
            self.lookup_jump_statement_target(label_id, /* lookup_break */ true);

        self.gen_jump_target(
            target.break_block,
            target.break_scope_id,
            jump_target_depth,
            /* is_break */ true,
        )?;

        Ok(StmtCompletion::Abrupt)
    }

    fn gen_continue_statement(
        &mut self,
        stmt: &ast::ContinueStatement,
    ) -> EmitResult<StmtCompletion> {
        let label_id = stmt.label.as_ref().map(|l| l.id);
        let (jump_target_depth, target) =
            self.lookup_jump_statement_target(label_id, /* lookup_break */ false);

        self.gen_jump_target(
            target.continue_block,
            target.continue_scope_id,
            jump_target_depth,
            /* is_break */ false,
        )?;

        Ok(StmtCompletion::Abrupt)
    }

    fn gen_global_names(&mut self, global_scope: &AstScopeNode) -> EmitResult<Handle<GlobalNames>> {
        // Collect all global variables and functions in scope
        let mut global_vars = HashSet::new();
        let mut global_funcs = HashSet::new();

        for (name, binding) in global_scope.iter_var_decls() {
            let name = InternedStrings::get_str(self.cx, name).as_flat();
            if let BindingKind::Function { .. } = binding.kind() {
                global_funcs.insert(name);
            } else {
                global_vars.insert(name);
            }
        }

        // VM scope node is guaranteed to exist, since at minimum the realm is always stored.
        let vm_scope_id = global_scope.vm_scope_id().unwrap();
        let vm_node = self.scope_tree.get_vm_node(vm_scope_id);

        // Create a ScopeNames object containing all lexical names in scope.
        let binding_flags = Self::gen_scope_name_flags(global_scope, self.scope_tree);

        let names = Self::gen_scope_name_strings(self.cx, vm_node);
        let scope_names =
            ScopeNames::new(self.cx, ScopeFlags::IS_VAR_SCOPE, &names, &binding_flags);

        // Add all var and lex names to the GlobalNames object, which will be used later when
        // instantiating the global scope.
        Ok(GlobalNames::new(self.cx, global_vars, global_funcs, scope_names))
    }

    /// Generate the name of a declaration that is part of a default export declaration, defaulting
    /// to "*default*" if the declaration has no name.
    fn gen_default_export_name(
        mut cx: Context,
        decl: &ast::ExportDefaultKind,
    ) -> Handle<FlatString> {
        let id = match decl {
            ast::ExportDefaultKind::Function(func) => &func.id,
            ast::ExportDefaultKind::Class(class) => &class.id,
            ast::ExportDefaultKind::Expression(_) => &None,
        };

        if let Some(id) = id {
            cx.alloc_string(&id.name)
        } else {
            cx.names.default_name().as_string().as_flat()
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum EmitError {
    ConstantTableTooLarge,
    TooManyFunctionParameters,
    TooManyCallArguments,
    TooManyRegisters,
    TooManyScopes,
    IndexTooLarge,
    IntegerTooLarge,
}

impl Error for EmitError {}

impl fmt::Display for EmitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EmitError::ConstantTableTooLarge => write!(f, "Constant table too large"),
            EmitError::TooManyFunctionParameters => write!(f, "Too many function parameters"),
            EmitError::TooManyCallArguments => write!(f, "Too many call arguments"),
            EmitError::TooManyRegisters => write!(f, "Too many registers"),
            EmitError::TooManyScopes => write!(f, "Too many scopes"),
            EmitError::IndexTooLarge => write!(f, "Index too large"),
            EmitError::IntegerTooLarge => write!(f, "Integer too large"),
        }
    }
}

pub type EmitResult<T> = Result<T, EmitError>;

impl<T: Escapable> Escapable for EmitResult<T> {
    #[inline]
    fn escape(&self, cx: Context) -> Self {
        match self {
            Ok(ok) => Ok(ok.escape(cx)),
            Err(err) => Err(*err),
        }
    }
}

type BlockId = usize;

struct ForwardJumpInfo {
    /// Offset of the jump instruction (including prefixes) from the start of the bytecode.
    jump_offset: usize,
    /// Width reserved in the constant table.
    constant_table_width: WidthEnum,
}

// Type aliases for operands referenced in the generator. These operands have the max width, and
// will be narrowed when possible when writing individual instructions.
pub type GenRegister = Register<ExtraWide>;
type GenUInt = UInt<ExtraWide>;
type GenSInt = SInt<ExtraWide>;
type GenConstantIndex = ConstantIndex<ExtraWide>;

enum PendingFunctionNode {
    Declaration(AstPtr<ast::Function>),
    Expression {
        node: AstPtr<ast::Function>,
        name: Option<Wtf8String>,
    },
    Arrow {
        node: AstPtr<ast::Function>,
        name: Option<Wtf8String>,
    },
    Method {
        node: AstPtr<ast::Function>,
        name: Option<Wtf8String>,
    },
    Constructor {
        node: Option<AstPtr<ast::Function>>,
        name: Wtf8String,
        fields: ClassFieldsInitializer,
        is_base: bool,
        source_range: Range<Pos>,
    },
    ClassFieldsInitializer {
        scope: AstPtr<AstScopeNode>,
        fields: Vec<ClassField>,
        class: AstPtr<ast::Class>,
    },
    ClassStaticInitializer {
        scope: AstPtr<AstScopeNode>,
        elements: Vec<ClassStaticElement>,
        class: AstPtr<ast::Class>,
    },
}

impl PendingFunctionNode {
    fn ast_ptr(&self) -> AstPtr<ast::Function> {
        match self {
            PendingFunctionNode::Declaration(node)
            | PendingFunctionNode::Expression { node, .. }
            | PendingFunctionNode::Arrow { node, .. }
            | PendingFunctionNode::Method { node, .. } => *node,
            PendingFunctionNode::Constructor { node, .. } => node.unwrap(),
            PendingFunctionNode::ClassFieldsInitializer { .. }
            | PendingFunctionNode::ClassStaticInitializer { .. } => unreachable!(),
        }
    }

    fn is_constructor(&self) -> bool {
        match self {
            PendingFunctionNode::Declaration(node)
            | PendingFunctionNode::Expression { node, .. } => {
                let func = node.as_ref();
                !func.is_async() && !func.is_generator()
            }
            PendingFunctionNode::Constructor { .. } => true,
            PendingFunctionNode::Arrow { .. }
            | PendingFunctionNode::Method { .. }
            | PendingFunctionNode::ClassFieldsInitializer { .. }
            | PendingFunctionNode::ClassStaticInitializer { .. } => false,
        }
    }

    /// Named given to this unnamed function (an anonymous expression or arrow function)
    fn default_name(&self) -> Option<&Wtf8String> {
        match self {
            PendingFunctionNode::Declaration(_)
            | PendingFunctionNode::ClassFieldsInitializer { .. }
            | PendingFunctionNode::ClassStaticInitializer { .. } => None,
            PendingFunctionNode::Expression { name, .. }
            | PendingFunctionNode::Arrow { name, .. }
            | PendingFunctionNode::Method { name, .. } => name.as_ref(),
            PendingFunctionNode::Constructor { name, .. } => Some(name),
        }
    }
}

/// Collection of functions that still need to be generated, along with their index in the
/// function's constant table and the scope they should be generated in.
type PendingFunctionNodes = Vec<(PendingFunctionNode, FuncGenPatch, Rc<ScopeStackNode>)>;

enum FuncGenPatch {
    ParentFunction(ConstantTableIndex),
    Export(usize),
}

/// Information needed to patch a function into some other object once the function is created.
enum Patch {
    ParentFunction {
        /// The already generated parent function which creates this function
        parent_function: Handle<BytecodeFunction>,
        /// The index into the parent function's constant table that needs to be patched with the
        /// function once it is created.
        constant_index: ConstantTableIndex,
    },
    Export {
        /// The slot in the module scope that contains a BoxedValue that needs to be patched with
        /// the function once it is created.
        slot_index: usize,
    },
}

struct PendingFunction {
    /// This pending function's AST node.
    func_node: PendingFunctionNode,
    /// The scope that this function should be generated in.
    scope: Rc<ScopeStackNode>,
    patch: Patch,
}

pub struct EmitFunctionResult {
    pub bytecode_function: Handle<BytecodeFunction>,
    pending_functions: PendingFunctionNodes,
}

impl EmitFunctionResult {
    fn empty() -> Self {
        EmitFunctionResult {
            bytecode_function: Handle::dangling(),
            pending_functions: Vec::new(),
        }
    }
}

/// The destination register for an expression's value.
#[derive(Clone, Copy)]
enum ExprDest {
    /// Value could be in any register, including locals and arguments.
    Any,
    /// Value must be placed in a newly allocated temporary register.
    NewTemporary,
    /// Value must be placed in a specific register.
    Fixed(GenRegister),
}

struct Reference<'a> {
    kind: ReferenceKind<'a>,
    init: Option<&'a ast::Expression>,
}

impl<'a> Reference<'a> {
    fn new(kind: ReferenceKind<'a>) -> Self {
        Reference { kind, init: None }
    }
}

enum ReferenceKind<'a> {
    Id(&'a ast::Identifier),
    NamedProperty {
        object: GenRegister,
        property: GenConstantIndex,
    },
    ComputedProperty {
        object: GenRegister,
        property: GenRegister,
    },
    PrivateProperty {
        object: GenRegister,
        property: GenRegister,
    },
    SuperProperty {
        home_object: GenRegister,
        this_value: GenRegister,
        property: GenRegister,
    },
    ArrayPattern(&'a ast::ArrayPattern),
    ObjectPattern(&'a ast::ObjectPattern),
}

pub enum JumpOperand {
    RelativeOffset(GenSInt),
    ConstantIndex(GenConstantIndex),
}

/// Information about where jump statements (break and continue) should jump to.
#[derive(Clone)]
struct JumpStatementTarget {
    /// Block that breaks jump to.
    break_block: BlockId,
    /// Block that continues jump to.
    continue_block: BlockId,
    /// The kind of statement this jump target is for.
    kind: TargetKind,
    /// Label id if this is a labeled statement, otherwise this is an unlabeled loop.
    label_id: Option<ast::LabelId>,
    /// Scope id of the scope to restore on a break.
    break_scope_id: usize,
    /// Scope id of the scope to restore on a continue.
    continue_scope_id: usize,
}

#[derive(Clone, PartialEq)]
enum TargetKind {
    Loop,
    Switch,
    FinallyScope,
    Other,
}

struct CallReceiver {
    /// Reference to be filled with the receiver for a call.
    receiver: GenRegister,
    /// Whether the receiver is within an optional chain.
    is_chain: bool,
}

bitflags! {
    #[derive(Clone, Copy)]
    struct StoreFlags: u8 {
        const INITIALIZATION = (1 << 0);
        const NEEDS_TDZ_CHECK = (1 << 1);
    }
}

/// A node in the stack of scopes the generator is currently inside, including scopes of parent
/// functions.
struct ScopeStackNode {
    /// The VM scope id of the scope.
    scope_id: usize,
    /// The parent scope, or none if this is the global scope
    parent: Option<Rc<ScopeStackNode>>,
}

/// Identifier for a branch that led to a finally block.
type FinallyBranchId = u32;

/// A finally scope tracks all abnormal completions that occur within it, since those branches must
/// all be threaded through the same finally block. Branches are distinguished with a branch id.
struct FinallyScope {
    /// Register in which to place an integer indicating the branch to take
    discriminant_register: GenRegister,
    /// Register in which to place the return value or exception
    result_register: GenRegister,
    /// Register which holds the scope to restore when exiting the finally body
    saved_scope_register: GenRegister,
    /// Start of the finally block
    finally_block: BlockId,
    /// Block to jump to after the normal completion branch of the finally is done
    normal_join_block: BlockId,
    /// Marks the highest jump statement target for which a break or continue to that target will
    /// not leave the finally scope. Saved as the depth of the jump statement target stack.
    break_jump_target_depth: usize,
    continue_jump_target_depth: usize,
    /// The branch to take if an exception is thrown. Throw branch always exists.
    throw_branch: FinallyBranchId,
    /// Normal branch is the normal exit of the target block, which must continue to the finally
    /// block before proceeding normally. Set if the target block has a normal exit.
    normal_branch: Option<FinallyBranchId>,
    /// Set if the target block has a return statement, which must first continue to the finally
    /// block before performing the return.
    return_branch: Option<FinallyBranchId>,
    /// The scope of the derived constructor that this finally scope is within, if any. Only set if
    /// there is a return branch within a derived constructor.
    derived_constructor_scope: Option<AstPtr<AstScopeNode>>,
    /// All break and continue statements in the target block, which muts first continue to the
    /// finally block before performing the break or continue.
    jump_target_branches: Vec<JumpTargetBranch>,
    /// Total number of branches that must be threaded through the finally block.
    num_branches: FinallyBranchId,
}

struct JumpTargetBranch {
    /// The unique id of the branch.
    branch_id: FinallyBranchId,
    /// The block id that the branch jumps to.
    block_id: BlockId,
    /// The scope id that the branch jumps to.
    scope_id: usize,
    /// The depth of the jump target stack for the target.
    jump_target_depth: usize,
    /// Whether this branch is for a break or continue.
    is_break: bool,
}

impl FinallyScope {
    fn new(
        discriminant_register: GenRegister,
        result_register: GenRegister,
        saved_scope_register: GenRegister,
        finally_block: BlockId,
        normal_join_block: BlockId,
        break_jump_target_depth: usize,
        continue_jump_target_depth: usize,
    ) -> Self {
        FinallyScope {
            discriminant_register,
            result_register,
            saved_scope_register,
            finally_block,
            normal_join_block,
            break_jump_target_depth,
            continue_jump_target_depth,
            throw_branch: 0,
            normal_branch: None,
            return_branch: None,
            derived_constructor_scope: None,
            jump_target_branches: Vec::new(),
            num_branches: 1,
        }
    }

    fn next_branch_id(&mut self) -> FinallyBranchId {
        let id = self.num_branches;
        self.num_branches += 1;
        id
    }

    fn throw_branch(&self) -> FinallyBranchId {
        self.throw_branch
    }

    fn has_non_throw_branch(&self) -> bool {
        self.normal_branch.is_some()
            || self.return_branch.is_some()
            || !self.jump_target_branches.is_empty()
    }

    fn add_normal_branch(&mut self) -> FinallyBranchId {
        if let Some(normal_branch) = self.normal_branch {
            normal_branch
        } else {
            let next_branch_id = self.next_branch_id();
            self.normal_branch = Some(next_branch_id);
            next_branch_id
        }
    }

    fn add_return_branch(
        &mut self,
        derived_constructor_scope: Option<AstPtr<AstScopeNode>>,
    ) -> FinallyBranchId {
        if let Some(return_branch) = self.return_branch {
            return_branch
        } else {
            let next_branch_id = self.next_branch_id();
            self.return_branch = Some(next_branch_id);
            self.derived_constructor_scope = derived_constructor_scope;
            next_branch_id
        }
    }

    fn add_jump_target_branch(
        &mut self,
        target_block_id: BlockId,
        target_scope_id: usize,
        jump_target_depth: usize,
        is_break: bool,
    ) -> FinallyBranchId {
        if let Some(target_branch) = self
            .jump_target_branches
            .iter()
            .find(|branch| target_block_id == branch.block_id && is_break == branch.is_break)
        {
            target_branch.branch_id
        } else {
            let next_branch_id = self.next_branch_id();
            self.jump_target_branches.push(JumpTargetBranch {
                branch_id: next_branch_id,
                block_id: target_block_id,
                scope_id: target_scope_id,
                jump_target_depth,
                is_break,
            });
            next_branch_id
        }
    }
}

enum ArrayElement<'a> {
    Expression(&'a ast::Expression),
    Spread(&'a ast::SpreadElement),
    Hole,
}

enum ClassField {
    Named {
        name: Wtf8String,
        field: AstPtr<ast::ClassProperty>,
    },
    Computed {
        scope_id: usize,
        scope_index: usize,
        field: AstPtr<ast::ClassProperty>,
    },
    PrivateField {
        field: AstPtr<ast::ClassProperty>,
    },
    PrivateMethodOrAccessor {
        name: AstPtr<ast::Identifier>,
        scope_id: usize,
        scope_index: usize,
        is_getter: bool,
        is_setter: bool,
    },
}

struct ClassFieldsInitializer {
    fields: Vec<ClassField>,
    scope: Option<AstPtr<AstScopeNode>>,
    class: AstPtr<ast::Class>,
}

impl ClassFieldsInitializer {
    fn none() -> Self {
        Self { fields: vec![], scope: None, class: AstPtr::uninit() }
    }
}

enum ClassStaticElement {
    Initializer(AstPtr<ast::ClassMethod>),
    Field(ClassField),
}

enum CallArgs {
    Normal { argv: GenRegister, argc: GenUInt },
    Varargs { args: GenRegister, receiver: GenRegister },
}

enum GenClassKind<'a> {
    Declaration,
    Expression,
    Export { name: &'a str, binding: &'a Binding },
}

/// A reference to a string that may be either wtf8 or utf8.
#[derive(Copy, Clone)]
enum AnyStr<'a> {
    Wtf8(&'a Wtf8String),
    Str(&'a str),
}

impl AnyStr<'_> {
    fn from_id(id: &ast::Identifier) -> AnyStr {
        AnyStr::Str(id.name.as_str())
    }

    fn to_wtf8_string(self) -> Wtf8String {
        match self {
            AnyStr::Wtf8(wtf8) => (*wtf8).clone(),
            AnyStr::Str(str) => Wtf8String::from_str(str),
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum StmtCompletion {
    /// Continue normally to the next statement in at least one path through this function.
    Normal,
    /// Do not continue to the next statement in any paths through this statement, e.g. due to a
    /// return, throw, break, continue, etc.
    Abrupt,
}

impl StmtCompletion {
    fn is_abrupt(&self) -> bool {
        *self == StmtCompletion::Abrupt
    }

    /// When two completions are joined, the result is only abrupt if all paths are abrupt.
    fn combine(&self, other: StmtCompletion) -> StmtCompletion {
        match (self, other) {
            (StmtCompletion::Abrupt, StmtCompletion::Abrupt) => StmtCompletion::Abrupt,
            _ => StmtCompletion::Normal,
        }
    }
}

type FunctionVec = BsVec<HeapPtr<BytecodeFunction>>;

struct FunctionVecField<'a>(&'a mut Option<Handle<FunctionVec>>);

impl<'a> BsVecField<HeapPtr<BytecodeFunction>> for FunctionVecField<'a> {
    fn new_vec(cx: Context, capacity: usize) -> HeapPtr<FunctionVec> {
        BsVec::new(cx, ObjectKind::ValueVec, capacity)
    }

    fn get(&self) -> HeapPtr<FunctionVec> {
        self.0.as_ref().unwrap().get_()
    }

    fn set(&mut self, vec: HeapPtr<FunctionVec>) {
        *self.0 = Some(vec.to_handle());
    }
}
