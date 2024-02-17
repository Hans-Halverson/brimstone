use std::{collections::HashSet, mem::size_of, rc::Rc};

use crate::{
    js::{
        parser::{
            ast::{self},
            parser::ParseProgramResult,
            scope_tree::BindingKind,
            source::Source,
        },
        runtime::{
            completion::{Completion, EvalResult},
            environment::{environment::Environment, global_environment::GlobalEnvironment},
            error::{syntax_error, type_error},
            execution_context::{ExecutionContext, ScriptOrModule},
            function::instantiate_function_object,
            gc::{Handle, HandleScope, HeapObject, HeapVisitor},
            interned_strings::InternedStrings,
            object_descriptor::{ObjectDescriptor, ObjectKind},
            realm::Realm,
            string_value::FlatString,
            Context, HeapPtr,
        },
    },
    maybe__, must, set_uninit,
};

use super::{pattern::id_string_value, statement::eval_toplevel_list};

// 16.1.4 Script Record
#[repr(C)]
pub struct Script {
    descriptor: HeapPtr<ObjectDescriptor>,
    realm: HeapPtr<Realm>,
    script_node: Rc<ParseProgramResult>,
}

impl Script {
    pub fn new(
        cx: Context,
        script_node: Rc<ParseProgramResult>,
        realm: Handle<Realm>,
    ) -> Handle<Script> {
        let mut script = cx.alloc_uninit::<Script>();

        set_uninit!(script.descriptor, cx.base_descriptors.get(ObjectKind::Script));
        set_uninit!(script.script_node, script_node);
        set_uninit!(script.realm, realm.get_());

        script.to_handle()
    }

    pub fn source(&self) -> &Rc<Source> {
        &self.script_node.program.source
    }
}

/// 16.1.6 ScriptEvaluation
pub fn eval_script(
    cx: Context,
    parse_result: Rc<ParseProgramResult>,
    realm: Handle<Realm>,
) -> Completion {
    HandleScope::new(cx, |mut cx| {
        let script = Script::new(cx, parse_result.clone(), realm);

        let global_env = realm.global_env();
        let global_env_object = global_env.into_dyn_env();

        let script_ctx = ExecutionContext::new(
            cx,
            /* function */ None,
            realm,
            Some(ScriptOrModule::Script(script)),
            /* lexical_env */ Some(global_env_object),
            /* variable_env */ Some(global_env_object),
            /* private_env */ None,
            parse_result.program.is_strict_mode,
        );

        cx.push_execution_context(script_ctx);

        let mut result = global_declaration_instantiation(cx, &parse_result.program, global_env);

        if result.is_normal() {
            result = eval_toplevel_list(cx, &parse_result.program.toplevels);
        }

        if result.is_empty() {
            result = cx.undefined().into();
        }

        cx.pop_execution_context();

        result
    })
}

/// 16.1.7 GlobalDeclarationInstantiation
fn global_declaration_instantiation(
    cx: Context,
    script: &ast::Program,
    mut env: Handle<GlobalEnvironment>,
) -> Completion {
    for (name, _) in script.scope.as_ref().iter_lex_decls() {
        let name_value = InternedStrings::get_str(cx, name);

        if env.has_var_declaration(name_value) || must!(env.has_lexical_declaration(cx, name_value))
        {
            return syntax_error(cx, &format!("redeclaration of {}", name_value));
        }

        if maybe__!(env.has_restricted_global_property(cx, name_value)) {
            return syntax_error(
                cx,
                &format!("cannot redeclare restricted global property {}", name_value),
            );
        }
    }

    for (name, _) in script.scope.as_ref().iter_var_decls() {
        let name_value = InternedStrings::get_str(cx, name);
        if must!(env.has_lexical_declaration(cx, name_value)) {
            return syntax_error(cx, &format!("redeclaration of {}", name_value));
        }
    }

    let mut declared_function_names = HashSet::new();
    // Functions to initialize are in reverse order from spec
    let mut functions_to_initialize = vec![];

    // Visit functions in reverse order, if functions have the same name only the last is used.
    for (name, binding) in script.scope.as_ref().iter_var_decls().rev() {
        if let BindingKind::Function { func_node, .. } = binding.kind() {
            if declared_function_names.insert(name) {
                let name_value = InternedStrings::get_str(cx, name);
                if !maybe__!(env.can_declare_global_function(cx, name_value)) {
                    return type_error(
                        cx,
                        &format!("cannot declare global function {}", name_value),
                    );
                }

                functions_to_initialize.push(func_node.as_ref());
            }
        }
    }

    // Order does not matter for declared var names, despite ordering in spec
    let mut declared_var_names: HashSet<Handle<FlatString>> = HashSet::new();

    for (name, binding) in script.scope.as_ref().iter_var_decls() {
        if let BindingKind::Var = binding.kind() {
            if !declared_function_names.contains(name) {
                let name_value = InternedStrings::get_str(cx, name);
                if !maybe__!(env.can_declare_global_var(cx, name_value)) {
                    return type_error(cx, &format!("cannot declare global var {}", name_value));
                }

                declared_var_names.insert(name_value.flatten());
            }
        }
    }

    for (name, binding) in script.scope.as_ref().iter_lex_decls() {
        let name_value = InternedStrings::get_str(cx, name);
        if binding.is_const() {
            maybe__!(env.create_immutable_binding(cx, name_value, true));
        } else {
            maybe__!(env.create_mutable_binding(cx, name_value, false));
        }
    }

    for func in functions_to_initialize.iter().rev() {
        let name_value = id_string_value(cx, func.id.as_deref().unwrap());
        let function_object = instantiate_function_object(cx, func, env.into_dyn_env(), None);
        maybe__!(env.create_global_function_binding(cx, name_value, function_object.into(), false));
    }

    for var_name in declared_var_names {
        maybe__!(env.create_global_var_binding(cx, var_name.as_string(), false));
    }

    Completion::empty(cx)
}

impl HeapObject for HeapPtr<Script> {
    fn byte_size(&self) -> usize {
        size_of::<Script>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.realm);
    }
}
