use std::{collections::HashSet, path::Path, rc::Rc};

use crate::{
    js::{
        parser::{
            analyze::analyze, parse_module, parser::ParseProgramResult, print_program,
            source::Source, ParseResult,
        },
        runtime::{
            abstract_operations::call_object,
            bytecode::generator::BytecodeProgramGenerator,
            error::{syntax_error, syntax_parse_error},
            eval_result::EvalResult,
            intrinsics::intrinsics::Intrinsic,
            promise_object::{PromiseCapability, PromiseObject},
            string_value::FlatString,
            Context, Handle, Realm,
        },
    },
    must,
};

use super::source_text_module::{ModuleId, ModuleState, SourceTextModule};

/// GraphLoadingStateRecord (https://tc39.es/ecma262/#graphloadingstate-record)
struct GraphLoader {
    is_loading: bool,
    pending_modules_count: usize,
    visited: HashSet<ModuleId>,
    promise_capability: Handle<PromiseCapability>,
    realm: Handle<Realm>,
}

impl GraphLoader {
    /// InnerModuleLoading (https://tc39.es/ecma262/#sec-InnerModuleLoading)
    fn inner_module_loading(&mut self, cx: Context, mut module: Handle<SourceTextModule>) {
        if module.state() == ModuleState::New && self.visited.insert(module.id()) {
            module.set_state(ModuleState::Unlinked);

            let specifiers = module.requested_module_specifiers();
            let loaded_modules = module.loaded_modules();

            self.pending_modules_count += specifiers.len();

            for i in 0..specifiers.len() {
                match loaded_modules.as_slice()[i] {
                    Some(loaded_module) => self.inner_module_loading(cx, loaded_module.to_handle()),
                    None => {
                        let module_specifier = specifiers.as_slice()[i].to_handle();

                        // Create the SourceTextModule for the module with the given specifier,
                        // or evaluate to an error.
                        let load_result = host_load_imported_module(
                            cx,
                            module.source_file_path(),
                            module_specifier,
                            self.realm,
                        );

                        // Continue module loading with the SourceTextModule or error result
                        self.finish_loading_imported_module(
                            cx,
                            module,
                            module_specifier,
                            load_result,
                        );
                    }
                }

                if !self.is_loading {
                    return;
                }
            }
        }

        self.pending_modules_count -= 1;

        if self.pending_modules_count == 0 {
            self.is_loading = false;

            must!(call_object(
                cx,
                self.promise_capability.resolve(),
                cx.undefined(),
                &[cx.undefined()]
            ));
        }
    }

    /// FinishLoadingImportedModule (https://tc39.es/ecma262/#sec-FinishLoadingImportedModule)
    fn finish_loading_imported_module(
        &mut self,
        cx: Context,
        mut referrer: Handle<SourceTextModule>,
        specifier: Handle<FlatString>,
        module_result: EvalResult<Handle<SourceTextModule>>,
    ) {
        if let Ok(module) = module_result {
            let module_index = referrer.lookup_specifier_index(*specifier).unwrap();
            if !referrer.has_loaded_module_at(module_index) {
                referrer.set_loaded_module_at(module_index, *module);
            }
        }

        self.continue_module_loading(cx, module_result);
    }

    /// ContinueModuleLoading (https://tc39.es/ecma262/#sec-ContinueModuleLoading)
    fn continue_module_loading(
        &mut self,
        cx: Context,
        module_result: EvalResult<Handle<SourceTextModule>>,
    ) {
        if !self.is_loading {
            return;
        }

        match module_result {
            Ok(module) => {
                self.inner_module_loading(cx, module);
            }
            Err(error) => {
                self.is_loading = false;
                must!(call_object(cx, self.promise_capability.reject(), cx.undefined(), &[error]));
            }
        }
    }
}

/// LoadRequestedModules (https://tc39.es/ecma262/#sec-LoadRequestedModules)
pub fn load_requested_modules(
    cx: Context,
    module: Handle<SourceTextModule>,
) -> Handle<PromiseObject> {
    let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
    let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));
    let realm = module.program_function_ptr().realm();

    let mut graph_loader = GraphLoader {
        is_loading: true,
        pending_modules_count: 1,
        visited: HashSet::new(),
        promise_capability: capability,
        realm,
    };

    graph_loader.inner_module_loading(cx, module);

    // Known to be a PromiseObject since it was created by the intrinsic Promise constructor
    capability.promise().cast::<PromiseObject>()
}

/// HostLoadImportedModule (https://tc39.es/ecma262/#sec-HostLoadImportedModule)
pub fn host_load_imported_module(
    mut cx: Context,
    source_file_path: Handle<FlatString>,
    module_specifier: Handle<FlatString>,
    realm: Handle<Realm>,
) -> EvalResult<Handle<SourceTextModule>> {
    let source_file_path = Path::new(&source_file_path.to_string())
        .canonicalize()
        .unwrap();
    let source_file_dir = source_file_path.parent().unwrap();

    // Join source file path with specifier path so that relative specifiers will be applied to
    // source path and absolute specifiers will overwrite source path.
    let new_module_path = source_file_dir
        .join(Path::new(&module_specifier.to_string()))
        .canonicalize();

    let new_module_path = match new_module_path {
        Ok(path) => path,
        Err(error) => return syntax_error(cx, &error.to_string()),
    };

    let new_module_path_string = new_module_path.to_str().unwrap().to_string();

    // Use the cached module if it has already been loaded
    if let Some(module) = cx.modules.get(&new_module_path_string) {
        return Ok(module.to_handle());
    }

    // Parse the file at the given path, returning AST
    let mut parse_result = match parse_file_at_path(cx, new_module_path.as_path()) {
        Ok(parse_result) => parse_result,
        Err(error) => return syntax_parse_error(cx, &error),
    };

    // Analyze AST
    if let Err(parse_errors) = analyze(&mut parse_result) {
        return syntax_parse_error(cx, &parse_errors.errors[0]);
    }

    if cx.options.print_ast {
        println!("{}", print_program(&parse_result.program));
    }

    // Finally generate the SourceTextModule for the parsed module
    let bytecode_result = BytecodeProgramGenerator::generate_from_parse_module_result(
        cx,
        &Rc::new(parse_result),
        realm,
    );

    let module = match bytecode_result {
        Ok(module) => module,
        Err(error) => return syntax_error(cx, &error.to_string()),
    };

    // Cache the module
    cx.modules.insert(new_module_path_string, *module);

    Ok(module)
}

fn parse_file_at_path(cx: Context, path: &Path) -> ParseResult<ParseProgramResult> {
    let source = Rc::new(Source::new_from_file(path.to_str().unwrap())?);
    parse_module(&source, cx.options.as_ref())
}
