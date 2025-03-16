use std::{collections::HashSet, path::Path, rc::Rc};

use crate::{
    js::{
        parser::{analyze::analyze, parse_module, print_program, source::Source, ParseContext},
        runtime::{
            abstract_operations::call_object,
            bytecode::generator::BytecodeProgramGenerator,
            context::ModuleCacheKey,
            error::{syntax_error, syntax_parse_error},
            eval_result::EvalResult,
            intrinsics::{intrinsics::Intrinsic, json_object::JSONObject},
            promise_object::{PromiseCapability, PromiseObject},
            string_value::FlatString,
            Context, Handle, Realm, Value,
        },
    },
    must,
};

use super::{
    module::{DynModule, ModuleId},
    source_text_module::{ModuleRequest, ModuleState, SourceTextModule},
    synthetic_module::SyntheticModule,
};

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
    fn inner_module_loading(&mut self, cx: Context, module: DynModule) {
        if let Some(mut module) = module.as_source_text_module() {
            if module.state() == ModuleState::New && self.visited.insert(module.id()) {
                module.set_state(ModuleState::Unlinked);

                let module_requests = module.requested_modules();
                let loaded_modules = module.loaded_modules();

                self.pending_modules_count += module_requests.len();

                for i in 0..module_requests.len() {
                    match loaded_modules.as_slice()[i] {
                        Some(loaded_module) => {
                            self.inner_module_loading(cx, DynModule::from_heap(&loaded_module))
                        }
                        None => {
                            let module_request =
                                ModuleRequest::from_heap(&module_requests.as_slice()[i]);

                            // Create the SourceTextModule for the module with the given specifier,
                            // or evaluate to an error.
                            let load_result = host_load_imported_module(
                                cx,
                                module.source_file_path(),
                                module_request,
                                self.realm,
                            );

                            // Continue module loading with the SourceTextModule or error result
                            self.finish_loading_imported_module(
                                cx,
                                module,
                                module_request,
                                load_result,
                            );
                        }
                    }

                    if !self.is_loading {
                        return;
                    }
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
        module_request: ModuleRequest,
        module_result: EvalResult<DynModule>,
    ) {
        if let Ok(module) = module_result {
            let module_index = referrer
                .lookup_module_request_index(&module_request.to_heap())
                .unwrap();
            if !referrer.has_loaded_module_at(module_index) {
                referrer.set_loaded_module_at(module_index, module);
            }
        }

        self.continue_module_loading(cx, module_result);
    }

    /// ContinueModuleLoading (https://tc39.es/ecma262/#sec-ContinueModuleLoading)
    fn continue_module_loading(&mut self, cx: Context, module_result: EvalResult<DynModule>) {
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

    graph_loader.inner_module_loading(cx, module.as_dyn_module());

    // Known to be a PromiseObject since it was created by the intrinsic Promise constructor
    capability.promise().cast::<PromiseObject>()
}

/// HostLoadImportedModule (https://tc39.es/ecma262/#sec-HostLoadImportedModule)
pub fn host_load_imported_module(
    mut cx: Context,
    source_file_path: Handle<FlatString>,
    module_request: ModuleRequest,
    realm: Handle<Realm>,
) -> EvalResult<DynModule> {
    let source_file_path = Path::new(&source_file_path.to_string())
        .canonicalize()
        .unwrap();
    let source_file_dir = source_file_path.parent().unwrap();

    // Join source file path with specifier path so that relative specifiers will be applied to
    // source path and absolute specifiers will overwrite source path.
    let new_module_path = source_file_dir
        .join(Path::new(&module_request.specifier.to_string()))
        .canonicalize();

    let new_module_path = match new_module_path {
        Ok(path) => path,
        Err(error) => return syntax_error(cx, &error.to_string()),
    };

    let new_module_path_string = new_module_path.to_str().unwrap().to_string();

    // Use the cached module if it has already been loaded
    {
        let module_cache_key =
            ModuleCacheKey::new(new_module_path_string.clone(), module_request.attributes);

        if let Some(module) = cx.modules.get(&module_cache_key.into_heap()) {
            return Ok(DynModule::from_heap(module));
        }
    }

    // Check if the module is a JSON module
    if let Some(attributes) = &module_request.attributes {
        if attributes.has_attribute_with_value("type", "json") {
            let json_value = parse_json_file_at_path(cx, new_module_path.as_path())?;
            let json_module = SyntheticModule::new_default_export(cx, realm, json_value);

            // Cache the JSON module
            let module_cache_key =
                ModuleCacheKey::new(new_module_path_string, module_request.attributes);
            cx.insert_module(module_cache_key, json_module.as_dyn_module());

            return Ok(json_module.as_dyn_module());
        }
    }

    // Find the source file at the given path
    let source = match Source::new_from_file(new_module_path.to_str().unwrap()) {
        Ok(source) => Rc::new(source),
        Err(error) => return syntax_parse_error(cx, &error),
    };

    // Parse the source, returning AST
    let pcx = ParseContext::new(source);
    let parse_result = match parse_module(&pcx, cx.options.clone()) {
        Ok(parse_result) => parse_result,
        Err(error) => return syntax_parse_error(cx, &error),
    };

    if cx.options.print_ast {
        println!("{}", print_program(&parse_result));
    }

    // Analyze AST
    let analyzed_result = match analyze(parse_result) {
        Ok(analyzed_result) => analyzed_result,
        Err(parse_errors) => return syntax_parse_error(cx, &parse_errors.errors[0]),
    };

    // Finally generate the SourceTextModule for the parsed module
    let bytecode_result = BytecodeProgramGenerator::generate_from_parse_module_result(
        cx,
        &Rc::new(analyzed_result),
        realm,
    );

    let module = match bytecode_result {
        Ok(module) => module,
        Err(error) => return syntax_error(cx, &error.to_string()),
    };

    // Cache the module
    let module_cache_key = ModuleCacheKey::new(new_module_path_string, module_request.attributes);
    cx.insert_module(module_cache_key, module.as_dyn_module());

    Ok(module.as_dyn_module())
}

fn parse_json_file_at_path(mut cx: Context, path: &Path) -> EvalResult<Handle<Value>> {
    // Read the contents of the file into the heap
    let file_contents = match Source::new_from_file(path.to_str().unwrap()) {
        Ok(source) => cx.alloc_wtf8_string(&source.contents),
        Err(error) => return syntax_parse_error(cx, &error),
    };

    JSONObject::parse(cx, cx.undefined(), &[file_contents.as_value()])
}
