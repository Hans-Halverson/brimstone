use std::{collections::HashMap, path::Path};

use crate::{
    if_abrupt_reject_promise, must,
    runtime::{
        abstract_operations::{call_object, enumerable_own_property_names, KeyOrValue},
        builtin_function::BuiltinFunction,
        context::ModuleCacheKey,
        error::type_error_value,
        function::get_argument,
        get,
        interned_strings::InternedStrings,
        intrinsics::{
            intrinsics::Intrinsic, promise_prototype::perform_promise_then,
            rust_runtime::RustRuntimeFunction,
        },
        module::{
            module::{DynModule, ModuleEnum},
            synthetic_module::SyntheticModule,
        },
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        promise_object::{PromiseCapability, PromiseObject},
        string_value::FlatString,
        to_string, Context, EvalResult, Handle, PropertyKey, Value,
    },
};

use super::{
    import_attributes::ImportAttributes,
    loader::host_load_imported_module,
    module::{Module, ModuleId},
    source_text_module::{ModuleRequest, ModuleState, SourceTextModule},
};

/// Execute a module - loading, linking, and evaluating it and its dependencies.
///
/// Returns a promise that resolves once the module has completed execution.
pub fn execute_module(mut cx: Context, module: Handle<SourceTextModule>) -> Handle<PromiseObject> {
    let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
    let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

    // Cache the module at its canonical source path
    let source_file_path = Path::new(&module.source_file_path().to_string())
        .canonicalize()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();

    // Modules executing directly are assumed to have no attributes
    let module_cache_key = ModuleCacheKey::new(source_file_path, None);
    cx.insert_module(module_cache_key, module.as_dyn_module());

    let promise = module.load_requested_modules(cx);

    let on_resolve = callback(cx, load_requested_modules_static_resolve);
    set_module(cx, on_resolve, module);
    set_capability(cx, on_resolve, capability);

    let on_reject = callback(cx, load_requested_modules_reject);
    set_capability(cx, on_reject, capability);

    perform_promise_then(cx, promise, on_resolve.into(), on_reject.into(), None);

    // Guaranteed to be a PromiseObject since created with the Promise constructor
    capability.promise().cast::<PromiseObject>()
}

fn get_module(cx: Context, function: Handle<ObjectValue>) -> Handle<SourceTextModule> {
    function
        .private_element_find(cx, cx.well_known_symbols.module().cast())
        .unwrap()
        .value()
        .as_object()
        .cast::<SourceTextModule>()
}

fn set_module(cx: Context, mut function: Handle<ObjectValue>, value: Handle<SourceTextModule>) {
    function.private_element_set(cx, cx.well_known_symbols.module().cast(), value.into());
}

fn get_dyn_module(cx: Context, function: Handle<ObjectValue>) -> DynModule {
    let item = function
        .private_element_find(cx, cx.well_known_symbols.module().cast())
        .unwrap()
        .value()
        .as_pointer();

    debug_assert!(
        item.descriptor().kind() == ObjectKind::SourceTextModule
            || item.descriptor().kind() == ObjectKind::SyntheticModule
    );

    if item.descriptor().kind() == ObjectKind::SourceTextModule {
        item.cast::<SourceTextModule>().to_handle().as_dyn_module()
    } else {
        item.cast::<SyntheticModule>().to_handle().as_dyn_module()
    }
}

fn set_dyn_module(cx: Context, mut function: Handle<ObjectValue>, value: DynModule) {
    function.private_element_set(
        cx,
        cx.well_known_symbols.module().cast(),
        value.as_heap_item().into(),
    );
}

fn get_capability(cx: Context, function: Handle<ObjectValue>) -> Handle<PromiseCapability> {
    function
        .private_element_find(cx, cx.well_known_symbols.capability().cast())
        .unwrap()
        .value()
        .as_object()
        .cast::<PromiseCapability>()
}

fn set_capability(
    cx: Context,
    mut function: Handle<ObjectValue>,
    value: Handle<PromiseCapability>,
) {
    function.private_element_set(cx, cx.well_known_symbols.capability().cast(), value.into());
}

pub fn load_requested_modules_static_resolve(
    mut cx: Context,
    _: Handle<Value>,
    _: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    // Fetch the module and capbility passed from `execute_module`
    let current_function = cx.current_function();
    let module = get_module(cx, current_function);
    let capability = get_capability(cx, current_function);

    if let Err(error) = module.link(cx) {
        must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
        return Ok(cx.undefined());
    }

    // Mark the module resolution phase as complete
    cx.has_finished_module_resolution = true;
    cx.vm().mark_stack_trace_top();

    let evaluate_promise = module.evaluate(cx);

    Ok(perform_promise_then(
        cx,
        evaluate_promise,
        cx.undefined(),
        cx.undefined(),
        Some(capability),
    ))
}

pub fn load_requested_modules_reject(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    // Fetch the capbility passed from `execute_module`
    let current_function = cx.current_function();
    let capability = get_capability(cx, current_function);

    let error = get_argument(cx, arguments, 0);
    must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));

    Ok(cx.undefined())
}

fn callback(cx: Context, func: RustRuntimeFunction) -> Handle<ObjectValue> {
    BuiltinFunction::create_builtin_function_without_properties(
        cx,
        func,
        /* name */ None,
        cx.current_realm(),
        /* prototype */ None,
        /* is_constructor */ false,
    )
    .into()
}

pub fn module_evaluate(cx: Context, module: Handle<SourceTextModule>) -> Handle<PromiseObject> {
    let mut evaluator = GraphEvaluator::new();
    evaluator.evaluate(cx, module)
}

struct GraphEvaluator {
    stack: Vec<Handle<SourceTextModule>>,
}

impl GraphEvaluator {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }

    /// Evaluate (https://tc39.es/ecma262/#sec-moduleevaluation)
    fn evaluate(
        &mut self,
        cx: Context,
        mut module: Handle<SourceTextModule>,
    ) -> Handle<PromiseObject> {
        if matches!(module.state(), ModuleState::Evaluated | ModuleState::EvaluatingAsync) {
            module = module.cycle_root().unwrap();
        } else {
            debug_assert!(module.state() == ModuleState::Linked);
        }

        if let Some(capability) = module.top_level_capability_ptr() {
            // Was created with promise constructor
            return capability.promise().cast::<PromiseObject>();
        }

        let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
        let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));
        module.set_top_level_capability(*capability);

        let evaluation_result = self.inner_evaluate(cx, module.as_dyn_module(), 0);

        match evaluation_result {
            Ok(_) => {
                debug_assert!(matches!(
                    module.state(),
                    ModuleState::Evaluated | ModuleState::EvaluatingAsync
                ));
                debug_assert!(module.evaluation_error_ptr().is_none());

                if !module.is_async_evaluation() {
                    debug_assert!(module.state() == ModuleState::Evaluated);
                    must!(call_object(cx, capability.resolve(), cx.undefined(), &[cx.undefined()]));
                }

                debug_assert!(self.stack.is_empty());
            }
            Err(error) => {
                for module in &mut self.stack {
                    debug_assert!(module.state() == ModuleState::Evaluating);
                    module.set_state(ModuleState::Evaluated);
                    module.set_evaluation_error(*error);
                }

                debug_assert!(module.state() == ModuleState::Evaluated);

                must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
            }
        }

        // Known to be a PromiseObject since created with the Promise constructor
        capability.promise().cast::<PromiseObject>()
    }

    /// InnerModuleEvaluation (https://tc39.es/ecma262/#sec-innermoduleevaluation)
    fn inner_evaluate(
        &mut self,
        mut cx: Context,
        module: DynModule,
        index: u32,
    ) -> EvalResult<u32> {
        let mut module = match module.as_enum() {
            // Directly evaluate synthetic modules
            ModuleEnum::Synthetic(module) => {
                let promise = module.evaluate(cx);

                // Propagate rejected value as error
                return if let Some(rejected_value) = promise.rejected_value() {
                    Err(rejected_value.to_handle(cx))
                } else {
                    Ok(index)
                };
            }
            ModuleEnum::SourceText(module) => module,
        };

        if matches!(module.state(), ModuleState::Evaluated | ModuleState::EvaluatingAsync) {
            if let Some(error) = module.evaluation_error(cx) {
                return Err(error);
            } else {
                return Ok(index);
            }
        }

        if module.state() == ModuleState::Evaluating {
            return Ok(index);
        }

        debug_assert!(module.state() == ModuleState::Linked);

        // Note that the value of [[PendingAsyncDependencies]] is already 0
        module.set_state(ModuleState::Evaluating);
        module.set_dfs_index(index);
        module.set_dfs_ancestor_index(index);

        self.stack.push(module);

        let mut index = index + 1;

        let loaded_modules = module.loaded_modules();
        for i in 0..loaded_modules.len() {
            let required_module = DynModule::from_heap(&loaded_modules.as_slice()[i].unwrap());

            index = self.inner_evaluate(cx, required_module, index)?;

            if let Some(mut required_module) = required_module.as_source_text_module() {
                if required_module.state() == ModuleState::Evaluating {
                    let new_index = module
                        .dfs_ancestor_index()
                        .min(required_module.dfs_ancestor_index());
                    module.set_dfs_ancestor_index(new_index)
                } else {
                    debug_assert!(matches!(
                        required_module.state(),
                        ModuleState::EvaluatingAsync | ModuleState::Evaluated
                    ));

                    required_module = required_module.cycle_root().unwrap();

                    debug_assert!(matches!(
                        required_module.state(),
                        ModuleState::EvaluatingAsync | ModuleState::Evaluated
                    ));

                    if let Some(error) = required_module.evaluation_error(cx) {
                        return Err(error);
                    }
                }

                if required_module.is_async_evaluation() {
                    module.inc_pending_async_dependencies();
                    required_module.push_async_parent_module(cx, module);
                }
            }
        }

        if module.pending_async_dependencies() > 0 || module.has_top_level_await() {
            debug_assert!(!module.is_async_evaluation());
            module.set_async_evaluation(cx, true);

            if module.pending_async_dependencies() == 0 {
                execute_async_module(cx, module);
            }
        } else {
            cx.vm().execute_module(module, &[])?;
        }

        debug_assert!(module.dfs_ancestor_index() <= module.dfs_index());

        if module.dfs_ancestor_index() == module.dfs_index() {
            loop {
                let mut required_module = self.stack.pop().unwrap();

                if !required_module.is_async_evaluation() {
                    required_module.set_state(ModuleState::Evaluated);
                } else {
                    required_module.set_state(ModuleState::EvaluatingAsync);
                }

                required_module.set_cycle_root(*module);

                if required_module.ptr_eq(&module) {
                    break;
                }
            }
        }

        Ok(index)
    }
}

/// ExecuteAsyncModule (https://tc39.es/ecma262/#sec-execute-async-module)
fn execute_async_module(mut cx: Context, module: Handle<SourceTextModule>) {
    debug_assert!(matches!(module.state(), ModuleState::Evaluating | ModuleState::EvaluatingAsync));
    debug_assert!(module.has_top_level_await());

    let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
    let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

    // Known to be a PromiseObject since it was created by the intrinsic Promise constructor
    let promise = capability.promise().cast::<PromiseObject>();

    let on_resolve = callback(cx, async_module_execution_fulfilled);
    set_module(cx, on_resolve, module);

    let on_reject = callback(cx, async_module_execution_rejected_runtime);
    set_module(cx, on_reject, module);

    // Set up resolve and reject callbacks which re-enter module graph evaluation
    perform_promise_then(cx, promise, on_resolve.into(), on_reject.into(), None);

    // Finally call the module function itself, which will resolve or reject the promise
    must!(cx.vm().execute_module(module, &[promise.into()]));
}

/// AsyncModuleExecutionFulfilled (https://tc39.es/ecma262/#sec-async-module-execution-fulfilled)
pub fn async_module_execution_fulfilled(
    mut cx: Context,
    _: Handle<Value>,
    _: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    // Fetch the module passed from `execute_async_module`
    let current_function = cx.current_function();
    let mut module = get_module(cx, current_function);

    if module.state() == ModuleState::Evaluated {
        debug_assert!(module.evaluation_error_ptr().is_some());
        return Ok(cx.undefined());
    }

    debug_assert!(module.state() == ModuleState::EvaluatingAsync);
    debug_assert!(module.is_async_evaluation());
    debug_assert!(module.evaluation_error_ptr().is_none());

    // Mark evaluation of module as complete
    module.set_async_evaluation(cx, false);
    module.set_state(ModuleState::Evaluated);

    // If an entire cycle has been completed, resolve the top-level capability for the cycle
    if let Some(capability) = module.top_level_capability_ptr() {
        debug_assert!(module.cycle_root_ptr().unwrap().ptr_eq(&module));
        must!(call_object(cx, capability.resolve(), cx.undefined(), &[cx.undefined()]));
    }

    // Gather available ancestors
    let mut ancestors = HashMap::new();
    gather_available_ancestors(module, &mut ancestors);

    // Sort ancestors by the order [[AsyncEvaluation]] was set
    let mut ancestors = ancestors.into_values().collect::<Vec<_>>();
    ancestors.sort_by_key(|module| module.async_evaluation_index().unwrap());

    for mut ancestor in ancestors {
        if ancestor.state() == ModuleState::Evaluated {
            debug_assert!(ancestor.evaluation_error_ptr().is_some());
            continue;
        }

        if ancestor.has_top_level_await() {
            execute_async_module(cx, ancestor);
            continue;
        }

        let execute_result = cx.vm().execute_module(ancestor, &[]);

        if let Err(error) = execute_result {
            async_module_execution_rejected(cx, ancestor, error);
            continue;
        }

        ancestor.set_async_evaluation(cx, false);
        ancestor.set_state(ModuleState::Evaluated);

        if let Some(capability) = ancestor.top_level_capability_ptr() {
            debug_assert!(ancestor.cycle_root_ptr().unwrap().ptr_eq(&ancestor));
            must!(call_object(cx, capability.resolve(), cx.undefined(), &[cx.undefined()]));
        }
    }

    Ok(cx.undefined())
}

/// GatherAvailableAncestors (https://tc39.es/ecma262/#sec-gather-available-ancestors)
fn gather_available_ancestors(
    module: Handle<SourceTextModule>,
    gathered: &mut HashMap<ModuleId, Handle<SourceTextModule>>,
) {
    if let Some(async_parent_modules) = module.async_parent_modules_ptr() {
        for parent_module in async_parent_modules.as_slice() {
            if !gathered.contains_key(&parent_module.id())
                || parent_module
                    .cycle_root_ptr()
                    .unwrap()
                    .evaluation_error_ptr()
                    .is_none()
            {
                debug_assert!(parent_module.state() == ModuleState::EvaluatingAsync);
                debug_assert!(parent_module.evaluation_error_ptr().is_none());
                debug_assert!(parent_module.is_async_evaluation());
                debug_assert!(parent_module.pending_async_dependencies() > 0);

                let mut parent_module = parent_module.to_handle();
                parent_module.dec_pending_async_dependencies();

                if parent_module.pending_async_dependencies() == 0 {
                    gathered.insert(parent_module.id(), parent_module);

                    if !parent_module.has_top_level_await() {
                        gather_available_ancestors(parent_module, gathered);
                    }
                }

                continue;
            }
        }
    }
}

/// AsyncModuleExecutionRejected (https://tc39.es/ecma262/#sec-async-module-execution-rejected)
fn async_module_execution_rejected(
    cx: Context,
    mut module: Handle<SourceTextModule>,
    error: Handle<Value>,
) {
    if module.state() == ModuleState::Evaluated {
        debug_assert!(module.evaluation_error_ptr().is_some());
        return;
    }

    debug_assert!(module.state() == ModuleState::EvaluatingAsync);
    debug_assert!(module.is_async_evaluation());
    debug_assert!(module.evaluation_error_ptr().is_none());

    // Mark evaluation of module as complete with an error
    module.set_evaluation_error(*error);
    module.set_state(ModuleState::Evaluated);
    module.set_async_evaluation(cx, false);

    // Reject execution of all async parent modules as well
    if let Some(async_parent_modules) = module.async_parent_modules() {
        // Reuse handle between iterations
        let mut parent_module_handle: Handle<SourceTextModule> = Handle::empty(cx);

        for i in 0..async_parent_modules.len() {
            parent_module_handle.replace(async_parent_modules.as_slice()[i]);
            async_module_execution_rejected(cx, parent_module_handle, error);
        }
    }

    // If entire cycle has been completed, reject the top-level capability for the cycle
    if let Some(capability) = module.top_level_capability_ptr() {
        debug_assert!(module.cycle_root_ptr().unwrap().ptr_eq(&module));
        must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
    }
}

pub fn async_module_execution_rejected_runtime(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    // Fetch the module passed from `execute_async_module`
    let current_function = cx.current_function();
    let module = get_module(cx, current_function);

    let error = get_argument(cx, arguments, 0);
    async_module_execution_rejected(cx, module, error);

    Ok(cx.undefined())
}

/// Start a dynamic import within a module, passing the argument provided to `import()`.
pub fn dynamic_import(
    cx: Context,
    source_file_path: Handle<FlatString>,
    specifier: Handle<Value>,
    options: Handle<Value>,
) -> EvalResult<Handle<ObjectValue>> {
    let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
    let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

    let specifier_string_completion = to_string(cx, specifier);
    let specifier = if_abrupt_reject_promise!(cx, specifier_string_completion, capability);

    let mut attribute_pairs = vec![];

    if !options.is_undefined() {
        if !options.is_object() {
            let error = type_error_value(cx, "Import options must be an object");
            must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
            return Ok(capability.promise());
        }

        let attributes_object_completion = get(cx, options.as_object(), cx.names.with());
        let attributes_object =
            if_abrupt_reject_promise!(cx, attributes_object_completion, capability);

        if !attributes_object.is_undefined() {
            if !attributes_object.is_object() {
                let error = type_error_value(cx, "Import attributes must be an object");
                must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
                return Ok(capability.promise());
            }

            let entries_completion = enumerable_own_property_names(
                cx,
                attributes_object.as_object(),
                KeyOrValue::KeyAndValue,
            );
            let entries = if_abrupt_reject_promise!(cx, entries_completion, capability);

            for entry in entries {
                // Entry is gaurenteed to be an array with two elements
                let entry = entry.as_object();
                let key = must!(get(cx, entry, PropertyKey::from_u8(0).to_handle(cx)));
                let value = must!(get(cx, entry, PropertyKey::from_u8(1).to_handle(cx)));

                if !value.is_string() {
                    let error = type_error_value(cx, "Import attribute values must be strings");
                    must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
                    return Ok(capability.promise());
                }

                // Intern the key and value strings
                let key_string = must!(to_string(cx, key));
                let key_flat_string = key_string.flatten();
                let key_interned_string = InternedStrings::get(cx, *key_flat_string).to_handle();

                let value_flat_string = value.as_string().flatten();
                let value_interned_string =
                    InternedStrings::get(cx, *value_flat_string).to_handle();

                attribute_pairs.push((key_interned_string, value_interned_string));
            }
        }
    }

    let attributes = if !attribute_pairs.is_empty() {
        // Sort keys in lexicographic order
        attribute_pairs.sort_by_key(|(key, _)| *key);

        Some(ImportAttributes::new(cx, &attribute_pairs))
    } else {
        None
    };

    let specifier = specifier.flatten();
    let specifier = InternedStrings::get(cx, *specifier).to_handle();

    let module_request = ModuleRequest { specifier, attributes };

    let load_completion =
        host_load_imported_module(cx, source_file_path, module_request, cx.current_realm());
    continue_dynamic_import(cx, capability, load_completion);

    Ok(capability.promise())
}

/// ContinueDynamicImport (https://tc39.es/ecma262/#sec-ContinueDynamicImport)
fn continue_dynamic_import(
    cx: Context,
    capability: Handle<PromiseCapability>,
    load_completion: EvalResult<DynModule>,
) {
    let module = match load_completion {
        Ok(module) => module,
        Err(error) => {
            must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
            return;
        }
    };

    let load_promise = module.load_requested_modules(cx);

    let on_resolve = callback(cx, load_requested_modules_dynamic_resolve);
    set_dyn_module(cx, on_resolve, module);
    set_capability(cx, on_resolve, capability);

    let on_reject = callback(cx, load_requested_modules_reject);
    set_capability(cx, on_reject, capability);

    perform_promise_then(cx, load_promise, on_resolve.into(), on_reject.into(), None);
}

pub fn load_requested_modules_dynamic_resolve(
    mut cx: Context,
    _: Handle<Value>,
    _: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    // Fetch the module and capability passed from the caller
    let current_function = cx.current_function();
    let module = get_dyn_module(cx, current_function);
    let capability = get_capability(cx, current_function);

    if let Err(error) = module.link(cx) {
        must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
        return Ok(cx.undefined());
    }

    // Missing condition in the spec. If the module has already been evaluated and throw an error
    // we should rethrow that error directly. Otherwise Evaluate will fail since it expects an
    // evaluated module to have a [[CycleRoot]], but [[CycleRoot]] is not set if module evaluation
    // errors.
    if let Some(module) = module.as_source_text_module() {
        if module.state() == ModuleState::Evaluated && module.evaluation_error_ptr().is_some() {
            must!(call_object(
                cx,
                capability.reject(),
                cx.undefined(),
                &[module.evaluation_error(cx).unwrap()]
            ));
            return Ok(cx.undefined());
        }
    }

    let evaluate_promise = module.evaluate(cx);

    let on_resolve = callback(cx, module_evaluate_dynamic_resolve);
    set_dyn_module(cx, on_resolve, module);
    set_capability(cx, on_resolve, capability);

    let on_reject = callback(cx, load_requested_modules_reject);
    set_capability(cx, on_reject, capability);

    perform_promise_then(cx, evaluate_promise, on_resolve.into(), on_reject.into(), None);

    Ok(cx.undefined())
}

pub fn module_evaluate_dynamic_resolve(
    mut cx: Context,
    _: Handle<Value>,
    _: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    // Fetch the module and capbility passed from the caller
    let current_function = cx.current_function();
    let mut module = get_dyn_module(cx, current_function);
    let capability = get_capability(cx, current_function);

    let namespace_object = module.get_namespace_object(cx).to_handle();

    must!(call_object(
        cx,
        capability.resolve(),
        cx.undefined(),
        &[namespace_object.into()]
    ));

    Ok(cx.undefined())
}
