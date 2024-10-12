use std::{collections::HashMap, path::Path};

use crate::{
    js::runtime::{
        abstract_operations::call_object,
        builtin_function::BuiltinFunction,
        function::get_argument,
        intrinsics::{intrinsics::Intrinsic, promise_prototype::perform_promise_then},
        module::linker::link,
        object_value::ObjectValue,
        promise_object::{PromiseCapability, PromiseObject},
        Context, EvalResult, Handle, Value,
    },
    maybe, must,
};

use super::{
    loader::load_requested_modules,
    source_text_module::{ModuleId, ModuleState, SourceTextModule},
};

/// Execute a module - loading, linking, and evaluating it and its dependencies.
///
/// Returns a promise that resolves once the module has completed execution.
pub fn execute_module(mut cx: Context, module: Handle<SourceTextModule>) -> Handle<PromiseObject> {
    let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
    let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

    // Cache the module at its canonical source path
    //
    // TODO: Move module caching right at module creation. May require first switching to storing
    // absolute paths in Source but relative paths in the heap.
    let source_file_path = Path::new(&module.source_file_path())
        .canonicalize()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();
    cx.modules.insert(source_file_path, module.get_());

    let promise = load_requested_modules(cx, module);

    // Resolve function needs access to module and capability
    let on_resolve = BuiltinFunction::create_builtin_function_without_properties(
        cx,
        load_requested_modules_resolve,
        /* name */ None,
        cx.current_realm(),
        /* prototype */ None,
        /* is_constructor */ false,
    )
    .into();
    set_module(cx, on_resolve, module);
    set_capability(cx, on_resolve, capability);

    // Reject function needs access to capability
    let on_reject = BuiltinFunction::create_builtin_function_without_properties(
        cx,
        load_requested_modules_reject,
        /* name */ None,
        cx.current_realm(),
        /* prototype */ None,
        /* is_constructor */ false,
    )
    .into();
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

pub fn load_requested_modules_resolve(
    mut cx: Context,
    _: Handle<Value>,
    _: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    // Fetch the module and capbility passed from `execute_module`
    let current_function = cx.current_function();
    let module = get_module(cx, current_function);
    let capability = get_capability(cx, current_function);

    if let EvalResult::Throw(error) = link(cx, module) {
        must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
        return cx.undefined().into();
    }

    // Mark the module resolution phase as complete
    cx.has_finished_module_resolution = true;

    let evaluate_promise = module_evaluate(cx, module);

    perform_promise_then(cx, evaluate_promise, cx.undefined(), cx.undefined(), Some(capability))
        .into()
}

pub fn load_requested_modules_reject(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    // Fetch the capbility passed from `execute_module`
    let current_function = cx.current_function();
    let capability = get_capability(cx, current_function);

    let error = get_argument(cx, arguments, 0);
    must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));

    cx.undefined().into()
}

fn module_evaluate(cx: Context, module: Handle<SourceTextModule>) -> Handle<PromiseObject> {
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
        module.set_top_level_capability(capability.get_());

        let evaluation_result = self.inner_evaluate(cx, module, 0);

        match evaluation_result {
            EvalResult::Ok(_) => {
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
            EvalResult::Throw(error) => {
                for module in &mut self.stack {
                    debug_assert!(module.state() == ModuleState::Evaluating);
                    module.set_state(ModuleState::Evaluated);
                    module.set_evaluation_error(error.get());
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
        mut module: Handle<SourceTextModule>,
        index: u32,
    ) -> EvalResult<u32> {
        if matches!(module.state(), ModuleState::Evaluated | ModuleState::EvaluatingAsync) {
            if let Some(error) = module.evaluation_error(cx) {
                return EvalResult::Throw(error);
            } else {
                return index.into();
            }
        }

        if module.state() == ModuleState::Evaluating {
            return index.into();
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
            let mut required_module = loaded_modules.as_slice()[i].unwrap().to_handle();

            index = maybe!(self.inner_evaluate(cx, required_module, index));

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
                    return EvalResult::Throw(error);
                }
            }

            if required_module.is_async_evaluation() {
                module.inc_pending_async_dependencies();
                required_module.push_async_parent_module(cx, module);
            }
        }

        if module.pending_async_dependencies() > 0 || module.has_top_level_await() {
            debug_assert!(!module.is_async_evaluation());
            module.set_async_evaluation(cx, true);

            if module.pending_async_dependencies() == 0 {
                execute_async_module(cx, module);
            }
        } else {
            maybe!(cx.vm().execute_module(module, &[]));
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

                required_module.set_cycle_root(module.get_());

                if required_module.ptr_eq(&module.get_()) {
                    break;
                }
            }
        }

        index.into()
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

    // Resolve function needs access to module
    let on_resolve = BuiltinFunction::create_builtin_function_without_properties(
        cx,
        async_module_execution_fulfilled,
        /* name */ None,
        cx.current_realm(),
        /* prototype */ None,
        /* is_constructor */ false,
    )
    .into();
    set_module(cx, on_resolve, module);

    // Reject function needs access to module
    let on_reject = BuiltinFunction::create_builtin_function_without_properties(
        cx,
        async_module_execution_rejected_runtime,
        /* name */ None,
        cx.current_realm(),
        /* prototype */ None,
        /* is_constructor */ false,
    )
    .into();
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
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    // Fetch the module passed from `execute_async_module`
    let current_function = cx.current_function();
    let mut module = get_module(cx, current_function);

    if module.state() == ModuleState::Evaluated {
        debug_assert!(module.evaluation_error_ptr().is_some());
        return cx.undefined().into();
    }

    debug_assert!(module.state() == ModuleState::EvaluatingAsync);
    debug_assert!(module.is_async_evaluation());
    debug_assert!(module.evaluation_error_ptr().is_none());

    // Mark evaluation of module as complete
    module.set_async_evaluation(cx, false);
    module.set_state(ModuleState::Evaluated);

    // If an entire cycle has been completed, resolve the top-level capability for the cycle
    if let Some(capability) = module.top_level_capability_ptr() {
        debug_assert!(module.cycle_root_ptr().unwrap().ptr_eq(&module.get_()));
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

        if let EvalResult::Throw(error) = execute_result {
            async_module_execution_rejected(cx, ancestor, error);
            continue;
        }

        ancestor.set_async_evaluation(cx, false);
        ancestor.set_state(ModuleState::Evaluated);

        if let Some(capability) = ancestor.top_level_capability_ptr() {
            debug_assert!(ancestor.cycle_root_ptr().unwrap().ptr_eq(&ancestor.get_()));
            must!(call_object(cx, capability.resolve(), cx.undefined(), &[cx.undefined()]));
        }
    }

    cx.undefined().into()
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
    module.set_evaluation_error(error.get());
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
        debug_assert!(module.cycle_root_ptr().unwrap().ptr_eq(&module.get_()));
        must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
    }
}

pub fn async_module_execution_rejected_runtime(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    // Fetch the module passed from `execute_async_module`
    let current_function = cx.current_function();
    let module = get_module(cx, current_function);

    let error = get_argument(cx, arguments, 0);
    async_module_execution_rejected(cx, module, error);

    cx.undefined().into()
}
