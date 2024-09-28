use std::path::Path;

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
    source_text_module::{ModuleState, SourceTextModule},
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
            module.set_async_evaluation(true);

            if module.pending_async_dependencies() == 0 {
                unimplemented!("ExecuteAsyncModule");
            }
        } else {
            let eval_result = cx.vm().execute_module(module);
            if let Err(error) = eval_result {
                return EvalResult::Throw(error);
            }
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
