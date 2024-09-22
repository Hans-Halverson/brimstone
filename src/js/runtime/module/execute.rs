use crate::{
    js::runtime::{
        builtin_function::BuiltinFunction,
        intrinsics::{intrinsics::Intrinsic, promise_prototype::perform_promise_then},
        object_value::ObjectValue,
        promise_object::{PromiseCapability, PromiseObject},
        Context, EvalResult, Handle, Value,
    },
    must,
};

use super::{loader::load_requested_modules, source_text_module::SourceTextModule};

/// Action to take when the promise for an execution is rejected.
pub enum ExecuteOnReject {
    /// Print the error and exit the process.
    PrintAndExit,
    /// Panic the process.
    #[allow(unused)]
    Panic,
}

/// Execute a module - loading, linking, and evaluating it and its dependencies.
///
/// Returns a promise that resolves once the module has completed execution.
pub fn execute_module(cx: Context, module: Handle<SourceTextModule>) -> Handle<PromiseObject> {
    let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
    let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

    let promise = load_requested_modules(cx, module);

    let on_resolve = BuiltinFunction::create(
        cx,
        load_requested_modules_resolve,
        1,
        cx.names.empty_string(),
        cx.current_realm(),
        None,
        None,
    );

    perform_promise_then(cx, promise, on_resolve.into(), cx.undefined(), Some(capability));

    // Guaranteed to be a PromiseObject since created with the Promise constructor
    capability.promise().cast::<PromiseObject>()
}

pub fn load_requested_modules_resolve(
    _: Context,
    _: Handle<Value>,
    _: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    unimplemented!("link and evaluate the modules");
}
