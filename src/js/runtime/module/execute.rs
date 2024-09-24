use std::path::Path;

use crate::{
    js::runtime::{
        abstract_operations::call_object,
        builtin_function::BuiltinFunction,
        intrinsics::{intrinsics::Intrinsic, promise_prototype::perform_promise_then},
        module::linker::link,
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

    let on_resolve = BuiltinFunction::create(
        cx,
        load_requested_modules_resolve,
        1,
        cx.names.empty_string(),
        cx.current_realm(),
        None,
        None,
    );

    // Pass module and capability to the resolve function
    set_module(cx, on_resolve, module);
    set_capability(cx, on_resolve, capability);

    perform_promise_then(cx, promise, on_resolve.into(), cx.undefined(), Some(capability));

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
    // Fetch the promise and capbility passed from `execute_module`
    let current_function = cx.current_function();
    let module = get_module(cx, current_function);
    let capability = get_capability(cx, current_function);

    if let EvalResult::Throw(error) = link(cx, module) {
        must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
        return cx.undefined().into();
    }

    unimplemented!("evaluate the modules");
}
