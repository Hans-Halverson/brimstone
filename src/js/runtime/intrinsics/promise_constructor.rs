use crate::{
    js::runtime::{
        abstract_operations::{call_object, create_data_property_or_throw},
        builtin_function::{BuiltinFunction, BuiltinFunctionPtr},
        completion::EvalResult,
        error::type_error,
        function::get_argument,
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create,
        promise_object::{promise_resolve, resolve, PromiseCapability, PromiseObject},
        realm::Realm,
        type_utilities::is_callable,
        Context, Handle, Value,
    },
    maybe, must,
};

use super::intrinsics::Intrinsic;

pub struct PromiseConstructor;

impl PromiseConstructor {
    /// 27.2.4 Properties of the Promise Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.promise(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::PromisePrototype).into(),
        );

        func.intrinsic_func(cx, cx.names.reject(), Self::reject, 1, realm);
        func.intrinsic_func(cx, cx.names.resolve(), Self::resolve, 1, realm);
        func.intrinsic_func(cx, cx.names.with_resolvers(), Self::with_resolvers, 0, realm);

        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, Self::get_species, realm);

        func
    }

    /// 27.2.3.1 Promise
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(target) = new_target {
            target
        } else {
            return type_error(cx, "Promise constructor must be called with new");
        };

        // Extract and check type of executor
        let executor = get_argument(cx, arguments, 0);
        if !is_callable(executor) {
            return type_error(cx, "Promise executor must be a function");
        }
        let executor = executor.as_object();

        let promise = maybe!(PromiseObject::new_from_constructor(cx, new_target));

        execute_then(cx, executor, cx.undefined(), promise)
    }

    /// 27.2.4.6 Promise.reject
    pub fn reject(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let result = get_argument(cx, arguments, 0);

        // Create a new promise and immediately reject it
        let capability = maybe!(PromiseCapability::new(cx, this_value));
        maybe!(call_object(cx, capability.reject(), cx.undefined(), &[result]));

        capability.promise().into()
    }

    /// 27.2.4.7 Promise.resolve
    pub fn resolve(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let result = get_argument(cx, arguments, 0);
        maybe!(promise_resolve(cx, this_value, result)).into()
    }

    /// 27.2.4.8 Promise.withResolvers
    pub fn with_resolvers(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let capability = maybe!(PromiseCapability::new(cx, this_value));

        let object = ordinary_object_create(cx);

        must!(create_data_property_or_throw(
            cx,
            object,
            cx.names.promise_(),
            capability.promise().into()
        ));
        must!(create_data_property_or_throw(
            cx,
            object,
            cx.names.resolve(),
            capability.resolve().into()
        ));
        must!(create_data_property_or_throw(
            cx,
            object,
            cx.names.reject(),
            capability.reject().into()
        ));

        object.into()
    }

    /// 27.2.4.9 get Promise [ @@species ]
    pub fn get_species(
        _: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_value.into()
    }
}

pub fn execute_then(
    cx: Context,
    executor: Handle<ObjectValue>,
    this_value: Handle<Value>,
    promise: Handle<PromiseObject>,
) -> EvalResult<Handle<Value>> {
    // Create resolve and reject functions, passing into the executor
    let resolve_function = create_resolve_function(cx, promise);
    let reject_function = create_reject_function(cx, promise);

    let completion =
        call_object(cx, executor, this_value, &[resolve_function.into(), reject_function.into()]);

    // Reject if the executor function throws
    if let EvalResult::Throw(error) = completion {
        maybe!(call_object(cx, reject_function, cx.undefined(), &[error]));
    }

    promise.into()
}

fn create_resolve_function(cx: Context, promise: Handle<PromiseObject>) -> Handle<ObjectValue> {
    create_settle_function(cx, promise, resolve_builtin_function)
}

fn create_reject_function(cx: Context, promise: Handle<PromiseObject>) -> Handle<ObjectValue> {
    create_settle_function(cx, promise, reject_builtin_function)
}

fn create_settle_function(
    cx: Context,
    promise: Handle<PromiseObject>,
    func: BuiltinFunctionPtr,
) -> Handle<ObjectValue> {
    let mut function = BuiltinFunction::create(
        cx,
        func,
        1,
        cx.names.empty_string(),
        cx.current_realm(),
        None,
        None,
    );

    function.private_element_set(cx, cx.well_known_symbols.promise().cast(), promise.into());

    function
}

fn get_promise(cx: Context, settle_function: Handle<ObjectValue>) -> Handle<PromiseObject> {
    settle_function
        .private_element_find(cx, cx.well_known_symbols.promise().cast())
        .unwrap()
        .value()
        .as_object()
        .cast::<PromiseObject>()
}

/// 27.2.1.3.2 Promise Resolve Functions
pub fn resolve_builtin_function(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let resolution = get_argument(cx, arguments, 0);

    let function = cx.current_function();
    let promise = get_promise(cx, function);

    resolve(cx, promise, resolution);

    cx.undefined().into()
}

/// 27.2.1.3.1 Promise Reject Functions
pub fn reject_builtin_function(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let resolution = get_argument(cx, arguments, 0);

    let function = cx.current_function();
    let mut promise = get_promise(cx, function);

    // Rejecting an already settled promise has no effect
    if promise.is_pending() {
        promise.reject(cx, resolution.get());
    }

    cx.undefined().into()
}
