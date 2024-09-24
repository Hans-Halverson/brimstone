use crate::{
    js::runtime::{
        abstract_operations::{call_object, invoke, species_constructor},
        builtin_function::BuiltinFunction,
        error::type_error,
        function::get_argument,
        object_value::ObjectValue,
        promise_object::{is_promise, promise_resolve, PromiseCapability, PromiseObject},
        property::Property,
        realm::Realm,
        type_utilities::is_callable,
        Context, EvalResult, Handle, Value,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

pub struct PromisePrototype;

impl PromisePrototype {
    /// Properties of the Promise Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-promise-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once PromiseConstructor has been created

        object.intrinsic_func(cx, cx.names.catch(), Self::catch, 1, realm);
        object.intrinsic_func(cx, cx.names.finally(), Self::finally, 1, realm);
        object.intrinsic_func(cx, cx.names.then(), Self::then, 2, realm);

        // Promise.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-promise.prototype-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.promise().as_string().into(), false, false, true),
        );

        object
    }

    /// Promise.prototype.catch (https://tc39.es/ecma262/#sec-promise.prototype.catch)
    pub fn catch(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let on_rejected = get_argument(cx, arguments, 0);
        invoke(cx, this_value, cx.names.then(), &[cx.undefined(), on_rejected])
    }

    /// Promise.prototype.finally (https://tc39.es/ecma262/#sec-promise.prototype.finally)
    pub fn finally(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Promise.prototype.finally called on non-object");
        }
        let promise = this_value.as_object().cast::<PromiseObject>();

        let on_finally = get_argument(cx, arguments, 0);

        let constructor =
            maybe!(species_constructor(cx, promise.into(), Intrinsic::PromiseConstructor));

        let then_finally;
        let catch_finally;
        if !is_callable(on_finally) {
            then_finally = on_finally;
            catch_finally = on_finally;
        } else {
            let on_finally = on_finally.as_object();

            // Create then and catch functions
            let finally_then = BuiltinFunction::create(
                cx,
                Self::finally_then,
                1,
                cx.names.empty_string(),
                cx.current_realm(),
                None,
                None,
            );

            let finally_catch = BuiltinFunction::create(
                cx,
                Self::finally_catch,
                1,
                cx.names.empty_string(),
                cx.current_realm(),
                None,
                None,
            );

            // And attach private properties
            Self::set_constructor(cx, finally_then, constructor);
            Self::set_on_finally(cx, finally_then, on_finally);

            Self::set_constructor(cx, finally_catch, constructor);
            Self::set_on_finally(cx, finally_catch, on_finally);

            then_finally = finally_then.into();
            catch_finally = finally_catch.into();
        }

        invoke(cx, promise.into(), cx.names.then(), &[then_finally, catch_finally])
    }

    fn get_constructor(cx: Context, function: Handle<ObjectValue>) -> Handle<ObjectValue> {
        function
            .private_element_find(cx, cx.well_known_symbols.constructor().cast())
            .unwrap()
            .value()
            .as_object()
    }

    fn set_constructor(cx: Context, mut function: Handle<ObjectValue>, value: Handle<ObjectValue>) {
        function.private_element_set(cx, cx.well_known_symbols.constructor().cast(), value.into());
    }

    fn get_on_finally(cx: Context, function: Handle<ObjectValue>) -> Handle<ObjectValue> {
        function
            .private_element_find(cx, cx.well_known_symbols.on_finally().cast())
            .unwrap()
            .value()
            .as_object()
    }

    fn set_on_finally(cx: Context, mut function: Handle<ObjectValue>, value: Handle<ObjectValue>) {
        function.private_element_set(cx, cx.well_known_symbols.on_finally().cast(), value.into());
    }

    fn get_value(cx: Context, function: Handle<ObjectValue>) -> Handle<Value> {
        function
            .private_element_find(cx, cx.well_known_symbols.values().cast())
            .unwrap()
            .value()
    }

    fn set_value(cx: Context, mut function: Handle<ObjectValue>, value: Handle<Value>) {
        function.private_element_set(cx, cx.well_known_symbols.values().cast(), value);
    }

    pub fn finally_then(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let current_function = cx.current_function();

        let constructor = Self::get_constructor(cx, current_function);
        let on_finally = Self::get_on_finally(cx, current_function);

        let result = maybe!(call_object(cx, on_finally, cx.undefined(), &[]));
        let promise = maybe!(promise_resolve(cx, constructor.into(), result));

        // Continue to a function that returns the value
        let continue_function = BuiltinFunction::create(
            cx,
            Self::finally_then_continue,
            0,
            cx.names.empty_string(),
            cx.current_realm(),
            None,
            None,
        );

        let value = get_argument(cx, arguments, 0);
        Self::set_value(cx, continue_function, value);

        invoke(cx, promise.into(), cx.names.then(), &[continue_function.into()])
    }

    pub fn finally_then_continue(
        mut cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let current_function = cx.current_function();
        let value = Self::get_value(cx, current_function);

        value.into()
    }

    pub fn finally_catch(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let current_function = cx.current_function();

        let constructor = Self::get_constructor(cx, current_function);
        let on_finally = Self::get_on_finally(cx, current_function);

        let result = maybe!(call_object(cx, on_finally, cx.undefined(), &[]));
        let promise = maybe!(promise_resolve(cx, constructor.into(), result));

        // Continue to a function that throws the value
        let continue_function = BuiltinFunction::create(
            cx,
            Self::finally_catch_continue,
            0,
            cx.names.empty_string(),
            cx.current_realm(),
            None,
            None,
        );

        let value = get_argument(cx, arguments, 0);
        Self::set_value(cx, continue_function, value);

        invoke(cx, promise.into(), cx.names.then(), &[continue_function.into()])
    }

    pub fn finally_catch_continue(
        mut cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let current_function = cx.current_function();
        let value = Self::get_value(cx, current_function);

        EvalResult::Throw(value)
    }

    /// Promise.prototype.then (https://tc39.es/ecma262/#sec-promise.prototype.then)
    pub fn then(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !is_promise(this_value.get()) {
            return type_error(cx, "Promise.prototype.then called on non-promise");
        }
        let promise = this_value.as_object().cast::<PromiseObject>();

        let constructor =
            maybe!(species_constructor(cx, promise.into(), Intrinsic::PromiseConstructor));
        let capability = maybe!(PromiseCapability::new(cx, constructor.into()));

        let on_fulfilled = get_argument(cx, arguments, 0);
        let on_rejected = get_argument(cx, arguments, 1);

        perform_promise_then(cx, promise, on_fulfilled, on_rejected, Some(capability)).into()
    }
}

/// PerformPromiseThen (https://tc39.es/ecma262/#sec-performpromisethen) with a capability provided.
pub fn perform_promise_then(
    cx: Context,
    mut promise: Handle<PromiseObject>,
    fulfill_handler: Handle<Value>,
    reject_handler: Handle<Value>,
    capability: Option<Handle<PromiseCapability>>,
) -> Handle<Value> {
    let fulfill_handler = if is_callable(fulfill_handler) {
        Some(fulfill_handler.as_object())
    } else {
        None
    };

    let reject_handler = if is_callable(reject_handler) {
        Some(reject_handler.as_object())
    } else {
        None
    };

    promise.add_then_reaction(cx, fulfill_handler, reject_handler, capability);

    if let Some(capability) = capability {
        capability.promise().into()
    } else {
        cx.undefined()
    }
}
