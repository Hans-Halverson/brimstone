use crate::{
    js::runtime::{
        abstract_operations::{invoke, species_constructor},
        error::type_error,
        function::get_argument,
        object_value::ObjectValue,
        promise_object::{is_promise, PromiseCapability, PromiseObject},
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
    // 27.2.5 Properties of the Promise Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once PromiseConstructor has been created

        object.intrinsic_func(cx, cx.names.catch(), Self::catch, 1, realm);
        object.intrinsic_func(cx, cx.names.then(), Self::then, 2, realm);

        // 27.2.5.5 Promise.prototype [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.promise().as_string().into(), false, false, true),
        );

        object
    }

    /// 27.2.5.1 Promise.prototype.catch
    pub fn catch(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let on_rejected = get_argument(cx, arguments, 0);
        invoke(cx, this_value, cx.names.then(), &[cx.undefined(), on_rejected])
    }

    /// 27.2.5.4 Promise.prototype.then
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

        perform_promise_then(cx, promise, on_fulfilled, on_rejected, capability)
    }
}

/// 27.2.5.4.1 PerformPromiseThen with a capability provided.
fn perform_promise_then(
    cx: Context,
    mut promise: Handle<PromiseObject>,
    fulfill_handler: Handle<Value>,
    reject_handler: Handle<Value>,
    capability: Handle<PromiseCapability>,
) -> EvalResult<Handle<Value>> {
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

    capability.promise().into()
}
