use crate::js::runtime::{
    error::type_error, eval_result::EvalResult, object_value::ObjectValue, property::Property,
    realm::Realm, Context, Handle, Value,
};

use super::{intrinsics::Intrinsic, weak_ref_constructor::WeakRefObject};

pub struct WeakRefPrototype;

impl WeakRefPrototype {
    /// Properties of the WeakRef Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-weak-ref-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once WeakRefConstructor has been created
        object.intrinsic_func(cx, cx.names.deref(), Self::deref, 0, realm);

        // [Symbol.toStringTag] property
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.weak_ref().as_string().into(), false, false, true),
        );

        object
    }

    /// WeakRef.prototype.deref (https://tc39.es/ecma262/#sec-weak-ref.prototype.deref)
    pub fn deref(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if let Some(weak_ref_object) = this_weak_ref_value(this_value) {
            Ok(weak_ref_object.weak_ref_target().to_handle(cx))
        } else {
            type_error(cx, "deref method must be called on WeakRef")
        }
    }
}

fn this_weak_ref_value(value: Handle<Value>) -> Option<Handle<WeakRefObject>> {
    if !value.is_object() {
        return None;
    }

    value.as_object().as_weak_ref_object()
}
