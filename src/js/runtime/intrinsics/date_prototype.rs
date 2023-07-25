use crate::js::runtime::{
    error::type_error_, object_value::ObjectValue, Context, EvalResult, Handle, Realm, Value,
};

use super::{date_object::DateObject, intrinsics::Intrinsic};

pub struct DatePrototype;

impl DatePrototype {
    // 21.4.4 Properties of the Date Prototype Object
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once DateConstructor has been created

        object.intrinsic_func(cx, cx.names.value_of(), Self::value_of, 0, realm);

        object
    }

    // 21.4.4.44 Date.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let date_value = if let Some(date_value) = this_date_value(this_value) {
            date_value
        } else {
            return type_error_(cx, "valueOf method must be called on date object");
        };

        Value::from(date_value).to_handle(cx).into()
    }
}

#[inline]
pub fn this_date_value(value: Handle<Value>) -> Option<f64> {
    if !value.is_object() {
        return None;
    }

    let object = value.as_object();
    if !object.is_date_object() {
        return None;
    }

    Some(object.cast::<DateObject>().date_value())
}
