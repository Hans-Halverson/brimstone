use crate::js::runtime::{object_value::ObjectValue, Context, Handle, Realm, Value};

use super::{date_object::DateObject, intrinsics::Intrinsic};

pub struct DatePrototype;

impl DatePrototype {
    // 21.4.4 Properties of the Date Prototype Object
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once DateConstructor has been created

        object
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
