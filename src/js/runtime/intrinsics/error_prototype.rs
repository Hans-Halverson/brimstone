use crate::{
    js::runtime::{
        abstract_operations::get, completion::AbstractResult, error::type_error_, gc::Gc,
        object_value::ObjectValue, ordinary_object::OrdinaryObject, realm::Realm,
        type_utilities::to_string, value::Value, Context,
    },
    maybe_,
};

use super::intrinsics::Intrinsic;

pub struct ErrorPrototype;

impl ErrorPrototype {
    // 20.5.3 Properties of the Error Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once ErrorConstructor has been created
        object.intrinsic_name_prop(cx, "Error");
        object.intrinsic_data_prop("message", cx.heap.alloc_string(String::new()).into());
        object.intrinsic_func(cx, "toString", Self::to_string, 0, realm);

        cx.heap.alloc(object).into()
    }

    // 20.5.3.4 Error.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: Value,
        _: Vec<Value>,
        _: Option<Gc<ObjectValue>>,
    ) -> AbstractResult<Value> {
        if !this_value.is_object() {
            return type_error_(cx, "expected object");
        }

        let this_object = this_value.as_object();

        let name_value = maybe_!(get(cx, this_object, "name"));
        let name_string = if name_value.is_undefined() {
            cx.heap.alloc_string("Error".to_owned()).into()
        } else {
            maybe_!(to_string(cx, name_value))
        };

        let message_value = maybe_!(get(cx, this_object, "message"));
        let message_string = if message_value.is_undefined() {
            cx.heap.alloc_string(String::new()).into()
        } else {
            maybe_!(to_string(cx, message_value))
        };

        if name_string.is_empty() {
            message_string.into()
        } else if message_string.is_empty() {
            name_string.into()
        } else {
            let error_string = format!("{}: {}", name_string.str(), message_string.str());
            cx.heap.alloc_string(error_string).into()
        }
    }
}
