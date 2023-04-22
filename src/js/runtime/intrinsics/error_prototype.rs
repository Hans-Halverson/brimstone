use crate::{
    js::runtime::{
        abstract_operations::get, completion::EvalResult, error::type_error_, gc::Gc,
        object_value::ObjectValue, realm::Realm, string_value::StringValue,
        type_utilities::to_string, value::Value, Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

pub struct ErrorPrototype;

impl ErrorPrototype {
    // 20.5.3 Properties of the Error Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once ErrorConstructor has been created
        object.intrinsic_name_prop(cx, "Error");
        object.intrinsic_data_prop(
            cx,
            &cx.names.message(),
            cx.names.empty_string().as_string().into(),
        );
        object.intrinsic_func(cx, &cx.names.to_string(), Self::to_string, 0, realm);

        object
    }

    // 20.5.3.4 Error.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if !this_value.is_object() {
            return type_error_(cx, "expected object");
        }

        let this_object = this_value.as_object();

        let name_value = maybe!(get(cx, this_object, &cx.names.name()));
        let name_string = if name_value.is_undefined() {
            cx.names.error().as_string().into()
        } else {
            maybe!(to_string(cx, name_value))
        };

        let message_value = maybe!(get(cx, this_object, &cx.names.message()));
        let message_string = if message_value.is_undefined() {
            cx.names.empty_string().as_string().into()
        } else {
            maybe!(to_string(cx, message_value))
        };

        if name_string.is_empty() {
            message_string.into()
        } else if message_string.is_empty() {
            name_string.into()
        } else {
            let separator = cx.alloc_string(String::from(": "));
            StringValue::concat_all(cx, &[name_string, separator, message_string]).into()
        }
    }
}
