use crate::{
    js::runtime::{
        abstract_operations::get, completion::EvalResult, error::type_error_,
        object_value::ObjectValue, realm::Realm, string_value::StringValue,
        type_utilities::to_string, Context, Handle, Value,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

pub struct ErrorPrototype;

impl ErrorPrototype {
    // 20.5.3 Properties of the Error Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once ErrorConstructor has been created
        object.intrinsic_data_prop(cx, cx.names.name(), cx.names.error().as_string().into());
        object.intrinsic_data_prop(
            cx,
            cx.names.message(),
            cx.names.empty_string().as_string().into(),
        );
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);

        object
    }

    // 20.5.3.4 Error.prototype.toString
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error_(cx, "expected object");
        }

        let this_object = this_value.as_object();

        let name_value = maybe!(get(cx, this_object, cx.names.name()));
        let name_string = if name_value.is_undefined() {
            cx.names.error().as_string().into()
        } else {
            maybe!(to_string(cx, name_value))
        };

        let message_value = maybe!(get(cx, this_object, cx.names.message()));
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
            let separator = cx.alloc_string(": ");
            StringValue::concat_all(cx, &[name_string, separator, message_string]).into()
        }
    }
}
