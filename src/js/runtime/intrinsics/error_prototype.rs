use crate::js::runtime::{
    abstract_operations::get, completion::EvalResult, error::type_error, object_value::ObjectValue,
    realm::Realm, string_value::StringValue, type_utilities::to_string, Context, Handle, Value,
};

use super::intrinsics::Intrinsic;

pub struct ErrorPrototype;

impl ErrorPrototype {
    /// Properties of the Error Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-error-prototype-object)
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

    /// Error.prototype.toString (https://tc39.es/ecma262/#sec-error.prototype.tostring)
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "expected object");
        }

        let this_object = this_value.as_object();

        let name_value = get(cx, this_object, cx.names.name())?;
        let name_string = if name_value.is_undefined() {
            cx.names.error().as_string()
        } else {
            to_string(cx, name_value)?
        };

        let message_value = get(cx, this_object, cx.names.message())?;
        let message_string = if message_value.is_undefined() {
            cx.names.empty_string().as_string()
        } else {
            to_string(cx, message_value)?
        };

        if name_string.is_empty() {
            Ok(message_string.as_value())
        } else if message_string.is_empty() {
            Ok(name_string.as_value())
        } else {
            let separator = cx.alloc_string(": ").as_string();
            Ok(StringValue::concat_all(cx, &[name_string, separator, message_string]).as_value())
        }
    }
}
