use crate::js::{
    common::error::FormatOptions,
    runtime::{
        abstract_operations::get, error::type_error, eval_result::EvalResult,
        object_value::ObjectValue, realm::Realm, string_value::StringValue, to_console_string,
        type_utilities::to_string, Context, Handle, Value,
    },
};

use super::{error_constructor::ErrorObject, intrinsics::Intrinsic};

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
        object.intrinsic_getter(cx, cx.names.stack(), Self::get_stack, realm);
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);

        object
    }

    /// Error.prototype.toString (https://tc39.es/ecma262/#sec-error.prototype.tostring)
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
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

    pub fn get_stack(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        // Check that `stack` getter was called on an error object
        if !this_value.is_object() || !this_value.as_object().is_error() {
            return Ok(cx.undefined());
        }

        let mut error = this_value.cast::<ErrorObject>();

        // Stack trace starts with error message on one line
        let mut stack_trace = format_error_one_line(cx, error);
        stack_trace.push('\n');

        // Followed by the stack trace
        stack_trace.push_str(&error.get_stack_trace(cx).frames.to_string());

        Ok(cx.alloc_string(&stack_trace).as_value())
    }
}

/// Format an error object into a one line string containing name and message
fn format_error_one_line(cx: Context, error: Handle<ErrorObject>) -> String {
    let name = error_name(cx, error);

    match error_message(cx, error) {
        Some(message) => format!("{}: {}", name, message),
        None => format!("{}", name),
    }
}

pub fn error_name(cx: Context, error: Handle<ErrorObject>) -> Handle<StringValue> {
    match get(cx, error.as_object(), cx.names.name()) {
        Ok(name_value) if name_value.is_string() => name_value.as_string(),
        _ => cx.names.error().as_string(),
    }
}

pub fn error_message(cx: Context, error: Handle<ErrorObject>) -> Option<String> {
    match get(cx, error.as_object(), cx.names.message()) {
        Ok(message_value) => Some(to_console_string(cx, message_value, &FormatOptions::default())),
        Err(_) => None,
    }
}
