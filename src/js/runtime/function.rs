use crate::must;

use super::{
    abstract_operations::define_property_or_throw, completion::EvalResult,
    object_value::ObjectValue, property_descriptor::PropertyDescriptor, property_key::PropertyKey,
    string_value::StringValue, value::Value, Context, Handle,
};

/// SetFunctionName (https://tc39.es/ecma262/#sec-setfunctionname)
pub fn set_function_name(
    cx: Context,
    func: Handle<ObjectValue>,
    name: Handle<PropertyKey>,
    prefix: Option<&str>,
) {
    let name_string = build_function_name(cx, name, prefix);
    let desc = PropertyDescriptor::data(name_string.into(), false, false, true);
    must!(define_property_or_throw(cx, func, cx.names.name(), desc))
}

pub fn build_function_name(
    mut cx: Context,
    name: Handle<PropertyKey>,
    prefix: Option<&str>,
) -> Handle<StringValue> {
    // Convert name to string value, property formatting symbol name
    let name_string = if name.is_symbol() {
        let symbol = name.as_symbol();
        if let Some(description) = symbol.description() {
            if symbol.is_private() {
                StringValue::concat(cx, cx.alloc_string("#").as_string(), description.as_string())
            } else {
                let left_paren = cx.alloc_string("[").as_string();
                let right_paren = cx.alloc_string("]").as_string();

                StringValue::concat_all(cx, &[left_paren, description.as_string(), right_paren])
            }
        } else {
            cx.names.empty_string().as_string()
        }
    } else {
        name.to_value(cx).as_string()
    };

    // Add prefix to name
    if let Some(prefix) = prefix {
        let prefix_string = cx.alloc_string(&format!("{} ", prefix)).as_string();
        StringValue::concat(cx, prefix_string, name_string)
    } else {
        name_string
    }
}

/// SetFunctionLength (https://tc39.es/ecma262/#sec-setfunctionlength)
pub fn set_function_length(cx: Context, func: Handle<ObjectValue>, length: u32) {
    let length_value = Value::from(length).to_handle(cx);
    let desc = PropertyDescriptor::data(length_value, false, false, true);
    must!(define_property_or_throw(cx, func, cx.names.length(), desc))
}

// Identical to SetFunctionLength, but a None value represents a length of positive infinity
pub fn set_function_length_maybe_infinity(
    cx: Context,
    func: Handle<ObjectValue>,
    length: Option<usize>,
) {
    let length = if let Some(length) = length {
        Value::from(length).to_handle(cx)
    } else {
        Value::number(f64::INFINITY).to_handle(cx)
    };

    let desc = PropertyDescriptor::data(length, false, false, true);
    must!(define_property_or_throw(cx, func, cx.names.length(), desc))
}

pub fn get_argument(cx: Context, arguments: &[Handle<Value>], i: usize) -> Handle<Value> {
    if i < arguments.len() {
        arguments[i]
    } else {
        cx.undefined()
    }
}
