use crate::{
    must, must_a,
    runtime::{
        Context, EvalResult, Handle, PropertyFlags, abstract_operations::define_property_or_throw,
        alloc_error::AllocResult, object_value::ObjectValue,
        property_descriptor::PropertyDescriptor, property_key::PropertyKey,
        string_value::StringValue,
    },
};

pub fn set_simple_function_name(
    cx: Context,
    func: Handle<ObjectValue>,
    name: Handle<StringValue>,
) -> AllocResult<()> {
    let desc = PropertyDescriptor::data(name.into(), PropertyFlags::empty().configurable());
    must_a!(define_property_or_throw(cx, func, cx.names.name(), desc));

    Ok(())
}

/// SetFunctionName (https://tc39.es/ecma262/#sec-setfunctionname)
pub fn set_function_name(
    cx: Context,
    func: Handle<ObjectValue>,
    name: Handle<PropertyKey>,
    prefix: Option<&str>,
) -> EvalResult<()> {
    let name_string = build_function_name(cx, name, prefix)?;
    let desc = PropertyDescriptor::data(name_string.into(), PropertyFlags::empty().configurable());
    must_a!(define_property_or_throw(cx, func, cx.names.name(), desc));

    Ok(())
}

pub fn build_function_name(
    mut cx: Context,
    name: Handle<PropertyKey>,
    prefix: Option<&str>,
) -> EvalResult<Handle<StringValue>> {
    // Convert name to string value, property formatting symbol name
    let name_string = if name.is_symbol() {
        let symbol = name.as_symbol();
        if let Some(description) = symbol.description() {
            if symbol.is_private() {
                StringValue::concat(cx, cx.alloc_static_string("#")?, description.as_string())?
            } else {
                let left_paren = cx.alloc_static_string("[")?;
                let right_paren = cx.alloc_static_string("]")?;

                StringValue::concat_all(cx, &[left_paren, description.as_string(), right_paren])?
            }
        } else {
            cx.names.empty_string().as_string()
        }
    } else {
        name.to_value(cx)?.as_string()
    };

    // Add prefix to name
    if let Some(prefix) = prefix {
        let prefix_string = cx.alloc_string(&format!("{prefix} "))?;
        StringValue::concat(cx, prefix_string, name_string)
    } else {
        Ok(name_string)
    }
}

/// SetFunctionLength (https://tc39.es/ecma262/#sec-setfunctionlength)
pub fn set_function_length(cx: Context, func: Handle<ObjectValue>, length: u32) -> AllocResult<()> {
    let length_value = cx.number(length);
    let desc = PropertyDescriptor::data(length_value, PropertyFlags::empty().configurable());
    must_a!(define_property_or_throw(cx, func, cx.names.length(), desc));

    Ok(())
}

// Identical to SetFunctionLength, but a None value represents a length of positive infinity
pub fn set_function_length_maybe_infinity(
    cx: Context,
    func: Handle<ObjectValue>,
    length: Option<usize>,
) -> EvalResult<()> {
    let length = if let Some(length) = length {
        cx.number(length)
    } else {
        cx.number(f64::INFINITY)
    };

    let desc = PropertyDescriptor::data(length, PropertyFlags::empty().configurable());
    must!(define_property_or_throw(cx, func, cx.names.length(), desc));

    Ok(())
}
