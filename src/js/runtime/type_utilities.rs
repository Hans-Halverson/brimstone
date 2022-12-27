use super::{
    completion::AbstractResult,
    error::type_error_,
    gc::Gc,
    object_value::ObjectValue,
    value::{
        Value, BIGINT_TAG, BOOL_TAG, NULL_TAG, OBJECT_TAG, STRING_TAG, SYMBOL_TAG, UNDEFINED_TAG,
    },
    Context,
};

// 7.1.2 ToBoolean
pub fn to_boolean(value: Value) -> bool {
    if value.is_number() {
        return value.as_number() != 0.0 && !value.is_nan();
    }

    match value.get_tag() {
        BOOL_TAG => value.as_bool(),
        NULL_TAG => false,
        UNDEFINED_TAG => false,
        OBJECT_TAG => true,
        STRING_TAG => !value.as_string().is_empty(),
        SYMBOL_TAG => true,
        BIGINT_TAG => unimplemented!("BigInt"),
        _ => unreachable!(),
    }
}

// 7.1.18 ToObject
pub fn to_object(cx: &mut Context, value: Value) -> AbstractResult<Gc<ObjectValue>> {
    if value.is_object() {
        return value.as_object().into();
    } else if value.is_number() {
        unimplemented!("number objects")
    }

    match value.get_tag() {
        NULL_TAG => type_error_(cx, "null value cannot be converted to object"),
        UNDEFINED_TAG => type_error_(cx, "null value cannot be converted to object"),
        BOOL_TAG => unimplemented!("bool objects"),
        STRING_TAG => unimplemented!("string objects"),
        SYMBOL_TAG => unimplemented!("symbol objects"),
        BIGINT_TAG => unimplemented!("BigInt objects"),
        _ => unreachable!(),
    }
}

pub fn to_property_key(value: Value) -> String {
    unimplemented!()
}

pub fn is_callable(value: Value) -> bool {
    unimplemented!()
}

pub fn same_value(value1: Value, value2: Value) -> bool {
    unimplemented!()
}

// Specialization of SameValue for objects, checks object identity
#[inline]
pub fn same_object_value(value1: Gc<ObjectValue>, value2: Gc<ObjectValue>) -> bool {
    value1 == value2
}

// Specialization of SameValue for optional objects, checks object identity
#[inline]
pub fn same_opt_object_value(
    value1: Option<Gc<ObjectValue>>,
    value2: Option<Gc<ObjectValue>>,
) -> bool {
    match (value1, value2) {
        (None, None) => true,
        (Some(value1), Some(value2)) => value1 == value2,
        _ => false,
    }
}
