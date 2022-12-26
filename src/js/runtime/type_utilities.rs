use super::{
    completion::AbstractResult,
    gc::Gc,
    object_value::ObjectValue,
    value::{
        Value, BIGINT_TAG, BOOL_TAG, NULL_TAG, OBJECT_TAG, STRING_TAG, SYMBOL_TAG, UNDEFINED_TAG,
    },
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

pub fn to_object(value: Value) -> AbstractResult<Gc<ObjectValue>> {
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
