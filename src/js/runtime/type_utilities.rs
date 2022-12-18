use super::{completion::AbstractResult, gc::Gc, object_value::ObjectValue, value::Value};

pub fn to_boolean(value: Value) -> bool {
    unimplemented!()
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
