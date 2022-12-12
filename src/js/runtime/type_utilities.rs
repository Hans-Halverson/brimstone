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
