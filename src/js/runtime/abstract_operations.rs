use super::{
    completion::AbstractResult,
    gc::Gc,
    value::{ObjectValue, Value},
};

pub fn has_own_property(object: Gc<ObjectValue>, prop: &str) -> AbstractResult<bool> {
    unimplemented!();
}

pub fn has_property(object: Gc<ObjectValue>, prop: &str) -> AbstractResult<bool> {
    unimplemented!();
}

pub fn is_extensible(object: Gc<ObjectValue>) -> bool {
    unimplemented!()
}

pub fn define_property_or_throw(
    object: Gc<ObjectValue>,
    prop: &str,
    prop_desc: ObjectValue,
) -> AbstractResult<()> {
    unimplemented!()
}

pub fn get(object: Gc<ObjectValue>, prop: &str) -> AbstractResult<Value> {
    unimplemented!()
}

pub fn set(
    object: Gc<ObjectValue>,
    prop: &str,
    value: Value,
    should_throw: bool,
) -> AbstractResult<bool> {
    unimplemented!()
}

pub fn to_boolean(value: &Value) -> bool {
    unimplemented!()
}
