use super::{
    completion::AbstractResult,
    value::{ObjectValue, Value},
};

pub fn has_own_property(object: &ObjectValue, prop: &str) -> AbstractResult<bool> {
    unimplemented!();
}

pub fn has_property(object: &ObjectValue, prop: &str) -> AbstractResult<bool> {
    unimplemented!();
}

pub fn is_extensible(object: &ObjectValue) -> bool {
    unimplemented!()
}

pub fn define_property_or_throw(
    object: &mut ObjectValue,
    prop: &str,
    prop_desc: ObjectValue,
) -> AbstractResult<()> {
    unimplemented!()
}

pub fn get(object: &ObjectValue, prop: &str) -> AbstractResult<Value> {
    unimplemented!()
}

pub fn set(
    object: &mut ObjectValue,
    prop: &str,
    value: Value,
    should_throw: bool,
) -> AbstractResult<bool> {
    unimplemented!()
}

pub fn to_boolean(value: &Value) -> bool {
    unimplemented!()
}
