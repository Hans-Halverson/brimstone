use super::{
    completion::AbstractResult, function::Function, gc::Gc, object_value::ObjectValue,
    property_descriptor::PropertyDescriptor, realm::Realm, value::Value,
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
    prop_desc: PropertyDescriptor,
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

pub fn call(func: Gc<Function>, receiver: Value, arguments: Vec<Value>) -> AbstractResult<Value> {
    unimplemented!()
}

pub fn create_data_property(
    object: Gc<ObjectValue>,
    key: &str,
    value: Value,
) -> AbstractResult<bool> {
    unimplemented!()
}

pub fn get_function_realm(func: Gc<Function>) -> AbstractResult<Realm> {
    unimplemented!()
}
