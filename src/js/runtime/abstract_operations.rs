use crate::maybe_;

use super::{
    completion::AbstractResult, error::type_error_, gc::Gc, object_value::ObjectValue,
    property_descriptor::PropertyDescriptor, realm::Realm, value::Value, Context,
};

// 7.2.5 IsExtensible
#[inline]
pub fn is_extensible(object: Gc<ObjectValue>) -> AbstractResult<bool> {
    object.is_extensible()
}

// 7.3.2 Get
pub fn get(cx: &mut Context, object: Gc<ObjectValue>, key: &str) -> AbstractResult<Value> {
    object.get(cx, key, object.into())
}

// 7.3.4 Set
pub fn set(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    key: &str,
    value: Value,
    should_throw: bool,
) -> AbstractResult<()> {
    let success = maybe_!(object.clone().set(cx, key, value, object.into()));
    if !success && should_throw {
        return type_error_(cx, &format!("Cannot set property {}", key));
    }

    ().into()
}

// 7.3.5 CreateDataProperty
pub fn create_data_property(
    cx: &mut Context,
    mut object: Gc<ObjectValue>,
    key: &str,
    value: Value,
) -> AbstractResult<bool> {
    let new_desc = PropertyDescriptor::data(value, true, true, true);
    object.define_own_property(cx, key, new_desc)
}

// 7.3.7 CreateDataPropertyOrThrow
pub fn create_data_property_or_throw(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    key: &str,
    value: Value,
) -> AbstractResult<()> {
    let success = maybe_!(create_data_property(cx, object, key, value));
    if !success {
        return type_error_(cx, &format!("Cannot create property {}", key));
    }

    ().into()
}

// 7.3.8 DefinePropertyOrThrow
pub fn define_property_or_throw(
    cx: &mut Context,
    mut object: Gc<ObjectValue>,
    key: &str,
    prop_desc: PropertyDescriptor,
) -> AbstractResult<()> {
    let success = maybe_!(object.define_own_property(cx, key, prop_desc));
    if !success {
        return type_error_(cx, &format!("Cannot define property {}", key));
    }

    ().into()
}

// 7.3.12 HasProperty
pub fn has_property(object: Gc<ObjectValue>, key: &str) -> AbstractResult<bool> {
    object.has_property(key)
}

// 7.3.13 HasOwnProperty
pub fn has_own_property(object: Gc<ObjectValue>, key: &str) -> AbstractResult<bool> {
    let desc = maybe_!(object.get_own_property(key));
    desc.is_some().into()
}

pub fn call(
    cx: &mut Context,
    func: Gc<ObjectValue>,
    receiver: Value,
    arguments: Vec<Value>,
) -> AbstractResult<Value> {
    unimplemented!()
}

pub fn get_function_realm(func: Gc<ObjectValue>) -> AbstractResult<Realm> {
    unimplemented!()
}

pub fn private_get(object: Gc<ObjectValue>, private_name: &str) -> AbstractResult<Value> {
    unimplemented!()
}

pub fn private_set(
    object: Gc<ObjectValue>,
    private_name: &str,
    value: Value,
) -> AbstractResult<()> {
    unimplemented!()
}
