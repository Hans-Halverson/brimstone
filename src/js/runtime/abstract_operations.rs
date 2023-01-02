use crate::{maybe_, must_};

use super::{
    completion::AbstractResult,
    error::type_error_,
    gc::Gc,
    object_value::ObjectValue,
    property_descriptor::PropertyDescriptor,
    realm::Realm,
    type_utilities::{is_callable, is_callable_object, to_object},
    value::Value,
    Context,
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

// 7.3.3 GetV
pub fn get_v(cx: &mut Context, value: Value, key: &str) -> AbstractResult<Value> {
    let object = maybe_!(to_object(cx, value));
    object.get(cx, key, value)
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

// 7.3.8 CreateNonEnumerableDataPropertyOrThrow
pub fn create_non_enumerable_data_property_or_throw(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    key: &str,
    value: Value,
) {
    let new_desc = PropertyDescriptor::data(value, true, false, true);
    must_!(define_property_or_throw(cx, object, key, new_desc));
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

// 7.3.11 GetMethod
pub fn get_method(
    cx: &mut Context,
    value: Value,
    key: &str,
) -> AbstractResult<Option<Gc<ObjectValue>>> {
    let func = maybe_!(get_v(cx, value, key));
    if func.is_nullish() {
        return None.into();
    }

    if !is_callable(func) {
        return type_error_(cx, "value is not a function");
    }

    (Some(func.as_object())).into()
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

// 7.3.14 Call
pub fn call(
    cx: &mut Context,
    func: Value,
    receiver: Value,
    arguments: &[Value],
) -> AbstractResult<Value> {
    if !is_callable(func) {
        return type_error_(cx, "value is not a function");
    }

    func.as_object().call(cx, receiver, arguments)
}

pub fn call_object(
    cx: &mut Context,
    func: Gc<ObjectValue>,
    receiver: Value,
    arguments: &[Value],
) -> AbstractResult<Value> {
    if !is_callable_object(func) {
        return type_error_(cx, "value is not a function");
    }

    func.call(cx, receiver, arguments)
}

// 7.3.15 Construct
pub fn construct(
    cx: &mut Context,
    func: Gc<ObjectValue>,
    arguments: &[Value],
    new_target: Option<Gc<ObjectValue>>,
) -> AbstractResult<Gc<ObjectValue>> {
    let new_target = new_target.unwrap_or(func);
    func.construct(cx, arguments, new_target)
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
