use std::collections::HashSet;

use crate::{js::runtime::eval::class::ClassFieldDefinitionName, maybe, must};

use super::{
    array_object::create_array_from_list,
    bound_function_object::BoundFunctionObject,
    completion::EvalResult,
    environment::private_environment::PrivateNameId,
    error::type_error_,
    eval::{class::ClassFieldDefinition, expression::eval_instanceof_expression},
    function::Function,
    gc::Gc,
    object_value::ObjectValue,
    property::PrivatePropertyKind,
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    realm::Realm,
    type_utilities::{is_callable, is_callable_object, same_object_value, to_length, to_object},
    value::Value,
    Context,
};

// 7.2.5 IsExtensible
#[inline]
pub fn is_extensible(object: Gc<ObjectValue>) -> EvalResult<bool> {
    object.is_extensible()
}

// 7.3.2 Get
pub fn get(cx: &mut Context, object: Gc<ObjectValue>, key: &PropertyKey) -> EvalResult<Value> {
    object.get(cx, key, object.into())
}

// 7.3.3 GetV
pub fn get_v(cx: &mut Context, value: Value, key: &PropertyKey) -> EvalResult<Value> {
    let object = maybe!(to_object(cx, value));
    object.get(cx, key, value)
}

// 7.3.4 Set
pub fn set(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    key: &PropertyKey,
    value: Value,
    should_throw: bool,
) -> EvalResult<()> {
    let success = maybe!(object.clone().set(cx, key, value, object.into()));
    if !success && should_throw {
        return type_error_(cx, &format!("Cannot set property {}", key));
    }

    ().into()
}

// 7.3.6 CreateMethodProperty
pub fn create_method_property(
    cx: &mut Context,
    mut object: Gc<ObjectValue>,
    key: &PropertyKey,
    value: Value,
) {
    let new_desc = PropertyDescriptor::data(value, true, false, true);
    must!(object.define_own_property(cx, key, new_desc));
}

// 7.3.5 CreateDataProperty
pub fn create_data_property(
    cx: &mut Context,
    mut object: Gc<ObjectValue>,
    key: &PropertyKey,
    value: Value,
) -> EvalResult<bool> {
    let new_desc = PropertyDescriptor::data(value, true, true, true);
    object.define_own_property(cx, key, new_desc)
}

// 7.3.7 CreateDataPropertyOrThrow
pub fn create_data_property_or_throw(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    key: &PropertyKey,
    value: Value,
) -> EvalResult<()> {
    let success = maybe!(create_data_property(cx, object, key, value));
    if !success {
        return type_error_(cx, &format!("Cannot create property {}", key));
    }

    ().into()
}

// 7.3.8 CreateNonEnumerableDataPropertyOrThrow
pub fn create_non_enumerable_data_property_or_throw(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    key: &PropertyKey,
    value: Value,
) {
    let new_desc = PropertyDescriptor::data(value, true, false, true);
    must!(define_property_or_throw(cx, object, key, new_desc));
}

// 7.3.8 DefinePropertyOrThrow
pub fn define_property_or_throw(
    cx: &mut Context,
    mut object: Gc<ObjectValue>,
    key: &PropertyKey,
    prop_desc: PropertyDescriptor,
) -> EvalResult<()> {
    let success = maybe!(object.define_own_property(cx, key, prop_desc));
    if !success {
        return type_error_(cx, &format!("Cannot define property {}", key));
    }

    ().into()
}

// 7.3.11 GetMethod
pub fn get_method(
    cx: &mut Context,
    value: Value,
    key: &PropertyKey,
) -> EvalResult<Option<Gc<ObjectValue>>> {
    let func = maybe!(get_v(cx, value, key));
    if func.is_nullish() {
        return None.into();
    }

    if !is_callable(func) {
        return type_error_(cx, "value is not a function");
    }

    (Some(func.as_object())).into()
}

// 7.3.12 HasProperty
pub fn has_property(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    key: &PropertyKey,
) -> EvalResult<bool> {
    object.has_property(cx, key)
}

// 7.3.13 HasOwnProperty
pub fn has_own_property(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    key: &PropertyKey,
) -> EvalResult<bool> {
    let desc = maybe!(object.get_own_property(cx, key));
    desc.is_some().into()
}

// 7.3.14 Call
pub fn call(
    cx: &mut Context,
    func: Value,
    receiver: Value,
    arguments: &[Value],
) -> EvalResult<Value> {
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
) -> EvalResult<Value> {
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
) -> EvalResult<Gc<ObjectValue>> {
    let new_target = new_target.unwrap_or(func);
    func.construct(cx, arguments, new_target)
}

// 7.3.19 LengthOfArrayLike
pub fn length_of_array_like(cx: &mut Context, object: Gc<ObjectValue>) -> EvalResult<u64> {
    let length_value = maybe!(get(cx, object, &cx.names.length()));
    to_length(cx, length_value).into()
}

// 7.3.20 CreateListFromArrayLike
pub fn create_list_from_array_like(cx: &mut Context, object: Value) -> EvalResult<Vec<Value>> {
    if !object.is_object() {
        return type_error_(cx, "value is not an object");
    }

    let object = object.as_object();
    let length = maybe!(length_of_array_like(cx, object));

    let mut vec = Vec::with_capacity(length as usize);

    for i in 0..length {
        let key = PropertyKey::array_index(i as u32);
        let next = maybe!(get(cx, object, &key));
        vec.push(next);
    }

    vec.into()
}

// 7.3.22 OrdinaryHasInstance
pub fn ordinary_has_instance(cx: &mut Context, func: Value, object: Value) -> EvalResult<bool> {
    if !is_callable(func) {
        return false.into();
    }

    let func = func.as_object();

    if func.is_bound_function() {
        let bound_func = func.cast::<BoundFunctionObject>();
        return eval_instanceof_expression(cx, object, bound_func.bound_target_function.into());
    }

    if !object.is_object() {
        return false.into();
    }

    let target_prototype = maybe!(get(cx, func, &cx.names.prototype()));
    if !target_prototype.is_object() {
        return type_error_(cx, "prototype must be object");
    }
    let target_prototype = target_prototype.as_object();

    // Walk prototype chain of object, looking for prototype of func
    let mut current_object = object.as_object();
    loop {
        match maybe!(current_object.get_prototype_of()) {
            None => return false.into(),
            Some(current_prototype) => {
                if same_object_value(target_prototype, current_prototype) {
                    return true.into();
                }

                current_object = current_prototype;
            }
        }
    }
}

pub enum KeyOrValue {
    Key,
    Value,
    KeyAndValue,
}

// 7.3.24 EnumerableOwnPropertyNames
pub fn enumerable_own_property_names(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    kind: KeyOrValue,
) -> EvalResult<Vec<Value>> {
    let keys = object.own_property_keys(cx);

    let mut properties = vec![];

    for key_value in keys {
        if key_value.is_symbol() {
            continue;
        }

        let key = must!(PropertyKey::from_value(cx, key_value));
        let desc = maybe!(object.get_own_property(cx, &key));

        if let Some(desc) = desc {
            if let Some(true) = desc.is_enumerable {
                match kind {
                    KeyOrValue::Key => properties.push(key_value),
                    KeyOrValue::Value => {
                        let value = maybe!(get(cx, object, &key));
                        properties.push(value);
                    }
                    KeyOrValue::KeyAndValue => {
                        let value = maybe!(get(cx, object, &key));
                        let entry = create_array_from_list(cx, &[key_value, value]);
                        properties.push(entry.into());
                    }
                }
            }
        }
    }

    properties.into()
}

// 7.3.26 CopyDataProperties
pub fn copy_data_properties(
    cx: &mut Context,
    target: Gc<ObjectValue>,
    source: Value,
    excluded_items: &HashSet<PropertyKey>,
) -> EvalResult<()> {
    if source.is_nullish() {
        return ().into();
    }

    let from = must!(to_object(cx, source));
    let keys = from.own_property_keys(cx);

    for next_key in keys {
        let next_key = must!(PropertyKey::from_value(cx, next_key));

        if !excluded_items.contains(&next_key) {
            let desc = maybe!(from.get_own_property(cx, &next_key));
            match desc {
                Some(desc) if desc.is_enumerable() => {
                    let prop_value = maybe!(get(cx, from, &next_key));
                    must!(create_data_property_or_throw(cx, target, &next_key, prop_value));
                }
                _ => {}
            }
        }
    }

    ().into()
}

// 7.3.30 PrivateGet
pub fn private_get(
    cx: &mut Context,
    mut object: Gc<ObjectValue>,
    private_id: PrivateNameId,
) -> EvalResult<Value> {
    let entry = match object.private_element_find(private_id) {
        None => return type_error_(cx, "can't access private field or method"),
        Some(entry) => entry,
    };

    if entry.kind() != PrivatePropertyKind::Accessor {
        return entry.value().into();
    }

    let accessor = entry.value().as_accessor();
    match accessor.get {
        None => return type_error_(cx, "cannot access private field or method"),
        Some(getter) => call_object(cx, getter, object.into(), &[]),
    }
}

// 7.3.31 PrivateSet
pub fn private_set(
    cx: &mut Context,
    mut object: Gc<ObjectValue>,
    private_id: PrivateNameId,
    value: Value,
) -> EvalResult<()> {
    let entry = match object.private_element_find(private_id) {
        None => return type_error_(cx, "cannot set private field or method"),
        Some(entry) => entry,
    };

    match entry.kind() {
        PrivatePropertyKind::Field => {
            entry.set_value(value);
            ().into()
        }
        PrivatePropertyKind::Method => type_error_(cx, "cannot assign to private method"),
        PrivatePropertyKind::Accessor => {
            let accessor = entry.value().as_accessor();
            match accessor.set {
                None => type_error_(cx, "cannot set getter-only private property"),
                Some(setter) => {
                    maybe!(call_object(cx, setter, object.into(), &[value]));
                    ().into()
                }
            }
        }
    }
}

// 7.3.32 DefineField
pub fn define_field(
    cx: &mut Context,
    mut receiver: Gc<ObjectValue>,
    field_def: &ClassFieldDefinition,
) -> EvalResult<()> {
    let init_value = match field_def.initializer {
        None => Value::undefined(),
        Some(func) => maybe!(call_object(cx, func.into(), receiver.into(), &[])),
    };

    match field_def.name {
        ClassFieldDefinitionName::Normal(ref property_key) => {
            maybe!(create_data_property_or_throw(cx, receiver, &property_key, init_value));
        }
        ClassFieldDefinitionName::Private(private_id) => {
            maybe!(receiver.private_field_add(cx, private_id, init_value))
        }
    }

    ().into()
}

// 7.3.33 InitializeInstanceElements
pub fn initialize_instance_elements(
    cx: &mut Context,
    mut object: Gc<ObjectValue>,
    constructor: Gc<Function>,
) -> EvalResult<()> {
    for (private_id, private_method) in &constructor.private_methods {
        maybe!(object.private_method_or_accessor_add(cx, *private_id, private_method.clone()));
    }

    for field_def in &constructor.fields {
        maybe!(define_field(cx, object, field_def));
    }

    ().into()
}

pub fn get_function_realm(func: Gc<ObjectValue>) -> EvalResult<Realm> {
    unimplemented!()
}
