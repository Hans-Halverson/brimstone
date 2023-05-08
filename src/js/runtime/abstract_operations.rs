use std::collections::HashSet;

use crate::{js::runtime::eval::class::ClassFieldDefinitionName, maybe, must};

use super::{
    array_object::create_array_from_list,
    bound_function_object::BoundFunctionObject,
    completion::EvalResult,
    environment::private_environment::PrivateName,
    error::type_error_,
    eval::{class::ClassFieldDefinition, expression::eval_instanceof_expression},
    function::Function,
    gc::{Handle, HandleValue, HeapPtr},
    object_value::ObjectValue,
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    realm::Realm,
    type_utilities::{
        is_callable, is_callable_object, is_constructor, same_object_value, to_length, to_object,
    },
    Context,
};

// 7.2.5 IsExtensible
#[inline]
pub fn is_extensible(cx: &mut Context, object: Handle<ObjectValue>) -> EvalResult<bool> {
    object.is_extensible(cx)
}

// 7.3.2 Get
pub fn get(
    cx: &mut Context,
    object: Handle<ObjectValue>,
    key: PropertyKey,
) -> EvalResult<HandleValue> {
    object.get(cx, key, object.into())
}

// 7.3.3 GetV
pub fn get_v(cx: &mut Context, value: HandleValue, key: PropertyKey) -> EvalResult<HandleValue> {
    let object = maybe!(to_object(cx, value));
    object.get(cx, key, value)
}

// 7.3.4 Set
pub fn set(
    cx: &mut Context,
    mut object: Handle<ObjectValue>,
    key: PropertyKey,
    value: HandleValue,
    should_throw: bool,
) -> EvalResult<()> {
    let success = maybe!(object.set(cx, key, value, object.into()));
    if !success && should_throw {
        return type_error_(cx, &format!("Cannot set property {}", key));
    }

    ().into()
}

// 7.3.6 CreateMethodProperty
pub fn create_method_property(
    cx: &mut Context,
    mut object: Handle<ObjectValue>,
    key: PropertyKey,
    value: HandleValue,
) {
    let new_desc = PropertyDescriptor::data(value, true, false, true);
    must!(object.define_own_property(cx, key, new_desc));
}

// 7.3.5 CreateDataProperty
pub fn create_data_property(
    cx: &mut Context,
    mut object: Handle<ObjectValue>,
    key: PropertyKey,
    value: HandleValue,
) -> EvalResult<bool> {
    let new_desc = PropertyDescriptor::data(value, true, true, true);
    object.define_own_property(cx, key, new_desc)
}

// 7.3.7 CreateDataPropertyOrThrow
pub fn create_data_property_or_throw(
    cx: &mut Context,
    object: Handle<ObjectValue>,
    key: PropertyKey,
    value: HandleValue,
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
    object: Handle<ObjectValue>,
    key: PropertyKey,
    value: HandleValue,
) {
    let new_desc = PropertyDescriptor::data(value, true, false, true);
    must!(define_property_or_throw(cx, object, key, new_desc));
}

// 7.3.8 DefinePropertyOrThrow
pub fn define_property_or_throw(
    cx: &mut Context,
    mut object: Handle<ObjectValue>,
    key: PropertyKey,
    prop_desc: PropertyDescriptor,
) -> EvalResult<()> {
    let success = maybe!(object.define_own_property(cx, key, prop_desc));
    if !success {
        return type_error_(cx, &format!("cannot define property {}", key));
    }

    ().into()
}

// 7.3.10 DeletePropertyOrThrow
pub fn delete_property_or_throw(
    cx: &mut Context,
    mut object: Handle<ObjectValue>,
    key: PropertyKey,
) -> EvalResult<()> {
    if !maybe!(object.delete(cx, key)) {
        return type_error_(cx, &format!("cannot delete property {}", key));
    }

    ().into()
}

// 7.3.11 GetMethod
pub fn get_method(
    cx: &mut Context,
    value: HandleValue,
    key: PropertyKey,
) -> EvalResult<Option<Handle<ObjectValue>>> {
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
    object: Handle<ObjectValue>,
    key: PropertyKey,
) -> EvalResult<bool> {
    object.has_property(cx, key)
}

// 7.3.13 HasOwnProperty
pub fn has_own_property(
    cx: &mut Context,
    object: Handle<ObjectValue>,
    key: PropertyKey,
) -> EvalResult<bool> {
    let desc = maybe!(object.get_own_property(cx, key));
    desc.is_some().into()
}

// 7.3.14 Call
pub fn call(
    cx: &mut Context,
    func: HandleValue,
    receiver: HandleValue,
    arguments: &[HandleValue],
) -> EvalResult<HandleValue> {
    if !is_callable(func) {
        return type_error_(cx, "value is not a function");
    }

    func.as_object().call(cx, receiver, arguments)
}

pub fn call_object(
    cx: &mut Context,
    func: Handle<ObjectValue>,
    receiver: HandleValue,
    arguments: &[HandleValue],
) -> EvalResult<HandleValue> {
    if !is_callable_object(func) {
        return type_error_(cx, "value is not a function");
    }

    func.call(cx, receiver, arguments)
}

// 7.3.15 Construct
pub fn construct(
    cx: &mut Context,
    func: Handle<ObjectValue>,
    arguments: &[HandleValue],
    new_target: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<ObjectValue>> {
    let new_target = new_target.unwrap_or(func);
    func.construct(cx, arguments, new_target)
}

#[derive(PartialEq)]
pub enum IntegrityLevel {
    Sealed,
    Frozen,
}

// 7.3.16 SetIntegrityLevel
pub fn set_integrity_level(
    cx: &mut Context,
    mut object: Handle<ObjectValue>,
    level: IntegrityLevel,
) -> EvalResult<bool> {
    if !maybe!(object.prevent_extensions(cx)) {
        return false.into();
    }

    let keys = maybe!(object.own_property_keys(cx));

    match level {
        IntegrityLevel::Sealed => {
            for key in keys {
                let key = must!(PropertyKey::from_value(cx, key));
                let desc = PropertyDescriptor::attributes(None, None, Some(false));
                maybe!(define_property_or_throw(cx, object, key, desc));
            }
        }
        IntegrityLevel::Frozen => {
            for key in keys {
                let key = must!(PropertyKey::from_value(cx, key));
                let current_desc = maybe!(object.get_own_property(cx, key));
                if let Some(current_desc) = current_desc {
                    let desc = if current_desc.is_accessor_descriptor() {
                        PropertyDescriptor::attributes(None, None, Some(false))
                    } else {
                        PropertyDescriptor::attributes(Some(false), None, Some(false))
                    };

                    maybe!(define_property_or_throw(cx, object, key, desc));
                }
            }
        }
    }

    true.into()
}

// 7.3.17 TestIntegrityLevel
pub fn test_integrity_level(
    cx: &mut Context,
    object: Handle<ObjectValue>,
    level: IntegrityLevel,
) -> EvalResult<bool> {
    if maybe!(object.is_extensible(cx)) {
        return false.into();
    }

    let keys = maybe!(object.own_property_keys(cx));

    for key in keys {
        let key = must!(PropertyKey::from_value(cx, key));
        let current_desc = maybe!(object.get_own_property(cx, key));
        if let Some(current_desc) = current_desc {
            if let Some(true) = current_desc.is_configurable {
                return false.into();
            }

            if level == IntegrityLevel::Frozen && current_desc.is_data_descriptor() {
                if let Some(true) = current_desc.is_writable {
                    return false.into();
                }
            }
        }
    }

    true.into()
}

// 7.3.19 LengthOfArrayLike
pub fn length_of_array_like(cx: &mut Context, object: Handle<ObjectValue>) -> EvalResult<u64> {
    let length_value = maybe!(get(cx, object, cx.names.length()));
    to_length(cx, length_value).into()
}

// 7.3.20 CreateListFromArrayLike
pub fn create_list_from_array_like(
    cx: &mut Context,
    object: HandleValue,
) -> EvalResult<Vec<HandleValue>> {
    if !object.is_object() {
        return type_error_(cx, "value is not an object");
    }

    let object = object.as_object();
    let length = maybe!(length_of_array_like(cx, object));

    let mut vec = Vec::with_capacity(length as usize);

    for i in 0..length {
        let key = PropertyKey::array_index(cx, i as u32);
        let next = maybe!(get(cx, object, key));
        vec.push(next);
    }

    vec.into()
}

// 7.3.21 Invoke
pub fn invoke(
    cx: &mut Context,
    value: HandleValue,
    key: PropertyKey,
    arguments: &[HandleValue],
) -> EvalResult<HandleValue> {
    let func = maybe!(get_v(cx, value, key));
    call(cx, func, value, arguments)
}

// 7.3.22 OrdinaryHasInstance
pub fn ordinary_has_instance(
    cx: &mut Context,
    func: HandleValue,
    object: HandleValue,
) -> EvalResult<bool> {
    if !is_callable(func) {
        return false.into();
    }

    let func = func.as_object();

    if func.is_bound_function() {
        let bound_func = func.cast::<BoundFunctionObject>();
        return eval_instanceof_expression(cx, object, bound_func.bound_target_function().into());
    }

    if !object.is_object() {
        return false.into();
    }

    let target_prototype = maybe!(get(cx, func, cx.names.prototype()));
    if !target_prototype.is_object() {
        return type_error_(cx, "prototype must be object");
    }
    let target_prototype = target_prototype.as_object();

    // Walk prototype chain of object, looking for prototype of func
    let mut current_object = object.as_object();
    loop {
        match maybe!(current_object.get_prototype_of(cx)) {
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

// 7.3.23 SpeciesConstructor
pub fn species_constructor(
    cx: &mut Context,
    object: Handle<ObjectValue>,
    default_constructor: Handle<ObjectValue>,
) -> EvalResult<Handle<ObjectValue>> {
    let constructor = maybe!(get(cx, object, cx.names.constructor()));

    if constructor.is_undefined() {
        return default_constructor.into();
    }

    if !constructor.is_object() {
        return type_error_(cx, "constructor must be a function");
    }

    let species_key = cx.well_known_symbols.species();
    let species = maybe!(get(cx, constructor.as_object(), species_key));

    if species.is_nullish() {
        return default_constructor.into();
    }

    if is_constructor(species) {
        return species.as_object().into();
    }

    type_error_(cx, "species must be a constructor")
}

pub enum KeyOrValue {
    Key,
    Value,
    KeyAndValue,
}

// 7.3.24 EnumerableOwnPropertyNames
pub fn enumerable_own_property_names(
    cx: &mut Context,
    object: Handle<ObjectValue>,
    kind: KeyOrValue,
) -> EvalResult<Vec<HandleValue>> {
    let keys = maybe!(object.own_property_keys(cx));

    let mut properties = vec![];

    for key_value in keys {
        if key_value.is_symbol() {
            continue;
        }

        let key = must!(PropertyKey::from_value(cx, key_value));
        let desc = maybe!(object.get_own_property(cx, key));

        if let Some(desc) = desc {
            if let Some(true) = desc.is_enumerable {
                match kind {
                    KeyOrValue::Key => properties.push(key_value),
                    KeyOrValue::Value => {
                        let value = maybe!(get(cx, object, key));
                        properties.push(value);
                    }
                    KeyOrValue::KeyAndValue => {
                        let value = maybe!(get(cx, object, key));
                        let entry = create_array_from_list(cx, &[key_value, value]);
                        properties.push(entry.into());
                    }
                }
            }
        }
    }

    properties.into()
}

// 7.3.25 GetFunctionRealm
pub fn get_function_realm(
    cx: &mut Context,
    func: Handle<ObjectValue>,
) -> EvalResult<HeapPtr<Realm>> {
    func.get_realm(cx)
}

// 7.3.26 CopyDataProperties
pub fn copy_data_properties(
    cx: &mut Context,
    target: Handle<ObjectValue>,
    source: HandleValue,
    excluded_items: &HashSet<PropertyKey>,
) -> EvalResult<()> {
    if source.is_nullish() {
        return ().into();
    }

    let from = must!(to_object(cx, source));
    let keys = maybe!(from.own_property_keys(cx));

    for next_key in keys {
        let next_key = must!(PropertyKey::from_value(cx, next_key));

        if !excluded_items.contains(&next_key) {
            let desc = maybe!(from.get_own_property(cx, next_key));
            match desc {
                Some(desc) if desc.is_enumerable() => {
                    let prop_value = maybe!(get(cx, from, next_key));
                    must!(create_data_property_or_throw(cx, target, next_key, prop_value));
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
    mut object: Handle<ObjectValue>,
    private_name: PrivateName,
) -> EvalResult<HandleValue> {
    let property = match object.private_element_find(private_name) {
        None => return type_error_(cx, "can't access private field or method"),
        Some(property) => property,
    };

    if !property.is_private_accessor() {
        return property.value().into();
    }

    let accessor = property.value().as_accessor();
    match accessor.get {
        None => return type_error_(cx, "cannot access private field or method"),
        Some(getter) => call_object(cx, getter, object.into(), &[]),
    }
}

// 7.3.31 PrivateSet
pub fn private_set(
    cx: &mut Context,
    mut object: Handle<ObjectValue>,
    private_name: PrivateName,
    value: HandleValue,
) -> EvalResult<()> {
    let property = match object.private_element_find(private_name) {
        None => return type_error_(cx, "cannot set private field or method"),
        Some(entry) => entry,
    };

    if property.is_private_field() {
        object.private_element_set(private_name, value.get());
        ().into()
    } else if property.is_private_method() {
        type_error_(cx, "cannot assign to private method")
    } else {
        // Property is an private accessor
        let accessor = property.value().as_accessor();
        match accessor.set {
            None => type_error_(cx, "cannot set getter-only private property"),
            Some(setter) => {
                maybe!(call_object(cx, setter, object.into(), &[value]));
                ().into()
            }
        }
    }
}

// 7.3.32 DefineField
pub fn define_field(
    cx: &mut Context,
    mut receiver: Handle<ObjectValue>,
    field_def: ClassFieldDefinition,
) -> EvalResult<()> {
    let init_value = match field_def.initializer {
        None => cx.undefined(),
        Some(func) => maybe!(call_object(cx, func.into(), receiver.into(), &[])),
    };

    match field_def.name {
        ClassFieldDefinitionName::Normal(property_key) => {
            maybe!(create_data_property_or_throw(cx, receiver, property_key, init_value));
        }
        ClassFieldDefinitionName::Private(private_name) => {
            maybe!(receiver.private_field_add(cx, private_name, init_value))
        }
    }

    ().into()
}

// 7.3.33 InitializeInstanceElements
pub fn initialize_instance_elements(
    cx: &mut Context,
    mut object: Handle<ObjectValue>,
    constructor: Handle<Function>,
) -> EvalResult<()> {
    maybe!(constructor.iter_private_methods(|private_name, private_method| {
        object.private_method_or_accessor_add(cx, private_name, private_method)
    }));

    maybe!(constructor.iter_fields(|field| { define_field(cx, object, field) }));

    ().into()
}
