use std::collections::HashSet;

use crate::{
    js::runtime::{
        iterator::iter_iterator_values,
        numeric_constants::MAX_SAFE_INTEGER_U64,
        type_utilities::{same_value_zero, to_property_key},
    },
    maybe, must,
};

use super::{
    array_object::create_array_from_list,
    bound_function_object::BoundFunctionObject,
    bytecode::function::Closure,
    completion::EvalResult,
    error::{err_cannot_set_property, type_error},
    eval::expression::eval_instanceof_expression,
    gc::{Handle, HeapPtr},
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::ObjectKind,
    object_value::ObjectValue,
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    proxy_object::ProxyObject,
    realm::Realm,
    type_utilities::{
        is_callable, is_constructor_value, require_object_coercible, same_object_value_handles,
        to_length, to_object,
    },
    value::SymbolValue,
    Context, Value,
};

/// IsExtensible (https://tc39.es/ecma262/#sec-isextensible-o)
#[inline]
pub fn is_extensible(cx: Context, object: Handle<ObjectValue>) -> EvalResult<bool> {
    object.is_extensible(cx)
}

/// Get (https://tc39.es/ecma262/#sec-get-o-p)
pub fn get(
    cx: Context,
    object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
) -> EvalResult<Handle<Value>> {
    object.get(cx, key, object.into())
}

/// GetV (https://tc39.es/ecma262/#sec-getv)
pub fn get_v(
    cx: Context,
    value: Handle<Value>,
    key: Handle<PropertyKey>,
) -> EvalResult<Handle<Value>> {
    let object = maybe!(to_object(cx, value));
    object.get(cx, key, value)
}

/// Set (https://tc39.es/ecma262/#sec-set-o-p-v-throw)
pub fn set(
    cx: Context,
    mut object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
    value: Handle<Value>,
    should_throw: bool,
) -> EvalResult<()> {
    let success = maybe!(object.set(cx, key, value, object.into()));
    if !success && should_throw {
        return err_cannot_set_property(cx, key);
    }

    ().into()
}

/// CreateDataProperty (https://tc39.es/ecma262/#sec-createdataproperty)
pub fn create_data_property(
    cx: Context,
    mut object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
    value: Handle<Value>,
) -> EvalResult<bool> {
    let new_desc = PropertyDescriptor::data(value, true, true, true);
    object.define_own_property(cx, key, new_desc)
}

/// CreateDataPropertyOrThrow (https://tc39.es/ecma262/#sec-createdatapropertyorthrow)
pub fn create_data_property_or_throw(
    cx: Context,
    object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
    value: Handle<Value>,
) -> EvalResult<()> {
    let success = maybe!(create_data_property(cx, object, key, value));
    if !success {
        return type_error(cx, &format!("Cannot create property {}", key));
    }

    ().into()
}

/// CreateNonEnumerableDataPropertyOrThrow (https://tc39.es/ecma262/#sec-createnonenumerabledatapropertyorthrow)
pub fn create_non_enumerable_data_property_or_throw(
    cx: Context,
    object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
    value: Handle<Value>,
) {
    let new_desc = PropertyDescriptor::data(value, true, false, true);
    must!(define_property_or_throw(cx, object, key, new_desc));
}

/// DefinePropertyOrThrow (https://tc39.es/ecma262/#sec-definepropertyorthrow)
pub fn define_property_or_throw(
    cx: Context,
    mut object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
    prop_desc: PropertyDescriptor,
) -> EvalResult<()> {
    let success = maybe!(object.define_own_property(cx, key, prop_desc));
    if !success {
        return type_error(cx, &format!("cannot define property {}", key));
    }

    ().into()
}

/// DeletePropertyOrThrow (https://tc39.es/ecma262/#sec-deletepropertyorthrow)
pub fn delete_property_or_throw(
    cx: Context,
    mut object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
) -> EvalResult<()> {
    if !maybe!(object.delete(cx, key)) {
        return type_error(cx, &format!("cannot delete property {}", key));
    }

    ().into()
}

/// GetMethod (https://tc39.es/ecma262/#sec-getmethod)
pub fn get_method(
    cx: Context,
    value: Handle<Value>,
    key: Handle<PropertyKey>,
) -> EvalResult<Option<Handle<ObjectValue>>> {
    let func = maybe!(get_v(cx, value, key));
    if func.is_nullish() {
        return None.into();
    }

    if !is_callable(func) {
        return type_error(cx, "value is not a function");
    }

    (Some(func.as_object())).into()
}

/// HasProperty (https://tc39.es/ecma262/#sec-hasproperty)
pub fn has_property(
    cx: Context,
    object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
) -> EvalResult<bool> {
    object.has_property(cx, key)
}

/// HasOwnProperty (https://tc39.es/ecma262/#sec-hasownproperty)
pub fn has_own_property(
    cx: Context,
    object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
) -> EvalResult<bool> {
    let desc = maybe!(object.get_own_property(cx, key));
    desc.is_some().into()
}

/// Call (https://tc39.es/ecma262/#sec-call)
pub fn call(
    mut cx: Context,
    func: Handle<Value>,
    receiver: Handle<Value>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    cx.vm().call_from_rust(func, receiver, arguments)
}

pub fn call_object(
    mut cx: Context,
    func: Handle<ObjectValue>,
    receiver: Handle<Value>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    cx.vm().call_from_rust(func.into(), receiver, arguments)
}

/// Construct (https://tc39.es/ecma262/#sec-construct)
pub fn construct(
    mut cx: Context,
    func: Handle<ObjectValue>,
    arguments: &[Handle<Value>],
    new_target: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<ObjectValue>> {
    let new_target = new_target.unwrap_or(func);
    cx.vm()
        .construct_from_rust(func.into(), arguments, new_target)
}

#[derive(PartialEq)]
pub enum IntegrityLevel {
    Sealed,
    Frozen,
}

/// SetIntegrityLevel (https://tc39.es/ecma262/#sec-setintegritylevel)
pub fn set_integrity_level(
    cx: Context,
    mut object: Handle<ObjectValue>,
    level: IntegrityLevel,
) -> EvalResult<bool> {
    if !maybe!(object.prevent_extensions(cx)) {
        return false.into();
    }

    let keys = maybe!(object.own_property_keys(cx));

    match level {
        IntegrityLevel::Sealed => {
            // Property key is shared between iterations
            let mut key = PropertyKey::uninit().to_handle(cx);

            for key_value in keys {
                key.replace(must!(PropertyKey::from_value(cx, key_value)));
                let desc = PropertyDescriptor::attributes(None, None, Some(false));
                maybe!(define_property_or_throw(cx, object, key, desc));
            }
        }
        IntegrityLevel::Frozen => {
            // Property key is shared between iterations
            let mut key = PropertyKey::uninit().to_handle(cx);

            for key_value in keys {
                key.replace(must!(PropertyKey::from_value(cx, key_value)));
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

/// TestIntegrityLevel (https://tc39.es/ecma262/#sec-testintegritylevel)
pub fn test_integrity_level(
    cx: Context,
    object: Handle<ObjectValue>,
    level: IntegrityLevel,
) -> EvalResult<bool> {
    if maybe!(object.is_extensible(cx)) {
        return false.into();
    }

    let keys = maybe!(object.own_property_keys(cx));

    // Property key is shared between iterations
    let mut key = PropertyKey::uninit().to_handle(cx);

    for key_value in keys {
        key.replace(must!(PropertyKey::from_value(cx, key_value)));
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

/// LengthOfArrayLike (https://tc39.es/ecma262/#sec-lengthofarraylike)
pub fn length_of_array_like(cx: Context, object: Handle<ObjectValue>) -> EvalResult<u64> {
    let length_value = maybe!(get(cx, object, cx.names.length()));
    to_length(cx, length_value)
}

/// CreateListFromArrayLike (https://tc39.es/ecma262/#sec-createlistfromarraylike)
pub fn create_list_from_array_like(
    cx: Context,
    object: Handle<Value>,
) -> EvalResult<Vec<Handle<Value>>> {
    if !object.is_object() {
        return type_error(cx, "value is not an object");
    }

    let object = object.as_object();
    let length = maybe!(length_of_array_like(cx, object));

    let mut vec = Vec::with_capacity(length as usize);

    // Property key is shared between iterations
    let mut key = PropertyKey::uninit().to_handle(cx);

    for i in 0..length {
        key.replace(PropertyKey::array_index(cx, i as u32));
        let next = maybe!(get(cx, object, key));
        vec.push(next);
    }

    vec.into()
}

/// Invoke (https://tc39.es/ecma262/#sec-invoke)
pub fn invoke(
    cx: Context,
    value: Handle<Value>,
    key: Handle<PropertyKey>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    let func = maybe!(get_v(cx, value, key));
    call(cx, func, value, arguments)
}

/// OrdinaryHasInstance (https://tc39.es/ecma262/#sec-ordinaryhasinstance)
pub fn ordinary_has_instance(
    cx: Context,
    func: Handle<Value>,
    object: Handle<Value>,
) -> EvalResult<bool> {
    if !is_callable(func) {
        return false.into();
    }

    let func = func.as_object();

    if let Some(target_func) = BoundFunctionObject::get_target_if_bound_function(cx, func) {
        return eval_instanceof_expression(cx, object, target_func.into());
    }

    if !object.is_object() {
        return false.into();
    }

    let target_prototype = maybe!(get(cx, func, cx.names.prototype()));
    if !target_prototype.is_object() {
        return type_error(cx, "prototype must be object");
    }
    let target_prototype = target_prototype.as_object();

    // Walk prototype chain of object, looking for prototype of func
    let mut current_object = object.as_object();
    loop {
        match maybe!(current_object.get_prototype_of(cx)) {
            None => return false.into(),
            Some(current_prototype) => {
                if same_object_value_handles(target_prototype, current_prototype) {
                    return true.into();
                }

                current_object = current_prototype;
            }
        }
    }
}

/// SpeciesConstructor (https://tc39.es/ecma262/#sec-speciesconstructor)
pub fn species_constructor(
    cx: Context,
    object: Handle<ObjectValue>,
    default_constructor: Intrinsic,
) -> EvalResult<Handle<ObjectValue>> {
    let constructor = maybe!(get(cx, object, cx.names.constructor()));

    if constructor.is_undefined() {
        return cx.get_intrinsic(default_constructor).into();
    }

    if !constructor.is_object() {
        return type_error(cx, "constructor must be a function");
    }

    let species_key = cx.well_known_symbols.species();
    let species = maybe!(get(cx, constructor.as_object(), species_key));

    if species.is_nullish() {
        return cx.get_intrinsic(default_constructor).into();
    }

    if is_constructor_value(species) {
        return species.as_object().into();
    }

    type_error(cx, "species must be a constructor")
}

pub enum KeyOrValue {
    Key,
    Value,
    KeyAndValue,
}

/// EnumerableOwnProperties (https://tc39.es/ecma262/#sec-enumerableownproperties)
pub fn enumerable_own_property_names(
    cx: Context,
    object: Handle<ObjectValue>,
    kind: KeyOrValue,
) -> EvalResult<Vec<Handle<Value>>> {
    let keys = maybe!(object.own_property_keys(cx));

    let mut properties = vec![];

    // Property key is shared between iterations
    let mut key = PropertyKey::uninit().to_handle(cx);

    for key_value in keys {
        if key_value.is_symbol() {
            continue;
        }

        key.replace(must!(PropertyKey::from_value(cx, key_value)));
        let desc = maybe!(object.get_own_property(cx, key));

        if let Some(desc) = desc {
            if let Some(true) = desc.is_enumerable {
                match kind {
                    KeyOrValue::Key => properties.push(key_value.to_handle(cx)),
                    KeyOrValue::Value => {
                        let value = maybe!(get(cx, object, key));
                        properties.push(value);
                    }
                    KeyOrValue::KeyAndValue => {
                        let value = maybe!(get(cx, object, key));
                        let key_and_value = [key_value.to_handle(cx), value];
                        let entry = create_array_from_list(cx, &key_and_value);
                        properties.push(entry.into());
                    }
                }
            }
        }
    }

    properties.into()
}

/// GetFunctionRealm (https://tc39.es/ecma262/#sec-getfunctionrealm)
pub fn get_function_realm(cx: Context, func: Handle<ObjectValue>) -> EvalResult<HeapPtr<Realm>> {
    match get_function_realm_no_error(cx, func) {
        Some(realm) => realm.into(),
        None => type_error(cx, "operation attempted on revoked proxy"),
    }
}

/// GetFunctionRealm (https://tc39.es/ecma262/#sec-getfunctionrealm)
/// but does not allocate an error on the error path.
pub fn get_function_realm_no_error(
    cx: Context,
    func: Handle<ObjectValue>,
) -> Option<HeapPtr<Realm>> {
    let kind = func.descriptor().kind();

    // Bound functions are also represented as closures with the correct realm set
    if kind == ObjectKind::Closure {
        if let Some(bound_target_func) = BoundFunctionObject::get_target_if_bound_function(cx, func)
        {
            get_function_realm_no_error(cx, bound_target_func)
        } else {
            Some(func.cast::<Closure>().function_ptr().realm_ptr())
        }
    } else if kind == ObjectKind::Proxy {
        let proxy_object = func.cast::<ProxyObject>();
        if proxy_object.is_revoked() {
            return None;
        }

        get_function_realm_no_error(cx, proxy_object.target().unwrap())
    } else {
        Some(cx.current_realm_ptr())
    }
}

/// CopyDataProperties (https://tc39.es/ecma262/#sec-copydataproperties)
pub fn copy_data_properties(
    cx: Context,
    target: Handle<ObjectValue>,
    source: Handle<Value>,
    excluded_items: &HashSet<Handle<PropertyKey>>,
) -> EvalResult<()> {
    if source.is_nullish() {
        return ().into();
    }

    let from = must!(to_object(cx, source));
    let keys = maybe!(from.own_property_keys(cx));

    // Property key is shared between iterations
    let mut next_key = PropertyKey::uninit().to_handle(cx);

    for next_key_value in keys {
        next_key.replace(must!(PropertyKey::from_value(cx, next_key_value)));

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

/// PrivateGet (https://tc39.es/ecma262/#sec-privateget)
pub fn private_get(
    cx: Context,
    object: Handle<ObjectValue>,
    private_name: Handle<SymbolValue>,
) -> EvalResult<Handle<Value>> {
    let property = match object.private_element_find(cx, private_name) {
        None => return type_error(cx, "can't access private field or method"),
        Some(property) => property,
    };

    if !property.is_private_accessor() {
        return property.value().into();
    }

    let accessor = property.value().as_accessor();
    match accessor.get {
        None => type_error(cx, "cannot access private field or method"),
        Some(getter) => {
            let getter_handle = getter.to_handle();
            call_object(cx, getter_handle, object.into(), &[])
        }
    }
}

/// PrivateSet (https://tc39.es/ecma262/#sec-privateset)
pub fn private_set(
    cx: Context,
    mut object: Handle<ObjectValue>,
    private_name: Handle<SymbolValue>,
    value: Handle<Value>,
) -> EvalResult<()> {
    let property = match object.private_element_find(cx, private_name) {
        None => return type_error(cx, "cannot set private field or method"),
        Some(entry) => entry,
    };

    if property.is_private_field() {
        object.private_element_set(cx, private_name, value);
        ().into()
    } else if property.is_private_method() {
        type_error(cx, "cannot assign to private method")
    } else {
        // Property is an private accessor
        let accessor = property.value().as_accessor();
        match accessor.set {
            None => type_error(cx, "cannot set getter-only private property"),
            Some(setter) => {
                let setter_handle = setter.to_handle();
                maybe!(call_object(cx, setter_handle, object.into(), &[value]));
                ().into()
            }
        }
    }
}

pub struct Group {
    pub key: Handle<Value>,
    pub items: Vec<Handle<Value>>,
}

pub enum GroupByKeyCoercion {
    /// Ensure that each item is a property key (can be cast directly to PropertyKey)
    Property,
    Collection,
}

/// GroupBy (https://tc39.es/ecma262/#sec-groupby)
pub fn group_by(
    cx: Context,
    items: Handle<Value>,
    callback: Handle<Value>,
    key_coercion: GroupByKeyCoercion,
) -> EvalResult<Vec<Group>> {
    maybe!(require_object_coercible(cx, items));

    if !is_callable(callback) {
        return type_error(cx, "callback must be a function");
    }

    let callback = callback.as_object();

    let mut groups: Vec<Group> = vec![];
    let mut k = 0;

    // Handle is shared between iterations
    let mut k_handle: Handle<Value> = Handle::empty(cx);

    maybe!(iter_iterator_values(cx, items, &mut |cx, item| {
        k_handle.replace(Value::from(k));

        let key = match call_object(cx, callback, cx.undefined(), &[item, k_handle]) {
            EvalResult::Ok(key) => key,
            EvalResult::Throw(error) => return Some(EvalResult::Throw(error)),
        };

        let key = match key_coercion {
            GroupByKeyCoercion::Property => match to_property_key(cx, key) {
                EvalResult::Ok(key) => key.cast::<Value>(),
                EvalResult::Throw(error) => return Some(EvalResult::Throw(error)),
            },
            // Do not canonicalize negative zero to positive zero. Instead use zero-unaware
            // comparisons and convert to positive zero when necessary.
            GroupByKeyCoercion::Collection => key,
        };

        // Inlined AddValueToKeyedGroup (https://tc39.es/ecma262/#sec-add-value-to-keyed-group)

        // Add to an existing group if one was found
        let mut found_group = false;
        for group in &mut groups {
            // Use zero-unaware comparisons because keys are not canonicalized
            if same_value_zero(key, group.key) {
                group.items.push(item);
                found_group = true;
                break;
            }
        }

        // Otherwise add a new group
        if !found_group {
            // Canonicalize key
            let key = if key.is_negative_zero() {
                Value::from(0).to_handle(cx)
            } else {
                key
            };

            groups.push(Group { key, items: vec![item] });
        }

        k += 1;

        if k >= MAX_SAFE_INTEGER_U64 {
            return Some(type_error(cx, "index is too large"));
        }

        None
    }));

    groups.into()
}

pub fn canonicalize_keyed_collection_key(cx: Context, key: Handle<Value>) -> Handle<Value> {
    if key.is_negative_zero() {
        Value::from(0).to_handle(cx)
    } else {
        key
    }
}
