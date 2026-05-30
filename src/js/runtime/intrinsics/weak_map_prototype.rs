use crate::runtime::{
    abstract_operations::call,
    alloc_error::AllocResult,
    error::type_error,
    eval_result::EvalResult,
    function::get_argument,
    intrinsics::{rust_runtime::RuntimeFunction, weak_ref_constructor::can_be_held_weakly},
    object_value::ObjectValue,
    property::Property,
    realm::Realm,
    type_utilities::is_callable,
    value::ValueCollectionKey,
    Context, Handle, Value,
};

use super::{intrinsics::Intrinsic, weak_map_object::WeakMapObject};

pub struct WeakMapPrototype;

impl WeakMapPrototype {
    /// Properties of the WeakMap Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-weakmap-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once WeakMapConstructor has been created
        object.intrinsic_func(
            cx,
            cx.names.delete(),
            RuntimeFunction::WeakMapPrototype_delete,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.get(),
            RuntimeFunction::WeakMapPrototype_get,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.get_or_insert(),
            RuntimeFunction::WeakMapPrototype_get_or_insert,
            2,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.get_or_insert_computed(),
            RuntimeFunction::WeakMapPrototype_get_or_insert_computed,
            2,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.has(),
            RuntimeFunction::WeakMapPrototype_has,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.set_(),
            RuntimeFunction::WeakMapPrototype_set,
            2,
            realm,
        )?;

        // [Symbol.toStringTag] property
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.weak_map().as_string().into(), false, false, true),
        )?;

        Ok(object)
    }

    /// WeakMap.prototype.delete (https://tc39.es/ecma262/#sec-weakmap.prototype.delete)
    pub fn delete(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let weak_map_object = this_weak_map_object(cx, this_value, "delete")?;

        // Do not need to call can_be_held_weakly, instead look up directly in the value map
        let value = get_argument(cx, arguments, 0);

        // May allocate
        let map_key = ValueCollectionKey::from(value)?;

        let removed_value = weak_map_object.weak_map_data().remove(&map_key);

        Ok(cx.bool(removed_value))
    }

    /// WeakMap.prototype.get (https://tc39.es/ecma262/#sec-weakmap.prototype.get)
    pub fn get(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let weak_map_object = this_weak_map_object(cx, this_value, "get")?;

        // Do not need to call can_be_held_weakly, instead look up directly in the value map
        let key = get_argument(cx, arguments, 0);

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        let weak_map_data = weak_map_object.weak_map_data();
        let value_opt = weak_map_data.get(&map_key);

        match value_opt {
            None => Ok(cx.undefined()),
            Some(value) => Ok(value.to_handle(cx)),
        }
    }

    /// WeakMap.prototype.getOrInsert (https://tc39.es/ecma262/#sec-weakmap.prototype.getorinsert)
    pub fn get_or_insert(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let weak_map_object = this_weak_map_object(cx, this_value, "getOrInsert")?;

        let key = get_argument(cx, arguments, 0);
        let value = get_argument(cx, arguments, 1);

        validate_can_be_held_weakly(cx, key, "getOrInsert")?;

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        if let Some(existing_value) = weak_map_object.weak_map_data().get(&map_key) {
            Ok(existing_value.to_handle(cx))
        } else {
            weak_map_object.insert(cx, key, value)?;
            Ok(value)
        }
    }

    /// WeakMap.prototype.getOrInsertComputed (https://tc39.es/ecma262/#sec-weakmap.prototype.getorinsertcomputed)
    pub fn get_or_insert_computed(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let weak_map_object = this_weak_map_object(cx, this_value, "getOrInsertComputed")?;

        let key = get_argument(cx, arguments, 0);
        let callback = get_argument(cx, arguments, 1);

        validate_can_be_held_weakly(cx, key, "getOrInsertComputed")?;

        if !is_callable(callback) {
            return type_error(
                cx,
                "WeakMap.prototype.getOrInsertComputed second argument must be a function",
            );
        }

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        if let Some(existing_value) = weak_map_object.weak_map_data().get(&map_key) {
            return Ok(existing_value.to_handle(cx));
        }

        let value = call(cx, callback, cx.undefined(), &[key])?;

        // Write the (key, value) pair, even if the callback already inserted a value for the key
        weak_map_object.insert(cx, key, value)?;

        Ok(value.to_handle(cx))
    }

    /// WeakMap.prototype.has (https://tc39.es/ecma262/#sec-weakmap.prototype.has)
    pub fn has(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let weak_map_object = this_weak_map_object(cx, this_value, "has")?;

        // Do not need to call can_be_held_weakly, instead look up directly in the value map
        let key = get_argument(cx, arguments, 0);

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        let has_key = weak_map_object.weak_map_data().contains_key(&map_key);

        Ok(cx.bool(has_key))
    }

    /// WeakMap.prototype.set (https://tc39.es/ecma262/#sec-weakmap.prototype.set)
    pub fn set(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let weak_map_object = this_weak_map_object(cx, this_value, "set")?;

        let key = get_argument(cx, arguments, 0);
        let value = get_argument(cx, arguments, 1);

        validate_can_be_held_weakly(cx, key, "set")?;

        weak_map_object.insert(cx, key, value)?;

        Ok(this_value)
    }
}

fn this_weak_map_object(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<WeakMapObject>> {
    if value.is_object() {
        if let Some(weak_map_object) = value.as_object().as_weak_map_object() {
            return Ok(weak_map_object);
        }
    }

    type_error(cx, &format!("WeakMap.prototype.{method_name} must be called on a WeakMap"))
}

/// Throw a TypeError if the given key cannot be held weakly.
fn validate_can_be_held_weakly(
    cx: Context,
    key: Handle<Value>,
    method_name: &str,
) -> EvalResult<()> {
    if !can_be_held_weakly(cx, *key) {
        return type_error(
            cx,
            &format!("WeakMap.prototype.{method_name} key must be an object or symbol"),
        );
    }

    Ok(())
}
