use crate::{
    intrinsic_methods,
    runtime::{
        Context, Handle, Value,
        abstract_operations::call,
        alloc_error::AllocResult,
        error::type_error,
        eval_result::EvalResult,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic, weak_map_object::WeakMapObject,
            weak_ref_constructor::can_be_held_weakly,
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::is_callable,
        value::ValueCollectionKey,
    },
    runtime_fn,
};

pub struct WeakMapPrototype;

impl WeakMapPrototype {
    /// Properties of the WeakMap Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-weakmap-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once WeakMapConstructor has been created
        intrinsic_methods!(cx, builder, {
            delete                 WeakMapPrototype_delete                 (1),
            get                    WeakMapPrototype_get                    (1),
            get_or_insert          WeakMapPrototype_get_or_insert          (2),
            get_or_insert_computed WeakMapPrototype_get_or_insert_computed (2),
            has                    WeakMapPrototype_has                    (1),
            set_                   WeakMapPrototype_set                    (2),
        });

        // WeakMap.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-weakmap.prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.weak_map())?;

        builder.build()
    }

    runtime_fn! {
    /// WeakMap.prototype.delete (https://tc39.es/ecma262/#sec-weakmap.prototype.delete)
    fn delete(cx, this_value, arguments) {
        let weak_map_object = this_weak_map_object(cx, this_value, "delete")?;

        // Do not need to call can_be_held_weakly, instead look up directly in the value map
        let value = arguments.get(cx, 0);

        // May allocate
        let map_key = ValueCollectionKey::from(value)?;

        let removed_value = weak_map_object.weak_map_data().remove(&map_key);

        Ok(cx.bool(removed_value))
    }}

    runtime_fn! {
    /// WeakMap.prototype.get (https://tc39.es/ecma262/#sec-weakmap.prototype.get)
    fn get(cx, this_value, arguments) {
        let weak_map_object = this_weak_map_object(cx, this_value, "get")?;

        // Do not need to call can_be_held_weakly, instead look up directly in the value map
        let key = arguments.get(cx, 0);

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        let weak_map_data = weak_map_object.weak_map_data();
        let value_opt = weak_map_data.get(&map_key);

        match value_opt {
            None => Ok(cx.undefined()),
            Some(value) => Ok(value.to_handle(cx)),
        }
    }}

    runtime_fn! {
    /// WeakMap.prototype.getOrInsert (https://tc39.es/ecma262/#sec-weakmap.prototype.getorinsert)
    fn get_or_insert(cx, this_value, arguments) {
        let weak_map_object = this_weak_map_object(cx, this_value, "getOrInsert")?;

        let key = arguments.get(cx, 0);
        let value = arguments.get(cx, 1);

        validate_can_be_held_weakly(cx, key, "getOrInsert")?;

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        if let Some(existing_value) = weak_map_object.weak_map_data().get(&map_key) {
            Ok(existing_value.to_handle(cx))
        } else {
            weak_map_object.insert(cx, key, value)?;
            Ok(value)
        }
    }}

    runtime_fn! {
    /// WeakMap.prototype.getOrInsertComputed (https://tc39.es/ecma262/#sec-weakmap.prototype.getorinsertcomputed)
    fn get_or_insert_computed(cx, this_value, arguments) {
        let weak_map_object = this_weak_map_object(cx, this_value, "getOrInsertComputed")?;

        let key = arguments.get(cx, 0);
        let callback = arguments.get(cx, 1);

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
    }}

    runtime_fn! {
    /// WeakMap.prototype.has (https://tc39.es/ecma262/#sec-weakmap.prototype.has)
    fn has(cx, this_value, arguments) {
        let weak_map_object = this_weak_map_object(cx, this_value, "has")?;

        // Do not need to call can_be_held_weakly, instead look up directly in the value map
        let key = arguments.get(cx, 0);

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        let has_key = weak_map_object.weak_map_data().contains_key(&map_key);

        Ok(cx.bool(has_key))
    }}

    runtime_fn! {
    /// WeakMap.prototype.set (https://tc39.es/ecma262/#sec-weakmap.prototype.set)
    fn set(cx, this_value, arguments) {
        let weak_map_object = this_weak_map_object(cx, this_value, "set")?;

        let key = arguments.get(cx, 0);
        let value = arguments.get(cx, 1);

        validate_can_be_held_weakly(cx, key, "set")?;

        weak_map_object.insert(cx, key, value)?;

        Ok(this_value)
    }}
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
