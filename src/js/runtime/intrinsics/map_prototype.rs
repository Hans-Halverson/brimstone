use crate::runtime::{
    Arguments, Context, Handle,
    abstract_operations::{call, call_object},
    alloc_error::AllocResult,
    builtin_function::BuiltinFunction,
    error::type_error,
    eval_result::EvalResult,
    intrinsics::{
        intrinsics::Intrinsic,
        map_iterator::{MapIterator, MapIteratorKind},
        map_object::MapObject,
        rust_runtime::RuntimeFunction,
    },
    object_value::ObjectValue,
    property::Property,
    realm::Realm,
    type_utilities::is_callable,
    value::{Value, ValueCollectionKey},
};

pub struct MapPrototype;

impl MapPrototype {
    /// Properties of the Map Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-map-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Create values function as it is referenced by multiple properties
        let entries_function = BuiltinFunction::create(
            cx,
            RuntimeFunction::MapPrototype_entries,
            0,
            cx.names.entries(),
            realm,
            None,
        )?
        .into();

        // Constructor property is added once MapConstructor has been created
        object.intrinsic_func(
            cx,
            cx.names.clear(),
            RuntimeFunction::MapPrototype_clear,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.delete(),
            RuntimeFunction::MapPrototype_delete,
            1,
            realm,
        )?;
        object.intrinsic_data_prop(cx, cx.names.entries(), entries_function)?;
        object.intrinsic_func(
            cx,
            cx.names.for_each(),
            RuntimeFunction::MapPrototype_for_each,
            1,
            realm,
        )?;
        object.intrinsic_func(cx, cx.names.get(), RuntimeFunction::MapPrototype_get, 1, realm)?;
        object.intrinsic_func(
            cx,
            cx.names.get_or_insert(),
            RuntimeFunction::MapPrototype_get_or_insert,
            2,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.get_or_insert_computed(),
            RuntimeFunction::MapPrototype_get_or_insert_computed,
            2,
            realm,
        )?;
        object.intrinsic_func(cx, cx.names.has(), RuntimeFunction::MapPrototype_has, 1, realm)?;
        object.intrinsic_func(cx, cx.names.keys(), RuntimeFunction::MapPrototype_keys, 0, realm)?;
        object.intrinsic_func(cx, cx.names.set_(), RuntimeFunction::MapPrototype_set, 2, realm)?;
        object.intrinsic_getter(cx, cx.names.size(), RuntimeFunction::MapPrototype_size, realm)?;
        object.intrinsic_func(
            cx,
            cx.names.values(),
            RuntimeFunction::MapPrototype_values,
            0,
            realm,
        )?;

        // Map.prototype [ @@iterator ] (https://tc39.es/ecma262/#sec-map.prototype-%symbol.iterator%)
        let iterator_key = cx.well_known_symbols.iterator();
        object.set_property(
            cx,
            iterator_key,
            Property::data(entries_function, true, false, true),
        )?;

        // Map.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-map.prototype-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.map().as_string().into(), false, false, true),
        )?;

        Ok(object)
    }

    /// Map.prototype.clear (https://tc39.es/ecma262/#sec-map.prototype.clear)
    pub fn clear(
        cx: Context,
        this_value: Handle<Value>,
        _: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let map = this_map_value(cx, this_value, "clear")?;

        map.map_data().clear();

        Ok(cx.undefined())
    }

    /// Map.prototype.delete (https://tc39.es/ecma262/#sec-map.prototype.delete)
    pub fn delete(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let map = this_map_value(cx, this_value, "delete")?;

        let key = arguments.get(cx, 0);

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        let existed = map.map_data().remove(&map_key);

        Ok(cx.bool(existed))
    }

    /// Map.prototype.entries (https://tc39.es/ecma262/#sec-map.prototype.entries)
    pub fn entries(
        cx: Context,
        this_value: Handle<Value>,
        _: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let map = this_map_value(cx, this_value, "entries")?;

        Ok(MapIterator::new(cx, map, MapIteratorKind::KeyAndValue)?.as_value())
    }

    /// Map.prototype.forEach (https://tc39.es/ecma262/#sec-map.prototype.foreach)
    pub fn for_each(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let map = this_map_value(cx, this_value, "forEach")?;

        let callback_function = arguments.get(cx, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "Map.prototype.forEach argument must be a function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = arguments.get(cx, 1);

        // Share key and value handles during iteration
        let mut key_handle = Handle::<Value>::empty(cx);
        let mut value_handle = Handle::<Value>::empty(cx);

        // Must use gc and invalidation safe iteration since arbitrary code can be executed between
        // iterations.
        for (key, value) in map.map_data().to_handle().iter_gc_safe() {
            key_handle.replace(key.into());
            value_handle.replace(value);

            let arguments = [value_handle, key_handle, this_value];
            call_object(cx, callback_function, this_arg, &arguments)?;
        }

        Ok(cx.undefined())
    }

    /// Map.prototype.get (https://tc39.es/ecma262/#sec-map.prototype.get)
    pub fn get(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let map = this_map_value(cx, this_value, "get")?;

        let key = arguments.get(cx, 0);

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        match map.map_data().get(&map_key) {
            Some(value) => Ok(value.to_handle(cx)),
            None => Ok(cx.undefined()),
        }
    }

    /// Map.prototype.getOrInsert (https://tc39.es/ecma262/#sec-map.prototype.getorinsert)
    pub fn get_or_insert(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let map = this_map_value(cx, this_value, "getOrInsert")?;

        let key = arguments.get(cx, 0);
        let value = arguments.get(cx, 1);

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        if let Some(existing_value) = map.map_data().get(&map_key) {
            Ok(existing_value.to_handle(cx))
        } else {
            map.insert(cx, key, value)?;
            Ok(value)
        }
    }

    /// Map.prototype.getOrInsertComputed (https://tc39.es/ecma262/#sec-map.prototype.getorinsertcomputed)
    pub fn get_or_insert_computed(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let map = this_map_value(cx, this_value, "getOrInsertComputed")?;

        let key = arguments.get(cx, 0);
        let callback = arguments.get(cx, 1);

        if !is_callable(callback) {
            return type_error(
                cx,
                "Map.prototype.getOrInsertComputed second argument must be a function",
            );
        }

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        if let Some(existing_value) = map.map_data().get(&map_key) {
            return Ok(existing_value.to_handle(cx));
        }

        // We don't actually perform key canonicalization when inserting into the map. The only
        // place where canonicalization is observable is the key passed to the callback.
        let canonicalized_key = canonicalize_map_key(cx, key);

        let value = call(cx, callback, cx.undefined(), &[canonicalized_key])?;

        // Write the (key, value) pair, even if the callback already inserted a value for the key
        map.insert(cx, key, value)?;

        Ok(value.to_handle(cx))
    }

    /// Map.prototype.has (https://tc39.es/ecma262/#sec-map.prototype.has)
    pub fn has(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let map = this_map_value(cx, this_value, "has")?;

        let key = arguments.get(cx, 0);

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        Ok(cx.bool(map.map_data().contains_key(&map_key)))
    }

    /// Map.prototype.keys (https://tc39.es/ecma262/#sec-map.prototype.keys)
    pub fn keys(cx: Context, this_value: Handle<Value>, _: Arguments) -> EvalResult<Handle<Value>> {
        let map = this_map_value(cx, this_value, "keys")?;

        Ok(MapIterator::new(cx, map, MapIteratorKind::Key)?.as_value())
    }

    /// Map.prototype.set (https://tc39.es/ecma262/#sec-map.prototype.set)
    pub fn set(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let map = this_map_value(cx, this_value, "set")?;

        let mut key = arguments.get(cx, 0);
        let value = arguments.get(cx, 1);

        // Convert negative zero to positive zero for key in map
        if key.is_negative_zero() {
            key = cx.zero();
        }

        map.insert(cx, key, value)?;

        Ok(this_value)
    }

    /// get Map.prototype.size (https://tc39.es/ecma262/#sec-get-map.prototype.size)
    pub fn size(cx: Context, this_value: Handle<Value>, _: Arguments) -> EvalResult<Handle<Value>> {
        let map = this_map_value(cx, this_value, "size")?;

        Ok(cx.number(map.map_data().num_entries_occupied()))
    }

    /// Map.prototype.values (https://tc39.es/ecma262/#sec-map.prototype.values)
    pub fn values(
        cx: Context,
        this_value: Handle<Value>,
        _: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let map = this_map_value(cx, this_value, "values")?;

        Ok(MapIterator::new(cx, map, MapIteratorKind::Value)?.as_value())
    }
}

fn this_map_value(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<MapObject>> {
    if value.is_object() {
        if let Some(map) = value.as_object().as_map_object() {
            return Ok(map);
        }
    }

    type_error(cx, &format!("Map.prototype.{} must be called on a Map", method_name))
}

/// Canonicalized form of each map key. Not actually stored in the map but still observable in
/// some cases.
fn canonicalize_map_key(cx: Context, key: Handle<Value>) -> Handle<Value> {
    if key.is_negative_zero() {
        cx.zero()
    } else {
        key
    }
}
