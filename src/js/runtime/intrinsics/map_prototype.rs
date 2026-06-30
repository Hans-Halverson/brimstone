use crate::{
    intrinsic_methods,
    runtime::{
        Context, Handle,
        abstract_operations::{call, call_object},
        alloc_error::AllocResult,
        error::type_error,
        eval_result::EvalResult,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            map_iterator_object::{MapIteratorKind, MapIteratorObject},
            map_object::MapObject,
            rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::is_callable,
        value::{Value, ValueCollectionKey},
    },
    runtime_fn,
};

pub struct MapPrototype;

impl MapPrototype {
    /// Properties of the Map Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-map-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once MapConstructor has been created
        intrinsic_methods!(cx, builder, {
            clear                  MapPrototype_clear                  (0),
            delete                 MapPrototype_delete                 (1),
            entries                MapPrototype_entries                (0),
            for_each               MapPrototype_for_each               (1),
            get                    MapPrototype_get                    (1),
            get_or_insert          MapPrototype_get_or_insert          (2),
            get_or_insert_computed MapPrototype_get_or_insert_computed (2),
            has                    MapPrototype_has                    (1),
            keys                   MapPrototype_keys                   (0),
            set_                   MapPrototype_set                    (2),
            values                 MapPrototype_values                 (0),
        });

        // Map.prototype [ @@iterator ] (https://tc39.es/ecma262/#sec-map.prototype-%symbol.iterator%)
        builder.alias(cx.names.entries(), cx.symbols.iterator())?;

        // get Map.prototype.size (https://tc39.es/ecma262/#sec-get-map.prototype.size)
        builder.getter(cx.names.size(), RuntimeFunction::MapPrototype_size)?;

        // Map.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-map.prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.map())?;

        builder.build()
    }

    runtime_fn! {
    /// Map.prototype.clear (https://tc39.es/ecma262/#sec-map.prototype.clear)
    fn clear(cx, this_value, _) {
        let map = this_map_value(cx, this_value, "clear")?;

        map.map_data().clear();

        Ok(cx.undefined())
    }}

    runtime_fn! {
    /// Map.prototype.delete (https://tc39.es/ecma262/#sec-map.prototype.delete)
    fn delete(cx, this_value, arguments) {
        let map = this_map_value(cx, this_value, "delete")?;

        let key = arguments.get(cx, 0);

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        let existed = map.map_data().remove(&map_key);

        Ok(cx.bool(existed))
    }}

    runtime_fn! {
    /// Map.prototype.entries (https://tc39.es/ecma262/#sec-map.prototype.entries)
    fn entries(cx, this_value, _) {
        let map = this_map_value(cx, this_value, "entries")?;

        Ok(MapIteratorObject::new(cx, map, MapIteratorKind::KeyAndValue)?.as_value())
    }}

    runtime_fn! {
    /// Map.prototype.forEach (https://tc39.es/ecma262/#sec-map.prototype.foreach)
    fn for_each(cx, this_value, arguments) {
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
        for (key, value) in map.map_data_inner().iter_gc_safe() {
            key_handle.replace(key.into());
            value_handle.replace(value);

            let arguments = [value_handle, key_handle, this_value];
            call_object(cx, callback_function, this_arg, &arguments)?;
        }

        Ok(cx.undefined())
    }}

    runtime_fn! {
    /// Map.prototype.get (https://tc39.es/ecma262/#sec-map.prototype.get)
    fn get(cx, this_value, arguments) {
        let map = this_map_value(cx, this_value, "get")?;

        let key = arguments.get(cx, 0);

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        match map.map_data().get(&map_key) {
            Some(value) => Ok(value.to_handle(cx)),
            None => Ok(cx.undefined()),
        }
    }}

    runtime_fn! {
    /// Map.prototype.getOrInsert (https://tc39.es/ecma262/#sec-map.prototype.getorinsert)
    fn get_or_insert(cx, this_value, arguments) {
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
    }}

    runtime_fn! {
    /// Map.prototype.getOrInsertComputed (https://tc39.es/ecma262/#sec-map.prototype.getorinsertcomputed)
    fn get_or_insert_computed(cx, this_value, arguments) {
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
    }}

    runtime_fn! {
    /// Map.prototype.has (https://tc39.es/ecma262/#sec-map.prototype.has)
    fn has(cx, this_value, arguments) {
        let map = this_map_value(cx, this_value, "has")?;

        let key = arguments.get(cx, 0);

        // May allocate
        let map_key = ValueCollectionKey::from(key)?;

        Ok(cx.bool(map.map_data().contains_key(&map_key)))
    }}

    runtime_fn! {
    /// Map.prototype.keys (https://tc39.es/ecma262/#sec-map.prototype.keys)
    fn keys(cx, this_value, _) {
        let map = this_map_value(cx, this_value, "keys")?;

        Ok(MapIteratorObject::new(cx, map, MapIteratorKind::Key)?.as_value())
    }}

    runtime_fn! {
    /// Map.prototype.set (https://tc39.es/ecma262/#sec-map.prototype.set)
    fn set(cx, this_value, arguments) {
        let map = this_map_value(cx, this_value, "set")?;

        let mut key = arguments.get(cx, 0);
        let value = arguments.get(cx, 1);

        // Convert negative zero to positive zero for key in map
        if key.is_negative_zero() {
            key = cx.zero();
        }

        map.insert(cx, key, value)?;

        Ok(this_value)
    }}

    runtime_fn! {
    /// get Map.prototype.size (https://tc39.es/ecma262/#sec-get-map.prototype.size)
    fn size(cx, this_value, _) {
        let map = this_map_value(cx, this_value, "size")?;

        Ok(cx.number(map.map_data().num_entries_occupied()))
    }}

    runtime_fn! {
    /// Map.prototype.values (https://tc39.es/ecma262/#sec-map.prototype.values)
    fn values(cx, this_value, _) {
        let map = this_map_value(cx, this_value, "values")?;

        Ok(MapIteratorObject::new(cx, map, MapIteratorKind::Value)?.as_value())
    }}
}

fn this_map_value(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<MapObject>> {
    if value.is_object() {
        if let Some(map) = value.as_opt::<MapObject>() {
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
