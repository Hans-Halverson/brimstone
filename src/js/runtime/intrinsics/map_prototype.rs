use crate::{
    js::runtime::{
        abstract_operations::call_object,
        builtin_function::BuiltinFunction,
        collections::BsIndexMapField,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        object_value::ObjectValue,
        property::Property,
        realm::Realm,
        type_utilities::is_callable,
        value::{Value, ValueCollectionKey},
        Context, Handle,
    },
    maybe,
};

use super::{
    intrinsics::Intrinsic,
    map_iterator::{MapIterator, MapIteratorKind},
    map_object::MapObject,
};

pub struct MapPrototype;

impl MapPrototype {
    // 24.1.3 Properties of the Map Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Create values function as it is referenced by multiple properties
        let entries_function =
            BuiltinFunction::create(cx, Self::entries, 0, cx.names.entries(), realm, None, None)
                .into();

        // Constructor property is added once MapConstructor has been created
        object.intrinsic_func(cx, cx.names.clear(), Self::clear, 0, realm);
        object.intrinsic_func(cx, cx.names.delete(), Self::delete, 1, realm);
        object.intrinsic_data_prop(cx, cx.names.entries(), entries_function);
        object.intrinsic_func(cx, cx.names.for_each(), Self::for_each, 1, realm);
        object.intrinsic_func(cx, cx.names.get(), Self::get, 1, realm);
        object.intrinsic_func(cx, cx.names.has(), Self::has, 1, realm);
        object.intrinsic_func(cx, cx.names.keys(), Self::keys, 0, realm);
        object.intrinsic_func(cx, cx.names.set_(), Self::set, 2, realm);
        object.intrinsic_getter(cx, cx.names.size(), Self::size, realm);
        object.intrinsic_func(cx, cx.names.values(), Self::values, 0, realm);

        // 24.1.3.12 Map.prototype [ @@iterator ]
        let iterator_key = cx.well_known_symbols.iterator();
        object.set_property(cx, iterator_key, Property::data(entries_function, true, false, true));

        // 24.1.3.13 Map.prototype [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.map().as_string().into(), false, false, true),
        );

        object
    }

    // 24.1.3.1 Map.prototype.clear
    pub fn clear(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "clear method must be called on map");
        };

        map.map_data().clear();

        cx.undefined().into()
    }

    // 24.1.3.3 Map.prototype.delete
    pub fn delete(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "delete method must be called on map");
        };

        let key = get_argument(cx, arguments, 0);
        let existed = map.map_data().remove(&ValueCollectionKey::from(key));

        cx.bool(existed).into()
    }

    // 24.1.3.4 Map.prototype.entries
    pub fn entries(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "entries method must be called on map");
        };

        MapIterator::new(cx, map, MapIteratorKind::KeyAndValue).into()
    }

    // 24.1.3.5 Map.prototype.forEach
    pub fn for_each(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "forEach method must be called on map");
        };

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Share key and value handles during iteration
        let mut key_handle = Handle::<Value>::empty(cx);
        let mut value_handle = Handle::<Value>::empty(cx);

        // Must use gc and invalidation safe iteration since arbitrary code can be executed between
        // iterations.
        for (key, value) in map.map_data().to_handle().iter_gc_safe() {
            key_handle.replace(key.into());
            value_handle.replace(value);

            let arguments = [value_handle, key_handle, this_value];
            maybe!(call_object(cx, callback_function, this_arg, &arguments));
        }

        cx.undefined().into()
    }

    // 24.1.3.6 Map.prototype.get
    pub fn get(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "get method must be called on map");
        };

        let key = get_argument(cx, arguments, 0);

        match map.map_data().get(&ValueCollectionKey::from(key)) {
            Some(value) => value.to_handle(cx).into(),
            None => cx.undefined().into(),
        }
    }

    // 24.1.3.7 Map.prototype.has
    pub fn has(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "has method must be called on map");
        };

        let key = get_argument(cx, arguments, 0);

        cx.bool(map.map_data().contains_key(&ValueCollectionKey::from(key)))
            .into()
    }

    // 24.1.3.8 Map.prototype.keys
    pub fn keys(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "keys method must be called on map");
        };

        MapIterator::new(cx, map, MapIteratorKind::Key).into()
    }

    // 24.1.3.9 Map.prototype.set
    pub fn set(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "set method must be called on map");
        };

        let mut key = get_argument(cx, arguments, 0);
        let value = get_argument(cx, arguments, 1);

        // Convert negative zero to positive zero for key in map
        if key.is_negative_zero() {
            key = Value::number(0.0).to_handle(cx);
        }

        map.map_data_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(ValueCollectionKey::from(key), value.get());

        this_value.into()
    }

    // 24.1.3.10 get Map.prototype.size
    pub fn size(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "size accessor must be called on map");
        };

        Value::from(map.map_data().num_entries_occupied())
            .to_handle(cx)
            .into()
    }

    // 24.1.3.11 Map.prototype.values
    pub fn values(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "values method must be called on map");
        };

        MapIterator::new(cx, map, MapIteratorKind::Value).into()
    }
}

fn this_map_value(value: Handle<Value>) -> Option<Handle<MapObject>> {
    if !value.is_object() {
        return None;
    }

    let object = value.as_object();
    if !object.is_map_object() {
        return None;
    }

    Some(object.cast::<MapObject>())
}
