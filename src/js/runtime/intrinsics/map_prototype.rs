use crate::{
    js::runtime::{
        abstract_operations::call_object,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        gc::Gc,
        object_value::{Object, ObjectValue},
        ordinary_object::OrdinaryObject,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::is_callable,
        value::Value,
        Context,
    },
    maybe,
};

use super::{
    intrinsics::Intrinsic,
    map_constructor::MapObject,
    map_iterator::{MapIterator, MapIteratorKind},
};

pub struct MapPrototype;

impl MapPrototype {
    // 24.1.3 Properties of the Map Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Create values function as it is referenced by multiple properties
        let entries_function = BuiltinFunction::create(
            cx,
            Self::entries,
            0,
            &cx.names.entries(),
            Some(realm),
            None,
            None,
        )
        .into();

        // Constructor property is added once MapConstructor has been created
        object.intrinsic_func(cx, &cx.names.clear(), Self::clear, 0, realm);
        object.intrinsic_func(cx, &cx.names.delete(), Self::delete, 1, realm);
        object.intrinsic_data_prop(&cx.names.entries(), entries_function);
        object.intrinsic_func(cx, &cx.names.for_each(), Self::for_each, 1, realm);
        object.intrinsic_func(cx, &cx.names.get(), Self::get, 1, realm);
        object.intrinsic_func(cx, &cx.names.has(), Self::has, 1, realm);
        object.intrinsic_func(cx, &cx.names.keys(), Self::keys, 0, realm);
        object.intrinsic_func(cx, &cx.names.set_(), Self::set, 2, realm);
        object.intrinsic_getter(cx, &cx.names.size(), Self::size, realm);
        object.intrinsic_func(cx, &cx.names.values(), Self::values, 0, realm);

        // 24.1.3.12 Map.prototype [ @@iterator ]
        let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
        object.set_property(&iterator_key, Property::data(entries_function, true, false, true));

        // 24.1.3.13 Map.prototype [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        object.set_property(
            &to_string_tag_key,
            Property::data(cx.names.map().as_string().into(), false, false, true),
        );

        cx.heap.alloc(object).into()
    }

    // 24.1.3.1 Map.prototype.clear
    fn clear(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "clear method must be called on map");
        };

        map.map_data().clear();

        Value::undefined().into()
    }

    // 24.1.3.3 Map.prototype.delete
    fn delete(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "delete method must be called on map");
        };

        let key = get_argument(arguments, 0);
        let existed = map.map_data().remove(key).is_some();

        existed.into()
    }

    // 24.1.3.4 Map.prototype.entries
    fn entries(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "entries method must be called on map");
        };

        MapIterator::new(cx, map, MapIteratorKind::KeyAndValue).into()
    }

    // 24.1.3.5 Map.prototype.forEach
    fn for_each(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "forEach method must be called on map");
        };

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(arguments, 1);

        for (key, value) in map.map_data().iter() {
            let key = (*key).into();
            let value = *value;
            let arguments = [value, key, this_value];

            maybe!(call_object(cx, callback_function, this_arg, &arguments));
        }

        Value::undefined().into()
    }

    // 24.1.3.6 Map.prototype.get
    fn get(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "get method must be called on map");
        };

        let key = get_argument(arguments, 0);

        match map.map_data().get(key) {
            Some(value) => (*value).into(),
            None => Value::undefined().into(),
        }
    }

    // 24.1.3.7 Map.prototype.has
    fn has(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "has method must be called on map");
        };

        let key = get_argument(arguments, 0);

        map.map_data().contains_key(key).into()
    }

    // 24.1.3.8 Map.prototype.keys
    fn keys(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "keys method must be called on map");
        };

        MapIterator::new(cx, map, MapIteratorKind::Key).into()
    }

    // 24.1.3.9 Map.prototype.set
    fn set(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "set method must be called on map");
        };

        let mut key = get_argument(arguments, 0);
        let value = get_argument(arguments, 1);

        // Convert negative zero to positive zero for key in map
        if key.is_negative_zero() {
            key = Value::number(0.0);
        }

        map.map_data().insert(key, value);

        this_value.into()
    }

    // 24.1.3.10 get Map.prototype.size
    fn size(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "size accessor must be called on map");
        };

        Value::from_u64(map.map_data().len() as u64).into()
    }

    // 24.1.3.11 Map.prototype.values
    fn values(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let map = if let Some(map) = this_map_value(this_value) {
            map
        } else {
            return type_error_(cx, "values method must be called on map");
        };

        MapIterator::new(cx, map, MapIteratorKind::Value).into()
    }
}

fn this_map_value(value: Value) -> Option<Gc<MapObject>> {
    if !value.is_object() {
        return None;
    }

    let object = value.as_object();
    if !object.is_map_object() {
        return None;
    }

    Some(object.cast::<MapObject>())
}
