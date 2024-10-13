use crate::js::runtime::{
    completion::EvalResult, error::type_error, function::get_argument,
    intrinsics::weak_ref_constructor::can_be_held_weakly, object_value::ObjectValue,
    property::Property, realm::Realm, value::ValueCollectionKey, Context, Handle, Value,
};

use super::{intrinsics::Intrinsic, weak_map_object::WeakMapObject};

pub struct WeakMapPrototype;

impl WeakMapPrototype {
    /// Properties of the WeakMap Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-weakmap-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once WeakMapConstructor has been created
        object.intrinsic_func(cx, cx.names.delete(), Self::delete, 1, realm);
        object.intrinsic_func(cx, cx.names.get(), Self::get, 1, realm);
        object.intrinsic_func(cx, cx.names.has(), Self::has, 1, realm);
        object.intrinsic_func(cx, cx.names.set_(), Self::set, 2, realm);

        // [Symbol.toStringTag] property
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.weak_map().as_string().into(), false, false, true),
        );

        object
    }

    /// WeakMap.prototype.delete (https://tc39.es/ecma262/#sec-weakmap.prototype.delete)
    pub fn delete(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let weak_map_object = if let Some(weak_map_object) = this_weak_map_value(this_value) {
            weak_map_object
        } else {
            return type_error(cx, "delete method must be called on WeakMap");
        };

        // Do not need to call can_be_held_weakly, instead look up directly in the value map
        let value = get_argument(cx, arguments, 0);

        let removed_value = weak_map_object
            .weak_map_data()
            .remove(&ValueCollectionKey::from(value));

        Ok(cx.bool(removed_value))
    }

    /// WeakMap.prototype.get (https://tc39.es/ecma262/#sec-weakmap.prototype.get)
    pub fn get(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let weak_map_object = if let Some(weak_map_object) = this_weak_map_value(this_value) {
            weak_map_object
        } else {
            return type_error(cx, "get method must be called on WeakMap");
        };

        // Do not need to call can_be_held_weakly, instead look up directly in the value map
        let key = get_argument(cx, arguments, 0);

        let weak_map_data = weak_map_object.weak_map_data();
        let value_opt = weak_map_data.get(&ValueCollectionKey::from(key));

        match value_opt {
            None => Ok(cx.undefined()),
            Some(value) => Ok(value.to_handle(cx)),
        }
    }

    /// WeakMap.prototype.has (https://tc39.es/ecma262/#sec-weakmap.prototype.has)
    pub fn has(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let weak_map_object = if let Some(weak_map_object) = this_weak_map_value(this_value) {
            weak_map_object
        } else {
            return type_error(cx, "has method must be called on WeakMap");
        };

        // Do not need to call can_be_held_weakly, instead look up directly in the value map
        let key = get_argument(cx, arguments, 0);

        let has_key = weak_map_object
            .weak_map_data()
            .contains_key(&ValueCollectionKey::from(key));

        Ok(cx.bool(has_key))
    }

    /// WeakMap.prototype.set (https://tc39.es/ecma262/#sec-weakmap.prototype.set)
    pub fn set(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let weak_map_object = if let Some(weak_map_object) = this_weak_map_value(this_value) {
            weak_map_object
        } else {
            return type_error(cx, "set method must be called on WeakMap");
        };

        let key = get_argument(cx, arguments, 0);
        let value = get_argument(cx, arguments, 1);

        if !can_be_held_weakly(cx, key.get()) {
            return type_error(cx, "WeakMap keys must be objects or symbols");
        }

        weak_map_object.insert(cx, key, value);

        Ok(this_value)
    }
}

fn this_weak_map_value(value: Handle<Value>) -> Option<Handle<WeakMapObject>> {
    if !value.is_object() {
        return None;
    }

    let object = value.as_object();
    if !object.is_weak_map_object() {
        return None;
    }

    Some(object.cast::<WeakMapObject>())
}
