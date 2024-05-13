use crate::js::runtime::{
    collections::BsHashMapField, completion::EvalResult, error::type_error, function::get_argument,
    intrinsics::weak_ref_constructor::can_be_held_weakly, object_value::ObjectValue,
    property::Property, realm::Realm, value::ValueCollectionKey, Context, Handle, Value,
};

use super::{intrinsics::Intrinsic, weak_map_object::WeakMapObject};

pub struct WeakMapPrototype;

impl WeakMapPrototype {
    // 24.3.3 Properties of the WeakMap Prototype Object
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

    // 24.3.3.2 WeakMap.prototype.delete
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

        cx.bool(removed_value).into()
    }

    // 24.3.3.3 WeakMap.prototype.get
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
            None => cx.undefined().into(),
            Some(value) => value.to_handle(cx).into(),
        }
    }

    // 24.3.3.4 WeakMap.prototype.has
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

        cx.bool(has_key).into()
    }

    // 24.3.3.5 WeakMap.prototype.set
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

        weak_map_object
            .weak_map_data_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(ValueCollectionKey::from(key), value.get());

        this_value.into()
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
