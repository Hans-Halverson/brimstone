use crate::js::runtime::{
    collections::BsHashSetField, completion::EvalResult, error::type_error_,
    function::get_argument, intrinsics::weak_ref_constructor::can_be_held_weakly,
    object_value::ObjectValue, property::Property, realm::Realm, value::ValueCollectionKey,
    Context, Handle, Value,
};

use super::{intrinsics::Intrinsic, weak_set_object::WeakSetObject};

pub struct WeakSetPrototype;

impl WeakSetPrototype {
    // 24.4.3 Properties of the WeakSet Prototype Object
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once WeakSetConstructor has been created
        object.intrinsic_func(cx, cx.names.add(), Self::add, 1, realm);
        object.intrinsic_func(cx, cx.names.delete(), Self::delete, 1, realm);
        object.intrinsic_func(cx, cx.names.has(), Self::has, 1, realm);

        // [Symbol.toStringTag] property
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.weak_set().as_string().into(), false, false, true),
        );

        object
    }

    // 24.4.3.1 WeakSet.prototype.add
    fn add(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let weak_set_object = if let Some(weak_set_object) = this_weak_set_value(this_value) {
            weak_set_object
        } else {
            return type_error_(cx, "add method must be called on WeakSet");
        };

        let value = get_argument(cx, arguments, 0);
        if !can_be_held_weakly(cx, value.get()) {
            return type_error_(cx, "WeakSet only holds objects and symbols");
        }

        weak_set_object
            .weak_set_data_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(ValueCollectionKey::from(value));

        this_value.into()
    }

    // 24.4.3.3 WeakSet.prototype.delete
    fn delete(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let weak_set_object = if let Some(weak_set_object) = this_weak_set_value(this_value) {
            weak_set_object
        } else {
            return type_error_(cx, "delete method must be called on WeakSet");
        };

        // Do not need to call can_be_held_weakly, instead look up directly in the value set
        let value = get_argument(cx, arguments, 0);

        let removed_value = weak_set_object
            .weak_set_data()
            .remove(&ValueCollectionKey::from(value));

        cx.bool(removed_value).into()
    }

    // 24.4.3.4 WeakSet.prototype.has
    fn has(
        cx: &mut Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let weak_set_object = if let Some(weak_set_object) = this_weak_set_value(this_value) {
            weak_set_object
        } else {
            return type_error_(cx, "has method must be called on WeakSet");
        };

        // Do not need to call can_be_held_weakly, instead look up directly in the value set
        let value = get_argument(cx, arguments, 0);

        let has_value = weak_set_object
            .weak_set_data()
            .contains(&ValueCollectionKey::from(value));

        cx.bool(has_value).into()
    }
}

fn this_weak_set_value(value: Handle<Value>) -> Option<Handle<WeakSetObject>> {
    if !value.is_object() {
        return None;
    }

    let object = value.as_object();
    if !object.is_weak_set_object() {
        return None;
    }

    Some(object.cast::<WeakSetObject>())
}
