use crate::runtime::{
    error::type_error, eval_result::EvalResult, function::get_argument,
    intrinsics::weak_ref_constructor::can_be_held_weakly, object_value::ObjectValue,
    property::Property, realm::Realm, value::ValueCollectionKey, Context, Handle, Value,
};

use super::{intrinsics::Intrinsic, weak_set_object::WeakSetObject};

pub struct WeakSetPrototype;

impl WeakSetPrototype {
    /// Properties of the WeakSet Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-weakset-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
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

    /// WeakSet.prototype.add (https://tc39.es/ecma262/#sec-weakset.prototype.add)
    pub fn add(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let weak_set_object = if let Some(weak_set_object) = this_weak_set_value(this_value) {
            weak_set_object
        } else {
            return type_error(cx, "add method must be called on WeakSet");
        };

        let value = get_argument(cx, arguments, 0);
        if !can_be_held_weakly(cx, *value) {
            return type_error(cx, "WeakSet only holds objects and symbols");
        }

        weak_set_object.insert(cx, value);

        Ok(this_value)
    }

    /// WeakSet.prototype.delete (https://tc39.es/ecma262/#sec-weakset.prototype.delete)
    pub fn delete(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let weak_set_object = if let Some(weak_set_object) = this_weak_set_value(this_value) {
            weak_set_object
        } else {
            return type_error(cx, "delete method must be called on WeakSet");
        };

        // Do not need to call can_be_held_weakly, instead look up directly in the value set
        let value = get_argument(cx, arguments, 0);

        let removed_value = weak_set_object
            .weak_set_data()
            .remove(&ValueCollectionKey::from(value));

        Ok(cx.bool(removed_value))
    }

    /// WeakSet.prototype.has (https://tc39.es/ecma262/#sec-weakset.prototype.has)
    pub fn has(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let weak_set_object = if let Some(weak_set_object) = this_weak_set_value(this_value) {
            weak_set_object
        } else {
            return type_error(cx, "has method must be called on WeakSet");
        };

        // Do not need to call can_be_held_weakly, instead look up directly in the value set
        let value = get_argument(cx, arguments, 0);

        let has_value = weak_set_object
            .weak_set_data()
            .contains(&ValueCollectionKey::from(value));

        Ok(cx.bool(has_value))
    }
}

fn this_weak_set_value(value: Handle<Value>) -> Option<Handle<WeakSetObject>> {
    if !value.is_object() {
        return None;
    }

    value.as_object().as_weak_set_object()
}
