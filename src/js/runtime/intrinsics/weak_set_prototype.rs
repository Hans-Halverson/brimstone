use crate::runtime::{
    Arguments, Context, Handle, Value,
    alloc_error::AllocResult,
    error::type_error,
    eval_result::EvalResult,
    intrinsics::{
        intrinsics::Intrinsic, rust_runtime::RuntimeFunction,
        weak_ref_constructor::can_be_held_weakly, weak_set_object::WeakSetObject,
    },
    object_value::ObjectValue,
    property::Property,
    realm::Realm,
    value::ValueCollectionKey,
};

pub struct WeakSetPrototype;

impl WeakSetPrototype {
    /// Properties of the WeakSet Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-weakset-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once WeakSetConstructor has been created
        object.intrinsic_func(
            cx,
            cx.names.add(),
            RuntimeFunction::WeakSetPrototype_add,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.delete(),
            RuntimeFunction::WeakSetPrototype_delete,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.has(),
            RuntimeFunction::WeakSetPrototype_has,
            1,
            realm,
        )?;

        // [Symbol.toStringTag] property
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.weak_set().as_string().into(), false, false, true),
        )?;

        Ok(object)
    }

    /// WeakSet.prototype.add (https://tc39.es/ecma262/#sec-weakset.prototype.add)
    pub fn add(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let weak_set_object = this_weak_set_value(cx, this_value, "add")?;

        let value = arguments.get(cx, 0);
        if !can_be_held_weakly(cx, *value) {
            return type_error(
                cx,
                "WeakSet.prototype.add argument must be an object or an unregistered symbol",
            );
        }

        weak_set_object.insert(cx, value)?;

        Ok(this_value)
    }

    /// WeakSet.prototype.delete (https://tc39.es/ecma262/#sec-weakset.prototype.delete)
    pub fn delete(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let weak_set_object = this_weak_set_value(cx, this_value, "delete")?;

        // Do not need to call can_be_held_weakly, instead look up directly in the value set
        let value = arguments.get(cx, 0);

        // May allocate
        let set_key = ValueCollectionKey::from(value)?;

        let removed_value = weak_set_object.weak_set_data().remove(&set_key);

        Ok(cx.bool(removed_value))
    }

    /// WeakSet.prototype.has (https://tc39.es/ecma262/#sec-weakset.prototype.has)
    pub fn has(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let weak_set_object = this_weak_set_value(cx, this_value, "has")?;

        // Do not need to call can_be_held_weakly, instead look up directly in the value set
        let value = arguments.get(cx, 0);

        // May allocate
        let set_key = ValueCollectionKey::from(value)?;

        let has_value = weak_set_object.weak_set_data().contains(&set_key);

        Ok(cx.bool(has_value))
    }
}

fn this_weak_set_value(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<WeakSetObject>> {
    if value.is_object() {
        if let Some(weak_set_object) = value.as_object().as_weak_set_object() {
            return Ok(weak_set_object);
        }
    }

    type_error(cx, &format!("WeakSet.prototype.{} must be called on a WeakSet", method_name))
}
