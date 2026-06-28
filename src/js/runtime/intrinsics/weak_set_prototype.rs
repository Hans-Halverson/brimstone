use crate::{
    intrinsic_methods,
    runtime::{
        Context, Handle, Value,
        alloc_error::AllocResult,
        error::type_error,
        eval_result::EvalResult,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic, weak_ref_constructor::can_be_held_weakly,
            weak_set_object::WeakSetObject,
        },
        object_value::ObjectValue,
        realm::Realm,
        value::ValueCollectionKey,
    },
    runtime_fn,
};

pub struct WeakSetPrototype;

impl WeakSetPrototype {
    /// Properties of the WeakSet Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-weakset-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once WeakSetConstructor has been created
        intrinsic_methods!(cx, builder, {
            add    WeakSetPrototype_add    (1),
            delete WeakSetPrototype_delete (1),
            has    WeakSetPrototype_has    (1),
        });

        // WeakSet.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-weakset.prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.weak_set())?;

        builder.build()
    }

    runtime_fn! {
    /// WeakSet.prototype.add (https://tc39.es/ecma262/#sec-weakset.prototype.add)
    fn add(cx, this_value, arguments) {
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
    }}

    runtime_fn! {
    /// WeakSet.prototype.delete (https://tc39.es/ecma262/#sec-weakset.prototype.delete)
    fn delete(cx, this_value, arguments) {
        let weak_set_object = this_weak_set_value(cx, this_value, "delete")?;

        // Do not need to call can_be_held_weakly, instead look up directly in the value set
        let value = arguments.get(cx, 0);

        // May allocate
        let set_key = ValueCollectionKey::from(value)?;

        let removed_value = weak_set_object.weak_set_data().remove(&set_key);

        Ok(cx.bool(removed_value))
    }}

    runtime_fn! {
    /// WeakSet.prototype.has (https://tc39.es/ecma262/#sec-weakset.prototype.has)
    fn has(cx, this_value, arguments) {
        let weak_set_object = this_weak_set_value(cx, this_value, "has")?;

        // Do not need to call can_be_held_weakly, instead look up directly in the value set
        let value = arguments.get(cx, 0);

        // May allocate
        let set_key = ValueCollectionKey::from(value)?;

        let has_value = weak_set_object.weak_set_data().contains(&set_key);

        Ok(cx.bool(has_value))
    }}
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
