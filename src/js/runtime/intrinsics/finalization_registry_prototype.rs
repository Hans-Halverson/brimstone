use crate::runtime::{
    Context, Handle, Value,
    alloc_error::AllocResult,
    error::type_error,
    eval_result::EvalResult,
    function::get_argument,
    intrinsics::{
        finalization_registry_object::{
            FinalizationRegistryCell, FinalizationRegistryCells, FinalizationRegistryObject,
        },
        intrinsics::Intrinsic,
        rust_runtime::RuntimeFunction,
        weak_ref_constructor::can_be_held_weakly,
    },
    object_value::ObjectValue,
    property::Property,
    realm::Realm,
    type_utilities::same_value,
};

pub struct FinalizationRegistryPrototype;

impl FinalizationRegistryPrototype {
    /// Properties of the FinalizationRegistry Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-finalization-registry-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once FinalizationRegistryConstructor has been created
        object.intrinsic_func(
            cx,
            cx.names.register(),
            RuntimeFunction::FinalizationRegistryPrototype_register,
            2,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.unregister(),
            RuntimeFunction::FinalizationRegistryPrototype_unregister,
            1,
            realm,
        )?;

        // [Symbol.toStringTag] property
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.finalization_registry().as_string().into(), false, false, true),
        )?;

        Ok(object)
    }

    /// FinalizationRegistry.prototype.register (https://tc39.es/ecma262/#sec-finalization-registry.prototype.register)
    pub fn register(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let registry_object = this_finalization_registry_value(cx, this_value, "register")?;

        let target = get_argument(cx, arguments, 0);
        let held_value = get_argument(cx, arguments, 1);
        let unregister_token = get_argument(cx, arguments, 2);

        if !can_be_held_weakly(cx, *target) {
            return type_error(
                cx,
                "FinalizationRegistry.prototype.register target must be an object or symbol",
            );
        }

        if same_value(target, held_value)? {
            return type_error(
                cx,
                "FinalizationRegistry.prototype.register target and held value cannot be the same",
            );
        }

        let unregister_token = if can_be_held_weakly(cx, *unregister_token) {
            Some(unregister_token)
        } else if unregister_token.is_undefined() {
            None
        } else {
            return type_error(
                cx,
                "FinalizationRegistry.prototype.register unregister token must be an object or symbol",
            );
        };

        FinalizationRegistryCells::maybe_grow_for_insertion(cx, registry_object)?
            .insert_without_growing(FinalizationRegistryCell {
                target: *target,
                held_value: *held_value,
                unregister_token: unregister_token.map(|t| *t),
            });

        Ok(cx.undefined())
    }

    /// FinalizationRegistry.prototype.unregister (https://tc39.es/ecma262/#sec-finalization-registry.prototype.unregister)
    pub fn unregister(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let registry_object = this_finalization_registry_value(cx, this_value, "unregister")?;

        let unregister_token = get_argument(cx, arguments, 0);

        if !can_be_held_weakly(cx, *unregister_token) {
            return type_error(
                cx,
                "FinalizationRegistry.prototype.unregister token must be an object or symbol",
            );
        }

        let did_remove = registry_object.cells().remove(*unregister_token);

        Ok(cx.bool(did_remove))
    }
}

fn this_finalization_registry_value(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<FinalizationRegistryObject>> {
    if value.is_object() {
        if let Some(registry_object) = value.as_object().as_finalization_registry_object() {
            return Ok(registry_object);
        }
    }

    type_error(
        cx,
        &format!(
            "FinalizationRegistry.prototype.{} must be called on a FinalizationRegistry",
            method_name
        ),
    )
}
