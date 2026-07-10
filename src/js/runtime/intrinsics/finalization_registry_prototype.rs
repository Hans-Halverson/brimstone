use crate::{
    intrinsic_methods,
    runtime::{
        Context, Handle, Value,
        alloc_error::AllocResult,
        error::type_error,
        eval_result::EvalResult,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            finalization_registry_object::{
                FinalizationRegistryCell, FinalizationRegistryCells, FinalizationRegistryObject,
            },
            intrinsics::Intrinsic,
            weak_ref_constructor::can_be_held_weakly,
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::same_value,
    },
    runtime_fn,
};

pub struct FinalizationRegistryPrototype;

impl FinalizationRegistryPrototype {
    /// Properties of the FinalizationRegistry Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-finalization-registry-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::new_object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once FinalizationRegistryConstructor has been created
        intrinsic_methods!(cx, builder, {
            register   FinalizationRegistryPrototype_register   (2),
            unregister FinalizationRegistryPrototype_unregister (1),
        });

        // FinalizationRegistry.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-finalization-registry.prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.finalization_registry())?;

        builder.build()
    }

    runtime_fn! {
    /// FinalizationRegistry.prototype.register (https://tc39.es/ecma262/#sec-finalization-registry.prototype.register)
    fn register(cx, this_value, arguments) {
        let registry_object = this_finalization_registry_value(cx, this_value, "register")?;

        let target = arguments.get(cx, 0);
        let held_value = arguments.get(cx, 1);
        let unregister_token = arguments.get(cx, 2);

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
    }}

    runtime_fn! {
    /// FinalizationRegistry.prototype.unregister (https://tc39.es/ecma262/#sec-finalization-registry.prototype.unregister)
    fn unregister(cx, this_value, arguments) {
        let registry_object = this_finalization_registry_value(cx, this_value, "unregister")?;

        let unregister_token = arguments.get(cx, 0);

        if !can_be_held_weakly(cx, *unregister_token) {
            return type_error(
                cx,
                "FinalizationRegistry.prototype.unregister token must be an object or symbol",
            );
        }

        let did_remove = registry_object.cells().remove(*unregister_token);

        Ok(cx.bool(did_remove))
    }}
}

fn this_finalization_registry_value(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<FinalizationRegistryObject>> {
    if value.is_object() {
        if let Some(registry_object) = value.as_opt::<FinalizationRegistryObject>() {
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
