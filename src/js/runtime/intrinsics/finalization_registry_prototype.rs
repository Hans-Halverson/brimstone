use crate::js::runtime::{
    error::type_error, eval_result::EvalResult, function::get_argument,
    intrinsics::weak_ref_constructor::can_be_held_weakly, object_value::ObjectValue,
    property::Property, realm::Realm, type_utilities::same_value, Context, Handle, Value,
};

use super::{
    finalization_registry_object::{
        FinalizationRegistryCell, FinalizationRegistryCells, FinalizationRegistryObject,
    },
    intrinsics::Intrinsic,
};

pub struct FinalizationRegistryPrototype;

impl FinalizationRegistryPrototype {
    /// Properties of the FinalizationRegistry Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-finalization-registry-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once FinalizationRegistryConstructor has been created
        object.intrinsic_func(cx, cx.names.register(), Self::register, 2, realm);
        object.intrinsic_func(cx, cx.names.unregister(), Self::unregister, 1, realm);

        // [Symbol.toStringTag] property
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.finalization_registry().as_string().into(), false, false, true),
        );

        object
    }

    /// FinalizationRegistry.prototype.register (https://tc39.es/ecma262/#sec-finalization-registry.prototype.register)
    pub fn register(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let registry_object =
            if let Some(registry_object) = this_finalization_registry_value(this_value) {
                registry_object
            } else {
                return type_error(cx, "register method must be called on FinalizationRegistry");
            };

        let target = get_argument(cx, arguments, 0);
        let held_value = get_argument(cx, arguments, 1);
        let unregister_token = get_argument(cx, arguments, 2);

        if !can_be_held_weakly(cx, *target) {
            return type_error(cx, "FinalizationRegistry targets must be objects or symbols");
        }

        if same_value(target, held_value) {
            return type_error(
                cx,
                "The target and held value arguments to register cannot be the same value",
            );
        }

        let unregister_token = if can_be_held_weakly(cx, *unregister_token) {
            Some(unregister_token)
        } else if unregister_token.is_undefined() {
            None
        } else {
            return type_error(
                cx,
                "FinalizationRegistry unregister tokens must be objects or symbols",
            );
        };

        FinalizationRegistryCells::maybe_grow_for_insertion(cx, registry_object)
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
        let registry_object =
            if let Some(registry_object) = this_finalization_registry_value(this_value) {
                registry_object
            } else {
                return type_error(cx, "unregister method must be called on FinalizationRegistry");
            };

        let unregister_token = get_argument(cx, arguments, 0);

        if !can_be_held_weakly(cx, *unregister_token) {
            return type_error(
                cx,
                "FinalizationRegistry unregister tokens must be objects or symbols",
            );
        }

        let did_remove = registry_object.cells().remove(*unregister_token);

        Ok(cx.bool(did_remove))
    }
}

fn this_finalization_registry_value(
    value: Handle<Value>,
) -> Option<Handle<FinalizationRegistryObject>> {
    if !value.is_object() {
        return None;
    }

    value.as_object().as_finalization_registry_object()
}
