use crate::runtime::{
    builtin_function::BuiltinFunction, error::type_error, eval_result::EvalResult,
    function::get_argument, object_value::ObjectValue, realm::Realm, type_utilities::is_callable,
    Context, Handle, Value,
};

use super::{finalization_registry_object::FinalizationRegistryObject, intrinsics::Intrinsic};

pub struct FinalizationRegistryConstructor;

impl FinalizationRegistryConstructor {
    /// Properties of the FinalizationRegistry Constructor (https://tc39.es/ecma262/#sec-properties-of-the-finalization-registry-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.finalization_registry(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm
                .get_intrinsic(Intrinsic::FinalizationRegistryPrototype)
                .into(),
        );

        func
    }

    /// FinalizationRegistry (https://tc39.es/ecma262/#sec-finalization-registry-cleanup-callback)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return type_error(cx, "FinalizationRegistry constructor must be called with new");
        };

        let cleanup_callback = get_argument(cx, arguments, 0);
        if !is_callable(cleanup_callback) {
            return type_error(cx, "FinalizationRegistry cleanup callback is not a function");
        }

        Ok(FinalizationRegistryObject::new_from_constructor(
            cx,
            new_target,
            cleanup_callback.as_object(),
        )?
        .as_value())
    }
}
