use crate::{
    js::runtime::{
        builtin_function::BuiltinFunction, completion::EvalResult, error::type_error,
        function::get_argument, object_value::ObjectValue, realm::Realm,
        type_utilities::is_callable, Context, Handle, Value,
    },
    maybe,
};

use super::{finalization_registry_object::FinalizationRegistryObject, intrinsics::Intrinsic};

pub struct FinalizationRegistryConstructor;

impl FinalizationRegistryConstructor {
    // 26.2.2 Properties of the FinalizationRegistry Constructor
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

    // 26.2.1.1 FinalizationRegistry
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return type_error(cx, "FinalizationRegistry constructor must be called with new");
        };

        let cleanup_callback = get_argument(cx, arguments, 0);
        if !is_callable(cleanup_callback) {
            return type_error(cx, "FinalizationRegistry cleanup callback is not a function");
        }

        maybe!(FinalizationRegistryObject::new_from_constructor(
            cx,
            new_target,
            cleanup_callback.as_object(),
        ))
        .into()
    }
}
