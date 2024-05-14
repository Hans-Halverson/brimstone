use crate::{
    js::runtime::{
        builtin_function::BuiltinFunction, completion::EvalResult,
        eval::create_dynamic_function::create_dynamic_function, object_value::ObjectValue,
        realm::Realm, Context, Handle, Value,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

pub struct AsyncFunctionConstructor;

impl AsyncFunctionConstructor {
    // 27.7.2 Properties of the AsyncFunction Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let proto = realm.get_intrinsic(Intrinsic::FunctionConstructor);
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.async_function(),
            realm,
            Some(proto),
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm
                .get_intrinsic(Intrinsic::AsyncFunctionPrototype)
                .into(),
        );

        func
    }

    // 27.7.1.1 AsyncFunction
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let constructor = cx.current_function();
        maybe!(create_dynamic_function(
            cx,
            constructor,
            new_target,
            arguments,
            /* is_async */ true,
            /* is_generator */ false,
        ))
        .into()
    }
}
