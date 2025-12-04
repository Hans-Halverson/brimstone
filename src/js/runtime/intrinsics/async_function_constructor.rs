use crate::runtime::{
    alloc_error::AllocResult, builtin_function::BuiltinFunction,
    eval::create_dynamic_function::create_dynamic_function, eval_result::EvalResult,
    object_value::ObjectValue, realm::Realm, Context, Handle, Value,
};

use super::intrinsics::Intrinsic;

pub struct AsyncFunctionConstructor;

impl AsyncFunctionConstructor {
    /// Properties of the AsyncFunction Constructor (https://tc39.es/ecma262/#sec-async-function-constructor-properties)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.async_function(),
            realm,
            Intrinsic::FunctionConstructor,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm
                .get_intrinsic(Intrinsic::AsyncFunctionPrototype)
                .into(),
        )?;

        Ok(func)
    }

    /// AsyncFunction (https://tc39.es/ecma262/#sec-async-function-constructor-arguments)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let constructor = cx.current_function();
        Ok(create_dynamic_function(
            cx,
            constructor,
            cx.current_new_target(),
            arguments,
            /* is_async */ true,
            /* is_generator */ false,
        )?
        .as_value())
    }
}
