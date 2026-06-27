use crate::runtime::{
    Arguments, Context, Handle, Value,
    alloc_error::AllocResult,
    builtin_function::BuiltinFunction,
    eval::create_dynamic_function::create_dynamic_function,
    eval_result::EvalResult,
    intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction},
    object_value::ObjectValue,
    realm::Realm,
};

pub struct AsyncGeneratorFunctionConstructor;

impl AsyncGeneratorFunctionConstructor {
    /// Properties of the AsyncGeneratorFunction Constructor (https://tc39.es/ecma262/#sec-properties-of-asyncgeneratorfunction)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            RuntimeFunction::AsyncGeneratorFunctionConstructor_construct,
            1,
            cx.names.async_generator_function(),
            realm,
            Intrinsic::FunctionConstructor,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm
                .get_intrinsic(Intrinsic::AsyncGeneratorFunctionPrototype)
                .into(),
        )?;

        Ok(func)
    }

    /// AsyncGeneratorFunction (https://tc39.es/ecma262/#sec-asyncgeneratorfunction)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: Arguments,
    ) -> EvalResult<Handle<Value>> {
        let constructor = cx.current_function();
        Ok(create_dynamic_function(
            cx,
            constructor,
            cx.current_new_target(),
            arguments.as_slice(),
            /* is_async */ true,
            /* is_generator */ true,
        )?
        .as_value())
    }
}
