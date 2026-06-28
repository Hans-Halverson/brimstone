use crate::{
    runtime::{
        Context, Handle,
        alloc_error::AllocResult,
        eval::create_dynamic_function::create_dynamic_function,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction},
        object_value::ObjectValue,
        realm::Realm,
    },
    runtime_fn,
};

pub struct AsyncGeneratorFunctionConstructor;

impl AsyncGeneratorFunctionConstructor {
    /// Properties of the AsyncGeneratorFunction Constructor (https://tc39.es/ecma262/#sec-properties-of-asyncgeneratorfunction)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::AsyncGeneratorFunctionConstructor_construct,
            1,
            cx.names.async_generator_function(),
            Intrinsic::FunctionConstructor,
        )?;

        builder.prototype(Intrinsic::AsyncGeneratorFunctionPrototype)?;

        builder.build()
    }

    runtime_fn! {
    /// AsyncGeneratorFunction (https://tc39.es/ecma262/#sec-asyncgeneratorfunction)
    fn construct(cx, _, arguments) {
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
    }}
}
