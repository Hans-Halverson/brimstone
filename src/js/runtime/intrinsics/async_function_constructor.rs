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

pub struct AsyncFunctionConstructor;

impl AsyncFunctionConstructor {
    /// Properties of the AsyncFunction Constructor (https://tc39.es/ecma262/#sec-async-function-constructor-properties)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::AsyncFunctionConstructor_construct,
            1,
            cx.names.async_function(),
            Intrinsic::FunctionConstructor,
        )?;

        builder.prototype(Intrinsic::AsyncFunctionPrototype)?;

        builder.build()
    }

    runtime_fn! {
    /// AsyncFunction (https://tc39.es/ecma262/#sec-async-function-constructor-arguments)
    fn construct(cx, _, arguments) {
        let constructor = cx.current_function();
        Ok(create_dynamic_function(
            cx,
            constructor,
            cx.current_new_target(),
            arguments.as_slice(),
            /* is_async */ true,
            /* is_generator */ false,
        )?
        .as_value())
    }}
}
