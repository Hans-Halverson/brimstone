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

pub struct GeneratorFunctionConstructor;

impl GeneratorFunctionConstructor {
    /// Properties of the GeneratorFunction Constructor (https://tc39.es/ecma262/#sec-properties-of-the-generatorfunction-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::GeneratorFunctionConstructor_construct,
            1,
            cx.names.generator_function(),
            Intrinsic::FunctionConstructor,
        )?;

        builder.prototype(Intrinsic::GeneratorFunctionPrototype)?;

        builder.build()
    }

    runtime_fn! {
    /// GeneratorFunction (https://tc39.es/ecma262/#sec-generatorfunction)
    fn construct(cx, _, arguments) {
        let constructor = cx.current_function();
        Ok(create_dynamic_function(
            cx,
            constructor,
            cx.current_new_target(),
            arguments.as_slice(),
            /* is_async */ false,
            /* is_generator */ true,
        )?
        .as_value())
    }}
}
