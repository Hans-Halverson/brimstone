use crate::{
    runtime::{
        Context, Handle,
        alloc_error::AllocResult,
        error::type_error,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            finalization_registry_object::FinalizationRegistryObject, intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::is_callable,
    },
    runtime_fn,
};

pub struct FinalizationRegistryConstructor;

impl FinalizationRegistryConstructor {
    /// Properties of the FinalizationRegistry Constructor (https://tc39.es/ecma262/#sec-properties-of-the-finalization-registry-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::FinalizationRegistryConstructor_construct,
            1,
            cx.names.finalization_registry(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::FinalizationRegistryPrototype)?;

        builder.build()
    }

    runtime_fn! {
    /// FinalizationRegistry (https://tc39.es/ecma262/#sec-finalization-registry-cleanup-callback)
    fn construct(cx, _, arguments) {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return type_error(cx, "FinalizationRegistry constructor must be called with new");
        };

        let cleanup_callback = arguments.get(cx, 0);
        if !is_callable(cleanup_callback) {
            return type_error(cx, "FinalizationRegistry constructor argument must be a function");
        }

        Ok(FinalizationRegistryObject::new_from_constructor(
            cx,
            new_target,
            cleanup_callback.as_object(),
        )?
        .as_value())
    }}
}
