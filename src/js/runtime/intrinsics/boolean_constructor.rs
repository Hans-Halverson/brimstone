use crate::{
    runtime::{
        Context,
        alloc_error::AllocResult,
        gc::Handle,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            boolean_object::BooleanObject, intrinsics::Intrinsic, rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::to_boolean,
    },
    runtime_fn,
};

pub struct BooleanConstructor;

impl BooleanConstructor {
    /// Properties of the Boolean Constructor (https://tc39.es/ecma262/#sec-properties-of-the-boolean-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::BooleanConstructor_construct,
            1,
            cx.names.boolean(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::BooleanPrototype)?;

        builder.build()
    }

    runtime_fn! {
    /// Boolean (https://tc39.es/ecma262/#sec-boolean-constructor-boolean-value)
    fn construct(cx, _, arguments) {
        let bool_value = to_boolean(*arguments.get(cx, 0));

        match cx.current_new_target() {
            None => Ok(cx.bool(bool_value)),
            Some(new_target) => {
                Ok(BooleanObject::new_from_constructor(cx, new_target, bool_value)?.as_value())
            }
        }
    }}
}
