use crate::{
    runtime::{
        Context, Handle,
        alloc_error::AllocResult,
        error::type_error,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            array_buffer_constructor::get_array_buffer_max_byte_length_option,
            array_buffer_object::ArrayBufferObject, intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::to_index,
    },
    runtime_fn,
};

pub struct SharedArrayBufferConstructor;

impl SharedArrayBufferConstructor {
    /// Properties of the SharedArrayBuffer Constructor (https://tc39.es/ecma262/#sec-properties-of-the-sharedarraybuffer-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::SharedArrayBufferConstructor_construct,
            1,
            cx.names.shared_array_buffer(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::SharedArrayBufferPrototype)?;

        // get SharedArrayBuffer [ @@species ] (https://tc39.es/ecma262/#sec-sharedarraybuffer-%symbol.species%)
        builder.getter(cx.symbols.species(), RuntimeFunction::ReturnThis)?;

        builder.build()
    }

    runtime_fn! {
    /// SharedArrayBuffer (https://tc39.es/ecma262/#sec-sharedarraybuffer-length)
    fn construct(cx, _, arguments) {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return type_error(cx, "SharedArrayBuffer constructor must be called with new");
        };

        let byte_length_arg = arguments.get(cx, 0);
        let byte_length = to_index(cx, byte_length_arg)?;

        let options_arg = arguments.get(cx, 1);
        let max_byte_length = get_array_buffer_max_byte_length_option(cx, options_arg)?;

        Ok(ArrayBufferObject::new(
            cx,
            new_target,
            byte_length,
            max_byte_length,
            /* data */ None,
            /* is_shared */ true,
        )?
        .as_value())
    }}
}
