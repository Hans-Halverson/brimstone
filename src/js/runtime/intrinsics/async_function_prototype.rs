use crate::runtime::{
    Context, Handle, alloc_error::AllocResult, intrinsic_builder::IntrinsicBuilder,
    intrinsics::intrinsics::Intrinsic, object_value::ObjectValue, realm::Realm,
};

pub struct AsyncFunctionPrototype;

impl AsyncFunctionPrototype {
    /// Properties of the AsyncFunction Prototype Object (https://tc39.es/ecma262/#sec-async-function-prototype-properties)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::FunctionPrototype)?;

        // Constructor property is added once AsyncFunctionConstructor has been created

        // AsyncFunction.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-async-function-prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.async_function())?;

        builder.build()
    }
}
