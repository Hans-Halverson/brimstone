use crate::runtime::{
    Context, Handle,
    alloc_error::AllocResult,
    intrinsic_builder::IntrinsicBuilder,
    intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction},
    object_value::ObjectValue,
    realm::Realm,
};

/// The %AsyncIteratorPrototype% Object (https://tc39.es/ecma262/#sec-asynciteratorprototype)
pub struct AsyncIteratorPrototype;

impl AsyncIteratorPrototype {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        // %AsyncIteratorPrototype% [ @@asyncIterator ] (https://tc39.es/ecma262/#sec-%asynciteratorprototype%-%symbol.asynciterator%)
        builder.method(cx.symbols.async_iterator(), RuntimeFunction::ReturnThis, 0)?;

        builder.build()
    }
}
