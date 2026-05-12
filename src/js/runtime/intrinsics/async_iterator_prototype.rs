use crate::runtime::{
    alloc_error::AllocResult, intrinsics::rust_runtime::RuntimeFunction, object_value::ObjectValue,
    realm::Realm, Context, Handle,
};

use super::intrinsics::Intrinsic;

/// The %AsyncIteratorPrototype% Object (https://tc39.es/ecma262/#sec-asynciteratorprototype)
pub struct AsyncIteratorPrototype;

impl AsyncIteratorPrototype {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // %AsyncIteratorPrototype% [ @@asyncIterator ] (https://tc39.es/ecma262/#sec-%asynciteratorprototype%-%symbol.asynciterator%)
        let async_iterator_key = cx.well_known_symbols.async_iterator();
        object.intrinsic_func(cx, async_iterator_key, RuntimeFunction::ReturnThis, 0, realm)?;

        Ok(object)
    }
}
