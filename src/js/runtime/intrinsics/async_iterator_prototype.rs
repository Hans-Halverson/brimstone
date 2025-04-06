use crate::runtime::{object_value::ObjectValue, realm::Realm, Context, Handle};

use super::{intrinsics::Intrinsic, rust_runtime::return_this};

/// The %AsyncIteratorPrototype% Object (https://tc39.es/ecma262/#sec-asynciteratorprototype)
pub struct AsyncIteratorPrototype;

impl AsyncIteratorPrototype {
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // %AsyncIteratorPrototype% [ @@asyncIterator ] (https://tc39.es/ecma262/#sec-%asynciteratorprototype%-%symbol.asynciterator%)
        let async_iterator_key = cx.well_known_symbols.async_iterator();
        object.intrinsic_func(cx, async_iterator_key, return_this, 0, realm);

        object
    }
}
