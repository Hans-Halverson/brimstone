use crate::js::runtime::{object_value::ObjectValue, realm::Realm, Context, Handle};

use super::{intrinsics::Intrinsic, rust_runtime::return_this};

/// 27.1.3 The %AsyncIteratorPrototype% Object
pub struct AsyncIteratorPrototype;

impl AsyncIteratorPrototype {
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // 27.1.3.1 %AsyncIteratorPrototype% [ @@asyncIterator ]
        let async_iterator_key = cx.well_known_symbols.async_iterator();
        object.intrinsic_func(cx, async_iterator_key, return_this, 0, realm);

        object
    }
}
