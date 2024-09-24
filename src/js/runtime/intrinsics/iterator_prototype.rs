use crate::js::runtime::{object_value::ObjectValue, realm::Realm, Context, Handle};

use super::{intrinsics::Intrinsic, rust_runtime::return_this};

/// The %IteratorPrototype% Object (https://tc39.es/ecma262/#sec-%iteratorprototype%-object)
pub struct IteratorPrototype;

impl IteratorPrototype {
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        let iterator_key = cx.well_known_symbols.iterator();
        object.intrinsic_func(cx, iterator_key, return_this, 0, realm);

        object
    }
}
