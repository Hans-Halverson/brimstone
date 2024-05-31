use crate::js::runtime::{
    completion::EvalResult, object_value::ObjectValue, realm::Realm, Context, Handle, Value,
};

use super::intrinsics::Intrinsic;

/// 27.1.3 The %AsyncIteratorPrototype% Object
pub struct AsyncIteratorPrototype;

impl AsyncIteratorPrototype {
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        let async_iterator_key = cx.well_known_symbols.async_iterator();
        object.intrinsic_func(cx, async_iterator_key, Self::async_iterator, 0, realm);

        object
    }

    // 27.1.3.1 %AsyncIteratorPrototype% [ @@asyncIterator ]
    pub fn async_iterator(
        _: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_value.into()
    }
}
