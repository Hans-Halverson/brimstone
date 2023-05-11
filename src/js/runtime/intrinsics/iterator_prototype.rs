use crate::js::runtime::{
    completion::EvalResult, object_value::ObjectValue, realm::Realm, Context, Handle, Value,
};

use super::intrinsics::Intrinsic;

// 27.1.2 The %IteratorPrototype% Object
pub struct IteratorPrototype;

impl IteratorPrototype {
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        let iterator_key = cx.well_known_symbols.iterator();
        object.intrinsic_func(cx, iterator_key, Self::iterator, 0, realm);

        object
    }

    // 27.1.2.1 %IteratorPrototype% [ @@iterator ]
    fn iterator(
        _: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_value.into()
    }
}
