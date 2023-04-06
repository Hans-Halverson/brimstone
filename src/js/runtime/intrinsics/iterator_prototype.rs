use crate::js::runtime::{
    completion::EvalResult, gc::Gc, object_value::ObjectValue, property_key::PropertyKey,
    realm::Realm, value::Value, Context,
};

use super::intrinsics::Intrinsic;

// 27.1.2 The %IteratorPrototype% Object
pub struct IteratorPrototype;

impl IteratorPrototype {
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
        object.intrinsic_func(cx, &iterator_key, Self::iterator, 0, realm);

        object
    }

    // 27.1.2.1 %IteratorPrototype% [ @@iterator ]
    fn iterator(
        _: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        this_value.into()
    }
}
