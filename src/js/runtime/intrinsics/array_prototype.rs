use crate::{
    js::runtime::{
        array_object::ArrayObject, gc::Gc, object_value::ObjectValue,
        ordinary_object::ordinary_object_create, realm::Realm, type_utilities::to_object, Context,
        EvalResult, Value,
    },
    maybe,
};

use super::{
    array_iterator::{ArrayIterator, ArrayIteratorKind},
    intrinsics::Intrinsic,
};

pub struct ArrayPrototype;

impl ArrayPrototype {
    // 23.1.3 Properties of the Array Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let mut object = ordinary_object_create(object_proto);

        // Constructor property is added once ArrayConstructor has been created
        object.intrinsic_func(cx, &cx.names.entries(), Self::entries, 0, realm);
        object.intrinsic_func(cx, &cx.names.keys(), Self::keys, 0, realm);
        object.intrinsic_func(cx, &cx.names.values(), Self::values, 0, realm);

        cx.heap.alloc(ArrayObject::new(object)).into()
    }

    // 23.1.3.5 Array.prototype.entries
    fn entries(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        ArrayIterator::new(cx, object, ArrayIteratorKind::KeyAndValue).into()
    }

    // 23.1.3.17 Array.prototype.keys
    fn keys(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        ArrayIterator::new(cx, object, ArrayIteratorKind::Key).into()
    }

    // 23.1.3.33 Array.prototype.values
    fn values(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        ArrayIterator::new(cx, object, ArrayIteratorKind::Value).into()
    }
}
