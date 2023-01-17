use crate::{
    js::runtime::{
        array_object::array_create, gc::Gc, object_value::ObjectValue, realm::Realm, Context,
        EvalResult,
    },
    must,
};

use super::intrinsics::Intrinsic;

pub struct ArrayPrototype;

impl ArrayPrototype {
    // 23.1.3 Properties of the Array Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let array_object = must!(array_create(cx, 0, Some(object_proto)));

        array_object.into()
    }
}
