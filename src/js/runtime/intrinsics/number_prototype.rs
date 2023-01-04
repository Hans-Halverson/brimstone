use crate::js::runtime::{
    completion::EvalResult, error::type_error_, gc::Gc, object_value::ObjectValue,
    ordinary_object::OrdinaryObject, realm::Realm, value::Value, Context,
};

use super::{intrinsics::Intrinsic, number_constructor::NumberObject};

pub struct NumberPrototype;

impl NumberPrototype {
    // 21.1.3 Properties of the Number Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once NumberConstructor has been created
        object.intrinsic_func(cx, "valueOf", Self::value_of, 0, realm);

        cx.heap.alloc(NumberObject::new(object, 0.0)).into()
    }

    // 21.1.3.7 Number.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        this_number_value(cx, this_value)
    }
}

fn this_number_value(cx: &mut Context, value: Value) -> EvalResult<Value> {
    if value.is_number() {
        return value.into();
    }

    if value.is_object() {
        let object_value = value.as_object();
        if object_value.is_number_object() {
            return object_value.cast::<NumberObject>().number_data().into();
        }
    }

    type_error_(cx, "value cannot be converted to number")
}
