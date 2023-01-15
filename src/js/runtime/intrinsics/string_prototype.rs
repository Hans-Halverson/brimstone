use crate::js::runtime::{
    completion::EvalResult, error::type_error_, gc::Gc, object_value::ObjectValue,
    ordinary_object::OrdinaryObject, realm::Realm, value::Value, Context,
};

use super::{intrinsics::Intrinsic, string_constructor::StringObject};

pub struct StringPrototype;

impl StringPrototype {
    // 22.1.3 Properties of the String Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once StringConstructor has been created
        object.intrinsic_func(cx, &cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_func(cx, &cx.names.value_of(), Self::value_of, 0, realm);

        let string_value = cx.heap.alloc_string(String::new());
        let string_object = StringObject::new(cx, object, string_value);
        cx.heap.alloc(string_object).into()
    }

    // 22.1.3.28 String.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        this_string_value(cx, this_value)
    }

    // 22.1.3.33 String.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        this_string_value(cx, this_value)
    }
}

fn this_string_value(cx: &mut Context, value: Value) -> EvalResult<Value> {
    if value.is_string() {
        return value.into();
    }

    if value.is_object() {
        let object_value = value.as_object();
        if object_value.is_string_object() {
            return object_value.cast::<StringObject>().string_data().into();
        }
    }

    type_error_(cx, "value cannot be converted to string")
}
