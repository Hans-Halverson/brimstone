use crate::{
    js::runtime::{
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        gc::Gc,
        object_value::ObjectValue,
        ordinary_object::OrdinaryObject,
        realm::Realm,
        string_object::StringObject,
        to_string,
        type_utilities::{require_object_coercible, to_integer_or_infinity},
        value::Value,
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

pub struct StringPrototype;

impl StringPrototype {
    // 22.1.3 Properties of the String Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once StringConstructor has been created
        object.intrinsic_func(cx, &cx.names.at(), Self::at, 1, realm);
        object.intrinsic_func(cx, &cx.names.char_at(), Self::char_at, 1, realm);
        object.intrinsic_func(cx, &cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_func(cx, &cx.names.value_of(), Self::value_of, 0, realm);

        let string_value = cx.heap.alloc_string(String::new());
        let string_object = StringObject::new(cx, object, string_value);
        cx.heap.alloc(string_object).into()
    }

    // 22.1.3.1 String.prototype.at
    fn at(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        let str = string.str();
        let length = str.len() as i64;

        let relative_index = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 0)));
        if relative_index == f64::INFINITY {
            return Value::undefined().into();
        }

        let index = if relative_index >= 0.0 {
            relative_index as i64
        } else {
            length + (relative_index as i64)
        };

        if index < 0 || index >= length {
            return Value::undefined().into();
        }

        let char_string = String::from(str.as_bytes()[index as usize] as char);
        cx.heap.alloc_string(char_string).into()
    }

    // 22.1.3.2 String.prototype.charAt
    fn char_at(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));
        let position = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 0)));

        let str = string.str();
        if position < 0.0 || position >= str.len() as f64 {
            return cx.names.empty_string.as_string().into();
        }

        let char_string = String::from(str.as_bytes()[position as usize] as char);
        cx.heap.alloc_string(char_string).into()
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
