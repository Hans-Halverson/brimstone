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
        string_value::StringValue,
        to_string,
        type_utilities::{is_regexp, require_object_coercible, to_integer_or_infinity, to_number},
        value::Value,
        Context, PropertyKey,
    },
    maybe,
};

use super::{intrinsics::Intrinsic, string_iterator::StringIterator};

pub struct StringPrototype;

impl StringPrototype {
    // 22.1.3 Properties of the String Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once StringConstructor has been created
        object.intrinsic_func(cx, &cx.names.at(), Self::at, 1, realm);
        object.intrinsic_func(cx, &cx.names.char_at(), Self::char_at, 1, realm);
        object.intrinsic_func(cx, &cx.names.char_code_at(), Self::char_code_at, 1, realm);
        object.intrinsic_func(cx, &cx.names.code_point_at(), Self::code_point_at, 1, realm);
        object.intrinsic_func(cx, &cx.names.includes(), Self::includes, 1, realm);
        object.intrinsic_func(cx, &cx.names.index_of(), Self::index_of, 1, realm);
        object.intrinsic_func(cx, &cx.names.last_index_of(), Self::last_index_of, 1, realm);
        object.intrinsic_func(cx, &cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_func(cx, &cx.names.value_of(), Self::value_of, 0, realm);

        // 22.1.3.34 String.prototype [ @@iterator ]
        let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
        object.intrinsic_func(cx, &iterator_key, Self::iterator, 0, realm);

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
        let mut string = maybe!(to_string(cx, object));

        let length = string.len() as i64;

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

        StringValue::from_code_unit(cx, string.code_unit_at(index as usize)).into()
    }

    // 22.1.3.2 String.prototype.charAt
    fn char_at(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let mut string = maybe!(to_string(cx, object));
        let position = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 0)));

        if position < 0.0 || position >= string.len() as f64 {
            return cx.names.empty_string.as_string().into();
        }

        // TODO: Handle UTF-16 strings and return correct char
        StringValue::from_code_unit(cx, string.code_unit_at(position as usize)).into()
    }

    // 22.1.3.3 String.prototype.charCodeAt
    fn char_code_at(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let mut string = maybe!(to_string(cx, object));
        let position = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 0)));

        if position < 0.0 || position >= string.len() as f64 {
            return cx.names.empty_string.as_string().into();
        }

        // TODO: Handle UTF-16 strings and return correct char code
        StringValue::from_code_unit(cx, string.code_unit_at(position as usize)).into()
    }

    // 22.1.3.4 String.prototype.codePointAt
    fn code_point_at(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let mut string = maybe!(to_string(cx, object));
        let position = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 0)));

        if position < 0.0 || position >= string.len() as f64 {
            return cx.names.empty_string.as_string().into();
        }

        // TODO: Handle UTF-16 strings and return correct code point
        let char = char::from_u32(string.code_point_at(position as usize)).unwrap();
        cx.heap.alloc_string(String::from(char)).into()
    }

    // 22.1.3.8 String.prototype.includes
    fn includes(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let mut string = maybe!(to_string(cx, object));

        let search_string = get_argument(arguments, 0);
        if maybe!(is_regexp(cx, search_string)) {
            return type_error_(cx, "String.prototype.includes cannot take a regular expression");
        }

        let search_string = maybe!(to_string(cx, search_string));

        let pos = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 1)));
        if pos == f64::INFINITY {
            return Value::smi(-1).into();
        }

        let pos = pos as usize;
        if pos >= string.len() {
            return Value::smi(-1).into();
        }

        string.find(search_string, pos).is_some().into()
    }

    // 22.1.3.9 String.prototype.indexOf
    fn index_of(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let mut string = maybe!(to_string(cx, object));

        let search_string = maybe!(to_string(cx, get_argument(arguments, 0)));

        let pos = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 1)));
        if pos == f64::INFINITY {
            return Value::smi(-1).into();
        }

        let pos = pos as usize;
        if pos >= string.len() {
            return Value::smi(-1).into();
        }

        match string.find(search_string, pos) {
            None => Value::smi(-1).into(),
            Some(index) => Value::from_u64((pos + index) as u64).into(),
        }
    }

    // 22.1.3.10 String.prototype.lastIndexOf
    fn last_index_of(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let mut string = maybe!(to_string(cx, object));

        let search_string = maybe!(to_string(cx, get_argument(arguments, 0)));

        let mut string_end = search_string.len();

        let num_pos = maybe!(to_number(cx, get_argument(arguments, 1)));

        if !num_pos.is_nan() {
            let pos = maybe!(to_integer_or_infinity(cx, num_pos));
            if pos != f64::INFINITY {
                string_end = usize::clamp(pos as usize, 0, string_end);
            }
        }

        match string.rfind(search_string, string_end) {
            None => Value::smi(-1).into(),
            Some(index) => Value::from_u64(index as u64).into(),
        }
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

    // 22.1.3.34 String.prototype [ @@iterator ]
    fn iterator(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(require_object_coercible(cx, this_value));
        let string = maybe!(to_string(cx, object));

        StringIterator::new(cx, string).into()
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
