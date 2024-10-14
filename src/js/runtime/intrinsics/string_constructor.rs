use crate::js::{
    common::unicode::CodePoint,
    runtime::{
        abstract_operations::length_of_array_like,
        builtin_function::BuiltinFunction,
        error::range_error,
        eval_result::EvalResult,
        function::get_argument,
        get,
        object_value::ObjectValue,
        realm::Realm,
        string_object::StringObject,
        string_value::{FlatString, StringValue},
        type_utilities::{to_number, to_object, to_string, to_uint16},
        Context, Handle, PropertyKey, Value,
    },
};

use super::{intrinsics::Intrinsic, symbol_prototype::symbol_descriptive_string};

pub struct StringConstructor;

impl StringConstructor {
    /// Properties of the String Constructor (https://tc39.es/ecma262/#sec-properties-of-the-string-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.string(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::StringPrototype).into(),
        );

        func.intrinsic_func(cx, cx.names.from_char_code(), Self::from_char_code, 1, realm);
        func.intrinsic_func(cx, cx.names.from_code_point(), Self::from_code_point, 1, realm);
        func.intrinsic_func(cx, cx.names.raw(), Self::raw, 1, realm);

        func
    }

    /// String (https://tc39.es/ecma262/#sec-string-constructor-string-value)
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let string_value = if arguments.is_empty() {
            cx.names.empty_string().as_string()
        } else {
            let value = get_argument(cx, arguments, 0);
            if new_target.is_none() && value.is_symbol() {
                return Ok(symbol_descriptive_string(cx, value.as_symbol()).as_value());
            }

            to_string(cx, value)?
        };

        match new_target {
            None => Ok(string_value.as_value()),
            Some(new_target) => {
                let string_object =
                    StringObject::new_from_constructor(cx, new_target, string_value)?;
                Ok(string_object.as_value())
            }
        }
    }

    /// String.fromCharCode (https://tc39.es/ecma262/#sec-string.fromcharcode)
    pub fn from_char_code(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        // Common case, return a single code unit string
        if arguments.len() == 1 {
            let code_unit = to_uint16(cx, arguments[0])?;
            return Ok(FlatString::from_code_unit(cx, code_unit).as_value());
        }

        let mut code_points = vec![];
        for arg in arguments {
            let code_unit = to_uint16(cx, *arg)?;
            code_points.push(code_unit as u32);
        }

        Ok(FlatString::from_code_points(cx, &code_points).as_value())
    }

    /// String.fromCodePoint (https://tc39.es/ecma262/#sec-string.fromcodepoint)
    pub fn from_code_point(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        macro_rules! get_code_point {
            ($arg:expr) => {{
                let arg = $arg;
                let code_point = to_number(cx, arg)?;

                // All valid code points are integers in the smi range
                if !code_point.is_smi() {
                    return range_error(
                        cx,
                        &format!("invalid code point {}", code_point.as_number()),
                    );
                }

                let code_point = code_point.as_smi();

                if !(0..=0x10FFFF).contains(&code_point) {
                    return range_error(cx, &format!("invalid code point {}", code_point));
                }

                code_point as CodePoint
            }};
        }

        // Common case, return a single code unit string
        if arguments.len() == 1 {
            let code_point = get_code_point!(arguments[0]);
            return Ok(FlatString::from_code_point(cx, code_point).as_value());
        }

        let mut code_points = vec![];
        for arg in arguments {
            code_points.push(get_code_point!(*arg));
        }

        Ok(FlatString::from_code_points(cx, &code_points).as_value())
    }

    /// String.raw (https://tc39.es/ecma262/#sec-string.raw)
    pub fn raw(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let substitution_count = arguments.len() - 1;

        let template_arg = get_argument(cx, arguments, 0);
        let cooked = to_object(cx, template_arg)?;

        let literals = get(cx, cooked, cx.names.raw())?;
        let literals = to_object(cx, literals)?;

        let literal_count = length_of_array_like(cx, literals)?;
        if literal_count == 0 {
            return Ok(cx.names.empty_string().as_string().as_value());
        }

        let mut result = cx.names.empty_string.as_string().to_handle();
        let mut next_index = 0;

        // Key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        loop {
            key.replace(PropertyKey::from_u64(cx, next_index));

            let next_literal_value = get(cx, literals, key)?;
            let next_literal_string = to_string(cx, next_literal_value)?;

            result = StringValue::concat(cx, result, next_literal_string);

            if next_index + 1 == literal_count {
                return Ok(result.as_value());
            }

            if next_index < substitution_count as u64 {
                let substitution_arg = get_argument(cx, arguments, next_index as usize + 1);
                let next_substitution = to_string(cx, substitution_arg)?;

                result = StringValue::concat(cx, result, next_substitution);
            }

            next_index += 1;
        }
    }
}
