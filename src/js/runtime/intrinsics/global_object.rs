use std::str::FromStr;

use crate::{
    js::runtime::{
        abstract_operations::define_property_or_throw,
        builtin_function::BuiltinFunction,
        console::ConsoleObject,
        eval::eval::perform_eval,
        function::get_argument,
        object_value::ObjectValue,
        property_descriptor::PropertyDescriptor,
        string_parsing::{parse_unsigned_decimal_literal, skip_string_whitespace, StringLexer},
        to_string,
        type_utilities::{to_int32, to_number},
        value::StringValue,
        Context, EvalResult, Gc, Realm, Value,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 9.3.4 SetDefaultGlobalBindings
pub fn set_default_global_bindings(cx: &mut Context, realm: Gc<Realm>) -> EvalResult<()> {
    macro_rules! value_prop {
        ($name:expr, $value:expr, $is_writable:expr, $is_enumerable:expr, $is_configurable:expr) => {
            maybe!(define_property_or_throw(
                cx,
                realm.global_object,
                $name,
                PropertyDescriptor::data($value, $is_writable, $is_enumerable, $is_configurable)
            ));
        };
    }

    macro_rules! func_prop {
        ($str_name:expr, $func_name:expr, $length:expr) => {{
            let func_object = BuiltinFunction::create(
                cx,
                $func_name,
                $length,
                $str_name,
                Some(realm),
                None,
                None,
            )
            .into();
            value_prop!($str_name, func_object, true, false, true);
        }};
    }

    macro_rules! intrinsic_prop {
        ($name:expr, $intrinsic:ident) => {
            let value = realm.get_intrinsic(Intrinsic::$intrinsic);
            maybe!(define_property_or_throw(
                cx,
                realm.global_object,
                $name,
                PropertyDescriptor::data(value.into(), true, false, true)
            ));
        };
    }

    // 19.1 Value Properties of the Global Object
    value_prop!(
        &cx.names.global_this(),
        realm.global_env.global_this_value.into(),
        true,
        false,
        true
    );
    value_prop!(&cx.names.infinity(), Value::number(f64::INFINITY), false, false, false);
    value_prop!(&cx.names.nan(), Value::nan(), false, false, false);
    value_prop!(&cx.names.undefined(), Value::undefined(), false, false, false);

    // 19.2 Function Properties of the Global Object
    intrinsic_prop!(&cx.names.eval(), Eval);
    func_prop!(&cx.names.is_nan(), is_nan, 1);
    func_prop!(&cx.names.is_finite(), is_finite, 1);
    func_prop!(&cx.names.parse_float(), parse_float, 1);
    func_prop!(&cx.names.parse_int(), parse_int, 2);

    // 19.3 Constructor Properties of the Global Object
    intrinsic_prop!(&cx.names.array(), ArrayConstructor);
    intrinsic_prop!(&cx.names.bigint(), BigIntConstructor);
    intrinsic_prop!(&cx.names.boolean(), BooleanConstructor);
    intrinsic_prop!(&cx.names.error(), ErrorConstructor);
    intrinsic_prop!(&cx.names.eval_error(), EvalErrorConstructor);
    intrinsic_prop!(&cx.names.function(), FunctionConstructor);
    intrinsic_prop!(&cx.names.number(), NumberConstructor);
    intrinsic_prop!(&cx.names.object(), ObjectConstructor);
    intrinsic_prop!(&cx.names.proxy(), ProxyConstructor);
    intrinsic_prop!(&cx.names.range_error(), RangeErrorConstructor);
    intrinsic_prop!(&cx.names.reference_error(), ReferenceErrorConstructor);
    intrinsic_prop!(&cx.names.string(), StringConstructor);
    intrinsic_prop!(&cx.names.symbol(), SymbolConstructor);
    intrinsic_prop!(&cx.names.syntax_error(), SyntaxErrorConstructor);
    intrinsic_prop!(&cx.names.type_error(), TypeErrorConstructor);
    intrinsic_prop!(&cx.names.uri_error(), URIErrorConstructor);

    // 19.4 Other Properties of the Global Object
    intrinsic_prop!(&cx.names.math(), Math);
    intrinsic_prop!(&cx.names.reflect(), Reflect);

    // Non-standard, environment specific properties of global object
    let console_object = ConsoleObject::new(cx, realm).into();
    value_prop!(&cx.names.console(), console_object, true, false, true);

    ().into()
}

pub fn create_eval(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
    BuiltinFunction::create(cx, eval, 1, &cx.names.eval(), Some(realm), None, None)
}

// 19.2.1 eval
fn eval(
    cx: &mut Context,
    _: Value,
    arguments: &[Value],
    _: Option<Gc<ObjectValue>>,
) -> EvalResult<Value> {
    perform_eval(cx, get_argument(arguments, 0), false, false)
}

// 19.2.2 isFinite
fn is_finite(
    cx: &mut Context,
    _: Value,
    arguments: &[Value],
    _: Option<Gc<ObjectValue>>,
) -> EvalResult<Value> {
    let num = maybe!(to_number(cx, get_argument(arguments, 0)));
    (!num.is_nan() && !num.is_infinity()).into()
}

// 19.2.3 isNaN
fn is_nan(
    cx: &mut Context,
    _: Value,
    arguments: &[Value],
    _: Option<Gc<ObjectValue>>,
) -> EvalResult<Value> {
    let num = maybe!(to_number(cx, get_argument(arguments, 0)));
    num.is_nan().into()
}

// 19.2.4 parseFloat
fn parse_float(
    cx: &mut Context,
    _: Value,
    arguments: &[Value],
    _: Option<Gc<ObjectValue>>,
) -> EvalResult<Value> {
    let input_string = maybe!(to_string(cx, get_argument(arguments, 0)));
    let str = input_string.str();

    match parse_float_with_string_lexer(str) {
        Some(float) => Value::number(float).into(),
        None => Value::nan().into(),
    }
}

fn parse_float_with_string_lexer(str: &str) -> Option<f64> {
    let mut lexer = StringLexer::new(str)?;

    skip_string_whitespace(&mut lexer);

    // Skip leading prefix
    let mut is_negative = false;
    if lexer.current() == '-' {
        lexer.advance()?;
        is_negative = true;
    } else if lexer.current() == '+' {
        lexer.advance()?;
    }

    let start_pos = lexer.current_start_pos();
    parse_unsigned_decimal_literal(&mut lexer)?;
    let end_pos = lexer.current_start_pos();

    // Parse portion of string using rust stdlib
    let number = f64::from_str(&str[start_pos..end_pos]).unwrap();

    if is_negative {
        Some(-number)
    } else {
        Some(number)
    }
}

// 19.2.5 parseInt
fn parse_int(
    cx: &mut Context,
    _: Value,
    arguments: &[Value],
    _: Option<Gc<ObjectValue>>,
) -> EvalResult<Value> {
    let input_string = maybe!(to_string(cx, get_argument(arguments, 0)));
    let radix = maybe!(to_int32(cx, get_argument(arguments, 1)));

    match parse_int_impl(input_string, radix) {
        Some(number) => Value::number(number).into(),
        None => Value::nan().into(),
    }
}

#[inline]
fn parse_int_impl(string: Gc<StringValue>, radix: i32) -> Option<f64> {
    let mut lexer = StringLexer::new(string.str())?;

    // Trim whitespace from start of string
    skip_string_whitespace(&mut lexer)?;

    // Strip + or - prefix from start of string
    let mut is_negative = false;
    if lexer.current() == '-' {
        is_negative = true;
        lexer.advance()?;
    } else if lexer.current() == '+' {
        lexer.advance()?;
    }

    let mut radix = radix;
    let mut strip_prefix = true;

    if radix != 0 {
        if radix < 2 || radix > 36 {
            return None;
        } else if radix != 16 {
            strip_prefix = false;
        }
    } else {
        radix = 10;
    }

    if strip_prefix {
        if lexer.current() == '0' {
            if let 'x' | 'X' = lexer.peek_ascii_char() {
                lexer.advance()?;
                lexer.advance()?;

                radix = 16;
            }
        }
    }

    let radix = radix as u32;

    // Calculate exclusive upper bound for digit ranges
    let mut numeric_digit_upper_bound;
    let mut lowercase_digit_upper_bound;
    let mut uppercase_digit_upper_bound;

    unsafe {
        numeric_digit_upper_bound = char::from_u32_unchecked('9' as u32 + 1);
        lowercase_digit_upper_bound = char::from_u32_unchecked('z' as u32 + 1);
        uppercase_digit_upper_bound = char::from_u32_unchecked('Z' as u32 + 1);

        if radix <= 10 {
            numeric_digit_upper_bound = char::from_u32_unchecked(('0' as u32) + radix);
        } else {
            let num_letter_digits = radix - 10;
            lowercase_digit_upper_bound = char::from_u32_unchecked('a' as u32 + num_letter_digits);
            uppercase_digit_upper_bound = char::from_u32_unchecked('A' as u32 + num_letter_digits);
        }
    }

    // Parse digits on at a time, building up value
    let mut value: f64 = 0.0;
    let mut has_digits = false;

    let radix_f64 = radix as f64;

    while !lexer.is_end() {
        let char = lexer.current();

        let digit = if '0' <= char && char < numeric_digit_upper_bound {
            char as u32 - '0' as u32
        } else if 'a' <= char && char < lowercase_digit_upper_bound {
            (char as u32 - 'a' as u32) + 10
        } else if 'A' <= char && char < uppercase_digit_upper_bound {
            (char as u32 - 'A' as u32) + 10
        } else {
            break;
        };

        value *= radix_f64;
        value += digit as f64;

        has_digits = true;
        lexer.advance()?;
    }

    if !has_digits {
        return None;
    }

    if is_negative {
        Some(-value)
    } else {
        Some(value)
    }
}
