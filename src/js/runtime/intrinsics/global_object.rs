use crate::{
    js::runtime::{
        abstract_operations::define_property_or_throw,
        builtin_function::BuiltinFunction,
        console::ConsoleObject,
        eval::eval::perform_eval,
        function::get_argument,
        gc::HandleValue,
        object_value::ObjectValue,
        property_descriptor::PropertyDescriptor,
        string_parsing::{
            parse_between_ptrs_to_f64, parse_unsigned_decimal_literal, skip_string_whitespace,
            StringLexer,
        },
        string_value::StringValue,
        to_string,
        type_utilities::{to_int32, to_number},
        Context, EvalResult, Handle, HeapPtr, Realm, Value,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 9.3.4 SetDefaultGlobalBindings
pub fn set_default_global_bindings(cx: &mut Context, realm: Handle<Realm>) -> EvalResult<()> {
    macro_rules! value_prop {
        ($name:expr, $value:expr, $is_writable:expr, $is_enumerable:expr, $is_configurable:expr) => {
            maybe!(define_property_or_throw(
                cx,
                realm.global_object(),
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
                realm.global_object(),
                $name,
                PropertyDescriptor::data(value.into(), true, false, true)
            ));
        };
    }

    // 19.1 Value Properties of the Global Object
    value_prop!(cx.names.global_this(), realm.global_this_value().into(), true, false, true);
    value_prop!(cx.names.infinity(), Value::number(f64::INFINITY), false, false, false);
    value_prop!(cx.names.nan(), Value::nan(), false, false, false);
    value_prop!(cx.names.undefined(), Value::undefined(), false, false, false);

    // 19.2 Function Properties of the Global Object
    intrinsic_prop!(cx.names.eval(), Eval);
    func_prop!(cx.names.is_nan(), is_nan, 1);
    func_prop!(cx.names.is_finite(), is_finite, 1);
    func_prop!(cx.names.parse_float(), parse_float, 1);
    func_prop!(cx.names.parse_int(), parse_int, 2);

    // 19.3 Constructor Properties of the Global Object
    intrinsic_prop!(cx.names.aggregate_error(), AggregateErrorConstructor);
    intrinsic_prop!(cx.names.array_buffer(), ArrayBufferConstructor);
    intrinsic_prop!(cx.names.array(), ArrayConstructor);
    intrinsic_prop!(cx.names.bigint(), BigIntConstructor);
    intrinsic_prop!(cx.names.big_int64_array(), BigInt64ArrayConstructor);
    intrinsic_prop!(cx.names.big_uint64_array(), BigUInt64ArrayConstructor);
    intrinsic_prop!(cx.names.boolean(), BooleanConstructor);
    intrinsic_prop!(cx.names.data_view(), DataViewConstructor);
    intrinsic_prop!(cx.names.error(), ErrorConstructor);
    intrinsic_prop!(cx.names.eval_error(), EvalErrorConstructor);
    intrinsic_prop!(cx.names.float32_array(), Float32ArrayConstructor);
    intrinsic_prop!(cx.names.float64_array(), Float64ArrayConstructor);
    intrinsic_prop!(cx.names.function(), FunctionConstructor);
    intrinsic_prop!(cx.names.int8_array(), Int8ArrayConstructor);
    intrinsic_prop!(cx.names.int16_array(), Int16ArrayConstructor);
    intrinsic_prop!(cx.names.int32_array(), Int32ArrayConstructor);
    intrinsic_prop!(cx.names.map(), MapConstructor);
    intrinsic_prop!(cx.names.number(), NumberConstructor);
    intrinsic_prop!(cx.names.object(), ObjectConstructor);
    intrinsic_prop!(cx.names.proxy(), ProxyConstructor);
    intrinsic_prop!(cx.names.range_error(), RangeErrorConstructor);
    intrinsic_prop!(cx.names.reference_error(), ReferenceErrorConstructor);
    intrinsic_prop!(cx.names.set(), SetConstructor);
    intrinsic_prop!(cx.names.string(), StringConstructor);
    intrinsic_prop!(cx.names.symbol(), SymbolConstructor);
    intrinsic_prop!(cx.names.syntax_error(), SyntaxErrorConstructor);
    intrinsic_prop!(cx.names.type_error(), TypeErrorConstructor);
    intrinsic_prop!(cx.names.uint8_array(), UInt8ArrayConstructor);
    intrinsic_prop!(cx.names.uint8_clamped_array(), UInt8ClampedArrayConstructor);
    intrinsic_prop!(cx.names.uint16_array(), UInt16ArrayConstructor);
    intrinsic_prop!(cx.names.uint32_array(), UInt32ArrayConstructor);
    intrinsic_prop!(cx.names.uri_error(), URIErrorConstructor);

    // 19.4 Other Properties of the Global Object
    intrinsic_prop!(cx.names.math(), Math);
    intrinsic_prop!(cx.names.reflect(), Reflect);

    // Non-standard, environment specific properties of global object
    let console_object = ConsoleObject::new(cx, realm).into();
    value_prop!(cx.names.console(), console_object, true, false, true);

    ().into()
}

pub fn create_eval(cx: &mut Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
    BuiltinFunction::create(cx, eval, 1, cx.names.eval(), Some(realm), None, None)
}

// 19.2.1 eval
fn eval(
    cx: &mut Context,
    _: HandleValue,
    arguments: &[HandleValue],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<HandleValue> {
    perform_eval(cx, get_argument(arguments, 0), false, false)
}

// 19.2.2 isFinite
fn is_finite(
    cx: &mut Context,
    _: HandleValue,
    arguments: &[HandleValue],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<HandleValue> {
    let num = maybe!(to_number(cx, get_argument(arguments, 0)));
    (!num.is_nan() && !num.is_infinity()).into()
}

// 19.2.3 isNaN
fn is_nan(
    cx: &mut Context,
    _: HandleValue,
    arguments: &[HandleValue],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<HandleValue> {
    let num = maybe!(to_number(cx, get_argument(arguments, 0)));
    num.is_nan().into()
}

// 19.2.4 parseFloat
fn parse_float(
    cx: &mut Context,
    _: HandleValue,
    arguments: &[HandleValue],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<HandleValue> {
    let input_string = maybe!(to_string(cx, get_argument(arguments, 0)));

    match parse_float_with_string_lexer(input_string.get_()) {
        Some(float) => Value::number(float).into(),
        None => Value::nan().into(),
    }
}

fn parse_float_with_string_lexer(string: HeapPtr<StringValue>) -> Option<f64> {
    let mut lexer = StringLexer::new(string);

    skip_string_whitespace(&mut lexer);

    // Skip leading prefix
    let mut is_negative = false;
    if lexer.current_equals('-') {
        lexer.advance();
        is_negative = true;
    } else if lexer.current_equals('+') {
        lexer.advance();
    }

    let start_ptr = lexer.current_ptr();
    parse_unsigned_decimal_literal(&mut lexer)?;
    let end_ptr = lexer.current_ptr();

    let number = parse_between_ptrs_to_f64(&lexer, start_ptr, end_ptr);

    if is_negative {
        Some(-number)
    } else {
        Some(number)
    }
}

// 19.2.5 parseInt
fn parse_int(
    cx: &mut Context,
    _: HandleValue,
    arguments: &[HandleValue],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<HandleValue> {
    let input_string = maybe!(to_string(cx, get_argument(arguments, 0)));
    let radix = maybe!(to_int32(cx, get_argument(arguments, 1)));

    match parse_int_impl(input_string.get_(), radix) {
        Some(number) => Value::number(number).into(),
        None => Value::nan().into(),
    }
}

#[inline]
fn parse_int_impl(string: HeapPtr<StringValue>, radix: i32) -> Option<f64> {
    let mut lexer = StringLexer::new(string);

    // Trim whitespace from start of string
    skip_string_whitespace(&mut lexer);

    // Strip + or - prefix from start of string
    let mut is_negative = false;
    if lexer.current_equals('-') {
        is_negative = true;
        lexer.advance();
    } else if lexer.current_equals('+') {
        lexer.advance();
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
        if lexer.current_equals('0') {
            if let Some('x' | 'X') = lexer.peek_ascii_char() {
                lexer.advance();
                lexer.advance();

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
        let digit = if let Some(digit) = lexer.current_digit_value('0', numeric_digit_upper_bound) {
            digit
        } else if let Some(digit) = lexer.current_digit_value('a', lowercase_digit_upper_bound) {
            digit + 10
        } else if let Some(digit) = lexer.current_digit_value('A', uppercase_digit_upper_bound) {
            digit + 10
        } else {
            break;
        };

        value *= radix_f64;
        value += digit as f64;

        has_digits = true;
        lexer.advance();
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
