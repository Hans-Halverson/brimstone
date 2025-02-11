use crate::js::{
    common::{
        unicode::{encode_utf8_codepoint, get_hex_value, is_continuation_byte},
        wtf_8::Wtf8String,
    },
    runtime::{
        abstract_operations::define_property_or_throw,
        builtin_function::BuiltinFunction,
        bytecode::instruction::EvalFlags,
        console::ConsoleObject,
        error::uri_error,
        eval::eval::perform_eval,
        function::get_argument,
        gc::HandleScope,
        object_value::ObjectValue,
        property_descriptor::PropertyDescriptor,
        string_parsing::{parse_signed_decimal_literal, skip_string_whitespace, StringLexer},
        string_value::{FlatString, StringValue},
        to_string,
        type_utilities::{to_int32, to_number},
        Context, EvalResult, Handle, Realm, Value,
    },
};

use super::intrinsics::Intrinsic;

/// SetDefaultGlobalBindings (https://tc39.es/ecma262/#sec-setdefaultglobalbindings)
pub fn set_default_global_bindings(cx: Context, realm: Handle<Realm>) -> EvalResult<()> {
    HandleScope::new(cx, |cx| {
        macro_rules! value_prop {
            ($name:expr, $value:expr, $is_writable:expr, $is_enumerable:expr, $is_configurable:expr) => {
                define_property_or_throw(
                    cx,
                    realm.global_object(),
                    $name,
                    PropertyDescriptor::data(
                        $value,
                        $is_writable,
                        $is_enumerable,
                        $is_configurable,
                    ),
                )?;
            };
        }

        macro_rules! func_prop {
            ($str_name:expr, $func_name:expr, $length:expr) => {{
                let func_object =
                    BuiltinFunction::create(cx, $func_name, $length, $str_name, realm, None, None)
                        .into();
                value_prop!($str_name, func_object, true, false, true);
            }};
        }

        macro_rules! intrinsic_prop {
            ($name:expr, $intrinsic:ident) => {
                let value = realm.get_intrinsic(Intrinsic::$intrinsic);
                define_property_or_throw(
                    cx,
                    realm.global_object(),
                    $name,
                    PropertyDescriptor::data(value.into(), true, false, true),
                )?;
            };
        }

        // Value Properties of the Global Object (https://tc39.es/ecma262/#sec-value-properties-of-the-global-object)
        let infinity_value = cx.number(f64::INFINITY);
        let nan_value = cx.nan();

        value_prop!(cx.names.global_this(), realm.global_object().into(), true, false, true);
        value_prop!(cx.names.infinity(), infinity_value, false, false, false);
        value_prop!(cx.names.nan(), nan_value, false, false, false);
        value_prop!(cx.names.undefined(), cx.undefined(), false, false, false);

        // Function Properties of the Global Object (https://tc39.es/ecma262/#sec-function-properties-of-the-global-object)
        func_prop!(cx.names.decode_uri(), decode_uri, 1);
        func_prop!(cx.names.decode_uri_component(), decode_uri_component, 1);
        func_prop!(cx.names.encode_uri(), encode_uri, 1);
        func_prop!(cx.names.encode_uri_component(), encode_uri_component, 1);
        intrinsic_prop!(cx.names.eval(), Eval);
        func_prop!(cx.names.is_nan(), is_nan, 1);
        func_prop!(cx.names.is_finite(), is_finite, 1);
        intrinsic_prop!(cx.names.parse_float(), ParseFloat);
        intrinsic_prop!(cx.names.parse_int(), ParseInt);

        // Constructor Properties of the Global Object (https://tc39.es/ecma262/#sec-constructor-properties-of-the-global-object)
        intrinsic_prop!(cx.names.aggregate_error(), AggregateErrorConstructor);
        intrinsic_prop!(cx.names.array_buffer(), ArrayBufferConstructor);
        intrinsic_prop!(cx.names.array(), ArrayConstructor);
        intrinsic_prop!(cx.names.bigint(), BigIntConstructor);
        intrinsic_prop!(cx.names.big_int64_array(), BigInt64ArrayConstructor);
        intrinsic_prop!(cx.names.big_uint64_array(), BigUInt64ArrayConstructor);
        intrinsic_prop!(cx.names.boolean(), BooleanConstructor);
        intrinsic_prop!(cx.names.data_view(), DataViewConstructor);
        intrinsic_prop!(cx.names.date(), DateConstructor);
        intrinsic_prop!(cx.names.error(), ErrorConstructor);
        intrinsic_prop!(cx.names.eval_error(), EvalErrorConstructor);
        intrinsic_prop!(cx.names.finalization_registry(), FinalizationRegistryConstructor);
        intrinsic_prop!(cx.names.float32_array(), Float32ArrayConstructor);
        intrinsic_prop!(cx.names.float64_array(), Float64ArrayConstructor);
        intrinsic_prop!(cx.names.function(), FunctionConstructor);
        intrinsic_prop!(cx.names.int8_array(), Int8ArrayConstructor);
        intrinsic_prop!(cx.names.int16_array(), Int16ArrayConstructor);
        intrinsic_prop!(cx.names.int32_array(), Int32ArrayConstructor);
        intrinsic_prop!(cx.names.iterator(), IteratorConstructor);
        intrinsic_prop!(cx.names.map(), MapConstructor);
        intrinsic_prop!(cx.names.number(), NumberConstructor);
        intrinsic_prop!(cx.names.object(), ObjectConstructor);
        intrinsic_prop!(cx.names.promise(), PromiseConstructor);
        intrinsic_prop!(cx.names.proxy(), ProxyConstructor);
        intrinsic_prop!(cx.names.range_error(), RangeErrorConstructor);
        intrinsic_prop!(cx.names.reference_error(), ReferenceErrorConstructor);
        intrinsic_prop!(cx.names.regexp(), RegExpConstructor);
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
        intrinsic_prop!(cx.names.weak_map(), WeakMapConstructor);
        intrinsic_prop!(cx.names.weak_ref(), WeakRefConstructor);
        intrinsic_prop!(cx.names.weak_set(), WeakSetConstructor);

        // Other Properties of the Global Object (https://tc39.es/ecma262/#sec-other-properties-of-the-global-object)
        intrinsic_prop!(cx.names.json(), JSON);
        intrinsic_prop!(cx.names.math(), Math);
        intrinsic_prop!(cx.names.reflect(), Reflect);

        // Non-standard, environment specific properties of global object
        let console_object = ConsoleObject::new(cx, realm).into();
        value_prop!(cx.names.console(), console_object, true, false, true);

        Ok(())
    })
}

pub fn create_eval(cx: Context, realm: Handle<Realm>) -> Handle<Value> {
    BuiltinFunction::create(cx, eval, 1, cx.names.eval(), realm, None, None).into()
}

pub fn create_parse_float(cx: Context, realm: Handle<Realm>) -> Handle<Value> {
    BuiltinFunction::create(cx, parse_float, 1, cx.names.parse_float(), realm, None, None).into()
}

pub fn create_parse_int(cx: Context, realm: Handle<Realm>) -> Handle<Value> {
    BuiltinFunction::create(cx, parse_int, 2, cx.names.parse_int(), realm, None, None).into()
}

/// eval (https://tc39.es/ecma262/#sec-eval-x)
pub fn eval(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let code_arg = get_argument(cx, arguments, 0);

    perform_eval(cx, code_arg, /* is_strict_caller */ false, None, EvalFlags::empty())
}

/// isFinite (https://tc39.es/ecma262/#sec-isfinite-number)
pub fn is_finite(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let argument = get_argument(cx, arguments, 0);
    let num = to_number(cx, argument)?;
    Ok(cx.bool(!num.is_nan() && !num.is_infinity()))
}

/// isNaN (https://tc39.es/ecma262/#sec-isnan-number)
pub fn is_nan(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let argument = get_argument(cx, arguments, 0);
    let num = to_number(cx, argument)?;
    Ok(cx.bool(num.is_nan()))
}

/// parseFloat (https://tc39.es/ecma262/#sec-parsefloat-string)
pub fn parse_float(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let input_string_arg = get_argument(cx, arguments, 0);
    let input_string = to_string(cx, input_string_arg)?;

    match parse_float_with_string_lexer(input_string) {
        Some(float) => Ok(cx.number(float)),
        None => Ok(cx.nan()),
    }
}

fn parse_float_with_string_lexer(string: Handle<StringValue>) -> Option<f64> {
    let mut lexer = StringLexer::new(string);

    skip_string_whitespace(&mut lexer);
    parse_signed_decimal_literal(&mut lexer)
}

/// parseInt (https://tc39.es/ecma262/#sec-parseint-string-radix)
pub fn parse_int(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let input_string_arg = get_argument(cx, arguments, 0);
    let input_string = to_string(cx, input_string_arg)?;

    let radix_arg = get_argument(cx, arguments, 1);
    let radix = to_int32(cx, radix_arg)?;

    match parse_int_impl(input_string, radix) {
        Some(number) => Ok(cx.number(number)),
        None => Ok(cx.nan()),
    }
}

#[inline]
fn parse_int_impl(string: Handle<StringValue>, radix: i32) -> Option<f64> {
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
        if !(2..=36).contains(&radix) {
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
    let numeric_digit_upper_bound;
    let lowercase_digit_upper_bound;
    let uppercase_digit_upper_bound;

    unsafe {
        if radix <= 10 {
            numeric_digit_upper_bound = char::from_u32_unchecked(('0' as u32) + radix);
            lowercase_digit_upper_bound = char::from_u32_unchecked('a' as u32);
            uppercase_digit_upper_bound = char::from_u32_unchecked('A' as u32);
        } else {
            let num_letter_digits = radix - 10;
            numeric_digit_upper_bound = char::from_u32_unchecked('9' as u32 + 1);
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

/// decodeURI (https://tc39.es/ecma262/#sec-decodeuri-encodeduri)
pub fn decode_uri(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let uri_arg = get_argument(cx, arguments, 0);
    let uri_string = to_string(cx, uri_arg)?;

    decode::<true>(cx, uri_string)
}

/// decodeURIComponent (https://tc39.es/ecma262/#sec-decodeuricomponent-encodeduricomponent)
pub fn decode_uri_component(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let uri_component_arg = get_argument(cx, arguments, 0);
    let uri_component_string = to_string(cx, uri_component_arg)?;

    decode::<false>(cx, uri_component_string)
}

/// Decode (https://tc39.es/ecma262/#sec-decode)
fn decode<const INCLUDE_URI_UNESCAPED: bool>(
    cx: Context,
    string: Handle<StringValue>,
) -> EvalResult<Handle<Value>> {
    let mut decoded_string = Wtf8String::new();

    let flat_string = string.flatten();
    let string_length = flat_string.len();

    let mut i = 0;

    macro_rules! parse_hex_byte {
        () => {{
            if i + 2 >= string_length {
                return uri_error(cx, "Invalid URI escape sequence");
            }

            if flat_string.code_unit_at(i) != '%' as u16 {
                return uri_error(cx, "Invalid URI escape sequence");
            }

            let byte = match (
                get_hex_value(flat_string.code_unit_at(i + 1) as u32),
                get_hex_value(flat_string.code_unit_at(i + 2) as u32),
            ) {
                (Some(first), Some(second)) => ((first << 4) | second) as u8,
                _ => return uri_error(cx, "Invalid URI escape sequence"),
            };

            i += 3;

            byte
        }};
    }

    macro_rules! parse_hex_continuation_byte {
        () => {{
            let code_unit = parse_hex_byte!();

            if !is_continuation_byte(code_unit) {
                return uri_error(cx, "Invalid URI escape sequence");
            }

            code_unit
        }};
    }

    while i < string_length {
        let code_unit = flat_string.code_unit_at(i);

        if code_unit == '%' as u16 {
            let first_byte = parse_hex_byte!();

            if first_byte & 0x80 == 0 {
                // Single byte UTF-8 sequence.
                match first_byte as char {
                    // If this is a preserved character then the encoded hex sequence must be a
                    // literal since preserved characters are not encoded.
                    ';' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',' | '#'
                        if INCLUDE_URI_UNESCAPED =>
                    {
                        decoded_string.push(flat_string.code_unit_at(i - 3) as u32);
                        decoded_string.push(flat_string.code_unit_at(i - 2) as u32);
                        decoded_string.push(flat_string.code_unit_at(i - 1) as u32);
                    }
                    _ => decoded_string.push(first_byte as u32),
                }
            } else if (first_byte & 0xE0) == 0xC0 {
                // Two byte UTF-8 sequence
                let mut code_point = (first_byte as u32 & 0x1F) << 6;
                code_point |= parse_hex_continuation_byte!() as u32 & 0x3F;

                decoded_string.push(code_point);
            } else if (first_byte & 0xF0) == 0xE0 {
                // Three byte UTF-8 sequence
                let mut code_point = (first_byte as u32 & 0x0F) << 12;
                code_point |= (parse_hex_continuation_byte!() as u32 & 0x3F) << 6;
                code_point |= parse_hex_continuation_byte!() as u32 & 0x3F;

                decoded_string.push(code_point);
            } else if (first_byte & 0xF8) == 0xF0 {
                // Four byte UTF-8 sequence
                let mut code_point = (first_byte as u32 & 0x07) << 18;
                code_point |= (parse_hex_continuation_byte!() as u32 & 0x3F) << 12;
                code_point |= (parse_hex_continuation_byte!() as u32 & 0x3F) << 6;
                code_point |= parse_hex_continuation_byte!() as u32 & 0x3F;

                decoded_string.push(code_point);
            } else {
                return uri_error(cx, "Invalid URI escape sequence");
            }
        } else {
            decoded_string.push(code_unit as u32);
            i += 1;
        }
    }

    Ok(FlatString::from_wtf8(cx, decoded_string.as_bytes())
        .to_handle()
        .as_value())
}

/// encodeURI (https://tc39.es/ecma262/#sec-encodeuri-uri)
pub fn encode_uri(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let uri_arg = get_argument(cx, arguments, 0);
    let uri_string = to_string(cx, uri_arg)?;

    encode::<true>(cx, uri_string)
}

/// encodeURIComponent (https://tc39.es/ecma262/#sec-encodeuricomponent-uricomponent)
pub fn encode_uri_component(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let uri_component_arg = get_argument(cx, arguments, 0);
    let uri_component_string = to_string(cx, uri_component_arg)?;

    encode::<false>(cx, uri_component_string)
}

/// Encode (https://tc39.es/ecma262/#sec-encode)
fn encode<const INCLUDE_URI_UNESCAPED: bool>(
    cx: Context,
    string: Handle<StringValue>,
) -> EvalResult<Handle<Value>> {
    let mut encoded_string = Wtf8String::new();

    for code_point in string.iter_code_points() {
        // Very that the char is not an unpaired surrogate
        let char = match char::from_u32(code_point) {
            Some(char) => char,
            None => {
                return uri_error(cx, "Unpaired surrogate cannot be encoded in URI");
            }
        };

        match char {
            // Characters that never need to be encoded
            'A'..='Z'
            | 'a'..='z'
            | '0'..='9'
            | '-'
            | '_'
            | '.'
            | '!'
            | '~'
            | '*'
            | '\''
            | '('
            | ')' => {
                encoded_string.push_char(char);
            }
            // Characters that don't need to be encoded when escaping a whole URI
            ';' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',' | '#'
                if INCLUDE_URI_UNESCAPED =>
            {
                encoded_string.push_char(char);
            }
            // Write each byte of the UTF-8 representation of the code point as `%XX`
            _ => {
                let mut buf = [0; 4];
                let num_bytes = encode_utf8_codepoint(&mut buf, code_point);

                for byte in &buf[..num_bytes] {
                    encoded_string.push_str(&format!("%{:02X}", byte));
                }
            }
        }
    }

    // Safe since only ASCII characters were used
    Ok(FlatString::from_one_byte_slice(cx, encoded_string.as_bytes())
        .to_handle()
        .as_value())
}
