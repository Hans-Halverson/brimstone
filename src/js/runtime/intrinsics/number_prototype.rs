use crate::runtime::{
    alloc_error::AllocResult,
    error::{range_error, type_error},
    eval_result::EvalResult,
    function::get_argument,
    object_value::ObjectValue,
    realm::Realm,
    string_value::FlatString,
    to_string,
    type_utilities::{number_to_string, to_integer_or_infinity},
    value::Value,
    Context, Handle,
};

use super::{intrinsics::Intrinsic, number_constructor::NumberObject};

pub struct NumberPrototype;

impl NumberPrototype {
    /// Properties of the Number Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-number-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let mut object = NumberObject::new_with_proto(cx, object_proto, 0.0)?.as_object();

        // Constructor property is added once NumberConstructor has been created
        object.intrinsic_func(cx, cx.names.to_exponential(), Self::to_exponential, 1, realm)?;
        object.intrinsic_func(cx, cx.names.to_fixed(), Self::to_fixed, 1, realm)?;
        object.intrinsic_func(cx, cx.names.to_locale_string(), Self::to_locale_string, 0, realm)?;
        object.intrinsic_func(cx, cx.names.to_precision(), Self::to_precision, 1, realm)?;
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 1, realm)?;
        object.intrinsic_func(cx, cx.names.value_of(), Self::value_of, 0, realm)?;

        Ok(object)
    }

    /// Number.prototype.toExponential (https://tc39.es/ecma262/#sec-number.prototype.toexponential)
    pub fn to_exponential(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let number_value = this_number_value(cx, this_value)?;
        let mut number = number_value.as_number();

        let fraction_digits_arg = get_argument(cx, arguments, 0);
        let num_fraction_digits = to_integer_or_infinity(cx, fraction_digits_arg)?;

        if !number.is_finite() {
            return Ok(to_string(cx, number_value.to_handle(cx))?.as_value());
        }

        if !(0.0..=100.0).contains(&num_fraction_digits) {
            return range_error(cx, "number of fraction digits must between 0 and 100");
        }

        // If number of fraction digits is not specified then we need to use the minimum number of
        // digits required to uniquely represent the number. Use Rust's default exponential
        // formatting for this, with some tweaks.
        if fraction_digits_arg.is_undefined() {
            let mut formatted = format!("{number:e}");

            // Exponent must include an explicit '+' sign, unlike Rust's default formatting
            let exponent_index = formatted.find('e').unwrap();
            if formatted.as_bytes()[exponent_index + 1] != b'-' {
                formatted.insert(exponent_index + 1, '+');
            }

            return Ok(cx.alloc_string(&formatted)?.as_value());
        }

        // Otherwise format string ourselves so that we control rounding to precision. We cannot
        // use Rust's default exponential formatting with precision as it will round ties in the
        // non-fractional component to even.
        //
        // e.g. if using Rust, (25).toPrecision(0) == '2e+1' while (35).toPrecision(0) == '4e+1'
        let mut result = String::new();

        if number < 0.0 {
            number = -number;
            result.push('-');
        }

        let exponent;
        let mantissa;

        if number == 0.0 {
            exponent = 0;
            mantissa = "0".repeat(num_fraction_digits as usize + 1);
        } else {
            (exponent, mantissa) =
                to_exponent_and_mantissa(number, num_fraction_digits as usize + 1);
        }

        // Insert decimal point after first digit (if there are any fractional digits)
        if num_fraction_digits != 0.0 {
            result.push_str(&mantissa[..1]);
            result.push('.');
            result.push_str(&mantissa[1..]);
        } else {
            result.push_str(&mantissa);
        }

        // Add exponent, with explicity '+' or '-' sign
        result.push('e');

        if exponent >= 0 {
            result.push('+');
        }

        result.push_str(&exponent.to_string());

        Ok(cx.alloc_string(&result)?.as_value())
    }

    /// Number.prototype.toFixed (https://tc39.es/ecma262/#sec-number.prototype.tofixed)
    pub fn to_fixed(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let number_value = this_number_value(cx, this_value)?;
        let mut number = number_value.as_number();

        let fraction_digits_arg = get_argument(cx, arguments, 0);
        let num_fraction_digits = to_integer_or_infinity(cx, fraction_digits_arg)?;
        if !num_fraction_digits.is_finite() || !(0.0..=100.0).contains(&num_fraction_digits) {
            return range_error(cx, "number of fraction digits must between 0 and 100");
        }

        let num_fraction_digits = num_fraction_digits as u8;

        if !number.is_finite() {
            return Ok(cx.alloc_string(&number_to_string(number))?.as_value());
        }

        let is_negative = number < 0.0;
        if is_negative {
            number = -number;
        }

        let mut m;
        if number >= 1e21 {
            m = number_to_string(number)
        } else {
            if number == 0.0 {
                number = 0.0;
            }

            let mut buf = ryu_js::Buffer::new();
            m = buf.format_to_fixed(number, num_fraction_digits).to_string();
        };

        if is_negative {
            m = format!("-{m}");
        }

        Ok(cx.alloc_string(&m)?.as_value())
    }

    /// Number.prototype.toLocaleString (https://tc39.es/ecma262/#sec-number.prototype.tolocalestring)
    pub fn to_locale_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        Self::to_string(cx, this_value, &[])
    }

    /// Number.prototype.toPrecision (https://tc39.es/ecma262/#sec-number.prototype.toprecision)
    pub fn to_precision(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let number_value = this_number_value(cx, this_value)?;

        let precision_arg = get_argument(cx, arguments, 0);
        if precision_arg.is_undefined() {
            return Ok(to_string(cx, number_value.to_handle(cx))?.as_value());
        }

        let precision = to_integer_or_infinity(cx, precision_arg)?;
        if !number_value.as_number().is_finite() {
            return Ok(to_string(cx, number_value.to_handle(cx))?.as_value());
        }

        let precision = precision as i64;
        if !(1..=100).contains(&precision) {
            return range_error(
                cx,
                "Number.prototype.toPrecision requires precision between 1 and 100",
            );
        }
        let precision = precision as i32;

        let mut result = String::new();

        let mut number = number_value.as_number();

        if number < 0.0 {
            number = -number;
            result.push('-');
        }

        // Exponent and mantissa ("e" and "m") from spec
        let mut exponent;
        let mut mantissa;

        if number == 0.0 {
            exponent = 0;
            mantissa = "0".repeat(precision as usize);
        } else {
            (exponent, mantissa) = to_exponent_and_mantissa(number, precision as usize);

            // Back to format from the spec now that we have calculated the exponent and mantissa
            if exponent < -6 || exponent >= precision {
                if precision != 1 {
                    mantissa.insert(1, '.');
                }

                let sign = if exponent > 0 {
                    '+'
                } else {
                    exponent = -exponent;
                    '-'
                };

                result.push_str(&mantissa);
                result.push('e');
                result.push(sign);
                result.push_str(&exponent.to_string());

                return Ok(cx.alloc_string(&result)?.as_value());
            }
        }

        if exponent == precision - 1 {
            result.push_str(&mantissa);
        } else if exponent >= 0 {
            let period_index = usize::min((exponent + 1) as usize, mantissa.len());
            result.push_str(&mantissa[..period_index]);
            result.push('.');
            result.push_str(&mantissa[period_index..]);
        } else {
            result.push_str("0.");
            result.push_str(&"0".repeat(-(exponent + 1) as usize));
            result.push_str(&mantissa);
        }

        Ok(cx.alloc_string(&result)?.as_value())
    }

    /// Number.prototype.toString (https://tc39.es/ecma262/#sec-number.prototype.tostring)
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let number_value = this_number_value(cx, this_value)?;

        let radix = get_argument(cx, arguments, 0);

        let radix = if radix.is_undefined() {
            10
        } else {
            if radix.is_smi() {
                let radix = radix.as_smi();

                if !(2..=36).contains(&radix) {
                    return range_error(cx, "radix must be between 2 and 36");
                }

                radix
            } else {
                let radix = to_integer_or_infinity(cx, radix)?;

                if !(2.0..=36.0).contains(&radix) {
                    return range_error(cx, "radix must be between 2 and 36");
                }

                radix as i32
            }
        };

        if number_value.is_nan() {
            return Ok(cx.names.nan().as_string().as_value());
        } else if number_value.is_zero() {
            return Ok(cx.names.zero().as_string().as_value());
        } else if number_value.is_infinity() {
            return if number_value.as_number() == f64::INFINITY {
                Ok(cx.names.infinity().as_string().as_value())
            } else {
                Ok(cx.names.negative_infinity_literal().as_string().as_value())
            };
        }

        if radix == 10 {
            let str = if number_value.is_smi() {
                number_value.as_smi().to_string()
            } else {
                number_value.as_double().to_string()
            };

            return Ok(cx.alloc_string(&str)?.as_value());
        }

        // Float to string conversion based on SerenityOS's LibJS
        let mut number = number_value.as_number();

        let is_negative = number.is_sign_negative();
        if is_negative {
            number = -number;
        }

        let mut int_part = number.floor();
        let mut dec_part = number - int_part;

        // Calculate int part characters in reverse order
        let mut result_int_part_bytes_rev = vec![];

        if int_part == 0.0 {
            result_int_part_bytes_rev.push(b'0');
        } else {
            while int_part > 0.0 {
                let digit = (int_part % radix as f64).floor() as usize;
                result_int_part_bytes_rev.push(DIGITS_OR_LETTERS[digit]);
                int_part = (int_part / radix as f64).floor();
            }
        }

        // Start result string with a sign followed by int part
        let mut result_bytes = vec![];

        if is_negative {
            result_bytes.push(b'-');
        }

        result_bytes.extend(result_int_part_bytes_rev.into_iter().rev());

        // Add decimal point and decimal part
        if dec_part != 0.0 {
            result_bytes.push(b'.');

            // Only calculate decimal digits up to precision
            for _ in 0..RADIX_TO_PRECISION[radix as usize] {
                dec_part *= radix as f64;
                let digit = dec_part.floor() as usize;
                result_bytes.push(DIGITS_OR_LETTERS[digit]);
                dec_part -= digit as f64;
            }
        }

        Ok(FlatString::from_one_byte_slice(cx, &result_bytes)?
            .to_handle()
            .as_value())
    }

    /// Number.prototype.valueOf (https://tc39.es/ecma262/#sec-number.prototype.valueof)
    pub fn value_of(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let number_value = this_number_value(cx, this_value)?;
        Ok(number_value.to_handle(cx))
    }
}

/// Character values that are used for each digit. May be a digit or letter e.g. `1` or `a`.
const DIGITS_OR_LETTERS: &[u8] = b"0123456789abcdefghijklmnopqrstuvwxyz";

/// Precision for each radix. Used with the range 2 to 36 (0 and 1 are unused).
const RADIX_TO_PRECISION: [u8; 37] = [
    0, 0, 52, 32, 26, 22, 20, 18, 17, 16, 15, 15, 14, 14, 13, 13, 13, 12, 12, 12, 12, 11, 11, 11,
    11, 11, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
];

fn this_number_value(cx: Context, value_handle: Handle<Value>) -> EvalResult<Value> {
    let value = *value_handle;
    if value.is_number() {
        return Ok(value);
    }

    if value.is_object() {
        let object_value = value.as_object();
        if let Some(number_object) = object_value.as_number_object() {
            let number_f64 = number_object.number_data();
            return Ok(Value::number(number_f64));
        }
    }

    type_error(cx, "value cannot be converted to number")
}

/// Decompose an f64 to its exponent and mantissa, where the mantissa is rounded to exactly the
/// specified precision (aka number of digits).
fn to_exponent_and_mantissa(number: f64, precision: usize) -> (i32, String) {
    let mut exponent;

    // We cannot perform floating point math to find exponent and mantissa due to the
    // imprecision of floating point numbers. Instead, we first convert the number to a
    // string and then perform numeric calculations on the string itself.

    // We need to make sure there are at least 100 significant digits written to the string
    // so that no rounding is performed. For numbers < 1 this means we need to find the
    // log10 to account for leading zeros, that way we get 100 significant non-zero digits.
    let format_precision = if number < 1.0 {
        -(number.log10().floor()) as usize + 101
    } else {
        101
    };

    let mut number_string = format!("{number:.format_precision$}");

    // Find the exponent of the number. For numbers >= 1 find the number of digits before
    // the decimal point. For numbers < 1 find the number of leading zeros after the decimal
    // point. This algorithm fails if number == 0, but this case is handled above.
    let decimal_point_index = number_string.find('.').unwrap();

    if number >= 1.0 {
        exponent = decimal_point_index as i32 - 1;

        // Remove the decimal point, leaving only significant digits
        number_string.remove(decimal_point_index);
    } else {
        let num_leading_zeros = number_string[decimal_point_index + 1..]
            .find(|digit| digit != '0')
            .unwrap();
        exponent = -(num_leading_zeros as i32) - 1;

        // Remove all digits before the first significant (non-zero) digit
        number_string = number_string[decimal_point_index + 1 + num_leading_zeros..].to_owned();
    };

    // Round to the correct precision, manually carrying upwards through digits if necessary
    let remaining_digits = number_string.split_off(precision);
    let mut carry = remaining_digits.chars().next().unwrap() >= '5';

    let mut number_bytes = number_string.into_bytes();
    for digit in number_bytes.iter_mut().rev() {
        if !carry {
            break;
        }

        if *digit == b'9' {
            *digit = b'0';
            carry = true;
        } else {
            *digit += 1;
            carry = false;
        }
    }

    // If we still need to carry at the end of the string, we need to add a leading one
    // then adjust the exponent.
    if carry {
        number_bytes.insert(0, b'1');
        number_bytes.pop();
        exponent += 1;
    }

    // Digits string is used as the mantissa
    let mantissa = String::from_utf8(number_bytes).unwrap();

    (exponent, mantissa)
}
