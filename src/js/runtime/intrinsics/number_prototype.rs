use crate::{
    js::runtime::{
        completion::EvalResult,
        error::{range_error_, type_error_},
        function::get_argument,
        interned_strings::InternedStrings,
        object_value::ObjectValue,
        realm::Realm,
        string_value::FlatString,
        to_string,
        type_utilities::{number_to_string, to_integer_or_infinity},
        value::Value,
        Context, Handle,
    },
    maybe,
};

use super::{intrinsics::Intrinsic, number_constructor::NumberObject};

pub struct NumberPrototype;

impl NumberPrototype {
    // 21.1.3 Properties of the Number Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let object = NumberObject::new_with_proto(cx, object_proto, 0.0);

        // Constructor property is added once NumberConstructor has been created
        object.object().intrinsic_func(
            cx,
            cx.names.to_locale_string(),
            Self::to_locale_string,
            0,
            realm,
        );
        object
            .object()
            .intrinsic_func(cx, cx.names.to_string(), Self::to_string, 1, realm);
        object
            .object()
            .intrinsic_func(cx, cx.names.to_fixed(), Self::to_fixed, 1, realm);
        object
            .object()
            .intrinsic_func(cx, cx.names.to_precision(), Self::to_precision, 1, realm);
        object
            .object()
            .intrinsic_func(cx, cx.names.value_of(), Self::value_of, 0, realm);

        object.into()
    }

    // 21.1.3.3 Number.prototype.toFixed
    fn to_fixed(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let number_value = maybe!(this_number_value(cx, this_value));
        let mut number = number_value.as_number();

        let fraction_digits_arg = get_argument(cx, arguments, 0);
        let num_fraction_digits = maybe!(to_integer_or_infinity(cx, fraction_digits_arg));
        if !num_fraction_digits.is_finite()
            || num_fraction_digits < 0.0
            || num_fraction_digits > 100.0
        {
            return range_error_(cx, "number of fraction digits must between 0 and 100");
        }

        let num_fraction_digits = num_fraction_digits as u8;

        if !number.is_finite() {
            return cx.alloc_string(&number_to_string(number)).into();
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

            let num_fraction_digits = num_fraction_digits as usize;
            m = format!("{number:.num_fraction_digits$}");
        };

        if is_negative {
            m = format!("-{}", m);
        }

        cx.alloc_string(&m).into()
    }

    // 21.1.3.4 Number.prototype.toLocaleString
    fn to_locale_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        Self::to_string(cx, this_value, &[], None)
    }

    // 21.1.3.5 Number.prototype.toPrecision
    fn to_precision(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let number_value = maybe!(this_number_value(cx, this_value));

        let precision_arg = get_argument(cx, arguments, 0);
        if precision_arg.is_undefined() {
            return maybe!(to_string(cx, number_value.to_handle(cx))).into();
        }

        let precision = maybe!(to_integer_or_infinity(cx, precision_arg));
        if !number_value.as_number().is_finite() {
            return maybe!(to_string(cx, number_value.to_handle(cx))).into();
        }

        let precision = precision as i64;
        if precision < 1 || precision > 100 {
            return range_error_(
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
            // We cannot perform floating point math to find exponent and mantissa due to the
            // imprecision of floating point numbers. Instead, we first convert the number to a
            // string and then perform numeric calculations on the string itself.

            // We need to make sure there are at least 100 significant digits written to the string
            // so that no rounding is performed. For numbers < 1 this means we need to find the
            // log10 to account for leading zeros, that way we get 100 significant non-zero digits.
            let format_precision = if number < 1.0 {
                -(number.log10().floor()) as usize + 100
            } else {
                100
            };

            let mut number_string = format!("{:.*}", format_precision, number);

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
                number_string =
                    number_string[decimal_point_index + 1 + num_leading_zeros..].to_owned();
            };

            // Round to the correct precision, manually carrying upwards through digits if necessary
            let remaining_digits = number_string.split_off(precision as usize);
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
            mantissa = String::from_utf8(number_bytes).unwrap();

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

                return cx.alloc_string(&result).into();
            }
        }

        if exponent == precision - 1 {
            result.push_str(&mantissa);
        } else if exponent >= 0 {
            let period_index = usize::min((exponent + 1) as usize, mantissa.len());
            result.push_str(&mantissa[..period_index as usize]);
            result.push('.');
            result.push_str(&mantissa[period_index..]);
        } else {
            result.push_str("0.");
            result.push_str(&"0".repeat(-(exponent + 1) as usize));
            result.push_str(&mantissa);
        }

        cx.alloc_string(&result).into()
    }

    // 21.1.3.6 Number.prototype.toString
    fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let number_value = maybe!(this_number_value(cx, this_value));

        let radix = get_argument(cx, arguments, 0);

        let radix = if radix.is_undefined() {
            10
        } else {
            if radix.is_smi() {
                let radix = radix.as_smi();

                if radix < 2 || radix > 36 {
                    return range_error_(cx, "radix must be between 2 and 36");
                }

                radix
            } else {
                let radix = maybe!(to_integer_or_infinity(cx, radix));

                if radix < 2.0 || radix > 36.0 {
                    return range_error_(cx, "radix must be between 2 and 36");
                }

                radix as i32
            }
        };

        if number_value.is_nan() {
            return cx.names.nan.as_string().to_handle().into();
        } else if number_value.is_zero() {
            return InternedStrings::get_str(cx, "0").into();
        } else if number_value.is_infinity() {
            return if number_value.as_number() == f64::INFINITY {
                cx.names.infinity.as_string().to_handle().into()
            } else {
                InternedStrings::get_str(cx, "-Infinity").into()
            };
        }

        if radix == 10 {
            let str = if number_value.is_smi() {
                number_value.as_smi().to_string()
            } else {
                number_value.as_double().to_string()
            };

            return cx.alloc_string(&str).into();
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
            result_int_part_bytes_rev.push('0' as u8);
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
            result_bytes.push('-' as u8);
        }

        result_bytes.extend(result_int_part_bytes_rev.into_iter().rev());

        // Add decimal point and decimal part
        if dec_part != 0.0 {
            result_bytes.push('.' as u8);

            // Only calculate decimal digits up to precision
            for _ in 0..RADIX_TO_PRECISION[radix as usize] {
                dec_part *= radix as f64;
                let digit = dec_part.floor() as usize;
                result_bytes.push(DIGITS_OR_LETTERS[digit]);
                dec_part -= digit as f64;
            }
        }

        FlatString::from_one_byte_slice(cx, &result_bytes)
            .as_string()
            .to_handle()
            .into()
    }

    // 21.1.3.7 Number.prototype.valueOf
    fn value_of(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let number_value = maybe!(this_number_value(cx, this_value));
        number_value.to_handle(cx).into()
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
    let value = value_handle.get();
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
