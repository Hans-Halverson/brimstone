use crate::{
    js::runtime::{
        completion::EvalResult,
        error::{range_error_, type_error_},
        function::get_argument,
        interned_strings::InternedStrings,
        object_value::ObjectValue,
        realm::Realm,
        string_value::FlatString,
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
