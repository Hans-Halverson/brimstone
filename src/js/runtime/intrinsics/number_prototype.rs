use crate::{
    js::runtime::{
        completion::EvalResult,
        error::{range_error_, type_error_},
        function::get_argument,
        gc::Gc,
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::{number_to_string, to_integer_or_infinity},
        value::Value,
        Context,
    },
    maybe,
};

use super::{intrinsics::Intrinsic, number_constructor::NumberObject};

pub struct NumberPrototype;

impl NumberPrototype {
    // 21.1.3 Properties of the Number Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
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
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let number_value = maybe!(this_number_value(cx, this_value));
        let mut number = number_value.as_number();

        let num_fraction_digits = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 0)));
        if !num_fraction_digits.is_finite()
            || num_fraction_digits < 0.0
            || num_fraction_digits > 100.0
        {
            return range_error_(cx, "number of fraction digits must between 0 and 100");
        }

        let num_fraction_digits = num_fraction_digits as u8;

        if !number.is_finite() {
            return cx.alloc_string(number_to_string(number)).into();
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

        cx.alloc_string(m).into()
    }

    // 21.1.3.4 Number.prototype.toLocaleString
    fn to_locale_string(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        Self::to_string(cx, this_value, &[], None)
    }

    // 21.1.3.6 Number.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let number_value = maybe!(this_number_value(cx, this_value));

        let radix = get_argument(arguments, 0);

        let is_radix_10 = if radix.is_undefined() {
            true
        } else {
            if radix.is_smi() {
                let radix = radix.as_smi();

                if radix < 2 || radix > 36 {
                    return range_error_(cx, "radix must be between 2 and 36");
                }

                radix == 10
            } else {
                let radix = maybe!(to_integer_or_infinity(cx, radix));

                if radix < 2.0 || radix > 36.0 {
                    return range_error_(cx, "radix must be between 2 and 36");
                }

                radix == 10.0
            }
        };

        if is_radix_10 {
            let str = if number_value.is_smi() {
                number_value.as_smi().to_string()
            } else {
                number_value.as_double().to_string()
            };

            cx.alloc_string(str).into()
        } else {
            unimplemented!("Number.prototype.toString with radix != 10")
        }
    }

    // 21.1.3.7 Number.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        this_number_value(cx, this_value)
    }
}

fn this_number_value(cx: &mut Context, value: Value) -> EvalResult<Value> {
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
