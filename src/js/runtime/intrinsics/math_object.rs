use crate::{
    js::runtime::{
        completion::EvalResult,
        function::get_argument,
        gc::Gc,
        numeric_operations::number_exponentiate,
        object_value::{Object, ObjectValue},
        ordinary_object::OrdinaryObject,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::to_number,
        value::Value,
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 21.3 The Math Object
pub struct MathObject;

impl MathObject {
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // 21.3.1 Value Properties of the Math Object
        object.intrinsic_frozen_property(&cx.names.e(), Value::number(std::f64::consts::E));
        object.intrinsic_frozen_property(&cx.names.ln10(), Value::number(std::f64::consts::LN_10));
        object.intrinsic_frozen_property(&cx.names.ln2(), Value::number(std::f64::consts::LN_2));
        object.intrinsic_frozen_property(
            &cx.names.log10e(),
            Value::number(std::f64::consts::LOG10_E),
        );
        object
            .intrinsic_frozen_property(&cx.names.log2e(), Value::number(std::f64::consts::LOG2_E));
        object.intrinsic_frozen_property(&cx.names.pi(), Value::number(std::f64::consts::PI));
        object.intrinsic_frozen_property(
            &cx.names.sqrt1_2(),
            Value::number(std::f64::consts::FRAC_1_SQRT_2),
        );
        object
            .intrinsic_frozen_property(&cx.names.sqrt2(), Value::number(std::f64::consts::SQRT_2));

        // 21.3.1.9 Math [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        let math_name_value = cx.names.math().as_string().into();
        object
            .set_property(&to_string_tag_key, Property::data(math_name_value, false, false, true));

        object.intrinsic_func(cx, &cx.names.abs(), Self::abs, 1, realm);
        object.intrinsic_func(cx, &cx.names.ceil(), Self::ceil, 1, realm);
        object.intrinsic_func(cx, &cx.names.floor(), Self::floor, 1, realm);
        object.intrinsic_func(cx, &cx.names.fround(), Self::fround, 1, realm);
        object.intrinsic_func(cx, &cx.names.max(), Self::max, 2, realm);
        object.intrinsic_func(cx, &cx.names.min(), Self::min, 2, realm);
        object.intrinsic_func(cx, &cx.names.pow(), Self::pow, 2, realm);
        object.intrinsic_func(cx, &cx.names.round(), Self::round, 1, realm);
        object.intrinsic_func(cx, &cx.names.sign(), Self::sign, 1, realm);
        object.intrinsic_func(cx, &cx.names.trunc(), Self::trunc, 1, realm);

        cx.heap.alloc(object).into()
    }

    // 21.3.2.1 Math.abs
    fn abs(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let n = maybe!(to_number(cx, get_argument(arguments, 0)));

        if n.is_smi() {
            Value::smi(i32::abs(n.as_smi())).into()
        } else {
            Value::number(f64::abs(n.as_double())).into()
        }
    }

    // 21.3.2.10 Math.ceil
    fn ceil(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let n = maybe!(to_number(cx, get_argument(arguments, 0)));

        if n.is_smi() {
            n.into()
        } else {
            Value::number(f64::ceil(n.as_double())).into()
        }
    }

    // 21.3.2.16 Math.floor
    fn floor(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let n = maybe!(to_number(cx, get_argument(arguments, 0)));

        if n.is_smi() {
            n.into()
        } else {
            Value::number(f64::floor(n.as_double())).into()
        }
    }

    // 21.3.2.17 Math.fround
    fn fround(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let n = maybe!(to_number(cx, get_argument(arguments, 0)));
        Value::number((n.as_number() as f32) as f64).into()
    }

    // 21.3.2.24 Math.max
    fn max(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut highest = Value::number(f64::NEG_INFINITY);
        let mut found_nan = false;

        for arg in arguments {
            let n = maybe!(to_number(cx, *arg));

            if found_nan || n.is_nan() {
                if !found_nan {
                    highest = n;
                }

                found_nan = true;
                continue;
            }

            if n.is_positive_zero() && highest.is_negative_zero() {
                highest = n;
            }

            if highest.as_number() < n.as_number() {
                highest = n;
            }
        }

        highest.into()
    }

    // 21.3.2.25 Math.min
    fn min(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut lowest = Value::number(f64::INFINITY);
        let mut found_nan = false;

        for arg in arguments {
            let n = maybe!(to_number(cx, *arg));

            if found_nan || n.is_nan() {
                if !found_nan {
                    lowest = n;
                }

                found_nan = true;
                continue;
            }

            if n.is_negative_zero() && lowest.is_positive_zero() {
                lowest = n;
            }

            if n.as_number() < lowest.as_number() {
                lowest = n;
            }
        }

        lowest.into()
    }

    // 21.3.2.26 Math.pow
    fn pow(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let base = maybe!(to_number(cx, get_argument(arguments, 0)));
        let exponent = maybe!(to_number(cx, get_argument(arguments, 1)));

        number_exponentiate(base.as_number(), exponent.as_number()).into()
    }

    // 21.3.2.28 Math.round
    fn round(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let n = maybe!(to_number(cx, get_argument(arguments, 0)));

        if n.is_smi() {
            n.into()
        } else {
            let n = n.as_double();

            // Unlike rust's f64::round, Math.round always round ties up, even for negative numbers
            let rounded = if n >= 0.0 || n.fract() != -0.5 {
                f64::round(n)
            } else {
                f64::ceil(n)
            };

            Value::number(rounded).into()
        }
    }

    // 21.3.2.29 Math.sign
    fn sign(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let n = maybe!(to_number(cx, get_argument(arguments, 0)));

        let value = if n.is_smi() {
            let n_smi = n.as_smi();
            if n_smi == 0 {
                n
            } else if n_smi > 0 {
                Value::smi(1)
            } else {
                Value::smi(-1)
            }
        } else {
            if n.is_negative_zero() || n.is_positive_zero() {
                n
            } else if n.as_double() > 0.0 {
                Value::smi(1)
            } else if n.is_nan() {
                Value::nan()
            } else {
                Value::smi(-1)
            }
        };

        value.into()
    }

    // 21.3.2.35 Math.trunc
    fn trunc(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let n = maybe!(to_number(cx, get_argument(arguments, 0)));

        if n.is_smi() {
            n.into()
        } else {
            Value::number(f64::trunc(n.as_double())).into()
        }
    }
}
