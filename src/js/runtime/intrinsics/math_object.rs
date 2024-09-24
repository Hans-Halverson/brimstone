use rand::Rng;

use crate::{
    js::runtime::{
        completion::EvalResult,
        function::get_argument,
        numeric_operations::number_exponentiate,
        object_value::ObjectValue,
        property::Property,
        realm::Realm,
        type_utilities::{to_number, to_uint32},
        value::Value,
        Context, Handle,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

/// The Math Object (https://tc39.es/ecma262/#sec-math-object)
pub struct MathObject;

impl MathObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Value Properties of the Math Object (https://tc39.es/ecma262/#sec-value-properties-of-the-math-object)
        let e_value = Value::number(std::f64::consts::E).to_handle(cx);
        object.intrinsic_frozen_property(cx, cx.names.e(), e_value);

        let ln_10_value = Value::number(std::f64::consts::LN_10).to_handle(cx);
        object.intrinsic_frozen_property(cx, cx.names.ln10(), ln_10_value);

        let ln_2_value = Value::number(std::f64::consts::LN_2).to_handle(cx);
        object.intrinsic_frozen_property(cx, cx.names.ln2(), ln_2_value);

        let log10_e_value = Value::number(std::f64::consts::LOG10_E).to_handle(cx);
        object.intrinsic_frozen_property(cx, cx.names.log10e(), log10_e_value);

        let log2_e_value = Value::number(std::f64::consts::LOG2_E).to_handle(cx);
        object.intrinsic_frozen_property(cx, cx.names.log2e(), log2_e_value);

        let pi_value = Value::number(std::f64::consts::PI).to_handle(cx);
        object.intrinsic_frozen_property(cx, cx.names.pi(), pi_value);

        let sqrt1_2_value = Value::number(std::f64::consts::FRAC_1_SQRT_2).to_handle(cx);
        object.intrinsic_frozen_property(cx, cx.names.sqrt1_2(), sqrt1_2_value);

        let sqrt_2_value = Value::number(std::f64::consts::SQRT_2).to_handle(cx);
        object.intrinsic_frozen_property(cx, cx.names.sqrt2(), sqrt_2_value);

        // Math [ @@toStringTag ] (https://tc39.es/ecma262/#sec-math-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let math_name_value = cx.names.math().as_string().into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(math_name_value, false, false, true),
        );

        object.intrinsic_func(cx, cx.names.abs(), Self::abs, 1, realm);
        object.intrinsic_func(cx, cx.names.acos(), Self::acos, 1, realm);
        object.intrinsic_func(cx, cx.names.acosh(), Self::acosh, 1, realm);
        object.intrinsic_func(cx, cx.names.asin(), Self::asin, 1, realm);
        object.intrinsic_func(cx, cx.names.asinh(), Self::asinh, 1, realm);
        object.intrinsic_func(cx, cx.names.atan(), Self::atan, 1, realm);
        object.intrinsic_func(cx, cx.names.atanh(), Self::atanh, 1, realm);
        object.intrinsic_func(cx, cx.names.atan2(), Self::atan2, 2, realm);
        object.intrinsic_func(cx, cx.names.cbrt(), Self::cbrt, 1, realm);
        object.intrinsic_func(cx, cx.names.ceil(), Self::ceil, 1, realm);
        object.intrinsic_func(cx, cx.names.clz32(), Self::clz32, 1, realm);
        object.intrinsic_func(cx, cx.names.cos(), Self::cos, 1, realm);
        object.intrinsic_func(cx, cx.names.cosh(), Self::cosh, 1, realm);
        object.intrinsic_func(cx, cx.names.exp(), Self::exp, 1, realm);
        object.intrinsic_func(cx, cx.names.expm1(), Self::expm1, 1, realm);
        object.intrinsic_func(cx, cx.names.floor(), Self::floor, 1, realm);
        object.intrinsic_func(cx, cx.names.fround(), Self::fround, 1, realm);
        object.intrinsic_func(cx, cx.names.hypot(), Self::hypot, 2, realm);
        object.intrinsic_func(cx, cx.names.imul(), Self::imul, 2, realm);
        object.intrinsic_func(cx, cx.names.log(), Self::log, 1, realm);
        object.intrinsic_func(cx, cx.names.log1p(), Self::log1p, 1, realm);
        object.intrinsic_func(cx, cx.names.log10(), Self::log10, 1, realm);
        object.intrinsic_func(cx, cx.names.log2(), Self::log2, 1, realm);
        object.intrinsic_func(cx, cx.names.max(), Self::max, 2, realm);
        object.intrinsic_func(cx, cx.names.min(), Self::min, 2, realm);
        object.intrinsic_func(cx, cx.names.pow(), Self::pow, 2, realm);
        object.intrinsic_func(cx, cx.names.random(), Self::random, 0, realm);
        object.intrinsic_func(cx, cx.names.round(), Self::round, 1, realm);
        object.intrinsic_func(cx, cx.names.sign(), Self::sign, 1, realm);
        object.intrinsic_func(cx, cx.names.sin(), Self::sin, 1, realm);
        object.intrinsic_func(cx, cx.names.sinh(), Self::sinh, 1, realm);
        object.intrinsic_func(cx, cx.names.sqrt(), Self::sqrt, 1, realm);
        object.intrinsic_func(cx, cx.names.tan(), Self::tan, 1, realm);
        object.intrinsic_func(cx, cx.names.tanh(), Self::tanh, 1, realm);
        object.intrinsic_func(cx, cx.names.trunc(), Self::trunc, 1, realm);

        object
    }

    /// Math.abs (https://tc39.es/ecma262/#sec-math.abs)
    pub fn abs(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));

        if n.is_smi() {
            Value::smi(i32::abs(n.as_smi())).to_handle(cx).into()
        } else {
            Value::number(f64::abs(n.as_double())).to_handle(cx).into()
        }
    }

    /// Math.acos (https://tc39.es/ecma262/#sec-math.acos)
    pub fn acos(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::acos(n.as_number())).to_handle(cx).into()
    }

    /// Math.acosh (https://tc39.es/ecma262/#sec-math.acosh)
    pub fn acosh(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::acosh(n.as_number()))
            .to_handle(cx)
            .into()
    }

    /// Math.asin (https://tc39.es/ecma262/#sec-math.asin)
    pub fn asin(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::asin(n.as_number())).to_handle(cx).into()
    }

    /// Math.asinh (https://tc39.es/ecma262/#sec-math.asinh)
    pub fn asinh(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::asinh(n.as_number()))
            .to_handle(cx)
            .into()
    }

    /// Math.atan (https://tc39.es/ecma262/#sec-math.atan)
    pub fn atan(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::atan(n.as_number())).to_handle(cx).into()
    }

    /// Math.atanh (https://tc39.es/ecma262/#sec-math.atanh)
    pub fn atanh(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::atanh(n.as_number()))
            .to_handle(cx)
            .into()
    }

    /// Math.atan2 (https://tc39.es/ecma262/#sec-math.atan2)
    pub fn atan2(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let y_arg = get_argument(cx, arguments, 0);
        let y = maybe!(to_number(cx, y_arg));

        let x_arg = get_argument(cx, arguments, 1);
        let x = maybe!(to_number(cx, x_arg));

        Value::number(y.as_number().atan2(x.as_number()))
            .to_handle(cx)
            .into()
    }

    /// Math.cbrt (https://tc39.es/ecma262/#sec-math.cbrt)
    pub fn cbrt(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::cbrt(n.as_number())).to_handle(cx).into()
    }

    /// Math.ceil (https://tc39.es/ecma262/#sec-math.ceil)
    pub fn ceil(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));

        if n.is_smi() {
            n.into()
        } else {
            Value::number(f64::ceil(n.as_double())).to_handle(cx).into()
        }
    }

    /// Math.clz32 (https://tc39.es/ecma262/#sec-math.clz32)
    pub fn clz32(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_uint32(cx, argument));
        Value::smi(n.leading_zeros() as i32).to_handle(cx).into()
    }

    /// Math.cos (https://tc39.es/ecma262/#sec-math.cos)
    pub fn cos(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::cos(n.as_number())).to_handle(cx).into()
    }

    /// Math.cosh (https://tc39.es/ecma262/#sec-math.cosh)
    pub fn cosh(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::cosh(n.as_number())).to_handle(cx).into()
    }

    /// Math.exp (https://tc39.es/ecma262/#sec-math.exp)
    pub fn exp(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::exp(n.as_number())).to_handle(cx).into()
    }

    /// Math.expm1 (https://tc39.es/ecma262/#sec-math.expm1)
    pub fn expm1(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::exp_m1(n.as_number()))
            .to_handle(cx)
            .into()
    }

    /// Math.floor (https://tc39.es/ecma262/#sec-math.floor)
    pub fn floor(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));

        if n.is_smi() {
            n.into()
        } else {
            Value::number(f64::floor(n.as_double()))
                .to_handle(cx)
                .into()
        }
    }

    /// Math.fround (https://tc39.es/ecma262/#sec-math.fround)
    pub fn fround(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number((n.as_number() as f32) as f64)
            .to_handle(cx)
            .into()
    }

    /// Math.hypot (https://tc39.es/ecma262/#sec-math.hypot)
    pub fn hypot(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let mut sum = Value::smi(0);
        let mut has_infinity: bool = false;
        let mut has_nan: bool = false;

        for arg in arguments {
            let n = maybe!(to_number(cx, *arg)).get();

            if has_infinity {
                continue;
            }

            if n.is_infinity() {
                sum = Value::number(f64::INFINITY);
                has_infinity = true;
                continue;
            }

            if has_nan {
                continue;
            }

            if n.is_nan() {
                sum = n;
                has_nan = true;
                continue;
            }

            let n_f64 = n.as_number();
            sum = Value::number(sum.as_number() + n_f64 * n_f64);
        }

        if has_infinity || has_nan {
            return sum.to_handle(cx).into();
        }

        Value::number(f64::sqrt(sum.as_number()))
            .to_handle(cx)
            .into()
    }

    /// Math.imul (https://tc39.es/ecma262/#sec-math.imul)
    pub fn imul(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let x_arg = get_argument(cx, arguments, 0);
        let x = maybe!(to_uint32(cx, x_arg));

        let y_arg = get_argument(cx, arguments, 1);
        let y = maybe!(to_uint32(cx, y_arg));

        let mod_mul = ((x as u64) * (y as u64)) as u32;

        Value::smi(mod_mul as i32).to_handle(cx).into()
    }

    /// Math.log (https://tc39.es/ecma262/#sec-math.log)
    pub fn log(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::ln(n.as_number())).to_handle(cx).into()
    }

    /// Math.log1p (https://tc39.es/ecma262/#sec-math.log1p)
    pub fn log1p(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::ln_1p(n.as_number()))
            .to_handle(cx)
            .into()
    }

    /// Math.log10 (https://tc39.es/ecma262/#sec-math.log10)
    pub fn log10(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::log10(n.as_number()))
            .to_handle(cx)
            .into()
    }

    /// Math.log2 (https://tc39.es/ecma262/#sec-math.log2)
    pub fn log2(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::log2(n.as_number())).to_handle(cx).into()
    }

    /// Math.max (https://tc39.es/ecma262/#sec-math.max)
    pub fn max(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let mut highest = Value::number(f64::NEG_INFINITY);
        let mut found_nan = false;

        for arg in arguments {
            let n = maybe!(to_number(cx, *arg)).get();

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

        highest.to_handle(cx).into()
    }

    /// Math.min (https://tc39.es/ecma262/#sec-math.min)
    pub fn min(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let mut lowest = Value::number(f64::INFINITY);
        let mut found_nan = false;

        for arg in arguments {
            let n = maybe!(to_number(cx, *arg)).get();

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

        lowest.to_handle(cx).into()
    }

    /// Math.pow (https://tc39.es/ecma262/#sec-math.pow)
    pub fn pow(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let base_arg = get_argument(cx, arguments, 0);
        let base = maybe!(to_number(cx, base_arg));

        let exponent_arg = get_argument(cx, arguments, 1);
        let exponent = maybe!(to_number(cx, exponent_arg));

        Value::from(number_exponentiate(base.as_number(), exponent.as_number()))
            .to_handle(cx)
            .into()
    }

    /// Math.random (https://tc39.es/ecma262/#sec-math.random)
    pub fn random(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let n = rand::thread_rng().gen::<f64>();
        Value::number(n).to_handle(cx).into()
    }

    /// Math.round (https://tc39.es/ecma262/#sec-math.round)
    pub fn round(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));

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

            Value::number(rounded).to_handle(cx).into()
        }
    }

    /// Math.sign (https://tc39.es/ecma262/#sec-math.sign)
    pub fn sign(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument)).get();

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

        value.to_handle(cx).into()
    }

    /// Math.sin (https://tc39.es/ecma262/#sec-math.sin)
    pub fn sin(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::sin(n.as_number())).to_handle(cx).into()
    }

    /// Math.sinh (https://tc39.es/ecma262/#sec-math.sinh)
    pub fn sinh(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::sinh(n.as_number())).to_handle(cx).into()
    }

    /// Math.sqrt (https://tc39.es/ecma262/#sec-math.sqrt)
    pub fn sqrt(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::sqrt(n.as_number())).to_handle(cx).into()
    }

    /// Math.tan (https://tc39.es/ecma262/#sec-math.tan)
    pub fn tan(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::tan(n.as_number())).to_handle(cx).into()
    }

    /// Math.tanh (https://tc39.es/ecma262/#sec-math.tanh)
    pub fn tanh(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));
        Value::number(f64::tanh(n.as_number())).to_handle(cx).into()
    }

    /// Math.trunc (https://tc39.es/ecma262/#sec-math.trunc)
    pub fn trunc(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let n = maybe!(to_number(cx, argument));

        if n.is_smi() {
            n.into()
        } else {
            Value::number(f64::trunc(n.as_double()))
                .to_handle(cx)
                .into()
        }
    }
}
