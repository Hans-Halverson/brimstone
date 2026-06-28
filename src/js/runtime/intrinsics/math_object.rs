use rand::Rng;
use xsum::{Xsum, XsumAuto};

use crate::{
    common::{
        math::{f64_to_f16, is_negative_zero},
        numeric::{MAX_I32_PLUS_ONE_AS_F64, MAX_SAFE_INTEGER_U64},
    },
    intrinsic_methods,
    runtime::{
        Context, Handle,
        alloc_error::AllocResult,
        error::{range_error, type_error},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::intrinsics::Intrinsic,
        iterator::{IteratorHint, get_iterator, iterator_close, iterator_step_value},
        numeric_operations::number_exponentiate,
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::{to_number, to_uint32},
        value::Value,
    },
    runtime_fn,
};

/// The Math Object (https://tc39.es/ecma262/#sec-math-object)
pub struct MathObject;

impl MathObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut math = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        intrinsic_methods!(cx, math, {
            abs         MathObject_abs         (1),
            acos        MathObject_acos        (1),
            acosh       MathObject_acosh       (1),
            asin        MathObject_asin        (1),
            asinh       MathObject_asinh       (1),
            atan        MathObject_atan        (1),
            atanh       MathObject_atanh       (1),
            atan2       MathObject_atan2       (2),
            cbrt        MathObject_cbrt        (1),
            ceil        MathObject_ceil        (1),
            clz32       MathObject_clz32       (1),
            cos         MathObject_cos         (1),
            cosh        MathObject_cosh        (1),
            exp         MathObject_exp         (1),
            expm1       MathObject_expm1       (1),
            f16_round   MathObject_f16_round   (1),
            floor       MathObject_floor       (1),
            fround      MathObject_fround      (1),
            hypot       MathObject_hypot       (2),
            imul        MathObject_imul        (2),
            log         MathObject_log         (1),
            log1p       MathObject_log1p       (1),
            log10       MathObject_log10       (1),
            log2        MathObject_log2        (1),
            max         MathObject_max         (2),
            min         MathObject_min         (2),
            pow         MathObject_pow         (2),
            random      MathObject_random      (0),
            round       MathObject_round       (1),
            sign        MathObject_sign        (1),
            sin         MathObject_sin         (1),
            sinh        MathObject_sinh        (1),
            sqrt        MathObject_sqrt        (1),
            sum_precise MathObject_sum_precise (1),
            tan         MathObject_tan         (1),
            tanh        MathObject_tanh        (1),
            trunc       MathObject_trunc       (1),
        });

        // Value Properties of the Math Object (https://tc39.es/ecma262/#sec-value-properties-of-the-math-object)
        math.frozen(cx.names.e(), cx.number(std::f64::consts::E))?;
        math.frozen(cx.names.ln10(), cx.number(std::f64::consts::LN_10))?;
        math.frozen(cx.names.ln2(), cx.number(std::f64::consts::LN_2))?;
        math.frozen(cx.names.log10e(), cx.number(std::f64::consts::LOG10_E))?;
        math.frozen(cx.names.log2e(), cx.number(std::f64::consts::LOG2_E))?;
        math.frozen(cx.names.pi(), cx.number(std::f64::consts::PI))?;
        math.frozen(cx.names.sqrt1_2(), cx.number(std::f64::consts::FRAC_1_SQRT_2))?;
        math.frozen(cx.names.sqrt2(), cx.number(std::f64::consts::SQRT_2))?;

        // Math [ @@toStringTag ] (https://tc39.es/ecma262/#sec-math-%symbol.tostringtag%)
        math.to_string_tag(cx.names.math())?;

        math.build()
    }

    runtime_fn! {
    /// Math.abs (https://tc39.es/ecma262/#sec-math.abs)
    fn abs(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;

        if n.is_smi() {
            if let Some(smi) = n.as_smi().checked_abs() {
                return Ok(cx.smi(smi));
            } else {
                // Only possible if i32::MIN which is larger than an i32 can represent
                return Ok(cx.number(MAX_I32_PLUS_ONE_AS_F64));
            }
        }

        Ok(cx.number(f64::abs(n.as_double())))
    }}

    runtime_fn! {
    /// Math.acos (https://tc39.es/ecma262/#sec-math.acos)
    fn acos(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::acos(n.as_number())))
    }}

    runtime_fn! {
    /// Math.acosh (https://tc39.es/ecma262/#sec-math.acosh)
    fn acosh(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::acosh(n.as_number())))
    }}

    runtime_fn! {
    /// Math.asin (https://tc39.es/ecma262/#sec-math.asin)
    fn asin(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::asin(n.as_number())))
    }}

    runtime_fn! {
    /// Math.asinh (https://tc39.es/ecma262/#sec-math.asinh)
    fn asinh(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::asinh(n.as_number())))
    }}

    runtime_fn! {
    /// Math.atan (https://tc39.es/ecma262/#sec-math.atan)
    fn atan(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::atan(n.as_number())))
    }}

    runtime_fn! {
    /// Math.atanh (https://tc39.es/ecma262/#sec-math.atanh)
    fn atanh(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::atanh(n.as_number())))
    }}

    runtime_fn! {
    /// Math.atan2 (https://tc39.es/ecma262/#sec-math.atan2)
    fn atan2(cx, _, arguments) {
        let y_arg = arguments.get(cx, 0);
        let y = to_number(cx, y_arg)?;

        let x_arg = arguments.get(cx, 1);
        let x = to_number(cx, x_arg)?;

        Ok(cx.number(y.as_number().atan2(x.as_number())))
    }}

    runtime_fn! {
    /// Math.cbrt (https://tc39.es/ecma262/#sec-math.cbrt)
    fn cbrt(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::cbrt(n.as_number())))
    }}

    runtime_fn! {
    /// Math.ceil (https://tc39.es/ecma262/#sec-math.ceil)
    fn ceil(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;

        if n.is_smi() {
            Ok(n)
        } else {
            Ok(cx.number(f64::ceil(n.as_double())))
        }
    }}

    runtime_fn! {
    /// Math.clz32 (https://tc39.es/ecma262/#sec-math.clz32)
    fn clz32(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_uint32(cx, argument)?;
        Ok(cx.smi(n.leading_zeros() as u8))
    }}

    runtime_fn! {
    /// Math.cos (https://tc39.es/ecma262/#sec-math.cos)
    fn cos(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::cos(n.as_number())))
    }}

    runtime_fn! {
    /// Math.cosh (https://tc39.es/ecma262/#sec-math.cosh)
    fn cosh(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::cosh(n.as_number())))
    }}

    runtime_fn! {
    /// Math.exp (https://tc39.es/ecma262/#sec-math.exp)
    fn exp(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::exp(n.as_number())))
    }}

    runtime_fn! {
    /// Math.expm1 (https://tc39.es/ecma262/#sec-math.expm1)
    fn expm1(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::exp_m1(n.as_number())))
    }}

    runtime_fn! {
    /// Math.f16Round (https://tc39.es/ecma262/#sec-math.f16round)
    fn f16_round(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;

        if n.is_nan() || n.is_zero() || n.is_infinity() {
            return Ok(n);
        }

        let rounded_f16 = f64_to_f16(n.as_number());

        Ok(cx.number(rounded_f16.to_f64()))
    }}

    runtime_fn! {
    /// Math.floor (https://tc39.es/ecma262/#sec-math.floor)
    fn floor(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;

        if n.is_smi() {
            Ok(n)
        } else {
            Ok(cx.number(f64::floor(n.as_double())))
        }
    }}

    runtime_fn! {
    /// Math.fround (https://tc39.es/ecma262/#sec-math.fround)
    fn fround(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number((n.as_number() as f32) as f64))
    }}

    runtime_fn! {
    /// Math.hypot (https://tc39.es/ecma262/#sec-math.hypot)
    fn hypot(cx, _, arguments) {
        let mut sum = Value::smi(0);
        let mut has_infinity: bool = false;
        let mut has_nan: bool = false;

        for arg in arguments.iter() {
            let n = *to_number(cx, *arg)?;

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
            return Ok(sum.to_handle(cx));
        }

        Ok(cx.number(f64::sqrt(sum.as_number())))
    }}

    runtime_fn! {
    /// Math.imul (https://tc39.es/ecma262/#sec-math.imul)
    fn imul(cx, _, arguments) {
        let x_arg = arguments.get(cx, 0);
        let x = to_uint32(cx, x_arg)?;

        let y_arg = arguments.get(cx, 1);
        let y = to_uint32(cx, y_arg)?;

        let mod_mul = ((x as u64) * (y as u64)) as u32;

        Ok(cx.smi(mod_mul as i32))
    }}

    runtime_fn! {
    /// Math.log (https://tc39.es/ecma262/#sec-math.log)
    fn log(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::ln(n.as_number())))
    }}

    runtime_fn! {
    /// Math.log1p (https://tc39.es/ecma262/#sec-math.log1p)
    fn log1p(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::ln_1p(n.as_number())))
    }}

    runtime_fn! {
    /// Math.log10 (https://tc39.es/ecma262/#sec-math.log10)
    fn log10(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::log10(n.as_number())))
    }}

    runtime_fn! {
    /// Math.log2 (https://tc39.es/ecma262/#sec-math.log2)
    fn log2(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::log2(n.as_number())))
    }}

    runtime_fn! {
    /// Math.max (https://tc39.es/ecma262/#sec-math.max)
    fn max(cx, _, arguments) {
        let mut highest = Value::number(f64::NEG_INFINITY);
        let mut found_nan = false;

        for arg in arguments.iter() {
            let n = *to_number(cx, *arg)?;

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

        Ok(highest.to_handle(cx))
    }}

    runtime_fn! {
    /// Math.min (https://tc39.es/ecma262/#sec-math.min)
    fn min(cx, _, arguments) {
        let mut lowest = Value::number(f64::INFINITY);
        let mut found_nan = false;

        for arg in arguments.iter() {
            let n = *to_number(cx, *arg)?;

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

        Ok(lowest.to_handle(cx))
    }}

    runtime_fn! {
    /// Math.pow (https://tc39.es/ecma262/#sec-math.pow)
    fn pow(cx, _, arguments) {
        let base_arg = arguments.get(cx, 0);
        let base = to_number(cx, base_arg)?;

        let exponent_arg = arguments.get(cx, 1);
        let exponent = to_number(cx, exponent_arg)?;

        Ok(cx.number(number_exponentiate(base.as_number(), exponent.as_number())))
    }}

    runtime_fn! {
    /// Math.random (https://tc39.es/ecma262/#sec-math.random)
    fn random(cx, _, _) {
        let n = cx.rand.r#gen::<f64>();
        Ok(cx.number(n))
    }}

    runtime_fn! {
    /// Math.round (https://tc39.es/ecma262/#sec-math.round)
    fn round(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;

        if n.is_smi() {
            Ok(n)
        } else {
            let n = n.as_double();

            // Unlike rust's f64::round, Math.round always round ties up, even for negative numbers
            let rounded = if n >= 0.0 || n.fract() != -0.5 {
                f64::round(n)
            } else {
                f64::ceil(n)
            };

            Ok(cx.number(rounded))
        }
    }}

    runtime_fn! {
    /// Math.sign (https://tc39.es/ecma262/#sec-math.sign)
    fn sign(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = *to_number(cx, argument)?;

        if n.is_smi() {
            let n_smi = n.as_smi();
            if n_smi == 0 {
                Ok(n.to_handle(cx))
            } else if n_smi > 0 {
                Ok(cx.one())
            } else {
                Ok(cx.negative_one())
            }
        } else {
            if n.is_negative_zero() || n.is_positive_zero() {
                Ok(n.to_handle(cx))
            } else if n.as_double() > 0.0 {
                Ok(cx.one())
            } else if n.is_nan() {
                Ok(cx.nan())
            } else {
                Ok(cx.negative_one())
            }
        }
    }}

    runtime_fn! {
    /// Math.sin (https://tc39.es/ecma262/#sec-math.sin)
    fn sin(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::sin(n.as_number())))
    }}

    runtime_fn! {
    /// Math.sinh (https://tc39.es/ecma262/#sec-math.sinh)
    fn sinh(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::sinh(n.as_number())))
    }}

    runtime_fn! {
    /// Math.sqrt (https://tc39.es/ecma262/#sec-math.sqrt)
    fn sqrt(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::sqrt(n.as_number())))
    }}

    runtime_fn! {
    /// Math.sumPrecise (https://tc39.es/proposal-math-sum/)
    fn sum_precise(cx, _, arguments) {
        let items_arg = arguments.get(cx, 0);

        if !items_arg.is_object() {
            return type_error(cx, "Math.sumPrecise argument must be an object");
        }

        let mut iterator = get_iterator(cx, items_arg, IteratorHint::Sync, None)?;

        let mut sum = XsumAuto::new();
        let mut count: u64 = 0;
        let mut state = SumPreciseState::NegativeZero;

        while let Some(next_value) = iterator_step_value(cx, &mut iterator)? {
            if count >= MAX_SAFE_INTEGER_U64 {
                let error =
                    range_error(cx, "Math.sumPrecise iterator exceeded maximum safe integer");
                return iterator_close(cx, iterator.iterator, error);
            }

            count += 1;

            if !next_value.is_number() {
                let error = type_error(cx, "Math.sumPrecise iterator value must be a number");
                return iterator_close(cx, iterator.iterator, error);
            }

            let next_number = next_value.as_number();

            if state == SumPreciseState::NaN {
                continue;
            }

            if next_number.is_nan() {
                state = SumPreciseState::NaN;
            } else if next_number == f64::INFINITY {
                if state == SumPreciseState::NegativeInfinity {
                    state = SumPreciseState::NaN;
                } else {
                    state = SumPreciseState::PositiveInfinity;
                }
            } else if next_number == f64::NEG_INFINITY {
                if state == SumPreciseState::PositiveInfinity {
                    state = SumPreciseState::NaN;
                } else {
                    state = SumPreciseState::NegativeInfinity;
                }
            } else if !is_negative_zero(next_number)
                && (state == SumPreciseState::NegativeZero || state == SumPreciseState::Finite)
            {
                state = SumPreciseState::Finite;
                sum.add(next_number);
            }
        }

        match state {
            SumPreciseState::Finite => Ok(cx.number(sum.sum())),
            SumPreciseState::PositiveInfinity => Ok(cx.number(f64::INFINITY)),
            SumPreciseState::NegativeInfinity => Ok(cx.number(f64::NEG_INFINITY)),
            SumPreciseState::NegativeZero => Ok(cx.number(-0.0)),
            SumPreciseState::NaN => Ok(cx.nan()),
        }
    }}

    runtime_fn! {
    /// Math.tan (https://tc39.es/ecma262/#sec-math.tan)
    fn tan(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::tan(n.as_number())))
    }}

    runtime_fn! {
    /// Math.tanh (https://tc39.es/ecma262/#sec-math.tanh)
    fn tanh(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;
        Ok(cx.number(f64::tanh(n.as_number())))
    }}

    runtime_fn! {
    /// Math.trunc (https://tc39.es/ecma262/#sec-math.trunc)
    fn trunc(cx, _, arguments) {
        let argument = arguments.get(cx, 0);
        let n = to_number(cx, argument)?;

        if n.is_smi() {
            Ok(n)
        } else {
            Ok(cx.number(f64::trunc(n.as_double())))
        }
    }}
}

#[derive(PartialEq)]
enum SumPreciseState {
    Finite,
    PositiveInfinity,
    NegativeInfinity,
    NegativeZero,
    NaN,
}
