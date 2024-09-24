use std::cmp::Ordering;

use super::{type_utilities::is_integral_number, Value};

/// Number::exponentiate (https://tc39.es/ecma262/#sec-numeric-types-number-exponentiate)
pub fn number_exponentiate(base: f64, exponent: f64) -> f64 {
    if exponent.is_nan() {
        return f64::NAN;
    } else if exponent == 0.0 {
        return 1.0;
    }

    if base.is_nan() {
        return f64::NAN;
    } else if base.is_infinite() {
        return if base == f64::INFINITY {
            if exponent > 0.0 {
                f64::INFINITY
            } else {
                0.0
            }
        } else {
            let is_exponent_integral_value = is_integral_number(Value::number(exponent));
            let is_exponent_odd = exponent % 2.0 != 0.0;
            let is_exponent_odd_integral_value = is_exponent_odd && is_exponent_integral_value;

            if exponent > 0.0 {
                if is_exponent_odd_integral_value {
                    f64::NEG_INFINITY
                } else {
                    f64::INFINITY
                }
            } else {
                if is_exponent_odd_integral_value {
                    -0.0
                } else {
                    0.0
                }
            }
        };
    } else if base == 0.0 {
        return if base.is_sign_positive() {
            if exponent > 0.0 {
                0.0
            } else {
                f64::INFINITY
            }
        } else {
            let is_exponent_integral_value = is_integral_number(Value::number(exponent));
            let is_exponent_odd = exponent % 2.0 != 0.0;
            let is_exponent_odd_integral_value = is_exponent_odd && is_exponent_integral_value;

            if exponent > 0.0 {
                if is_exponent_odd_integral_value {
                    -0.0
                } else {
                    0.0
                }
            } else {
                if is_exponent_odd_integral_value {
                    f64::NEG_INFINITY
                } else {
                    f64::INFINITY
                }
            }
        };
    }

    if exponent.is_infinite() {
        let abs_base = base.abs();

        return if exponent == f64::INFINITY {
            match abs_base.partial_cmp(&1.0) {
                Some(Ordering::Greater) => f64::INFINITY,
                Some(Ordering::Equal) | None => f64::NAN,
                Some(Ordering::Less) => 0.0,
            }
        } else {
            match abs_base.partial_cmp(&1.0) {
                Some(Ordering::Greater) => 0.0,
                Some(Ordering::Equal) | None => f64::NAN,
                Some(Ordering::Less) => f64::INFINITY,
            }
        };
    }

    if base < 0.0 && !is_integral_number(Value::number(exponent)) {
        return f64::NAN;
    }

    base.powf(exponent)
}
