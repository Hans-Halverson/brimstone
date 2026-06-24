use temporal_rs::{TemporalResult, error::ErrorKind, options::Overflow};

use crate::runtime::{
    Context, EvalResult, Handle, Value,
    error::{range_error, syntax_error, type_error},
    get,
    numeric_constants::{MAX_I32_AS_F64, MAX_U8_AS_F64, MIN_I32_AS_F64},
    object_value::ObjectValue,
    to_string,
};

/// Map a temporal result to an equivalent EvalResult.
pub fn map_temporal_result<T>(
    cx: Context,
    result: TemporalResult<T>,
    method_name: &str,
) -> EvalResult<T> {
    match result {
        Ok(value) => Ok(value),
        Err(temporal_error) => {
            let kind = temporal_error.kind();
            let message = format!("{method_name}: {}", temporal_error.into_message());

            match kind {
                ErrorKind::Range => range_error(cx, &message),
                ErrorKind::Syntax => syntax_error(cx, &message),
                ErrorKind::Type => type_error(cx, &message),
                ErrorKind::Generic | ErrorKind::Assert => panic!("Unsupported temporal error"),
            }
        }
    }
}

pub fn validate_options_object(
    cx: Context,
    options: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<ObjectValue>> {
    if !options.is_object() {
        return type_error(cx, &format!("{method_name} options argument must be an object"));
    }

    Ok(options.as_object())
}

/// GetTemporalOverflowOption (https://tc39.es/proposal-temporal/#sec-temporal-gettemporaloverflowoption)
pub fn get_overflow_option(
    cx: Context,
    options: Handle<ObjectValue>,
    method_name: &str,
) -> EvalResult<Overflow> {
    let overflow_value = get(cx, options, cx.names.overflow())?;

    if overflow_value.is_undefined() {
        return Ok(Overflow::default());
    }

    let overflow_string = to_string(cx, overflow_value)?.flatten()?;
    if overflow_string.eq_str("constrain") {
        Ok(Overflow::Constrain)
    } else if overflow_string.eq_str("reject") {
        Ok(Overflow::Reject)
    } else {
        range_error(cx, &format!("{method_name} overflow option must be 'overflow' or 'reject'"))
    }
}

pub fn clamp_year_arg_for_temporal_rs(
    cx: Context,
    year_arg: f64,
    method_name: &str,
) -> EvalResult<i32> {
    if !(MIN_I32_AS_F64..=MAX_I32_AS_F64).contains(&year_arg) {
        return range_error(cx, &format!("{method_name} year is invalid"));
    }

    Ok(year_arg as i32)
}

pub fn clamp_month_arg_for_temporal_rs(
    cx: Context,
    month_arg: f64,
    method_name: &str,
) -> EvalResult<u8> {
    if !(0.0..=MAX_U8_AS_F64).contains(&month_arg) {
        return range_error(cx, &format!("{method_name} month is invalid"));
    }

    Ok(month_arg as u8)
}
pub fn clamp_day_arg_for_temporal_rs(
    cx: Context,
    day_arg: f64,
    method_name: &str,
) -> EvalResult<u8> {
    if !(0.0..=MAX_U8_AS_F64).contains(&day_arg) {
        return range_error(cx, &format!("{method_name} day is invalid"));
    }

    Ok(day_arg as u8)
}
