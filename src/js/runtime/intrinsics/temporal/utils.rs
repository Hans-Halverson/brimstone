use std::str::FromStr;

use temporal_rs::{
    TemporalResult,
    error::ErrorKind,
    options::{DisplayCalendar, Overflow},
};

use crate::runtime::{
    Context, EvalResult, Handle, PropertyKey, Value,
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
) -> EvalResult<Option<Handle<ObjectValue>>> {
    if options.is_undefined() {
        return Ok(None);
    } else if !options.is_object() {
        return type_error(cx, &format!("{method_name} options argument must be an object"));
    }

    Ok(Some(options.as_object()))
}

/// Parse a temporal option from an optional options object. Parameterized by an option in the
/// `temporal_rs` crate.
pub fn get_temporal_option<T: Default + FromStr>(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    option_name: Handle<PropertyKey>,
    create_error: impl FnOnce() -> String,
) -> EvalResult<T> {
    let Some(options) = options else {
        return Ok(T::default());
    };

    let option_value = get(cx, options, option_name)?;

    if option_value.is_undefined() {
        return Ok(T::default());
    }

    let option_string = to_string(cx, option_value)?.to_wtf8_string()?;

    if let Ok(option_str) = str::from_utf8(option_string.as_bytes()) {
        if let Ok(parsed_value) = T::from_str(option_str) {
            return Ok(parsed_value);
        }
    }

    range_error(cx, &create_error())
}

/// GetTemporalOverflowOption (https://tc39.es/proposal-temporal/#sec-temporal-gettemporaloverflowoption)
pub fn get_overflow_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<Overflow> {
    get_temporal_option(cx, options, cx.names.overflow(), || {
        format!("{method_name} overflow option must be 'overflow' or 'reject'")
    })
}

/// GetTemporalShowCalendarNameOptions (https://tc39.es/proposal-temporal/#sec-temporal-gettemporalshowcalendarnameoption)
pub fn get_show_calendar_name_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<DisplayCalendar> {
    get_temporal_option(cx, options, cx.names.calendar_name(), || {
        format!(
            "{method_name} calendar name option must be 'auto', 'always', 'never', or 'critical'"
        )
    })
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
