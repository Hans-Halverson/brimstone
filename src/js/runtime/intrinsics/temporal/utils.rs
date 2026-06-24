use std::str::FromStr;

use temporal_rs::{
    TemporalResult, TimeZone,
    error::ErrorKind,
    options::{DisplayCalendar, Overflow, RoundingIncrement, RoundingMode, Unit},
    parsers::Precision,
};

use crate::runtime::{
    Context, EvalResult, Handle, PropertyKey, Value,
    error::{range_error, syntax_error, type_error},
    get,
    numeric_constants::{MAX_I32_AS_F64, MAX_U8_AS_F64, MAX_U32_AS_F64, MIN_I32_AS_F64},
    object_value::ObjectValue,
    to_string,
    type_utilities::to_integer_with_truncation,
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
    let Some(option_value) = get_temporal_option_property(cx, options, option_name)? else {
        return Ok(T::default());
    };

    match parse_option_fom_string(cx, option_value)? {
        Some(parsed_value) => Ok(parsed_value),
        None => range_error(cx, &create_error()),
    }
}

fn get_temporal_option_property(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    option_name: Handle<PropertyKey>,
) -> EvalResult<Option<Handle<Value>>> {
    let Some(options) = options else {
        return Ok(None);
    };

    let option_value = get(cx, options, option_name)?;

    if option_value.is_undefined() {
        return Ok(None);
    }

    Ok(Some(option_value))
}

fn parse_option_fom_string<T: FromStr>(
    cx: Context,
    option_value: Handle<Value>,
) -> EvalResult<Option<T>> {
    let option_string = to_string(cx, option_value)?.to_wtf8_string()?;

    if let Ok(option_str) = str::from_utf8(option_string.as_bytes()) {
        if let Ok(parsed_value) = T::from_str(option_str) {
            return Ok(Some(parsed_value));
        }
    }

    Ok(None)
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

/// GetTemporalFractionalSecondDigitsOption (https://tc39.es/proposal-temporal/#sec-temporal-gettemporalfractionalseconddigitsoption)
pub fn get_fractional_second_digits_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<Precision> {
    let Some(option_value) =
        get_temporal_option_property(cx, options, cx.names.fractional_second_digits())?
    else {
        return Ok(Precision::Auto);
    };

    if !option_value.is_number() {
        let option_string = to_string(cx, option_value)?.flatten()?;
        if !option_string.eq_str("auto") {
            return range_error(
                cx,
                &format!("{method_name} fractionalSecondDigits option must be a number or 'auto'"),
            );
        }

        return Ok(Precision::Auto);
    }

    let option_number = option_value.as_number();
    if !option_number.is_finite() {
        return range_error(
            cx,
            &format!("{method_name} fractionalSecondDigits option must be a number from 0 to 9"),
        );
    }

    let option_count = option_number.floor();
    if !(0.0..=9.0).contains(&option_count) {
        return range_error(
            cx,
            &format!("{method_name} fractionalSecondDigits option must be a number from 0 to 9"),
        );
    }

    Ok(Precision::Digit(option_count as u8))
}

/// GetRoundingModeOption (https://tc39.es/proposal-temporal/#sec-temporal-getroundingmodeoption)
pub fn get_rounding_mode_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    default: RoundingMode,
    method_name: &str,
) -> EvalResult<RoundingMode> {
    let Some(option_value) = get_temporal_option_property(cx, options, cx.names.rounding_mode())?
    else {
        return Ok(default);
    };

    match parse_option_fom_string(cx, option_value)? {
        Some(parsed_value) => Ok(parsed_value),
        None => range_error(
            cx,
            &format!(
                "{method_name} rounding mode option must be 'ceil', 'floor', 'expand', 'trunc', 'halfCeil', 'halfFloor', 'halfExpand', 'halfTrunc', or 'halfEven'"
            ),
        ),
    }
}

// GetTemporalUnitValuedOption (https://tc39.es/proposal-temporal/#sec-temporal-gettemporalunitvaluedoption)
pub fn get_unit_valued_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<Option<Unit>> {
    let Some(option_value) = get_temporal_option_property(cx, options, cx.names.smallest_unit())?
    else {
        return Ok(None);
    };

    match parse_option_fom_string(cx, option_value)? {
        Some(parsed_value) => Ok(Some(parsed_value)),
        None => {
            range_error(cx, &format!("{method_name} smallest unit option must be a valid unit"))
        }
    }
}

/// GetRoundingIncrementOption (https://tc39.es/proposal-temporal/#sec-temporal-getroundingincrementoption)
pub fn get_rounding_increment_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<RoundingIncrement> {
    let Some(option_value) =
        get_temporal_option_property(cx, options, cx.names.rounding_increment())?
    else {
        return Ok(RoundingIncrement::ONE);
    };

    let option_f64 = to_integer_with_truncation(
        cx,
        option_value,
        &format!("{method_name} rounding increment option"),
    )?;

    if (0.0..=MAX_U32_AS_F64).contains(&option_f64) {
        let parsed_increment_result = RoundingIncrement::try_new(option_f64 as u32);
        return map_temporal_result(cx, parsed_increment_result, method_name);
    }

    range_error(
        cx,
        &format!("{method_name} rounding increment option must be a number from 1 to 10^9"),
    )
}

pub fn get_timezone_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<Option<TimeZone>> {
    match get_temporal_option_property(cx, options, cx.names.timezone())? {
        Some(option_value) => Ok(Some(to_timezone_identifier(cx, option_value, method_name)?)),
        None => Ok(None),
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

/// ToTemporalTimeZoneIdentifier (https://tc39.es/proposal-temporal/#sec-temporal-totemporaltimezoneidentifier)
pub fn to_timezone_identifier(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<TimeZone> {
    // TODO: Check if ZonedDateTime object

    if !value.is_string() {
        return type_error(cx, &format!("{method_name} time zone option must be a string"));
    }

    let wtf8_string = value.as_string().to_wtf8_string()?;

    if let Ok(option_str) = str::from_utf8(wtf8_string.as_bytes()) {
        let parsed_timezone_result = TimeZone::try_from_str(option_str);
        return map_temporal_result(cx, parsed_timezone_result, method_name);
    }

    range_error(cx, &format!("{method_name} time zone option must be a valid time zone"))
}
