use std::str::FromStr;

use num_traits::{AsPrimitive, PrimInt};
use temporal_rs::{
    Calendar, TemporalResult, TimeZone,
    error::ErrorKind,
    options::{DisplayCalendar, Overflow, RelativeTo, RoundingIncrement, RoundingMode, Unit},
    parsers::Precision,
    primitive::FiniteF64,
};

use crate::runtime::{
    Context, EvalResult, Handle, PropertyKey, Value,
    error::{range_error, syntax_error, type_error},
    get,
    object_value::ObjectValue,
    to_string,
    type_utilities::to_number,
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

    match parse_option_from_string(cx, option_value)? {
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

fn parse_option_from_string<T: FromStr>(
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
        format!("{method_name} `overflow` option must be 'constrain' or 'reject'")
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
            "{method_name} `calendarName` option must be 'auto', 'always', 'never', or 'critical'"
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
                &format!(
                    "{method_name} `fractionalSecondDigits` option must be a number or 'auto'"
                ),
            );
        }

        return Ok(Precision::Auto);
    }

    let option_number = option_value.as_number();
    if !option_number.is_finite() {
        return range_error(
            cx,
            &format!("{method_name} `fractionalSecondDigits` option must be a number from 0 to 9"),
        );
    }

    let option_count = option_number.floor();
    if !(0.0..=9.0).contains(&option_count) {
        return range_error(
            cx,
            &format!("{method_name} `fractionalSecondDigits` option must be a number from 0 to 9"),
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

    match parse_option_from_string(cx, option_value)? {
        Some(parsed_value) => Ok(parsed_value),
        None => range_error(
            cx,
            &format!(
                "{method_name} `roundingMode` option must be 'ceil', 'floor', 'expand', 'trunc', 'halfCeil', 'halfFloor', 'halfExpand', 'halfTrunc', or 'halfEven'"
            ),
        ),
    }
}

// GetTemporalUnitValuedOption (https://tc39.es/proposal-temporal/#sec-temporal-gettemporalunitvaluedoption)
pub fn get_unit_valued_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    option_name: Handle<PropertyKey>,
    method_name: &str,
) -> EvalResult<Option<Unit>> {
    let Some(option_value) = get_temporal_option_property(cx, options, option_name)? else {
        return Ok(None);
    };

    match parse_option_from_string(cx, option_value)? {
        Some(parsed_value) => Ok(Some(parsed_value)),
        None => range_error(
            cx,
            &format!("{method_name} `{}` option must be a valid unit", option_name.format()?),
        ),
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

    let option_number = to_integer_with_truncation(
        cx,
        option_value,
        &format!("{method_name} `roundingIncrement` option"),
    )?;

    let parsed_increment_result = RoundingIncrement::try_new(option_number);
    map_temporal_result(cx, parsed_increment_result, method_name)
}

/// GetTemporalRelativeToOption (https://tc39.es/proposal-temporal/#sec-temporal-gettemporalrelativetooption)
pub fn get_relative_to_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<Option<RelativeTo>> {
    let Some(option_value) = get_temporal_option_property(cx, options, cx.names.relative_to())?
    else {
        return Ok(None);
    };

    if option_value.is_object() {
        let option_object = option_value.as_object();
        if let Some(plain_date_object) = option_object.as_plain_date_object() {
            return Ok(Some(RelativeTo::PlainDate(plain_date_object.date().clone())));
        }

        // TODO: Check for PlainDateTime and ZonedDateTime

        unimplemented!("GetTemporalRelativeToOption for date-like object")
    } else if !option_value.is_string() {
        return type_error(
            cx,
            &format!("{method_name} `relativeTo` option must be a string or object"),
        );
    }

    let wtf8_string = option_value.as_string().to_wtf8_string()?;

    if let Ok(option_str) = str::from_utf8(wtf8_string.as_bytes()) {
        let parsed_option_result = RelativeTo::try_from_str(option_str);
        let parsed_option = map_temporal_result(cx, parsed_option_result, method_name)?;

        return Ok(Some(parsed_option));
    }

    range_error(cx, &format!("{method_name} `relativeTo` option must be valid"))
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

pub fn parse_calendar_argument(
    cx: Context,
    calendar_arg: Handle<Value>,
    method_name: &str,
) -> EvalResult<Calendar> {
    if calendar_arg.is_undefined() {
        return Ok(Calendar::ISO);
    } else if !calendar_arg.is_string() {
        return type_error(cx, &format!("{method_name} calendar argument must be a string"));
    }

    let wtf8_string = calendar_arg.as_string().to_wtf8_string()?;
    let parsed_calendar_result = Calendar::try_from_utf8(wtf8_string.as_bytes());
    map_temporal_result(cx, parsed_calendar_result, method_name)
}

/// ToIntegerIfIntegral (https://tc39.es/proposal-temporal/#sec-tointegerifintegral)
pub fn to_integer_if_integral<T: PrimInt + AsPrimitive<f64>>(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<T>
where
    f64: AsPrimitive<T>,
{
    if value.is_undefined() {
        return Ok(T::zero());
    }

    let number_value = to_number(cx, value)?.as_number();

    let integral_result =
        FiniteF64::try_from(number_value).and_then(|n| n.as_integer_if_integral());

    map_temporal_result(cx, integral_result, method_name)
}

/// ToIntegerWithTruncation (https://tc39.es/proposal-temporal/#sec-tointegerwithtruncation)
pub fn to_integer_with_truncation<T: PrimInt + AsPrimitive<f64>>(
    cx: Context,
    value: Handle<Value>,
    err_prefix: &str,
) -> EvalResult<T>
where
    f64: AsPrimitive<T>,
{
    let number_value = to_number(cx, value)?;
    let finite_value_result = FiniteF64::try_from(number_value.as_number());
    let finite_value = map_temporal_result(cx, finite_value_result, err_prefix)?;

    Ok(finite_value.as_integer_with_truncation())
}

pub fn to_integer_with_truncation_or_zero<T: PrimInt + AsPrimitive<f64>>(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<T>
where
    f64: AsPrimitive<T>,
{
    if value.is_undefined() {
        Ok(T::zero())
    } else {
        to_integer_with_truncation(cx, value, method_name)
    }
}

/// ToTemporalTimeZoneIdentifier (https://tc39.es/proposal-temporal/#sec-temporal-totemporaltimezoneidentifier)
pub fn to_timezone_identifier(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<TimeZone> {
    // TODO: Check if ZonedDateTime object

    if !value.is_string() {
        return type_error(cx, &format!("{method_name} `timeZone` option must be a string"));
    }

    let wtf8_string = value.as_string().to_wtf8_string()?;

    if let Ok(option_str) = str::from_utf8(wtf8_string.as_bytes()) {
        let parsed_timezone_result = TimeZone::try_from_str(option_str);
        return map_temporal_result(cx, parsed_timezone_result, method_name);
    }

    range_error(cx, &format!("{method_name} `timeZone` option must be a valid time zone"))
}

/// IsPartialTemporalObject (https://tc39.es/proposal-temporal/#sec-temporal-ispartialtemporalobject)
pub fn is_partial_temporal_object(cx: Context, value: Handle<Value>) -> EvalResult<bool> {
    if !value.is_object() {
        return Ok(false);
    }

    let object = value.as_object();

    // TODO: Add all other Temporal objects

    if object.is_plain_date_object() || object.is_plain_time_object() {
        return Ok(false);
    }

    let calendar_value = get(cx, object, cx.names.calendar())?;
    if !calendar_value.is_undefined() {
        return Ok(false);
    }

    let time_zone_value = get(cx, object, cx.names.timezone())?;
    if !time_zone_value.is_undefined() {
        return Ok(false);
    }

    Ok(true)
}
