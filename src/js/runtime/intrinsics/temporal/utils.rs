use std::str::FromStr;

use num_bigint::BigInt;
use num_traits::{AsPrimitive, PrimInt, ToPrimitive};
use temporal_rs::{
    Calendar, TemporalResult, TimeZone,
    error::ErrorKind,
    options::{
        DifferenceSettings, Disambiguation, DisplayCalendar, DisplayOffset, DisplayTimeZone,
        OffsetDisambiguation, Overflow, RelativeTo, RoundingIncrement, RoundingMode, Unit,
    },
    parsers::Precision,
    primitive::FiniteF64,
};

use crate::{
    must,
    runtime::{
        Context, EvalResult, Handle, PropertyKey, Value,
        abstract_operations::create_data_property_or_throw,
        error::{range_error, syntax_error, type_error},
        get,
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create_without_proto,
        to_string,
        type_utilities::to_number,
    },
};

/// The diff operations `until` and `since`.
pub enum DiffOperation {
    Until,
    Since,
}

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

/// GetTemporalShowTimeZoneNameOption (https://tc39.es/proposal-temporal/#sec-temporal-gettemporalshowtimezoneoption)
pub fn get_show_time_zone_name_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<DisplayTimeZone> {
    get_temporal_option(cx, options, cx.names.time_zone_name(), || {
        format!(
            "{method_name} `timeZoneName` option must be 'auto', 'never', 'critical', or 'always'"
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
        if let Some(plain_date) = option_object.as_plain_date_object() {
            return Ok(Some(RelativeTo::PlainDate(plain_date.date().clone())));
        } else if let Some(plain_date_time) = option_object.as_plain_date_time_object() {
            return Ok(Some(RelativeTo::PlainDate(plain_date_time.date_time().to_plain_date())));
        } else if let Some(zoned_date_time) = option_object.as_zoned_date_time_object() {
            return Ok(Some(RelativeTo::ZonedDateTime(zoned_date_time.zoned_date_time().clone())));
        }

        let _ = get_calendar_identifier_with_iso_default(cx, option_object, method_name)?;

        // Otherwise treat as a "date-like" object
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

/// GetTemporalDisambiguationOption (https://tc39.es/proposal-temporal/#sec-temporal-gettemporaldisambiguationoption)
pub fn get_disambiguation_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<Disambiguation> {
    get_temporal_option(cx, options, cx.names.disambiguation(), || {
        format!(
            "{method_name} `disambiguation` option must be 'compatible', 'earlier', 'later', or 'reject'"
        )
    })
}

/// GetTemporalOffsetOption (https://tc39.es/proposal-temporal/#sec-temporal-gettemporaloffsetoption)
pub fn get_offset_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    default: OffsetDisambiguation,
    method_name: &str,
) -> EvalResult<OffsetDisambiguation> {
    let Some(option_value) = get_temporal_option_property(cx, options, cx.names.offset())? else {
        return Ok(default);
    };

    match parse_option_from_string(cx, option_value)? {
        Some(parsed_value) => Ok(parsed_value),
        None => range_error(
            cx,
            &format!(
                "{method_name} `offset` option must be 'prefer', 'use', 'ignore', or 'reject'"
            ),
        ),
    }
}

/// GetTemporalShowOffsetOption (https://tc39.es/proposal-temporal/#sec-temporal-gettemporalshowoffsetoption)
pub fn get_show_offset_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<DisplayOffset> {
    get_temporal_option(cx, options, cx.names.offset(), || {
        format!("{method_name} `offset` option must be 'auto', 'never', or 'always'")
    })
}

pub fn get_time_zone_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<Option<TimeZone>> {
    match get_temporal_option_property(cx, options, cx.names.time_zone())? {
        Some(option_value) => Ok(Some(to_time_zone_identifier(cx, option_value, method_name)?)),
        None => Ok(None),
    }
}

/// GetDifferenceSettings (https://tc39.es/proposal-temporal/#sec-temporal-getdifferencesettings)
pub fn get_difference_settings(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<DifferenceSettings> {
    let largest_unit = get_unit_valued_option(cx, options, cx.names.largest_unit(), method_name)?;
    let increment = get_rounding_increment_option(cx, options, method_name)?;
    let rounding_mode = get_rounding_mode_option(cx, options, RoundingMode::Trunc, method_name)?;
    let smallest_unit = get_unit_valued_option(cx, options, cx.names.smallest_unit(), method_name)?;

    let mut settings = DifferenceSettings::default();
    settings.largest_unit = largest_unit;
    settings.smallest_unit = smallest_unit;
    settings.increment = Some(increment);
    settings.rounding_mode = Some(rounding_mode);

    Ok(settings)
}

/// GetTemporalCalendarIdentifierWithISODefault (https://tc39.es/proposal-temporal/#sec-temporal-gettemporalcalendarslotvaluewithisodefault)
pub fn get_calendar_identifier_with_iso_default(
    cx: Context,
    item_object: Handle<ObjectValue>,
    method_name: &str,
) -> EvalResult<Calendar> {
    // Use the calendar of a Temporal object
    if let Some(date) = item_object.as_plain_date_object() {
        return Ok(date.date().calendar().clone());
    } else if let Some(date_time) = item_object.as_plain_date_time_object() {
        return Ok(date_time.date_time().calendar().clone());
    } else if let Some(month_day) = item_object.as_plain_month_day_object() {
        return Ok(month_day.month_day().calendar().clone());
    } else if let Some(year_month) = item_object.as_plain_year_month_object() {
        return Ok(year_month.year_month().calendar().clone());
    } else if let Some(zoned_date_time) = item_object.as_zoned_date_time_object() {
        return Ok(zoned_date_time.zoned_date_time().calendar().clone());
    }

    let calendar_like = get(cx, item_object, cx.names.calendar())?;

    if calendar_like.is_undefined() {
        Ok(Calendar::ISO)
    } else {
        to_temporal_calendar_identifier(cx, calendar_like, method_name)
    }
}

/// Parse the options argument for a `round` method. May be a string which is shorthand for the
/// `smallestUnit` option.
pub fn parse_round_options_argument(
    cx: Context,
    options_arg: Handle<Value>,
    method_name: &str,
) -> EvalResult<Option<Handle<ObjectValue>>> {
    if options_arg.is_undefined() {
        type_error(cx, &format!("{method_name} argument must be a string or options object"))
    } else if options_arg.is_string() {
        let options = ordinary_object_create_without_proto(cx)?;
        must!(create_data_property_or_throw(
            cx,
            options,
            cx.names.smallest_unit(),
            options_arg
        ));
        Ok(Some(options))
    } else {
        validate_options_object(cx, options_arg, method_name)
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

/// If BigInt is out of i128 range then use `i128::MAX` which will trigger a RangeError.
pub fn clamp_epoch_nanos_to_i128(epoch_nanos: &BigInt) -> i128 {
    epoch_nanos.to_i128().unwrap_or(i128::MAX)
}

/// ToTemporalTimeZoneIdentifier (https://tc39.es/proposal-temporal/#sec-temporal-totemporaltimezoneidentifier)
pub fn to_time_zone_identifier(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<TimeZone> {
    if value.is_object()
        && let Some(zoned_date_time) = value.as_object().as_zoned_date_time_object()
    {
        return Ok(*zoned_date_time.zoned_date_time().time_zone());
    }

    if !value.is_string() {
        return type_error(cx, &format!("{method_name} `timeZone` option must be a string"));
    }

    let wtf8_string = value.as_string().to_wtf8_string()?;

    if let Ok(option_str) = str::from_utf8(wtf8_string.as_bytes()) {
        let parsed_time_zone_result = TimeZone::try_from_str(option_str);
        return map_temporal_result(cx, parsed_time_zone_result, method_name);
    }

    range_error(cx, &format!("{method_name} `timeZone` option must be a valid time zone"))
}

pub fn parse_time_zone_identifier_argument(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<TimeZone> {
    if !value.is_string() {
        return type_error(cx, &format!("{method_name} `timeZone` argument must be a string"));
    }

    let wtf8_string = value.as_string().to_wtf8_string()?;

    if let Ok(str) = str::from_utf8(wtf8_string.as_bytes()) {
        let parsed_time_zone_result = TimeZone::try_from_identifier_str(str);
        return map_temporal_result(cx, parsed_time_zone_result, method_name);
    }

    range_error(cx, &format!("{method_name} `timeZone` argument must be a valid time zone"))
}

/// ToTemporalCalendarIdentifier (https://tc39.es/proposal-temporal/#sec-temporal-totemporalcalendaridentifier)
pub fn to_temporal_calendar_identifier(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Calendar> {
    // Use the calendar of a Temporal object
    if value.is_object() {
        if let Some(plain_date) = value.as_object().as_plain_date_object() {
            return Ok(plain_date.date().calendar().clone());
        } else if let Some(plain_date_time) = value.as_object().as_plain_date_time_object() {
            return Ok(plain_date_time.date_time().calendar().clone());
        } else if let Some(plain_year_month) = value.as_object().as_plain_year_month_object() {
            return Ok(plain_year_month.year_month().calendar().clone());
        } else if let Some(plain_month_day) = value.as_object().as_plain_month_day_object() {
            return Ok(plain_month_day.month_day().calendar().clone());
        } else if let Some(zoned_date_time) = value.as_object().as_zoned_date_time_object() {
            return Ok(zoned_date_time.zoned_date_time().calendar().clone());
        }
    }

    // Otherwise parse calendar from string
    if !value.is_string() {
        return type_error(cx, &format!("{method_name} `calendar` argument must be a string"));
    }

    let wtf8_string = value.as_string().to_wtf8_string()?;
    let parsed_calendar_result = Calendar::try_from_utf8(wtf8_string.as_bytes());

    map_temporal_result(cx, parsed_calendar_result, method_name)
}

/// IsPartialTemporalObject (https://tc39.es/proposal-temporal/#sec-temporal-ispartialtemporalobject)
pub fn is_partial_temporal_object(cx: Context, value: Handle<Value>) -> EvalResult<bool> {
    if !value.is_object() {
        return Ok(false);
    }

    let object = value.as_object();

    if object.is_plain_date_object()
        || object.is_plain_time_object()
        || object.is_plain_date_time_object()
        || object.is_zoned_date_time_object()
        || object.is_plain_year_month_object()
        || object.is_plain_month_day_object()
    {
        return Ok(false);
    }

    let calendar_value = get(cx, object, cx.names.calendar())?;
    if !calendar_value.is_undefined() {
        return Ok(false);
    }

    let time_zone_value = get(cx, object, cx.names.time_zone())?;
    if !time_zone_value.is_undefined() {
        return Ok(false);
    }

    Ok(true)
}
