use std::str::FromStr;

use num_bigint::BigInt;
use num_traits::{AsPrimitive, PrimInt, ToPrimitive};
use temporal_rs::{
    Calendar, MonthCode, PlainDate, TemporalResult, TimeZone, UtcOffset, ZonedDateTime,
    error::ErrorKind,
    fields::{CalendarFields, DateTimeFields, ZonedDateTimeFields},
    options::{
        DifferenceSettings, Disambiguation, DisplayCalendar, DisplayOffset, DisplayTimeZone,
        OffsetDisambiguation, Overflow, RelativeTo, RoundingIncrement, RoundingMode, Unit,
    },
    parsers::Precision,
    partial::{PartialDate, PartialTime, PartialZonedDateTime},
    primitive::FiniteF64,
    provider::TransitionDirection,
};

use crate::runtime::intrinsics::temporal::plain_date_object::PlainDateObject;
use crate::runtime::intrinsics::temporal::plain_date_time_object::PlainDateTimeObject;
use crate::runtime::intrinsics::temporal::plain_month_day_object::PlainMonthDayObject;
use crate::runtime::intrinsics::temporal::plain_time_object::PlainTimeObject;
use crate::runtime::intrinsics::temporal::plain_year_month_object::PlainYearMonthObject;
use crate::runtime::intrinsics::temporal::zoned_date_time_object::ZonedDateTimeObject;
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
        type_utilities::{ToPrimitivePreferredType, to_number, to_primitive},
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
        if let Some(plain_date) = option_object.as_opt::<PlainDateObject>() {
            return Ok(Some(RelativeTo::PlainDate(plain_date.date().clone())));
        } else if let Some(plain_date_time) = option_object.as_opt::<PlainDateTimeObject>() {
            return Ok(Some(RelativeTo::PlainDate(plain_date_time.date_time().to_plain_date())));
        } else if let Some(zoned_date_time) = option_object.as_opt::<ZonedDateTimeObject>() {
            return Ok(Some(RelativeTo::ZonedDateTime(zoned_date_time.zoned_date_time().clone())));
        }

        // Otherwise treat as a date-like object
        let calendar = get_calendar_identifier_with_iso_default(cx, option_object, method_name)?;

        let prepared_fields = prepare_calendar_fields(
            cx,
            option_object,
            &[
                DateField::Year,
                DateField::Month,
                DateField::MonthCode,
                DateField::Day,
            ],
            &[
                TimeField::Hour,
                TimeField::Minute,
                TimeField::Second,
                TimeField::Millisecond,
                TimeField::Microsecond,
                TimeField::Nanosecond,
                TimeField::Offset,
                TimeField::TimeZone,
            ],
            RequiredFieldNames::Defaults,
            method_name,
        )?;

        // If no time zone is present interpret as a PlainDate
        let Some(time_zone) = prepared_fields.time_zone else {
            let calendar_fields = prepared_fields.into_validated_date_fields();
            let partial_date = PartialDate { calendar, calendar_fields };
            let date_result = PlainDate::from_partial(partial_date, None);
            let date = map_temporal_result(cx, date_result, method_name)?;

            return Ok(Some(RelativeTo::PlainDate(date)));
        };

        // If time zone is present interpret as a ZonedDateTime
        let overflow = Overflow::Constrain;
        let fields =
            prepared_fields.into_validated_zoned_date_time_fields(cx, overflow, method_name)?;

        let partial_zoned_date_time =
            PartialZonedDateTime { calendar, timezone: Some(time_zone), fields };

        let zoned_date_time_result = ZonedDateTime::from_partial_with_provider(
            partial_zoned_date_time,
            None,
            None,
            None,
            cx.temporal_provider(),
        );
        let zoned_date_time = map_temporal_result(cx, zoned_date_time_result, method_name)?;

        return Ok(Some(RelativeTo::ZonedDateTime(zoned_date_time)));
    } else if !option_value.is_string() {
        return type_error(
            cx,
            &format!("{method_name} `relativeTo` option must be a string or object"),
        );
    }

    let wtf8_string = option_value.as_string().to_wtf8_string()?;

    if let Ok(option_str) = str::from_utf8(wtf8_string.as_bytes()) {
        let parsed_option_result =
            RelativeTo::try_from_str_with_provider(option_str, cx.temporal_provider());
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

/// GetDirectionOption (https://tc39.es/proposal-temporal/#sec-temporal-getdirectionoption)
pub fn get_transition_direction_option(
    cx: Context,
    options: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<TransitionDirection> {
    if let Some(option_value) = get_temporal_option_property(cx, options, cx.names.direction())? {
        if let Some(parsed_value) = parse_option_from_string(cx, option_value)? {
            return Ok(parsed_value);
        }
    }

    range_error(cx, &format!("{method_name} `direction` option must be 'next' or 'previous'"))
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
    if let Some(date) = item_object.as_opt::<PlainDateObject>() {
        return Ok(date.date().calendar().clone());
    } else if let Some(date_time) = item_object.as_opt::<PlainDateTimeObject>() {
        return Ok(date_time.date_time().calendar().clone());
    } else if let Some(month_day) = item_object.as_opt::<PlainMonthDayObject>() {
        return Ok(month_day.month_day().calendar().clone());
    } else if let Some(year_month) = item_object.as_opt::<PlainYearMonthObject>() {
        return Ok(year_month.year_month().calendar().clone());
    } else if let Some(zoned_date_time) = item_object.as_opt::<ZonedDateTimeObject>() {
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

/// ToPositiveIntegerWithTruncation (https://tc39.es/proposal-temporal/#sec-topositiveintegerwithtruncation)
pub fn to_positive_integer_with_truncation<T: PrimInt + AsPrimitive<f64>>(
    cx: Context,
    value: Handle<Value>,
    err_prefix: &str,
) -> EvalResult<T>
where
    f64: AsPrimitive<T>,
    i8: AsPrimitive<T>,
{
    let number_value = to_number(cx, value)?;
    let finite_value_result = FiniteF64::try_from(number_value.as_number());
    let finite_value = map_temporal_result(cx, finite_value_result, err_prefix)?;

    let positive_integer_result = finite_value.as_positive_integer_with_truncation();
    map_temporal_result(cx, positive_integer_result, err_prefix)
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
        && let Some(zoned_date_time) = value.as_opt::<ZonedDateTimeObject>()
    {
        return Ok(*zoned_date_time.zoned_date_time().time_zone());
    }

    if !value.is_string() {
        return type_error(cx, &format!("{method_name} `timeZone` option must be a string"));
    }

    let wtf8_string = value.as_string().to_wtf8_string()?;

    if let Ok(option_str) = str::from_utf8(wtf8_string.as_bytes()) {
        let parsed_time_zone_result =
            TimeZone::try_from_str_with_provider(option_str, cx.temporal_provider());
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
        let parsed_time_zone_result =
            TimeZone::try_from_identifier_str_with_provider(str, cx.temporal_provider());
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
        if let Some(plain_date) = value.as_opt::<PlainDateObject>() {
            return Ok(plain_date.date().calendar().clone());
        } else if let Some(plain_date_time) = value.as_opt::<PlainDateTimeObject>() {
            return Ok(plain_date_time.date_time().calendar().clone());
        } else if let Some(plain_year_month) = value.as_opt::<PlainYearMonthObject>() {
            return Ok(plain_year_month.year_month().calendar().clone());
        } else if let Some(plain_month_day) = value.as_opt::<PlainMonthDayObject>() {
            return Ok(plain_month_day.month_day().calendar().clone());
        } else if let Some(zoned_date_time) = value.as_opt::<ZonedDateTimeObject>() {
            return Ok(zoned_date_time.zoned_date_time().calendar().clone());
        }
    }

    // Otherwise parse calendar from string
    if !value.is_string() {
        return type_error(cx, &format!("{method_name} `calendar` argument must be a string"));
    }

    let wtf8_string = value.as_string().to_wtf8_string()?;

    if let Ok(option_str) = str::from_utf8(wtf8_string.as_bytes()) {
        let parsed_calendar_result = Calendar::from_str(option_str);
        return map_temporal_result(cx, parsed_calendar_result, method_name);
    }

    range_error(
        cx,
        &format!("{method_name} `calendar` argument must be a valid calendar identifier"),
    )
}

/// IsPartialTemporalObject (https://tc39.es/proposal-temporal/#sec-temporal-ispartialtemporalobject)
pub fn is_partial_temporal_object(cx: Context, value: Handle<Value>) -> EvalResult<bool> {
    if !value.is_object() {
        return Ok(false);
    }

    let object = value.as_object();

    if object.is::<PlainDateObject>()
        || object.is::<PlainTimeObject>()
        || object.is::<PlainDateTimeObject>()
        || object.is::<ZonedDateTimeObject>()
        || object.is::<PlainYearMonthObject>()
        || object.is::<PlainMonthDayObject>()
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

/// ParseMonthCode (https://tc39.es/proposal-temporal/#sec-temporal-parsemonthcode)
fn parse_month_code(cx: Context, value: Handle<Value>, method_name: &str) -> EvalResult<MonthCode> {
    let primitive_value = to_primitive(cx, value, ToPrimitivePreferredType::String)?;
    if !primitive_value.is_string() {
        return type_error(cx, &format!("{method_name} `monthCode` must be a string"));
    }

    let wtf8_string = primitive_value.as_string().to_wtf8_string()?;
    let parsed_month_code_result = MonthCode::try_from_utf8(wtf8_string.as_bytes());

    map_temporal_result(cx, parsed_month_code_result, method_name)
}

/// ToOffsetString (https://tc39.es/proposal-temporal/#sec-temporal-tooffsetstring)
fn parse_offset_string(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<UtcOffset> {
    let primitive_value = to_primitive(cx, value, ToPrimitivePreferredType::String)?;
    if !primitive_value.is_string() {
        return type_error(cx, &format!("{method_name} `offset` must be a string"));
    }

    let wtf8_string = primitive_value.as_string().to_wtf8_string()?;
    let parsed_offset_result = UtcOffset::from_utf8(wtf8_string.as_bytes());

    map_temporal_result(cx, parsed_offset_result, method_name)
}

#[derive(Clone, Copy, PartialEq)]
pub enum DateField {
    Year,
    Month,
    MonthCode,
    Day,
}

#[derive(Clone, Copy, PartialEq)]
pub enum TimeField {
    Hour,
    Minute,
    Second,
    Millisecond,
    Microsecond,
    Nanosecond,
    Offset,
    TimeZone,
}

pub enum RequiredFieldNames {
    /// Do not require or provide a default value for fields.
    Partial,
    /// Provide a default value for missing fields.
    Defaults,
    /// Require the time zone field but otherwise provide default values for missing fields.
    TimeZoneWithDefaults,
}

impl RequiredFieldNames {
    pub fn provides_defaults(&self) -> bool {
        matches!(self, RequiredFieldNames::Defaults | RequiredFieldNames::TimeZoneWithDefaults)
    }

    pub fn requires_time_zone(&self) -> bool {
        matches!(self, RequiredFieldNames::TimeZoneWithDefaults)
    }
}

#[derive(Clone, Copy, PartialEq)]
enum CalendarField {
    Date(DateField),
    Time(TimeField),
}

/// Fields are iterated in lexicographic order.
const FIELD_ITERATION_ORDER: &[CalendarField] = &[
    CalendarField::Date(DateField::Day),
    CalendarField::Time(TimeField::Hour),
    CalendarField::Time(TimeField::Microsecond),
    CalendarField::Time(TimeField::Millisecond),
    CalendarField::Time(TimeField::Minute),
    CalendarField::Date(DateField::Month),
    CalendarField::Date(DateField::MonthCode),
    CalendarField::Time(TimeField::Nanosecond),
    CalendarField::Time(TimeField::Offset),
    CalendarField::Time(TimeField::Second),
    CalendarField::Time(TimeField::TimeZone),
    CalendarField::Date(DateField::Year),
];

/// A partial time record with unvalidated fields (clamped to their signed bounds).
#[derive(Default, PartialEq)]
pub struct PartialTimeRecord {
    hour: Option<i8>,
    minute: Option<i8>,
    second: Option<i8>,
    milli: Option<i16>,
    micro: Option<i16>,
    nano: Option<i16>,
}

impl PartialTimeRecord {
    pub fn is_empty(&self) -> bool {
        self == &PartialTimeRecord::default()
    }

    /// Clamp and validate the partial time record fields, depending on the overflow option.
    pub fn clamp_and_validate(
        &self,
        cx: Context,
        overflow: Overflow,
        method_name: &str,
    ) -> EvalResult<PartialTime> {
        let hour = if let Some(hour) = self.hour {
            Some(validate_hour_argument(cx, hour, overflow, method_name)?)
        } else {
            None
        };

        let minute = if let Some(minute) = self.minute {
            Some(validate_minute_argument(cx, minute, overflow, method_name)?)
        } else {
            None
        };

        let second = if let Some(second) = self.second {
            Some(validate_second_argument(cx, second, overflow, method_name)?)
        } else {
            None
        };

        let milli = if let Some(milli) = self.milli {
            Some(validate_milli_argument(cx, milli, overflow, method_name)?)
        } else {
            None
        };

        let micro = if let Some(micro) = self.micro {
            Some(validate_micro_argument(cx, micro, overflow, method_name)?)
        } else {
            None
        };

        let nano = if let Some(nano) = self.nano {
            Some(validate_nano_argument(cx, nano, overflow, method_name)?)
        } else {
            None
        };

        Ok(PartialTime {
            hour,
            minute,
            second,
            millisecond: milli,
            microsecond: micro,
            nanosecond: nano,
        })
    }
}

pub struct PreparedCalendarFields {
    /// Unvalidated date fields. Clamped to bounds, which triggers correct validation error later.
    pub partial_date: CalendarFields,
    /// Unvalidated time fields. Clamped to signed bounds and requires manual validation.
    pub partial_time_record: PartialTimeRecord,
    pub offset: Option<UtcOffset>,
    pub time_zone: Option<TimeZone>,
}

impl PreparedCalendarFields {
    fn to_validated_partial_time(
        &self,
        cx: Context,
        overflow: Overflow,
        method_name: &str,
    ) -> EvalResult<PartialTime> {
        self.partial_time_record
            .clamp_and_validate(cx, overflow, method_name)
    }

    pub fn into_validated_date_fields(self) -> CalendarFields {
        self.partial_date
    }

    pub fn into_validated_date_time_fields(
        self,
        cx: Context,
        overflow: Overflow,
        method_name: &str,
    ) -> EvalResult<DateTimeFields> {
        let partial_time = self.to_validated_partial_time(cx, overflow, method_name)?;
        Ok(DateTimeFields::new()
            .with_partial_date(self.partial_date)
            .with_partial_time(partial_time))
    }

    pub fn into_validated_zoned_date_time_fields(
        self,
        cx: Context,
        overflow: Overflow,
        method_name: &str,
    ) -> EvalResult<ZonedDateTimeFields> {
        let partial_time = self.to_validated_partial_time(cx, overflow, method_name)?;
        Ok(ZonedDateTimeFields {
            calendar_fields: self.partial_date,
            time: partial_time,
            offset: self.offset,
        })
    }
}

/// PrepareCalendarFields (https://tc39.es/proposal-temporal/#sec-temporal-preparecalendarfields)
pub fn prepare_calendar_fields(
    cx: Context,
    object: Handle<ObjectValue>,
    date_fields: &[DateField],
    time_fields: &[TimeField],
    required: RequiredFieldNames,
    method_name: &str,
) -> EvalResult<PreparedCalendarFields> {
    let mut partial_date = CalendarFields::new();
    let mut partial_time_record = PartialTimeRecord::default();
    let mut offset = None;
    let mut time_zone = None;

    for field in FIELD_ITERATION_ORDER {
        match field {
            CalendarField::Date(DateField::Year) if date_fields.contains(&DateField::Year) => {
                let value = get(cx, object, cx.names.year())?;
                if !value.is_undefined() {
                    let converted_value = to_integer_with_truncation(cx, value, method_name)?;
                    partial_date.year = Some(converted_value);
                }
            }
            CalendarField::Date(DateField::Month) if date_fields.contains(&DateField::Month) => {
                let value = get(cx, object, cx.names.month())?;
                if !value.is_undefined() {
                    let converted_value =
                        to_positive_integer_with_truncation(cx, value, method_name)?;
                    partial_date.month = Some(converted_value);
                }
            }
            CalendarField::Date(DateField::MonthCode)
                if date_fields.contains(&DateField::MonthCode) =>
            {
                let value = get(cx, object, cx.names.month_code())?;
                if !value.is_undefined() {
                    let converted_value = parse_month_code(cx, value, method_name)?;
                    partial_date.month_code = Some(converted_value);
                }
            }
            CalendarField::Date(DateField::Day) if date_fields.contains(&DateField::Day) => {
                let value = get(cx, object, cx.names.day())?;
                if !value.is_undefined() {
                    let converted_value =
                        to_positive_integer_with_truncation(cx, value, method_name)?;
                    partial_date.day = Some(converted_value);
                }
            }
            CalendarField::Time(TimeField::Hour) if time_fields.contains(&TimeField::Hour) => {
                let value = get(cx, object, cx.names.hour())?;
                if !value.is_undefined() {
                    partial_time_record.hour =
                        Some(to_integer_with_truncation(cx, value, method_name)?);
                } else if required.provides_defaults() {
                    partial_time_record.hour = Some(0);
                }
            }
            CalendarField::Time(TimeField::Minute) if time_fields.contains(&TimeField::Minute) => {
                let value = get(cx, object, cx.names.minute())?;
                if !value.is_undefined() {
                    partial_time_record.minute =
                        Some(to_integer_with_truncation(cx, value, method_name)?);
                } else if required.provides_defaults() {
                    partial_time_record.minute = Some(0);
                }
            }
            CalendarField::Time(TimeField::Second) if time_fields.contains(&TimeField::Second) => {
                let value = get(cx, object, cx.names.second())?;
                if !value.is_undefined() {
                    partial_time_record.second =
                        Some(to_integer_with_truncation(cx, value, method_name)?);
                } else if required.provides_defaults() {
                    partial_time_record.second = Some(0);
                }
            }
            CalendarField::Time(TimeField::Millisecond)
                if time_fields.contains(&TimeField::Millisecond) =>
            {
                let value = get(cx, object, cx.names.millisecond())?;
                if !value.is_undefined() {
                    partial_time_record.milli =
                        Some(to_integer_with_truncation(cx, value, method_name)?);
                } else if required.provides_defaults() {
                    partial_time_record.milli = Some(0);
                }
            }
            CalendarField::Time(TimeField::Microsecond)
                if time_fields.contains(&TimeField::Microsecond) =>
            {
                let value = get(cx, object, cx.names.microsecond())?;
                if !value.is_undefined() {
                    partial_time_record.micro =
                        Some(to_integer_with_truncation(cx, value, method_name)?);
                } else if required.provides_defaults() {
                    partial_time_record.micro = Some(0);
                }
            }
            CalendarField::Time(TimeField::Nanosecond)
                if time_fields.contains(&TimeField::Nanosecond) =>
            {
                let value = get(cx, object, cx.names.nanosecond())?;
                if !value.is_undefined() {
                    partial_time_record.nano =
                        Some(to_integer_with_truncation(cx, value, method_name)?);
                } else if required.provides_defaults() {
                    partial_time_record.nano = Some(0);
                }
            }
            CalendarField::Time(TimeField::Offset) if time_fields.contains(&TimeField::Offset) => {
                let value = get(cx, object, cx.names.offset())?;
                if !value.is_undefined() {
                    let parsed_offset = parse_offset_string(cx, value, method_name)?;
                    offset = Some(parsed_offset);
                }
            }
            CalendarField::Time(TimeField::TimeZone)
                if time_fields.contains(&TimeField::TimeZone) =>
            {
                let value = get(cx, object, cx.names.time_zone())?;
                if !value.is_undefined() {
                    let parsed_time_zone = to_time_zone_identifier(cx, value, method_name)?;
                    time_zone = Some(parsed_time_zone);
                } else if required.requires_time_zone() {
                    return type_error(
                        cx,
                        &format!("{method_name} date-like object must have a `timeZone` property"),
                    );
                }
            }
            _ => continue,
        }
    }

    if !required.provides_defaults()
        && partial_date.is_empty()
        && partial_time_record.is_empty()
        && offset.is_none()
        && time_zone.is_none()
    {
        return type_error(cx, &format!("{method_name} date-like object is empty"));
    }

    Ok(PreparedCalendarFields { partial_date, partial_time_record, offset, time_zone })
}

/// ToTemporalTimeRecord (https://tc39.es/proposal-temporal/#sec-temporal-totemporaltimerecord)
pub fn to_partial_time_record(
    cx: Context,
    time_like_object: Handle<Value>,
    method_name: &str,
) -> EvalResult<PartialTimeRecord> {
    if !time_like_object.is_object() {
        return type_error(cx, &format!("{method_name} time must be an object"));
    }

    let object = time_like_object.as_object();

    let mut record = PartialTimeRecord::default();

    let hour_value = get(cx, object, cx.names.hour())?;
    if !hour_value.is_undefined() {
        record.hour = Some(to_integer_with_truncation(cx, hour_value, method_name)?);
    }

    let micro_value = get(cx, object, cx.names.microsecond())?;
    if !micro_value.is_undefined() {
        record.micro = Some(to_integer_with_truncation(cx, micro_value, method_name)?);
    }

    let milli_value = get(cx, object, cx.names.millisecond())?;
    if !milli_value.is_undefined() {
        record.milli = Some(to_integer_with_truncation(cx, milli_value, method_name)?);
    }

    let minute_value = get(cx, object, cx.names.minute())?;
    if !minute_value.is_undefined() {
        record.minute = Some(to_integer_with_truncation(cx, minute_value, method_name)?);
    }

    let nano_value = get(cx, object, cx.names.nanosecond())?;
    if !nano_value.is_undefined() {
        record.nano = Some(to_integer_with_truncation(cx, nano_value, method_name)?);
    }

    let second_value = get(cx, object, cx.names.second())?;
    if !second_value.is_undefined() {
        record.second = Some(to_integer_with_truncation(cx, second_value, method_name)?);
    }

    if record.is_empty() {
        return type_error(cx, &format!("{method_name} time object is empty"));
    }

    Ok(record)
}

pub fn validate_time_arguments(
    cx: Context,
    hour: i8,
    minute: i8,
    second: i8,
    millis: i16,
    micros: i16,
    nanos: i16,
    method_name: &str,
) -> EvalResult<(u8, u8, u8, u16, u16, u16)> {
    let overflow = Overflow::Reject;

    let hour = validate_hour_argument(cx, hour, overflow, method_name)?;
    let minute = validate_minute_argument(cx, minute, overflow, method_name)?;
    let second = validate_second_argument(cx, second, overflow, method_name)?;
    let millis = validate_milli_argument(cx, millis, overflow, method_name)?;
    let micros = validate_micro_argument(cx, micros, overflow, method_name)?;
    let nanos = validate_nano_argument(cx, nanos, overflow, method_name)?;

    Ok((hour, minute, second, millis, micros, nanos))
}

pub fn validate_hour_argument(
    cx: Context,
    hour: i8,
    overflow: Overflow,
    method_name: &str,
) -> EvalResult<u8> {
    match overflow {
        Overflow::Reject if !(0..=23).contains(&hour) => {
            range_error(cx, &format!("{method_name} hour must be in range 0-23"))
        }
        Overflow::Reject => Ok(hour as u8),
        Overflow::Constrain => Ok(hour.max(0) as u8),
    }
}

pub fn validate_minute_argument(
    cx: Context,
    minute: i8,
    overflow: Overflow,
    method_name: &str,
) -> EvalResult<u8> {
    match overflow {
        Overflow::Reject if !(0..=59).contains(&minute) => {
            range_error(cx, &format!("{method_name} minute must be in range 0-59"))
        }
        Overflow::Reject => Ok(minute as u8),
        Overflow::Constrain => Ok(minute.max(0) as u8),
    }
}

pub fn validate_second_argument(
    cx: Context,
    second: i8,
    overflow: Overflow,
    method_name: &str,
) -> EvalResult<u8> {
    match overflow {
        Overflow::Reject if !(0..=59).contains(&second) => {
            range_error(cx, &format!("{method_name} second must be in range 0-59"))
        }
        Overflow::Reject => Ok(second as u8),
        Overflow::Constrain => Ok(second.max(0) as u8),
    }
}

pub fn validate_milli_argument(
    cx: Context,
    millis: i16,
    overflow: Overflow,
    method_name: &str,
) -> EvalResult<u16> {
    match overflow {
        Overflow::Reject if !(0..=999).contains(&millis) => {
            range_error(cx, &format!("{method_name} millisecond must be in range 0-999"))
        }
        Overflow::Reject => Ok(millis as u16),
        Overflow::Constrain => Ok(millis.max(0) as u16),
    }
}

pub fn validate_micro_argument(
    cx: Context,
    micros: i16,
    overflow: Overflow,
    method_name: &str,
) -> EvalResult<u16> {
    match overflow {
        Overflow::Reject if !(0..=999).contains(&micros) => {
            range_error(cx, &format!("{method_name} microsecond must be in range 0-999"))
        }
        Overflow::Reject => Ok(micros as u16),
        Overflow::Constrain => Ok(micros.max(0) as u16),
    }
}

pub fn validate_nano_argument(
    cx: Context,
    nanos: i16,
    overflow: Overflow,
    method_name: &str,
) -> EvalResult<u16> {
    match overflow {
        Overflow::Reject if !(0..=999).contains(&nanos) => {
            range_error(cx, &format!("{method_name} nanosecond must be in range 0-999"))
        }
        Overflow::Reject => Ok(nanos as u16),
        Overflow::Constrain => Ok(nanos.max(0) as u16),
    }
}
