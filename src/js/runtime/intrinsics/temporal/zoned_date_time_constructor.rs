use temporal_rs::{
    ZonedDateTime,
    options::{Disambiguation, OffsetDisambiguation, Overflow},
    parsed_intermediates::ParsedZonedDateTime,
    partial::PartialZonedDateTime,
};

use crate::runtime::{
    Context, Handle, Realm, Value,
    alloc_error::AllocResult,
    builtin_function::BuiltinFunction,
    error::type_error,
    eval_result::EvalResult,
    function::get_argument,
    intrinsics::{
        intrinsics::Intrinsic,
        rust_runtime::RuntimeFunction,
        temporal::{
            utils::{
                DateField, RequiredFieldNames, TimeField, clamp_epoch_nanos_to_i128,
                get_calendar_identifier_with_iso_default, get_disambiguation_option,
                get_offset_option, get_overflow_option, map_temporal_result,
                parse_calendar_argument, parse_time_zone_identifier_argument,
                prepare_calendar_fields, validate_options_object,
            },
            zoned_date_time_object::ZonedDateTimeObject,
        },
    },
    object_value::ObjectValue,
    type_utilities::to_bigint,
};

pub struct ZonedDateTimeConstructor;

impl ZonedDateTimeConstructor {
    /// Temporal.ZonedDateTime Constructor (https://tc39.es/proposal-temporal/#sec-temporal-zoneddatetime-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            RuntimeFunction::ZonedDateTimeConstructor_construct,
            2,
            cx.names.zoned_date_time(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm
                .get_intrinsic(Intrinsic::ZonedDateTimePrototype)
                .into(),
        )?;

        func.intrinsic_func(
            cx,
            cx.names.compare(),
            RuntimeFunction::ZonedDateTimeConstructor_compare,
            2,
            realm,
        )?;
        func.intrinsic_func(
            cx,
            cx.names.from(),
            RuntimeFunction::ZonedDateTimeConstructor_from,
            1,
            realm,
        )?;

        Ok(func)
    }

    /// Temporal.ZonedDateTime (https://tc39.es/proposal-temporal/#sec-temporal-zoneddatetime)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Temporal.ZonedDateTime constructor";

        let Some(new_target) = cx.current_new_target() else {
            return type_error(cx, "Temporal.ZonedDateTime constructor must be called with new");
        };

        let epoch_nanos_arg = get_argument(cx, arguments, 0);
        let epoch_nanos_bigint = to_bigint(cx, epoch_nanos_arg)?;
        let epoch_nanos_i128 = clamp_epoch_nanos_to_i128(&epoch_nanos_bigint.bigint());

        let time_zone_arg = get_argument(cx, arguments, 1);
        let time_zone = parse_time_zone_identifier_argument(cx, time_zone_arg, NAME)?;

        let calendar_arg = get_argument(cx, arguments, 2);
        let calendar = parse_calendar_argument(cx, calendar_arg, NAME)?;

        let zoned_date_time_result = ZonedDateTime::try_new(epoch_nanos_i128, time_zone, calendar);
        let zoned_date_time = map_temporal_result(cx, zoned_date_time_result, NAME)?;

        Ok(ZonedDateTimeObject::new_from_constructor(cx, new_target, zoned_date_time)?.as_value())
    }

    /// Temporal.ZonedDateTime.compare (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.compare)
    pub fn compare(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "ZonedDateTime.compare";

        let arg_1 = get_argument(cx, arguments, 0);
        let arg_2 = get_argument(cx, arguments, 1);

        let zoned_date_time_1 = to_temporal_zoned_date_time(cx, arg_1, NAME)?;
        let zoned_date_time_2 = to_temporal_zoned_date_time(cx, arg_2, NAME)?;

        Ok(cx.smi(zoned_date_time_1.compare_instant(&zoned_date_time_2) as i32))
    }

    /// Temporal.ZonedDateTime.from (https://tc39.es/proposal-temporal/#sec-temporal.zoneddatetime.from)
    pub fn from(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let item_arg = get_argument(cx, arguments, 0);
        let options_arg = get_argument(cx, arguments, 1);

        let zoned_date_time = to_temporal_zoned_date_time_with_options(
            cx,
            item_arg,
            options_arg,
            "ZonedDateTime.from",
        )?;

        Ok(ZonedDateTimeObject::new(cx, zoned_date_time)?.as_value())
    }
}

/// ToTemporalZonedDateTime (https://tc39.es/proposal-temporal/#sec-temporal-totemporalzoneddatetime)
pub fn to_temporal_zoned_date_time(
    cx: Context,
    item: Handle<Value>,
    method_name: &str,
) -> EvalResult<ZonedDateTime> {
    to_temporal_zoned_date_time_with_options(cx, item, cx.undefined(), method_name)
}

pub fn to_temporal_zoned_date_time_with_options(
    cx: Context,
    item: Handle<Value>,
    options: Handle<Value>,
    method_name: &str,
) -> EvalResult<ZonedDateTime> {
    fn validate_options(
        cx: Context,
        options: Handle<Value>,
        method_name: &str,
    ) -> EvalResult<(Disambiguation, OffsetDisambiguation, Overflow)> {
        let options = validate_options_object(cx, options, method_name)?;

        let disambiguation = get_disambiguation_option(cx, options, method_name)?;
        let offset = get_offset_option(cx, options, OffsetDisambiguation::Reject, method_name)?;
        let overflow = get_overflow_option(cx, options, method_name)?;

        Ok((disambiguation, offset, overflow))
    }

    if item.is_object() {
        // Check if item is another ZonedDateTime object
        let item_object = item.as_object();
        if let Some(zdt) = item_object.as_zoned_date_time_object() {
            validate_options(cx, options, method_name)?;
            return Ok(zdt.zoned_date_time().clone());
        }

        // Otherwise treat like a date-like object
        let calendar = get_calendar_identifier_with_iso_default(cx, item_object, method_name)?;

        let prepared_fields = prepare_calendar_fields(
            cx,
            item_object,
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
            RequiredFieldNames::TimeZoneWithDefaults,
            method_name,
        )?;

        let (disambiguation, offset, overflow) = validate_options(cx, options, method_name)?;

        let partial_zoned_date_time = PartialZonedDateTime {
            calendar,
            timezone: prepared_fields.time_zone,
            fields: prepared_fields.into_partial_zoned_date_time(),
        };

        let zoned_date_time_result = ZonedDateTime::from_partial(
            partial_zoned_date_time,
            Some(overflow),
            Some(disambiguation),
            Some(offset),
        );

        return map_temporal_result(cx, zoned_date_time_result, method_name);
    }

    // Otherwise parse ZonedDateTime from string
    if !item.is_string() {
        return type_error(
            cx,
            &format!("{method_name} zoned date-time must be a string or object"),
        );
    }

    let wtf8_string = item.as_string().to_wtf8_string()?;
    let parsed_result = ParsedZonedDateTime::from_utf8(wtf8_string.as_bytes());
    let parsed = map_temporal_result(cx, parsed_result, method_name)?;

    let (disambiguation, offset, _) = validate_options(cx, options, method_name)?;

    let zoned_date_time_result = ZonedDateTime::from_parsed(parsed, disambiguation, offset);
    let zoned_date_time = map_temporal_result(cx, zoned_date_time_result, method_name)?;

    Ok(zoned_date_time)
}
