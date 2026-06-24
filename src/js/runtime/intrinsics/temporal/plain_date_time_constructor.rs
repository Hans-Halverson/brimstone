use temporal_rs::PlainDateTime;

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
            plain_date_time_object::PlainDateTimeObject,
            utils::{
                get_overflow_option, map_temporal_result, parse_calendar_argument,
                to_integer_with_truncation, to_integer_with_truncation_or_zero,
                validate_options_object,
            },
        },
    },
    object_value::ObjectValue,
};

pub struct PlainDateTimeConstructor;

impl PlainDateTimeConstructor {
    /// Temporal.PlainDateTime Constructor (https://tc39.es/proposal-temporal/#sec-temporal-plaindatetime-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            RuntimeFunction::PlainDateTimeConstructor_construct,
            3,
            cx.names.plain_date_time(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm
                .get_intrinsic(Intrinsic::PlainDateTimePrototype)
                .into(),
        )?;

        func.intrinsic_func(
            cx,
            cx.names.from(),
            RuntimeFunction::PlainDateTimeConstructor_from,
            1,
            realm,
        )?;
        func.intrinsic_func(
            cx,
            cx.names.compare(),
            RuntimeFunction::PlainDateTimeConstructor_compare,
            2,
            realm,
        )?;

        Ok(func)
    }

    /// Temporal.PlainDateTime (https://tc39.es/proposal-temporal/#sec-temporal-plaindatetime)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Temporal.PlainDateTime constructor";

        let Some(new_target) = cx.current_new_target() else {
            return type_error(cx, "Temporal.PlainDateTime constructor must be called with new");
        };

        let year_arg = get_argument(cx, arguments, 0);
        let year = to_integer_with_truncation(cx, year_arg, NAME)?;

        let month_arg = get_argument(cx, arguments, 1);
        let month = to_integer_with_truncation(cx, month_arg, NAME)?;

        let day_arg = get_argument(cx, arguments, 2);
        let day = to_integer_with_truncation(cx, day_arg, NAME)?;

        let hour_arg = get_argument(cx, arguments, 3);
        let hour = to_integer_with_truncation_or_zero(cx, hour_arg, NAME)?;

        let minute_arg = get_argument(cx, arguments, 4);
        let minute = to_integer_with_truncation_or_zero(cx, minute_arg, NAME)?;

        let second_arg = get_argument(cx, arguments, 5);
        let second = to_integer_with_truncation_or_zero(cx, second_arg, NAME)?;

        let millis_arg = get_argument(cx, arguments, 6);
        let millis = to_integer_with_truncation_or_zero(cx, millis_arg, NAME)?;

        let micros_arg = get_argument(cx, arguments, 7);
        let micros = to_integer_with_truncation_or_zero(cx, micros_arg, NAME)?;

        let nanos_arg = get_argument(cx, arguments, 8);
        let nanos = to_integer_with_truncation_or_zero(cx, nanos_arg, NAME)?;

        let calendar_arg = get_argument(cx, arguments, 9);
        let calendar = parse_calendar_argument(cx, calendar_arg, NAME)?;

        let plain_date_time_result = PlainDateTime::try_new(
            year, month, day, hour, minute, second, millis, micros, nanos, calendar,
        );
        let plain_date_time = map_temporal_result(cx, plain_date_time_result, NAME)?;

        Ok(PlainDateTimeObject::new_from_constructor(cx, new_target, plain_date_time)?.as_value())
    }

    /// Temporal.PlainDateTime.from (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.from)
    pub fn from(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let item_arg = get_argument(cx, arguments, 0);
        let options_arg = get_argument(cx, arguments, 1);

        let plain_date_time =
            to_temporal_date_time_with_options(cx, item_arg, options_arg, "PlainDateTime.from")?;

        Ok(PlainDateTimeObject::new(cx, plain_date_time)?.as_value())
    }

    /// Temporal.PlainDateTime.compare (https://tc39.es/proposal-temporal/#sec-temporal.plaindatetime.compare)
    pub fn compare(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainDateTime.compare";

        let arg_1 = get_argument(cx, arguments, 0);
        let arg_2 = get_argument(cx, arguments, 1);

        let date_time_1 = to_temporal_date_time(cx, arg_1, NAME)?;
        let date_time_2 = to_temporal_date_time(cx, arg_2, NAME)?;

        Ok(cx.smi(date_time_1.compare_iso(&date_time_2) as i32))
    }
}

/// ToTemporalDateTime (https://tc39.es/proposal-temporal/#sec-temporal-totemporaldatetime)
pub fn to_temporal_date_time(
    cx: Context,
    item: Handle<Value>,
    method_name: &str,
) -> EvalResult<PlainDateTime> {
    to_temporal_date_time_with_options(cx, item, cx.undefined(), method_name)
}

fn to_temporal_date_time_with_options(
    cx: Context,
    item: Handle<Value>,
    options: Handle<Value>,
    method_name: &str,
) -> EvalResult<PlainDateTime> {
    if item.is_object() {
        // Check if item is a Temporal object of some kind
        let item_object = item.as_object();
        if let Some(plain_date_time) = item_object.as_plain_date_time_object() {
            let options = validate_options_object(cx, options, method_name)?;
            get_overflow_option(cx, options, method_name)?;

            return Ok(plain_date_time.date_time().clone());
        } else if let Some(plain_date) = item_object.as_plain_date_object() {
            let options = validate_options_object(cx, options, method_name)?;
            get_overflow_option(cx, options, method_name)?;

            let plain_date_time_result = plain_date.date().to_plain_date_time(None);

            return map_temporal_result(cx, plain_date_time_result, method_name);
        }

        // TODO: Check for ZonedDateTime objects

        // Otherwise treat as a date-time-like object
        unimplemented!("ToTemporalDateTime for date-like object")
    }

    // Otherwise parse PlainDateTime from string
    if !item.is_string() {
        return type_error(cx, &format!("{method_name} date-time must be a string or object"));
    }

    let item_string = item.as_string().to_wtf8_string()?;

    let parsed_result = PlainDateTime::from_utf8(item_string.as_bytes());
    let parsed = map_temporal_result(cx, parsed_result, method_name)?;

    let options = validate_options_object(cx, options, method_name)?;
    get_overflow_option(cx, options, method_name)?;

    Ok(parsed)
}
