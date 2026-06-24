use num_traits::{AsPrimitive, PrimInt};
use temporal_rs::{PlainTime, partial::PartialTime};

use crate::runtime::{
    Context, Handle, Realm, Value,
    alloc_error::AllocResult,
    builtin_function::BuiltinFunction,
    error::type_error,
    eval_result::EvalResult,
    function::get_argument,
    get,
    intrinsics::{
        intrinsics::Intrinsic,
        rust_runtime::RuntimeFunction,
        temporal::{
            plain_time_object::PlainTimeObject,
            utils::{
                get_overflow_option, map_temporal_result, to_integer_with_truncation,
                validate_options_object,
            },
        },
    },
    object_value::ObjectValue,
};

pub struct PlainTimeConstructor;

impl PlainTimeConstructor {
    /// Temporal.PlainTime Constructor (https://tc39.es/proposal-temporal/#sec-temporal-plaintime-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            RuntimeFunction::PlainTimeConstructor_construct,
            0,
            cx.names.plain_time(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::PlainTimePrototype).into(),
        )?;

        func.intrinsic_func(
            cx,
            cx.names.from(),
            RuntimeFunction::PlainTimeConstructor_from,
            1,
            realm,
        )?;
        func.intrinsic_func(
            cx,
            cx.names.compare(),
            RuntimeFunction::PlainTimeConstructor_compare,
            2,
            realm,
        )?;

        Ok(func)
    }

    /// Temporal.PlainTime (https://tc39.es/proposal-temporal/#sec-temporal-plaintime)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Temporal.PlainTime constructor";

        fn to_integer_with_truncation_or_zero<T: PrimInt + AsPrimitive<f64>>(
            cx: Context,
            value: Handle<Value>,
        ) -> EvalResult<T>
        where
            f64: AsPrimitive<T>,
        {
            if value.is_undefined() {
                Ok(T::zero())
            } else {
                to_integer_with_truncation(cx, value, NAME)
            }
        }

        let Some(new_target) = cx.current_new_target() else {
            return type_error(cx, "Temporal.PlainTime constructor must be called with new");
        };

        let hour_arg = get_argument(cx, arguments, 0);
        let hour = to_integer_with_truncation_or_zero(cx, hour_arg)?;

        let minute_arg = get_argument(cx, arguments, 1);
        let minute = to_integer_with_truncation_or_zero(cx, minute_arg)?;

        let second_arg = get_argument(cx, arguments, 2);
        let second = to_integer_with_truncation_or_zero(cx, second_arg)?;

        let millis_arg = get_argument(cx, arguments, 3);
        let millis = to_integer_with_truncation_or_zero(cx, millis_arg)?;

        let micros_arg = get_argument(cx, arguments, 4);
        let micros = to_integer_with_truncation_or_zero(cx, micros_arg)?;

        let nanos_arg = get_argument(cx, arguments, 5);
        let nanos = to_integer_with_truncation_or_zero(cx, nanos_arg)?;

        let plain_time_result = PlainTime::new(hour, minute, second, millis, micros, nanos);
        let plain_time = map_temporal_result(cx, plain_time_result, NAME)?;

        Ok(PlainTimeObject::new_from_constructor(cx, new_target, plain_time)?.as_value())
    }

    /// Temporal.PlainTime.from (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.from)
    pub fn from(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let item_arg = get_argument(cx, arguments, 0);
        let options_arg = get_argument(cx, arguments, 1);

        let plain_time =
            to_temporal_time_with_options(cx, item_arg, options_arg, "PlainTime.from")?;

        Ok(PlainTimeObject::new(cx, plain_time)?.as_value())
    }

    /// Temporal.PlainTime.compare (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.compare)
    pub fn compare(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainTime.compare";

        let arg_1 = get_argument(cx, arguments, 0);
        let arg_2 = get_argument(cx, arguments, 1);

        let time_1 = to_temporal_time(cx, arg_1, NAME)?;
        let time_2 = to_temporal_time(cx, arg_2, NAME)?;

        Ok(cx.smi(time_1.cmp(&time_2) as i32))
    }
}

/// ToTemporalTime (https://tc39.es/proposal-temporal/#sec-temporal-totemporaltime)
pub fn to_temporal_time(
    cx: Context,
    item: Handle<Value>,
    method_name: &str,
) -> EvalResult<PlainTime> {
    to_temporal_time_with_options(cx, item, cx.undefined(), method_name)
}

pub fn to_temporal_time_with_options(
    cx: Context,
    item: Handle<Value>,
    options_arg: Handle<Value>,
    method_name: &str,
) -> EvalResult<PlainTime> {
    if item.is_object() {
        // Check if item is a Temporal object of some kind
        let object = item.as_object();
        if let Some(plain_time) = object.as_plain_time_object() {
            return Ok(plain_time.time());
        }

        // TODO: Check for DateTime and ZonedDateTime objects

        // Otherwise treat item as a partial time object
        let partial_time = to_partial_time_record(cx, item, method_name)?;

        // Parse overflow option from options argument
        let options = validate_options_object(cx, options_arg, method_name)?;
        let overflow = get_overflow_option(cx, options, method_name)?;

        let plain_time_result = PlainTime::from_partial(partial_time, Some(overflow));

        return map_temporal_result(cx, plain_time_result, method_name);
    } else if !item.is_string() {
        return type_error(cx, &format!("{method_name} argument must be a string or object"));
    }

    // Otherwise parse PlainTime from string
    let wtf8_string = item.as_string().to_wtf8_string()?;

    let parsed_time_result = PlainTime::from_utf8(wtf8_string.as_bytes());
    let parsed_time = map_temporal_result(cx, parsed_time_result, method_name)?;

    let options = validate_options_object(cx, options_arg, method_name)?;
    get_overflow_option(cx, options, method_name)?;

    Ok(parsed_time)
}

/// ToTemporalTimeRecord (https://tc39.es/proposal-temporal/#sec-temporal-totemporaltimerecord)
pub fn to_partial_time_record(
    cx: Context,
    time_like_object: Handle<Value>,
    method_name: &str,
) -> EvalResult<PartialTime> {
    if !time_like_object.is_object() {
        return type_error(cx, &format!("{method_name} time must be an object"));
    }

    let object = time_like_object.as_object();

    let mut partial_time = PartialTime::default();

    let hour_value = get(cx, object, cx.names.hour())?;
    if !hour_value.is_undefined() {
        let hour = to_integer_with_truncation(cx, hour_value, method_name)?;
        partial_time = partial_time.with_hour(Some(hour));
    }

    let micro_value = get(cx, object, cx.names.microsecond())?;
    if !micro_value.is_undefined() {
        let micro = to_integer_with_truncation(cx, micro_value, method_name)?;
        partial_time = partial_time.with_microsecond(Some(micro));
    }

    let milli_value = get(cx, object, cx.names.millisecond())?;
    if !milli_value.is_undefined() {
        let milli = to_integer_with_truncation(cx, milli_value, method_name)?;
        partial_time = partial_time.with_millisecond(Some(milli));
    }

    let minute_value = get(cx, object, cx.names.minute())?;
    if !minute_value.is_undefined() {
        let minute = to_integer_with_truncation(cx, minute_value, method_name)?;
        partial_time = partial_time.with_minute(Some(minute));
    }

    let nano_value = get(cx, object, cx.names.nanosecond())?;
    if !nano_value.is_undefined() {
        let nano = to_integer_with_truncation(cx, nano_value, method_name)?;
        partial_time = partial_time.with_nanosecond(Some(nano));
    }

    let second_value = get(cx, object, cx.names.second())?;
    if !second_value.is_undefined() {
        let second = to_integer_with_truncation(cx, second_value, method_name)?;
        partial_time = partial_time.with_second(Some(second));
    }

    if partial_time.is_empty() {
        return type_error(cx, &format!("{method_name} time object is empty"));
    }

    Ok(partial_time)
}
