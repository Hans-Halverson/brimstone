use temporal_rs::PlainTime;

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
            plain_time_object::PlainTimeObject,
            utils::{
                get_overflow_option, map_temporal_result, to_integer_with_truncation_or_zero,
                to_partial_time_record, validate_options_object, validate_time_arguments,
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

        let Some(new_target) = cx.current_new_target() else {
            return type_error(cx, "Temporal.PlainTime constructor must be called with new");
        };

        // Truncate time arguments to signed ints. This effectively clamps the value on the upper
        // bound (since an error will be triggered later). But the lower bound must be checked
        // manually later. Signed width is enough to hold the maximum value of each time field.
        let hour_arg = get_argument(cx, arguments, 0);
        let hour = to_integer_with_truncation_or_zero::<i8>(cx, hour_arg, NAME)?;

        let minute_arg = get_argument(cx, arguments, 1);
        let minute = to_integer_with_truncation_or_zero::<i8>(cx, minute_arg, NAME)?;

        let second_arg = get_argument(cx, arguments, 2);
        let second = to_integer_with_truncation_or_zero::<i8>(cx, second_arg, NAME)?;

        let millis_arg = get_argument(cx, arguments, 3);
        let millis = to_integer_with_truncation_or_zero::<i16>(cx, millis_arg, NAME)?;

        let micros_arg = get_argument(cx, arguments, 4);
        let micros = to_integer_with_truncation_or_zero::<i16>(cx, micros_arg, NAME)?;

        let nanos_arg = get_argument(cx, arguments, 5);
        let nanos = to_integer_with_truncation_or_zero::<i16>(cx, nanos_arg, NAME)?;

        let (hour, minute, second, millis, micros, nanos) =
            validate_time_arguments(cx, hour, minute, second, millis, micros, nanos, NAME)?;

        let plain_time_result = PlainTime::try_new(hour, minute, second, millis, micros, nanos);
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
            let options = validate_options_object(cx, options_arg, method_name)?;
            get_overflow_option(cx, options, method_name)?;

            return Ok(plain_time.time());
        } else if let Some(plain_date_time) = object.as_plain_date_time_object() {
            let options = validate_options_object(cx, options_arg, method_name)?;
            get_overflow_option(cx, options, method_name)?;

            return Ok(plain_date_time.date_time().to_plain_time());
        } else if let Some(zoned_date_time) = object.as_zoned_date_time_object() {
            let options = validate_options_object(cx, options_arg, method_name)?;
            get_overflow_option(cx, options, method_name)?;

            return Ok(zoned_date_time.zoned_date_time().to_plain_time());
        }

        // Otherwise treat item as a partial time object
        let partial_record = to_partial_time_record(cx, item, method_name)?;

        // Parse overflow option from options argument
        let options = validate_options_object(cx, options_arg, method_name)?;
        let overflow = get_overflow_option(cx, options, method_name)?;

        let partial_time = partial_record.clamp_and_validate(cx, overflow, method_name)?;

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
