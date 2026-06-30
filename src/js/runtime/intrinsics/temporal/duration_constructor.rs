use temporal_rs::{Duration, partial::PartialDuration};

use crate::{
    intrinsic_methods,
    runtime::{
        Context, Handle, Realm, Value,
        alloc_error::AllocResult,
        error::type_error,
        eval_result::EvalResult,
        get,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
            temporal::{
                duration_object::DurationObject,
                utils::{
                    get_relative_to_option, map_temporal_result, to_integer_if_integral,
                    validate_options_object,
                },
            },
        },
        object_value::ObjectValue,
    },
    runtime_fn,
};

pub struct DurationConstructor;

impl DurationConstructor {
    /// Temporal.Duration Constructor (https://tc39.es/proposal-temporal/#sec-temporal-duration-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::DurationConstructor_construct,
            0,
            cx.names.duration(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::DurationPrototype)?;

        intrinsic_methods!(cx, builder, {
            from    DurationConstructor_from    (1),
            compare DurationConstructor_compare (2),
        });

        builder.build()
    }

    runtime_fn! {
    /// Temporal.Duration (https://tc39.es/proposal-temporal/#sec-temporal-duration)
    fn construct(cx, _, arguments) {
        const NAME: &str = "Temporal.Duration constructor";

        let Some(new_target) = cx.current_new_target() else {
            return type_error(cx, "Temporal.Duration constructor must be called with new");
        };

        let years_arg = arguments.get(cx, 0);
        let months_arg = arguments.get(cx, 1);
        let weeks_arg = arguments.get(cx, 2);
        let days_arg = arguments.get(cx, 3);
        let hours_arg = arguments.get(cx, 4);
        let minutes_arg = arguments.get(cx, 5);
        let seconds_arg = arguments.get(cx, 6);
        let millis_arg = arguments.get(cx, 7);
        let micros_arg = arguments.get(cx, 8);
        let nanos_arg = arguments.get(cx, 9);

        // Convert duration arguments into truncated integers
        let years = to_integer_if_integral(cx, years_arg, NAME)?;
        let months = to_integer_if_integral(cx, months_arg, NAME)?;
        let weeks = to_integer_if_integral(cx, weeks_arg, NAME)?;
        let days = to_integer_if_integral(cx, days_arg, NAME)?;
        let hours = to_integer_if_integral(cx, hours_arg, NAME)?;
        let minutes = to_integer_if_integral(cx, minutes_arg, NAME)?;
        let seconds = to_integer_if_integral(cx, seconds_arg, NAME)?;
        let millis = to_integer_if_integral(cx, millis_arg, NAME)?;
        let micros = to_integer_if_integral(cx, micros_arg, NAME)?;
        let nanos = to_integer_if_integral(cx, nanos_arg, NAME)?;

        let duration_result = Duration::new(
            years, months, weeks, days, hours, minutes, seconds, millis, micros, nanos,
        );

        let duration = map_temporal_result(cx, duration_result, NAME)?;

        Ok(DurationObject::new_from_constructor(cx, new_target, duration)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Duration.from (https://tc39.es/proposal-temporal/#sec-temporal.duration.from)
    fn from(cx, _, arguments) {
        let item_arg = arguments.get(cx, 0);
        let duration = to_temporal_duration(cx, item_arg, "Duration.from")?;

        Ok(DurationObject::new(cx, duration)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Duration.compare (https://tc39.es/proposal-temporal/#sec-temporal.duration.compare)
    fn compare(cx, _, arguments) {
        const NAME: &str = "Duration.compare";

        let arg_1 = arguments.get(cx, 0);
        let arg_2 = arguments.get(cx, 1);
        let options_arg = arguments.get(cx, 2);

        let duration_1 = to_temporal_duration(cx, arg_1, NAME)?;
        let duration_2 = to_temporal_duration(cx, arg_2, NAME)?;

        let options = validate_options_object(cx, options_arg, NAME)?;
        let relative_to = get_relative_to_option(cx, options, NAME)?;

        let ordering_result =
            duration_1.compare_with_provider(&duration_2, relative_to, cx.temporal_provider());
        let ordering = map_temporal_result(cx, ordering_result, NAME)?;

        Ok(cx.smi(ordering as i8))
    }}
}

/// ToTemporalDuration (https://tc39.es/proposal-temporal/#sec-temporal-totemporalduration)
pub fn to_temporal_duration(
    cx: Context,
    item: Handle<Value>,
    method_name: &str,
) -> EvalResult<Duration> {
    if item.is_object() {
        if let Some(duration_object) = item.as_opt::<DurationObject>() {
            return Ok(duration_object.duration());
        }
    } else if item.is_string() {
        let wtf8_string = item.as_string().to_wtf8_string()?;
        let parsed_duration_result = Duration::from_utf8(wtf8_string.as_bytes());
        return map_temporal_result(cx, parsed_duration_result, method_name);
    } else {
        return type_error(cx, &format!("{method_name} duration must be a string or object"));
    }

    let partial_duration = to_temporal_partial_duration_record(cx, item, method_name)?;
    let duration_result = Duration::from_partial_duration(partial_duration);

    map_temporal_result(cx, duration_result, method_name)
}

/// ToTemporalPartialDurationRecord (https://tc39.es/proposal-temporal/#sec-temporal-totemporalpartialdurationrecord)
pub fn to_temporal_partial_duration_record(
    cx: Context,
    item: Handle<Value>,
    method_name: &str,
) -> EvalResult<PartialDuration> {
    if !item.is_object() {
        return type_error(cx, &format!("{method_name} duration must be an object"));
    }

    let object = item.as_object();

    let mut partial_duration = PartialDuration::default();

    let days_value = get(cx, object, cx.names.days())?;
    if !days_value.is_undefined() {
        let days = to_integer_if_integral(cx, days_value, method_name)?;
        partial_duration = partial_duration.with_days(days);
    }

    let hours_value = get(cx, object, cx.names.hours())?;
    if !hours_value.is_undefined() {
        let hours = to_integer_if_integral(cx, hours_value, method_name)?;
        partial_duration = partial_duration.with_hours(hours);
    }

    let micros_value = get(cx, object, cx.names.microseconds())?;
    if !micros_value.is_undefined() {
        let micros = to_integer_if_integral(cx, micros_value, method_name)?;
        partial_duration = partial_duration.with_microseconds(micros);
    }

    let millis_value = get(cx, object, cx.names.milliseconds())?;
    if !millis_value.is_undefined() {
        let millis = to_integer_if_integral(cx, millis_value, method_name)?;
        partial_duration = partial_duration.with_milliseconds(millis);
    }

    let minutes_value = get(cx, object, cx.names.minutes())?;
    if !minutes_value.is_undefined() {
        let minutes = to_integer_if_integral(cx, minutes_value, method_name)?;
        partial_duration = partial_duration.with_minutes(minutes);
    }

    let months_value = get(cx, object, cx.names.months())?;
    if !months_value.is_undefined() {
        let months = to_integer_if_integral(cx, months_value, method_name)?;
        partial_duration = partial_duration.with_months(months);
    }

    let nanos_value = get(cx, object, cx.names.nanoseconds())?;
    if !nanos_value.is_undefined() {
        let nanos = to_integer_if_integral(cx, nanos_value, method_name)?;
        partial_duration = partial_duration.with_nanoseconds(nanos);
    }

    let seconds_value = get(cx, object, cx.names.seconds())?;
    if !seconds_value.is_undefined() {
        let seconds = to_integer_if_integral(cx, seconds_value, method_name)?;
        partial_duration = partial_duration.with_seconds(seconds);
    }

    let weeks_value = get(cx, object, cx.names.weeks())?;
    if !weeks_value.is_undefined() {
        let weeks = to_integer_if_integral(cx, weeks_value, method_name)?;
        partial_duration = partial_duration.with_weeks(weeks);
    }

    let years_value = get(cx, object, cx.names.years())?;
    if !years_value.is_undefined() {
        let years = to_integer_if_integral(cx, years_value, method_name)?;
        partial_duration = partial_duration.with_years(years);
    }

    if partial_duration.is_empty() {
        return type_error(cx, &format!("{method_name} duration object is empty"));
    }

    Ok(partial_duration)
}
