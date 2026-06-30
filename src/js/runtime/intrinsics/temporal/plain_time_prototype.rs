use temporal_rs::options::{RoundingMode, RoundingOptions, ToStringRoundingOptions};

use crate::{
    intrinsic_getter_methods, intrinsic_methods,
    runtime::{
        Arguments, Context, EvalResult, Handle, Realm, Value,
        alloc_error::AllocResult,
        error::type_error,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            temporal::{
                duration_constructor::to_temporal_duration,
                duration_object::DurationObject,
                plain_time_constructor::to_temporal_time,
                plain_time_object::PlainTimeObject,
                utils::{
                    DiffOperation, get_difference_settings, get_fractional_second_digits_option,
                    get_overflow_option, get_rounding_increment_option, get_rounding_mode_option,
                    get_unit_valued_option, is_partial_temporal_object, map_temporal_result,
                    parse_round_options_argument, to_partial_time_record, validate_options_object,
                },
            },
        },
        object_value::ObjectValue,
    },
    runtime_fn,
};

pub struct PlainTimePrototype;

impl PlainTimePrototype {
    /// Properties of the Temporal.PlainTime Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaintime-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once PlainTimeConstructor has been created

        intrinsic_methods!(cx, builder, {
            add              PlainTimePrototype_add              (1),
            subtract         PlainTimePrototype_subtract         (1),
            with             PlainTimePrototype_with             (1),
            until            PlainTimePrototype_until            (1),
            since            PlainTimePrototype_since            (1),
            round            PlainTimePrototype_round            (1),
            equals           PlainTimePrototype_equals           (1),
            to_string        PlainTimePrototype_toString         (0),
            to_locale_string PlainTimePrototype_toLocaleString   (0),
            to_json          PlainTimePrototype_toJSON           (0),
            value_of         PlainTimePrototype_valueOf          (0),
        });

        intrinsic_getter_methods!(cx, builder, {
            hour        PlainTimePrototype_hour,
            minute      PlainTimePrototype_minute,
            second      PlainTimePrototype_second,
            millisecond PlainTimePrototype_millisecond,
            microsecond PlainTimePrototype_microsecond,
            nanosecond  PlainTimePrototype_nanosecond,
        });

        builder.to_string_tag(cx.names.temporal_plain_time())?;

        builder.build()
    }

    runtime_fn! {
    /// get Temporal.PlainTime.prototype.hour (https://tc39.es/proposal-temporal/#sec-get-temporal.plaintime.prototype.hour)
    fn hour(cx, this_value, _) {
        let this_time = this_plain_time(cx, this_value, "PlainTime.prototype.hour")?;
        let hour = this_time.time().hour();

        Ok(cx.smi(hour))
    }}

    runtime_fn! {
    /// get Temporal.PlainTime.prototype.minute (https://tc39.es/proposal-temporal/#sec-get-temporal.plaintime.prototype.minute)
    fn minute(cx, this_value, _) {
        let this_time = this_plain_time(cx, this_value, "PlainTime.prototype.minute")?;
        let minute = this_time.time().minute();

        Ok(cx.smi(minute))
    }}

    runtime_fn! {
    /// get Temporal.PlainTime.prototype.second (https://tc39.es/proposal-temporal/#sec-get-temporal.plaintime.prototype.second)
    fn second(cx, this_value, _) {
        let this_time = this_plain_time(cx, this_value, "PlainTime.prototype.second")?;
        let second = this_time.time().second();

        Ok(cx.smi(second))
    }}

    runtime_fn! {
    /// get Temporal.PlainTime.prototype.millisecond (https://tc39.es/proposal-temporal/#sec-get-temporal.plaintime.prototype.millisecond)
    fn millisecond(cx, this_value, _) {
        let this_time = this_plain_time(cx, this_value, "PlainTime.prototype.millisecond")?;
        let millis = this_time.time().millisecond();

        Ok(cx.smi(millis))
    }}

    runtime_fn! {
    /// get Temporal.PlainTime.prototype.microsecond (https://tc39.es/proposal-temporal/#sec-get-temporal.plaintime.prototype.microsecond)
    fn microsecond(cx, this_value, _) {
        let this_time = this_plain_time(cx, this_value, "PlainTime.prototype.microsecond")?;
        let micros = this_time.time().microsecond();

        Ok(cx.smi(micros))
    }}

    runtime_fn! {
    /// get Temporal.PlainTime.prototype.nanosecond (https://tc39.es/proposal-temporal/#sec-get-temporal.plaintime.prototype.nanosecond)
    fn nanosecond(cx, this_value, _) {
        let this_time = this_plain_time(cx, this_value, "PlainTime.prototype.nanosecond")?;
        let nanos = this_time.time().nanosecond();

        Ok(cx.smi(nanos))
    }}

    runtime_fn! {
    /// Temporal.PlainTime.prototype.add (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.add)
    fn add(cx, this_value, arguments) {
        const NAME: &str = "PlainTime.prototype.add";

        let this_time = this_plain_time(cx, this_value, NAME)?;

        let duration_arg = arguments.get(cx, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let new_time_result = this_time.time().add(&duration);
        let new_time = map_temporal_result(cx, new_time_result, NAME)?;

        Ok(PlainTimeObject::new(cx, new_time)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainTime.prototype.subtract (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.subtract)
    fn subtract(cx, this_value, arguments) {
        const NAME: &str = "PlainTime.prototype.subtract";

        let this_time = this_plain_time(cx, this_value, NAME)?;

        let duration_arg = arguments.get(cx, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let new_time_result = this_time.time().subtract(&duration);
        let new_time = map_temporal_result(cx, new_time_result, NAME)?;

        Ok(PlainTimeObject::new(cx, new_time)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainTime.prototype.with (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.with)
    fn with(cx, this_value, arguments) {
        const NAME: &str = "PlainTime.prototype.with";

        let plain_time = this_plain_time(cx, this_value, NAME)?;
        let time_like_arg = arguments.get(cx, 0);

        if !is_partial_temporal_object(cx, time_like_arg)? {
            return type_error(
                cx,
                "PlainTime.prototype.with argument must be a partial time object",
            );
        }

        let partial_record = to_partial_time_record(cx, time_like_arg, NAME)?;

        let options_arg = arguments.get(cx, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let partial_time = partial_record.clamp_and_validate(cx, overflow, NAME)?;

        let new_time_result = plain_time.time().with(partial_time, Some(overflow));
        let new_time = map_temporal_result(cx, new_time_result, NAME)?;

        Ok(PlainTimeObject::new(cx, new_time)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainTime.prototype.until (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.until)
    fn until(cx, this_value, arguments) {
        Self::diff(cx, this_value, arguments, DiffOperation::Until, "PlainTime.prototype.until")
    }}

    runtime_fn! {
    /// Temporal.PlainTime.prototype.since (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.since)
    fn since(cx, this_value, arguments) {
        Self::diff(cx, this_value, arguments, DiffOperation::Since, "PlainTime.prototype.since")
    }}

    fn diff(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
        operation: DiffOperation,
        method_name: &str,
    ) -> EvalResult<Handle<Value>> {
        let plain_time = this_plain_time(cx, this_value, method_name)?;

        let other_arg = arguments.get(cx, 0);
        let other = to_temporal_time(cx, other_arg, method_name)?;

        let options_arg = arguments.get(cx, 1);
        let options = validate_options_object(cx, options_arg, method_name)?;
        let difference_settings = get_difference_settings(cx, options, method_name)?;

        let duration_result = match operation {
            DiffOperation::Until => plain_time.time().until(&other, difference_settings),
            DiffOperation::Since => plain_time.time().since(&other, difference_settings),
        };

        let duration = map_temporal_result(cx, duration_result, method_name)?;

        Ok(DurationObject::new(cx, duration)?.as_value())
    }

    runtime_fn! {
    /// Temporal.PlainTime.prototype.round (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.round)
    fn round(cx, this_value, arguments) {
        const NAME: &str = "PlainTime.prototype.round";

        let plain_time = this_plain_time(cx, this_value, NAME)?;

        let options_arg = arguments.get(cx, 0);
        let options = parse_round_options_argument(cx, options_arg, NAME)?;

        // Parse rounding from options object
        let increment = get_rounding_increment_option(cx, options, NAME)?;
        let rounding_mode = get_rounding_mode_option(cx, options, RoundingMode::HalfExpand, NAME)?;
        let smallest_unit = get_unit_valued_option(cx, options, cx.names.smallest_unit(), NAME)?;

        let mut rounding_options = RoundingOptions::default();
        rounding_options.increment = Some(increment);
        rounding_options.rounding_mode = Some(rounding_mode);
        rounding_options.smallest_unit = smallest_unit;

        let rounded_result = plain_time.time().round(rounding_options);
        let rounded = map_temporal_result(cx, rounded_result, NAME)?;

        Ok(PlainTimeObject::new(cx, rounded)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainTime.prototype.equals (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.equals)
    fn equals(cx, this_value, arguments) {
        const NAME: &str = "PlainTime.prototype.equals";

        let this_time = this_plain_time(cx, this_value, NAME)?;

        let other_arg = arguments.get(cx, 0);
        let other_time = to_temporal_time(cx, other_arg, NAME)?;

        Ok(cx.bool(this_time.time() == other_time))
    }}

    runtime_fn! {
    /// Temporal.PlainTime.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.tostring)
    fn to_string(cx, this_value, arguments) {
        const NAME: &str = "PlainTime.prototype.toString";

        let this_time = this_plain_time(cx, this_value, NAME)?;

        let options_arg = arguments.get(cx, 0);
        let options = validate_options_object(cx, options_arg, NAME)?;

        // Parse rounding options from options object
        let precision = get_fractional_second_digits_option(cx, options, NAME)?;
        let rounding_mode = get_rounding_mode_option(cx, options, RoundingMode::Trunc, NAME)?;
        let smallest_unit = get_unit_valued_option(cx, options, cx.names.smallest_unit(), NAME)?;

        let to_string_options = ToStringRoundingOptions {
            precision,
            smallest_unit,
            rounding_mode: Some(rounding_mode),
        };

        let time_string_result = this_time.time().to_ixdtf_string(to_string_options);
        let time_string = map_temporal_result(cx, time_string_result, NAME)?;

        Ok(cx.alloc_string(&time_string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainTime.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.tolocalestring)
    fn to_locale_string(cx, this_value, _) {
        const NAME: &str = "PlainTime.prototype.toLocaleString";

        let this_time = this_plain_time(cx, this_value, NAME)?;
        let time_string_result = this_time
            .time()
            .to_ixdtf_string(ToStringRoundingOptions::default());
        let time_string = map_temporal_result(cx, time_string_result, NAME)?;

        Ok(cx.alloc_string(&time_string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainTime.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.tojson)
    fn to_json(cx, this_value, _) {
        const NAME: &str = "PlainTime.prototype.toJSON";

        let this_time = this_plain_time(cx, this_value, NAME)?;
        let time_string_result = this_time
            .time()
            .to_ixdtf_string(ToStringRoundingOptions::default());
        let time_string = map_temporal_result(cx, time_string_result, NAME)?;

        Ok(cx.alloc_string(&time_string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.PlainTime.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.valueof)
    fn value_of(cx, _, _) {
        type_error(cx, "PlainTime.prototype.valueOf must not be called")
    }}
}

fn this_plain_time(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<PlainTimeObject>> {
    if value.is_object() {
        if let Some(time) = value.as_opt::<PlainTimeObject>() {
            return Ok(time);
        }
    }

    type_error(cx, &format!("{method_name} must be called on a PlainTime"))
}
