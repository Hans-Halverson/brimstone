use temporal_rs::{
    Duration,
    options::{RoundingMode, RoundingOptions, ToStringRoundingOptions},
};

use crate::{
    intrinsic_getter_methods, intrinsic_methods, must,
    runtime::{
        Context, EvalResult, Handle, Realm, Value,
        abstract_operations::create_data_property_or_throw,
        alloc_error::AllocResult,
        error::{range_error, type_error},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            temporal::{
                duration_constructor::{to_temporal_duration, to_temporal_partial_duration_record},
                duration_object::DurationObject,
                utils::{
                    get_fractional_second_digits_option, get_relative_to_option,
                    get_rounding_increment_option, get_rounding_mode_option,
                    get_unit_valued_option, map_temporal_result, parse_round_options_argument,
                    validate_options_object,
                },
            },
        },
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create_without_proto,
    },
    runtime_fn,
};

pub struct DurationPrototype;

impl DurationPrototype {
    /// Properties of the Temporal.Duration Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-duration-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once DurationConstructor has been created

        intrinsic_methods!(cx, builder, {
            with             DurationPrototype_with             (1),
            negated          DurationPrototype_negated          (0),
            abs              DurationPrototype_abs              (0),
            add              DurationPrototype_add              (1),
            subtract         DurationPrototype_subtract         (1),
            round            DurationPrototype_round            (1),
            total            DurationPrototype_total            (1),
            to_string        DurationPrototype_toString         (0),
            to_locale_string DurationPrototype_toLocaleString   (0),
            to_json          DurationPrototype_toJSON           (0),
            value_of         DurationPrototype_valueOf          (0),
        });

        intrinsic_getter_methods!(cx, builder, {
            years        DurationPrototype_years,
            months       DurationPrototype_months,
            weeks        DurationPrototype_weeks,
            days         DurationPrototype_days,
            hours        DurationPrototype_hours,
            minutes      DurationPrototype_minutes,
            seconds      DurationPrototype_seconds,
            milliseconds DurationPrototype_milliseconds,
            microseconds DurationPrototype_microseconds,
            nanoseconds  DurationPrototype_nanoseconds,
            sign         DurationPrototype_sign,
            blank        DurationPrototype_blank,
        });

        builder.to_string_tag(cx.names.temporal_duration())?;

        builder.build()
    }

    runtime_fn! {
    /// get Temporal.Duration.prototype.years (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.years)
    fn years(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.years")?;
        let years = duration.duration().years();

        Ok(cx.number(years))
    }}

    runtime_fn! {
    /// get Temporal.Duration.prototype.months (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.months)
    fn months(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.months")?;
        let months = duration.duration().months();

        Ok(cx.number(months))
    }}

    runtime_fn! {
    /// get Temporal.Duration.prototype.weeks (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.weeks)
    fn weeks(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.weeks")?;
        let weeks = duration.duration().weeks();

        Ok(cx.number(weeks))
    }}

    runtime_fn! {
    /// get Temporal.Duration.prototype.days (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.days)
    fn days(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.days")?;
        let days = duration.duration().days();

        Ok(cx.number(days))
    }}

    runtime_fn! {
    /// get Temporal.Duration.prototype.hours (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.hours)
    fn hours(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.hours")?;
        let hours = duration.duration().hours();

        Ok(cx.number(hours))
    }}

    runtime_fn! {
    /// get Temporal.Duration.prototype.minutes (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.minutes)
    fn minutes(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.minutes")?;
        let minutes = duration.duration().minutes();

        Ok(cx.number(minutes))
    }}

    runtime_fn! {
    /// get Temporal.Duration.prototype.seconds (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.seconds)
    fn seconds(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.seconds")?;
        let seconds = duration.duration().seconds();

        Ok(cx.number(seconds))
    }}

    runtime_fn! {
    /// get Temporal.Duration.prototype.milliseconds (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.milliseconds)
    fn milliseconds(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.milliseconds")?;
        let millis = duration.duration().milliseconds();

        Ok(cx.number(millis))
    }}

    runtime_fn! {
    /// get Temporal.Duration.prototype.microseconds (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.microseconds)
    fn microseconds(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.microseconds")?;
        let micros = duration.duration().microseconds();
        Ok(cx.number(micros as f64))
    }}

    runtime_fn! {
    /// get Temporal.Duration.prototype.nanoseconds (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.nanoseconds)
    fn nanoseconds(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.nanoseconds")?;
        let nanos = duration.duration().nanoseconds();
        Ok(cx.number(nanos as f64))
    }}

    runtime_fn! {
    /// get Temporal.Duration.prototype.sign (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.sign)
    fn sign(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.sign")?;
        let sign = duration.duration().sign();

        Ok(cx.smi(sign as i8))
    }}

    runtime_fn! {
    /// get Temporal.Duration.prototype.blank (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.blank)
    fn blank(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.blank")?;
        let is_zero = duration.duration().is_zero();

        Ok(cx.bool(is_zero))
    }}

    runtime_fn! {
    /// Temporal.Duration.prototype.with (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.with)
    fn with(cx, this_value, arguments) {
        const NAME: &str = "Duration.prototype.with";

        let duration_object = this_duration(cx, this_value, NAME)?;
        let duration_like_arg = arguments.get(cx, 0);

        let partial = to_temporal_partial_duration_record(cx, duration_like_arg, NAME)?;
        let duration = duration_object.duration();

        let years = partial.years.unwrap_or_else(|| duration.years());
        let months = partial.months.unwrap_or_else(|| duration.months());
        let weeks = partial.weeks.unwrap_or_else(|| duration.weeks());
        let days = partial.days.unwrap_or_else(|| duration.days());
        let hours = partial.hours.unwrap_or_else(|| duration.hours());
        let minutes = partial.minutes.unwrap_or_else(|| duration.minutes());
        let seconds = partial.seconds.unwrap_or_else(|| duration.seconds());
        let millis = partial
            .milliseconds
            .unwrap_or_else(|| duration.milliseconds());
        let micros = partial
            .microseconds
            .unwrap_or_else(|| duration.microseconds());
        let nanos = partial
            .nanoseconds
            .unwrap_or_else(|| duration.nanoseconds());

        let new_duration_result = Duration::new(
            years, months, weeks, days, hours, minutes, seconds, millis, micros, nanos,
        );
        let new_duration = map_temporal_result(cx, new_duration_result, NAME)?;

        Ok(DurationObject::new(cx, new_duration)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Duration.prototype.negated (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.negated)
    fn negated(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.negated")?;
        let negated_duration = duration.duration().negated();

        Ok(DurationObject::new(cx, negated_duration)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Duration.prototype.abs (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.abs)
    fn abs(cx, this_value, _) {
        let duration = this_duration(cx, this_value, "Duration.prototype.abs")?;
        let abs_duration = duration.duration().abs();

        Ok(DurationObject::new(cx, abs_duration)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Duration.prototype.add (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.add)
    fn add(cx, this_value, arguments) {
        const NAME: &str = "Duration.prototype.add";

        let duration = this_duration(cx, this_value, NAME)?;
        let other_arg = arguments.get(cx, 0);
        let other = to_temporal_duration(cx, other_arg, NAME)?;

        let sum_result = duration.duration().add(&other);
        let sum = map_temporal_result(cx, sum_result, NAME)?;

        Ok(DurationObject::new(cx, sum)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Duration.prototype.subtract (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.subtract)
    fn subtract(cx, this_value, arguments) {
        const NAME: &str = "Duration.prototype.subtract";

        let duration = this_duration(cx, this_value, NAME)?;
        let other_arg = arguments.get(cx, 0);
        let other = to_temporal_duration(cx, other_arg, NAME)?;

        let difference_result = duration.duration().subtract(&other);
        let difference = map_temporal_result(cx, difference_result, NAME)?;

        Ok(DurationObject::new(cx, difference)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Duration.prototype.round (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.round)
    fn round(cx, this_value, arguments) {
        const NAME: &str = "Duration.prototype.round";

        let duration = this_duration(cx, this_value, NAME)?;

        let options_arg = arguments.get(cx, 0);
        let options = parse_round_options_argument(cx, options_arg, NAME)?;

        // Parse rounding options from options object
        let largest_unit = get_unit_valued_option(cx, options, cx.names.largest_unit(), NAME)?;
        let relative_to = get_relative_to_option(cx, options, NAME)?;
        let increment = get_rounding_increment_option(cx, options, NAME)?;
        let rounding_mode = get_rounding_mode_option(cx, options, RoundingMode::HalfExpand, NAME)?;
        let smallest_unit = get_unit_valued_option(cx, options, cx.names.smallest_unit(), NAME)?;

        let mut rounding_options = RoundingOptions::default();
        rounding_options.largest_unit = largest_unit;
        rounding_options.smallest_unit = smallest_unit;
        rounding_options.rounding_mode = Some(rounding_mode);
        rounding_options.increment = Some(increment);

        let rounded_result = duration.duration().round_with_provider(
            rounding_options,
            relative_to,
            cx.temporal_provider(),
        );
        let rounded = map_temporal_result(cx, rounded_result, NAME)?;

        Ok(DurationObject::new(cx, rounded)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Duration.prototype.total (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.total)
    fn total(cx, this_value, arguments) {
        const NAME: &str = "Duration.prototype.total";

        let duration = this_duration(cx, this_value, NAME)?;
        let total_of_arg = arguments.get(cx, 0);

        // Options object. A string is shorthand for the `unit` option.
        let options = if total_of_arg.is_undefined() {
            return type_error(
                cx,
                "Duration.prototype.total argument must be a string or options object",
            );
        } else if total_of_arg.is_string() {
            let options = ordinary_object_create_without_proto(cx)?;
            must!(create_data_property_or_throw(cx, options, cx.names.unit(), total_of_arg));
            Some(options)
        } else {
            validate_options_object(cx, total_of_arg, NAME)?
        };

        // Parse total options from options object
        let relative_to = get_relative_to_option(cx, options, NAME)?;
        let unit = match get_unit_valued_option(cx, options, cx.names.unit(), NAME)? {
            Some(unit) => unit,
            None => return range_error(cx, "Duration.prototype.total requires a unit option"),
        };

        let total_result =
            duration
                .duration()
                .total_with_provider(unit, relative_to, cx.temporal_provider());
        let total = map_temporal_result(cx, total_result, NAME)?;

        Ok(cx.number(total.as_inner()))
    }}

    runtime_fn! {
    /// Temporal.Duration.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.tostring)
    fn to_string(cx, this_value, arguments) {
        const NAME: &str = "Duration.prototype.toString";

        let duration = this_duration(cx, this_value, NAME)?;

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

        let string_result = duration.duration().as_temporal_string(to_string_options);
        let string = map_temporal_result(cx, string_result, NAME)?;

        Ok(cx.alloc_string(&string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Duration.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.tolocalestring)
    fn to_locale_string(cx, this_value, _) {
        const NAME: &str = "Duration.prototype.toLocaleString";

        let duration = this_duration(cx, this_value, NAME)?;

        let string_result = duration
            .duration()
            .as_temporal_string(ToStringRoundingOptions::default());
        let string = map_temporal_result(cx, string_result, NAME)?;

        Ok(cx.alloc_string(&string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Duration.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.tojson)
    fn to_json(cx, this_value, _) {
        const NAME: &str = "Duration.prototype.toJSON";

        let duration = this_duration(cx, this_value, NAME)?;

        let string_result = duration
            .duration()
            .as_temporal_string(ToStringRoundingOptions::default());
        let string = map_temporal_result(cx, string_result, NAME)?;

        Ok(cx.alloc_string(&string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Duration.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.valueof)
    fn value_of(cx, _, _) {
        type_error(cx, "Duration.prototype.valueOf must not be called")
    }}
}

fn this_duration(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<DurationObject>> {
    if value.is_object() {
        if let Some(duration) = value.as_opt::<DurationObject>() {
            return Ok(duration);
        }
    }

    type_error(cx, &format!("{method_name} must be called on a Duration"))
}
