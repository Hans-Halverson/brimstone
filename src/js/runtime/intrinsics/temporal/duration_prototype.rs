use temporal_rs::{
    Duration,
    options::{RoundingMode, RoundingOptions, ToStringRoundingOptions},
};

use crate::{
    must,
    runtime::{
        Context, EvalResult, Handle, Realm, Value,
        abstract_operations::create_data_property_or_throw,
        alloc_error::AllocResult,
        error::{range_error, type_error},
        function::get_argument,
        intrinsics::{
            intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
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
        property::Property,
    },
};

pub struct DurationPrototype;

impl DurationPrototype {
    /// Properties of the Temporal.Duration Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-duration-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once DurationConstructor has been created

        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.temporal_duration().as_string().into(), false, false, true),
        )?;

        // Getters
        object.intrinsic_getter(
            cx,
            cx.names.years(),
            RuntimeFunction::DurationPrototype_years,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.months(),
            RuntimeFunction::DurationPrototype_months,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.weeks(),
            RuntimeFunction::DurationPrototype_weeks,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.days(),
            RuntimeFunction::DurationPrototype_days,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.hours(),
            RuntimeFunction::DurationPrototype_hours,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.minutes(),
            RuntimeFunction::DurationPrototype_minutes,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.seconds(),
            RuntimeFunction::DurationPrototype_seconds,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.milliseconds(),
            RuntimeFunction::DurationPrototype_milliseconds,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.microseconds(),
            RuntimeFunction::DurationPrototype_microseconds,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.nanoseconds(),
            RuntimeFunction::DurationPrototype_nanoseconds,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.sign(),
            RuntimeFunction::DurationPrototype_sign,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.blank(),
            RuntimeFunction::DurationPrototype_blank,
            realm,
        )?;

        // Methods
        object.intrinsic_func(
            cx,
            cx.names.with(),
            RuntimeFunction::DurationPrototype_with,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.negated(),
            RuntimeFunction::DurationPrototype_negated,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.abs(),
            RuntimeFunction::DurationPrototype_abs,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.add(),
            RuntimeFunction::DurationPrototype_add,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.subtract(),
            RuntimeFunction::DurationPrototype_subtract,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.round(),
            RuntimeFunction::DurationPrototype_round,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.total(),
            RuntimeFunction::DurationPrototype_total,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_string(),
            RuntimeFunction::DurationPrototype_toString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_locale_string(),
            RuntimeFunction::DurationPrototype_toLocaleString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_json(),
            RuntimeFunction::DurationPrototype_toJSON,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.value_of(),
            RuntimeFunction::DurationPrototype_valueOf,
            0,
            realm,
        )?;

        Ok(object)
    }

    /// get Temporal.Duration.prototype.years (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.years)
    pub fn years(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.years")?;
        let years = duration.duration().years();

        Ok(cx.number(years))
    }

    /// get Temporal.Duration.prototype.months (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.months)
    pub fn months(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.months")?;
        let months = duration.duration().months();

        Ok(cx.number(months))
    }

    /// get Temporal.Duration.prototype.weeks (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.weeks)
    pub fn weeks(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.weeks")?;
        let weeks = duration.duration().weeks();

        Ok(cx.number(weeks))
    }

    /// get Temporal.Duration.prototype.days (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.days)
    pub fn days(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.days")?;
        let days = duration.duration().days();

        Ok(cx.number(days))
    }

    /// get Temporal.Duration.prototype.hours (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.hours)
    pub fn hours(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.hours")?;
        let hours = duration.duration().hours();

        Ok(cx.number(hours))
    }

    /// get Temporal.Duration.prototype.minutes (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.minutes)
    pub fn minutes(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.minutes")?;
        let minutes = duration.duration().minutes();

        Ok(cx.number(minutes))
    }

    /// get Temporal.Duration.prototype.seconds (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.seconds)
    pub fn seconds(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.seconds")?;
        let seconds = duration.duration().seconds();

        Ok(cx.number(seconds))
    }

    /// get Temporal.Duration.prototype.milliseconds (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.milliseconds)
    pub fn milliseconds(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.milliseconds")?;
        let millis = duration.duration().milliseconds();

        Ok(cx.number(millis))
    }

    /// get Temporal.Duration.prototype.microseconds (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.microseconds)
    pub fn microseconds(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.microseconds")?;
        let micros = duration.duration().microseconds();
        Ok(cx.number(micros as f64))
    }

    /// get Temporal.Duration.prototype.nanoseconds (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.nanoseconds)
    pub fn nanoseconds(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.nanoseconds")?;
        let nanos = duration.duration().nanoseconds();
        Ok(cx.number(nanos as f64))
    }

    /// get Temporal.Duration.prototype.sign (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.sign)
    pub fn sign(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.sign")?;
        let sign = duration.duration().sign();

        Ok(cx.smi(sign as i8))
    }

    /// get Temporal.Duration.prototype.blank (https://tc39.es/proposal-temporal/#sec-get-temporal.duration.prototype.blank)
    pub fn blank(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.blank")?;
        let is_zero = duration.duration().is_zero();

        Ok(cx.bool(is_zero))
    }

    /// Temporal.Duration.prototype.with (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.with)
    pub fn with(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Duration.prototype.with";

        let duration_object = this_duration(cx, this_value, NAME)?;
        let duration_like_arg = get_argument(cx, arguments, 0);

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
    }

    /// Temporal.Duration.prototype.negated (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.negated)
    pub fn negated(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.negated")?;
        let negated_duration = duration.duration().negated();

        Ok(DurationObject::new(cx, negated_duration)?.as_value())
    }

    /// Temporal.Duration.prototype.abs (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.abs)
    pub fn abs(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let duration = this_duration(cx, this_value, "Duration.prototype.abs")?;
        let abs_duration = duration.duration().abs();

        Ok(DurationObject::new(cx, abs_duration)?.as_value())
    }

    /// Temporal.Duration.prototype.add (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.add)
    pub fn add(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Duration.prototype.add";

        let duration = this_duration(cx, this_value, NAME)?;
        let other_arg = get_argument(cx, arguments, 0);
        let other = to_temporal_duration(cx, other_arg, NAME)?;

        let sum_result = duration.duration().add(&other);
        let sum = map_temporal_result(cx, sum_result, NAME)?;

        Ok(DurationObject::new(cx, sum)?.as_value())
    }

    /// Temporal.Duration.prototype.subtract (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.subtract)
    pub fn subtract(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Duration.prototype.subtract";

        let duration = this_duration(cx, this_value, NAME)?;
        let other_arg = get_argument(cx, arguments, 0);
        let other = to_temporal_duration(cx, other_arg, NAME)?;

        let difference_result = duration.duration().subtract(&other);
        let difference = map_temporal_result(cx, difference_result, NAME)?;

        Ok(DurationObject::new(cx, difference)?.as_value())
    }

    /// Temporal.Duration.prototype.round (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.round)
    pub fn round(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Duration.prototype.round";

        let duration = this_duration(cx, this_value, NAME)?;

        let options_arg = get_argument(cx, arguments, 0);
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
    }

    /// Temporal.Duration.prototype.total (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.total)
    pub fn total(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Duration.prototype.total";

        let duration = this_duration(cx, this_value, NAME)?;
        let total_of_arg = get_argument(cx, arguments, 0);

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
    }

    /// Temporal.Duration.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.tostring)
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Duration.prototype.toString";

        let duration = this_duration(cx, this_value, NAME)?;

        let options_arg = get_argument(cx, arguments, 0);
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
    }

    /// Temporal.Duration.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.tolocalestring)
    pub fn to_locale_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Duration.prototype.toLocaleString";

        let duration = this_duration(cx, this_value, NAME)?;

        let string_result = duration
            .duration()
            .as_temporal_string(ToStringRoundingOptions::default());
        let string = map_temporal_result(cx, string_result, NAME)?;

        Ok(cx.alloc_string(&string)?.as_value())
    }

    /// Temporal.Duration.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.tojson)
    pub fn to_json(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Duration.prototype.toJSON";

        let duration = this_duration(cx, this_value, NAME)?;

        let string_result = duration
            .duration()
            .as_temporal_string(ToStringRoundingOptions::default());
        let string = map_temporal_result(cx, string_result, NAME)?;

        Ok(cx.alloc_string(&string)?.as_value())
    }

    /// Temporal.Duration.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.duration.prototype.valueof)
    pub fn value_of(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        type_error(cx, "Duration.prototype.valueOf must not be called")
    }
}

fn this_duration(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<DurationObject>> {
    if value.is_object() {
        if let Some(duration) = value.as_object().as_duration_object() {
            return Ok(duration);
        }
    }

    type_error(cx, &format!("{method_name} must be called on a Duration"))
}
