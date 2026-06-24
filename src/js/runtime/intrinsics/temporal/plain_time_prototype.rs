use temporal_rs::options::{RoundingMode, RoundingOptions, ToStringRoundingOptions};

use crate::{
    must,
    runtime::{
        Context, EvalResult, Handle, Realm, Value,
        abstract_operations::create_data_property_or_throw,
        alloc_error::AllocResult,
        error::type_error,
        function::get_argument,
        intrinsics::{
            intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
            temporal::{
                duration_constructor::to_temporal_duration,
                plain_time_constructor::{to_partial_time_record, to_temporal_time},
                plain_time_object::PlainTimeObject,
                utils::{
                    get_fractional_second_digits_option, get_overflow_option,
                    get_rounding_increment_option, get_rounding_mode_option,
                    get_unit_valued_option, is_partial_temporal_object, map_temporal_result,
                    validate_options_object,
                },
            },
        },
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create_without_proto,
        property::Property,
    },
};

pub struct PlainTimePrototype;

impl PlainTimePrototype {
    /// Properties of the Temporal.PlainTime Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaintime-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once PlainTimeConstructor has been created

        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.temporal_plain_time().as_string().into(), false, false, true),
        )?;

        // Getters
        object.intrinsic_getter(
            cx,
            cx.names.hour(),
            RuntimeFunction::PlainTimePrototype_hour,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.minute(),
            RuntimeFunction::PlainTimePrototype_minute,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.second(),
            RuntimeFunction::PlainTimePrototype_second,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.millisecond(),
            RuntimeFunction::PlainTimePrototype_millisecond,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.microsecond(),
            RuntimeFunction::PlainTimePrototype_microsecond,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.nanosecond(),
            RuntimeFunction::PlainTimePrototype_nanosecond,
            realm,
        )?;

        // Methods
        object.intrinsic_func(
            cx,
            cx.names.add(),
            RuntimeFunction::PlainTimePrototype_add,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.subtract(),
            RuntimeFunction::PlainTimePrototype_subtract,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.with(),
            RuntimeFunction::PlainTimePrototype_with,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.until(),
            RuntimeFunction::PlainTimePrototype_until,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.since(),
            RuntimeFunction::PlainTimePrototype_since,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.round(),
            RuntimeFunction::PlainTimePrototype_round,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.equals(),
            RuntimeFunction::PlainTimePrototype_equals,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_string(),
            RuntimeFunction::PlainTimePrototype_toString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_locale_string(),
            RuntimeFunction::PlainTimePrototype_toLocaleString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_json(),
            RuntimeFunction::PlainTimePrototype_toJSON,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.value_of(),
            RuntimeFunction::PlainTimePrototype_valueOf,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_plain_date_time(),
            RuntimeFunction::PlainTimePrototype_toPlainDateTime,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_zoned_date_time(),
            RuntimeFunction::PlainTimePrototype_toZonedDateTime,
            1,
            realm,
        )?;

        Ok(object)
    }

    /// get Temporal.PlainTime.prototype.hour (https://tc39.es/proposal-temporal/#sec-get-temporal.plaintime.prototype.hour)
    pub fn hour(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_time = this_plain_time(cx, this_value, "PlainTime.prototype.hour")?;
        let hour = this_time.time().hour();

        Ok(cx.smi(hour as i32))
    }

    /// get Temporal.PlainTime.prototype.minute (https://tc39.es/proposal-temporal/#sec-get-temporal.plaintime.prototype.minute)
    pub fn minute(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_time = this_plain_time(cx, this_value, "PlainTime.prototype.minute")?;
        let minute = this_time.time().minute();

        Ok(cx.smi(minute as i32))
    }

    /// get Temporal.PlainTime.prototype.second (https://tc39.es/proposal-temporal/#sec-get-temporal.plaintime.prototype.second)
    pub fn second(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_time = this_plain_time(cx, this_value, "PlainTime.prototype.second")?;
        let second = this_time.time().second();

        Ok(cx.smi(second as i32))
    }

    /// get Temporal.PlainTime.prototype.millisecond (https://tc39.es/proposal-temporal/#sec-get-temporal.plaintime.prototype.millisecond)
    pub fn millisecond(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_time = this_plain_time(cx, this_value, "PlainTime.prototype.millisecond")?;
        let millis = this_time.time().millisecond();

        Ok(cx.smi(millis as i32))
    }

    /// get Temporal.PlainTime.prototype.microsecond (https://tc39.es/proposal-temporal/#sec-get-temporal.plaintime.prototype.microsecond)
    pub fn microsecond(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_time = this_plain_time(cx, this_value, "PlainTime.prototype.microsecond")?;
        let micros = this_time.time().microsecond();

        Ok(cx.smi(micros as i32))
    }

    /// get Temporal.PlainTime.prototype.nanosecond (https://tc39.es/proposal-temporal/#sec-get-temporal.plaintime.prototype.nanosecond)
    pub fn nanosecond(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_time = this_plain_time(cx, this_value, "PlainTime.prototype.nanosecond")?;
        let nanos = this_time.time().nanosecond();

        Ok(cx.smi(nanos as i32))
    }

    /// Temporal.PlainTime.prototype.add (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.add)
    pub fn add(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainTime.prototype.add";

        let this_time = this_plain_time(cx, this_value, NAME)?;

        let duration_arg = get_argument(cx, arguments, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let new_time_result = this_time.time().add(&duration);
        let new_time = map_temporal_result(cx, new_time_result, NAME)?;

        Ok(PlainTimeObject::new(cx, new_time)?.as_value())
    }

    /// Temporal.PlainTime.prototype.subtract (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.subtract)
    pub fn subtract(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainTime.prototype.subtract";

        let this_time = this_plain_time(cx, this_value, NAME)?;

        let duration_arg = get_argument(cx, arguments, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let new_time_result = this_time.time().subtract(&duration);
        let new_time = map_temporal_result(cx, new_time_result, NAME)?;

        Ok(PlainTimeObject::new(cx, new_time)?.as_value())
    }

    /// Temporal.PlainTime.prototype.with (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.with)
    pub fn with(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainTime.prototype.with";

        let plain_time = this_plain_time(cx, this_value, NAME)?;
        let time_like_arg = get_argument(cx, arguments, 0);

        if !is_partial_temporal_object(cx, time_like_arg)? {
            return type_error(
                cx,
                "PlainTime.prototype.with argument must be a partial time object",
            );
        }

        let partial = to_partial_time_record(cx, time_like_arg, NAME)?;

        let options_arg = get_argument(cx, arguments, 1);
        let options = validate_options_object(cx, options_arg, NAME)?;
        let overflow = get_overflow_option(cx, options, NAME)?;

        let new_time_result = plain_time.time().with(partial, Some(overflow));
        let new_time = map_temporal_result(cx, new_time_result, NAME)?;

        Ok(PlainTimeObject::new(cx, new_time)?.as_value())
    }

    /// Temporal.PlainTime.prototype.until (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.until)
    pub fn until(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_time(cx, this_value, "PlainTime.prototype.until")?;
        unimplemented!("PlainTime.prototype.until")
    }

    /// Temporal.PlainTime.prototype.since (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.since)
    pub fn since(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_time(cx, this_value, "PlainTime.prototype.since")?;
        unimplemented!("PlainTime.prototype.since")
    }

    /// Temporal.PlainTime.prototype.round (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.round)
    pub fn round(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainTime.prototype.round";

        let plain_time = this_plain_time(cx, this_value, NAME)?;
        let round_to_arg = get_argument(cx, arguments, 0);

        // Options object. A string is shorthand for the `smallestUnit` option.
        let options = if round_to_arg.is_undefined() {
            return type_error(
                cx,
                "PlainTime.prototype.round argument must be a string or options object",
            );
        } else if round_to_arg.is_string() {
            let options = ordinary_object_create_without_proto(cx)?;
            must!(create_data_property_or_throw(
                cx,
                options,
                cx.names.smallest_unit(),
                round_to_arg
            ));
            Some(options)
        } else {
            validate_options_object(cx, round_to_arg, NAME)?
        };

        // Parse rounding from options object
        let mut rounding_options = RoundingOptions::default();

        rounding_options.increment = Some(get_rounding_increment_option(cx, options, NAME)?);
        rounding_options.rounding_mode =
            Some(get_rounding_mode_option(cx, options, RoundingMode::HalfExpand, NAME)?);
        rounding_options.smallest_unit =
            get_unit_valued_option(cx, options, cx.names.smallest_unit(), NAME)?;

        let rounded_result = plain_time.time().round(rounding_options);
        let rounded = map_temporal_result(cx, rounded_result, NAME)?;

        Ok(PlainTimeObject::new(cx, rounded)?.as_value())
    }

    /// Temporal.PlainTime.prototype.equals (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.equals)
    pub fn equals(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainTime.prototype.equals";

        let this_time = this_plain_time(cx, this_value, NAME)?;

        let other_arg = get_argument(cx, arguments, 0);
        let other_time = to_temporal_time(cx, other_arg, NAME)?;

        Ok(cx.bool(this_time.time() == other_time))
    }

    /// Temporal.PlainTime.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.tostring)
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainTime.prototype.toString";

        let this_time = this_plain_time(cx, this_value, NAME)?;

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

        let time_string_result = this_time.time().to_ixdtf_string(to_string_options);
        let time_string = map_temporal_result(cx, time_string_result, NAME)?;

        Ok(cx.alloc_string(&time_string)?.as_value())
    }

    /// Temporal.PlainTime.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.tolocalestring)
    pub fn to_locale_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainTime.prototype.toLocaleString";

        let this_time = this_plain_time(cx, this_value, NAME)?;
        let time_string_result = this_time
            .time()
            .to_ixdtf_string(ToStringRoundingOptions::default());
        let time_string = map_temporal_result(cx, time_string_result, NAME)?;

        Ok(cx.alloc_string(&time_string)?.as_value())
    }

    /// Temporal.PlainTime.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.tojson)
    pub fn to_json(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "PlainTime.prototype.toJSON";

        let this_time = this_plain_time(cx, this_value, NAME)?;
        let time_string_result = this_time
            .time()
            .to_ixdtf_string(ToStringRoundingOptions::default());
        let time_string = map_temporal_result(cx, time_string_result, NAME)?;

        Ok(cx.alloc_string(&time_string)?.as_value())
    }

    /// Temporal.PlainTime.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.valueof)
    pub fn value_of(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        type_error(cx, "PlainTime.prototype.valueOf must not be called")
    }

    /// Temporal.PlainTime.prototype.toPlainDateTime (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.toplaindatetime)
    pub fn to_plain_date_time(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_time(cx, this_value, "PlainTime.prototype.toPlainDateTime")?;
        unimplemented!("PlainTime.prototype.toPlainDateTime")
    }

    /// Temporal.PlainTime.prototype.toZonedDateTime (https://tc39.es/proposal-temporal/#sec-temporal.plaintime.prototype.tozoneddatetime)
    pub fn to_zoned_date_time(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_plain_time(cx, this_value, "PlainTime.prototype.toZonedDateTime")?;
        unimplemented!("PlainTime.prototype.toZonedDateTime")
    }
}

fn this_plain_time(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<PlainTimeObject>> {
    if value.is_object() {
        if let Some(time) = value.as_object().as_plain_time_object() {
            return Ok(time);
        }
    }

    type_error(cx, &format!("{method_name} must be called on a PlainTime"))
}
