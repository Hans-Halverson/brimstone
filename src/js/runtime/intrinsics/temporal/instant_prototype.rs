use num_bigint::BigInt;
use temporal_rs::options::{RoundingMode, RoundingOptions, ToStringRoundingOptions};

use crate::runtime::{
    Context, EvalResult, Handle, Realm, Value,
    alloc_error::AllocResult,
    error::type_error,
    function::get_argument,
    intrinsics::{
        intrinsics::Intrinsic,
        rust_runtime::RuntimeFunction,
        temporal::{
            duration_constructor::to_temporal_duration,
            instant_constructor::to_temporal_instant,
            instant_object::InstantObject,
            utils::{
                get_fractional_second_digits_option, get_rounding_increment_option,
                get_rounding_mode_option, get_time_zone_option, get_unit_valued_option,
                map_temporal_result, parse_round_options_argument, to_time_zone_identifier,
                validate_options_object,
            },
            zoned_date_time_object::ZonedDateTimeObject,
        },
    },
    object_value::ObjectValue,
    property::Property,
    value::BigIntValue,
};

pub struct InstantPrototype;

impl InstantPrototype {
    /// Properties of the Temporal.Instant Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-instant-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once InstantConstructor has been created

        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.temporal_instant().as_string().into(), false, false, true),
        )?;

        // Getters
        object.intrinsic_getter(
            cx,
            cx.names.epoch_milliseconds(),
            RuntimeFunction::InstantPrototype_epochMilliseconds,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.epoch_nanoseconds(),
            RuntimeFunction::InstantPrototype_epochNanoseconds,
            realm,
        )?;

        // Methods
        object.intrinsic_func(
            cx,
            cx.names.add(),
            RuntimeFunction::InstantPrototype_add,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.subtract(),
            RuntimeFunction::InstantPrototype_subtract,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.until(),
            RuntimeFunction::InstantPrototype_until,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.since(),
            RuntimeFunction::InstantPrototype_since,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.round(),
            RuntimeFunction::InstantPrototype_round,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.equals(),
            RuntimeFunction::InstantPrototype_equals,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_string(),
            RuntimeFunction::InstantPrototype_toString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_locale_string(),
            RuntimeFunction::InstantPrototype_toLocaleString,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_json(),
            RuntimeFunction::InstantPrototype_toJSON,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.value_of(),
            RuntimeFunction::InstantPrototype_valueOf,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.to_zoned_date_time_iso(),
            RuntimeFunction::InstantPrototype_toZonedDateTimeISO,
            1,
            realm,
        )?;

        Ok(object)
    }

    /// get Temporal.Instant.prototype.epochMilliseconds (https://tc39.es/proposal-temporal/#sec-get-temporal.instant.prototype.epochmilliseconds)
    pub fn epoch_milliseconds(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let instant = this_instant(cx, this_value, "Instant.prototype.epochMilliseconds")?;
        let epoch_millis = instant.instant().epoch_milliseconds();

        Ok(Value::from(epoch_millis).to_handle(cx))
    }

    /// get Temporal.Instant.prototype.epochNanoseconds (https://tc39.es/proposal-temporal/#sec-get-temporal.instant.prototype.epochnanoseconds)
    pub fn epoch_nanoseconds(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let instant = this_instant(cx, this_value, "Instant.prototype.epochNanoseconds")?;
        let epoch_nanos = instant.instant().epoch_nanoseconds().as_i128();

        Ok(BigIntValue::new(cx, BigInt::from(epoch_nanos))?.into())
    }

    /// Temporal.Instant.prototype.add (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.add)
    pub fn add(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Instant.prototype.add";

        let instant = this_instant(cx, this_value, NAME)?;

        let duration_arg = get_argument(cx, arguments, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let new_instant_result = instant.instant().add(&duration);
        let new_instant = map_temporal_result(cx, new_instant_result, NAME)?;

        Ok(InstantObject::new(cx, new_instant)?.as_value())
    }

    /// Temporal.Instant.prototype.subtract (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.subtract)
    pub fn subtract(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Instant.prototype.subtract";

        let instant = this_instant(cx, this_value, NAME)?;

        let duration_arg = get_argument(cx, arguments, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let new_instant_result = instant.instant().subtract(&duration);
        let new_instant = map_temporal_result(cx, new_instant_result, NAME)?;

        Ok(InstantObject::new(cx, new_instant)?.as_value())
    }

    /// Temporal.Instant.prototype.until (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.until)
    pub fn until(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_instant(cx, this_value, "Instant.prototype.until")?;
        unimplemented!("Instant.prototype.until")
    }

    /// Temporal.Instant.prototype.since (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.since)
    pub fn since(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let _ = this_instant(cx, this_value, "Instant.prototype.since")?;
        unimplemented!("Instant.prototype.since")
    }

    /// Temporal.Instant.prototype.round (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.round)
    pub fn round(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Instant.prototype.round";

        let instant = this_instant(cx, this_value, NAME)?;

        let options_arg = get_argument(cx, arguments, 0);
        let options = parse_round_options_argument(cx, options_arg, NAME)?;

        // Parse rounding from options object
        let increment = get_rounding_increment_option(cx, options, NAME)?;
        let rounding_mode = get_rounding_mode_option(cx, options, RoundingMode::HalfExpand, NAME)?;
        let smallest_unit = get_unit_valued_option(cx, options, cx.names.smallest_unit(), NAME)?;

        let mut rounding_options = RoundingOptions::default();
        rounding_options.increment = Some(increment);
        rounding_options.rounding_mode = Some(rounding_mode);
        rounding_options.smallest_unit = smallest_unit;

        let rounded_result = instant.instant().round(rounding_options);
        let rounded = map_temporal_result(cx, rounded_result, NAME)?;

        Ok(InstantObject::new(cx, rounded)?.as_value())
    }

    /// Temporal.Instant.prototype.equals (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.equals)
    pub fn equals(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Instant.prototype.equals";

        let instant = this_instant(cx, this_value, NAME)?;

        let other_arg = get_argument(cx, arguments, 0);
        let other_instant = to_temporal_instant(cx, other_arg, NAME)?;

        Ok(cx.bool(instant.instant() == other_instant))
    }

    /// Temporal.Instant.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.tostring)
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Instant.prototype.toString";

        let instant = this_instant(cx, this_value, NAME)?;

        let options_arg = get_argument(cx, arguments, 0);
        let options = validate_options_object(cx, options_arg, NAME)?;

        // Parse rounding options from options object
        let precision = get_fractional_second_digits_option(cx, options, NAME)?;
        let rounding_mode = get_rounding_mode_option(cx, options, RoundingMode::Trunc, NAME)?;
        let smallest_unit = get_unit_valued_option(cx, options, cx.names.smallest_unit(), NAME)?;
        let timezone = get_time_zone_option(cx, options, NAME)?;

        let to_string_options = ToStringRoundingOptions {
            precision,
            smallest_unit,
            rounding_mode: Some(rounding_mode),
        };

        let instant_string_result = instant
            .instant()
            .to_ixdtf_string(timezone, to_string_options);
        let instant_string = map_temporal_result(cx, instant_string_result, NAME)?;

        Ok(cx.alloc_string(&instant_string)?.as_value())
    }

    /// Temporal.Instant.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.tolocalestring)
    pub fn to_locale_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Instant.prototype.toLocaleString";

        let instant = this_instant(cx, this_value, NAME)?;

        let instant_string_result = instant
            .instant()
            .to_ixdtf_string(None, ToStringRoundingOptions::default());
        let instant_string = map_temporal_result(cx, instant_string_result, NAME)?;

        Ok(cx.alloc_string(&instant_string)?.as_value())
    }

    /// Temporal.Instant.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.tojson)
    pub fn to_json(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Instant.prototype.toJSON";

        let instant = this_instant(cx, this_value, NAME)?;

        let instant_string_result = instant
            .instant()
            .to_ixdtf_string(None, ToStringRoundingOptions::default());
        let instant_string = map_temporal_result(cx, instant_string_result, NAME)?;

        Ok(cx.alloc_string(&instant_string)?.as_value())
    }

    /// Temporal.Instant.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.valueof)
    pub fn value_of(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        type_error(cx, "Instant.prototype.valueOf must not be called")
    }

    /// Temporal.Instant.prototype.toZonedDateTimeISO (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.tozoneddatetimeiso)
    pub fn to_zoned_date_time_iso(
        cx: Context,
        this_value: Handle<Value>,
        argument: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Instant.prototype.toZonedDateTimeISO";

        let instant = this_instant(cx, this_value, NAME)?;

        let time_zone_arg = get_argument(cx, argument, 0);
        let time_zone = to_time_zone_identifier(cx, time_zone_arg, NAME)?;

        let zoned_date_time_result = instant.instant().to_zoned_date_time_iso(time_zone);
        let zoned_date_time = map_temporal_result(cx, zoned_date_time_result, NAME)?;

        Ok(ZonedDateTimeObject::new(cx, zoned_date_time)?.as_value())
    }
}

fn this_instant(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<InstantObject>> {
    if value.is_object() {
        if let Some(instant) = value.as_object().as_instant_object() {
            return Ok(instant);
        }
    }

    type_error(cx, &format!("{method_name} must be called on an Instant"))
}
