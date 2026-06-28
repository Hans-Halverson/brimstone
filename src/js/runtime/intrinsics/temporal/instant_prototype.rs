use num_bigint::BigInt;
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
                instant_constructor::to_temporal_instant,
                instant_object::InstantObject,
                utils::{
                    DiffOperation, get_difference_settings, get_fractional_second_digits_option,
                    get_rounding_increment_option, get_rounding_mode_option, get_time_zone_option,
                    get_unit_valued_option, map_temporal_result, parse_round_options_argument,
                    to_time_zone_identifier, validate_options_object,
                },
                zoned_date_time_object::ZonedDateTimeObject,
            },
        },
        object_value::ObjectValue,
        value::BigIntValue,
    },
    runtime_fn,
};

pub struct InstantPrototype;

impl InstantPrototype {
    /// Properties of the Temporal.Instant Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-instant-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once InstantConstructor has been created

        intrinsic_methods!(cx, builder, {
            add                    InstantPrototype_add                (1),
            subtract               InstantPrototype_subtract           (1),
            until                  InstantPrototype_until              (1),
            since                  InstantPrototype_since              (1),
            round                  InstantPrototype_round              (1),
            equals                 InstantPrototype_equals             (1),
            to_string              InstantPrototype_toString           (0),
            to_locale_string       InstantPrototype_toLocaleString     (0),
            to_json                InstantPrototype_toJSON             (0),
            value_of               InstantPrototype_valueOf            (0),
            to_zoned_date_time_iso InstantPrototype_toZonedDateTimeISO (1),
        });

        intrinsic_getter_methods!(cx, builder, {
            epoch_milliseconds InstantPrototype_epochMilliseconds,
            epoch_nanoseconds  InstantPrototype_epochNanoseconds,
        });

        builder.to_string_tag(cx.names.temporal_instant())?;

        builder.build()
    }

    runtime_fn! {
    /// get Temporal.Instant.prototype.epochMilliseconds (https://tc39.es/proposal-temporal/#sec-get-temporal.instant.prototype.epochmilliseconds)
    fn epoch_milliseconds(cx, this_value, _) {
        let instant = this_instant(cx, this_value, "Instant.prototype.epochMilliseconds")?;
        let epoch_millis = instant.instant().epoch_milliseconds();

        Ok(cx.number(epoch_millis))
    }}

    runtime_fn! {
    /// get Temporal.Instant.prototype.epochNanoseconds (https://tc39.es/proposal-temporal/#sec-get-temporal.instant.prototype.epochnanoseconds)
    fn epoch_nanoseconds(cx, this_value, _) {
        let instant = this_instant(cx, this_value, "Instant.prototype.epochNanoseconds")?;
        let epoch_nanos = instant.instant().epoch_nanoseconds().as_i128();

        Ok(BigIntValue::new(cx, BigInt::from(epoch_nanos))?.into())
    }}

    runtime_fn! {
    /// Temporal.Instant.prototype.add (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.add)
    fn add(cx, this_value, arguments) {
        const NAME: &str = "Instant.prototype.add";

        let instant = this_instant(cx, this_value, NAME)?;

        let duration_arg = arguments.get(cx, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let new_instant_result = instant.instant().add(&duration);
        let new_instant = map_temporal_result(cx, new_instant_result, NAME)?;

        Ok(InstantObject::new(cx, new_instant)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Instant.prototype.subtract (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.subtract)
    fn subtract(cx, this_value, arguments) {
        const NAME: &str = "Instant.prototype.subtract";

        let instant = this_instant(cx, this_value, NAME)?;

        let duration_arg = arguments.get(cx, 0);
        let duration = to_temporal_duration(cx, duration_arg, NAME)?;

        let new_instant_result = instant.instant().subtract(&duration);
        let new_instant = map_temporal_result(cx, new_instant_result, NAME)?;

        Ok(InstantObject::new(cx, new_instant)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Instant.prototype.until (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.until)
    fn until(cx, this_value, arguments) {
        Self::diff(cx, this_value, arguments, DiffOperation::Until, "Instant.prototype.until")
    }}

    runtime_fn! {
    /// Temporal.Instant.prototype.since (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.since)
    fn since(cx, this_value, arguments) {
        Self::diff(cx, this_value, arguments, DiffOperation::Since, "Instant.prototype.since")
    }}

    fn diff(
        cx: Context,
        this_value: Handle<Value>,
        arguments: Arguments,
        operation: DiffOperation,
        method_name: &str,
    ) -> EvalResult<Handle<Value>> {
        let instant = this_instant(cx, this_value, method_name)?;

        let other_arg = arguments.get(cx, 0);
        let other = to_temporal_instant(cx, other_arg, method_name)?;

        let options_arg = arguments.get(cx, 1);
        let options = validate_options_object(cx, options_arg, method_name)?;
        let difference_settings = get_difference_settings(cx, options, method_name)?;

        let duration_result = match operation {
            DiffOperation::Until => instant.instant().until(&other, difference_settings),
            DiffOperation::Since => instant.instant().since(&other, difference_settings),
        };

        let duration = map_temporal_result(cx, duration_result, method_name)?;

        Ok(DurationObject::new(cx, duration)?.as_value())
    }

    runtime_fn! {
    /// Temporal.Instant.prototype.round (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.round)
    fn round(cx, this_value, arguments) {
        const NAME: &str = "Instant.prototype.round";

        let instant = this_instant(cx, this_value, NAME)?;

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

        let rounded_result = instant.instant().round(rounding_options);
        let rounded = map_temporal_result(cx, rounded_result, NAME)?;

        Ok(InstantObject::new(cx, rounded)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Instant.prototype.equals (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.equals)
    fn equals(cx, this_value, arguments) {
        const NAME: &str = "Instant.prototype.equals";

        let instant = this_instant(cx, this_value, NAME)?;

        let other_arg = arguments.get(cx, 0);
        let other_instant = to_temporal_instant(cx, other_arg, NAME)?;

        Ok(cx.bool(instant.instant() == other_instant))
    }}

    runtime_fn! {
    /// Temporal.Instant.prototype.toString (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.tostring)
    fn to_string(cx, this_value, arguments) {
        const NAME: &str = "Instant.prototype.toString";

        let instant = this_instant(cx, this_value, NAME)?;

        let options_arg = arguments.get(cx, 0);
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

        let instant_string_result = instant.instant().to_ixdtf_string_with_provider(
            timezone,
            to_string_options,
            cx.temporal_provider(),
        );
        let instant_string = map_temporal_result(cx, instant_string_result, NAME)?;

        Ok(cx.alloc_string(&instant_string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Instant.prototype.toLocaleString (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.tolocalestring)
    fn to_locale_string(cx, this_value, _) {
        const NAME: &str = "Instant.prototype.toLocaleString";

        let instant = this_instant(cx, this_value, NAME)?;

        let instant_string_result = instant.instant().to_ixdtf_string_with_provider(
            None,
            ToStringRoundingOptions::default(),
            cx.temporal_provider(),
        );
        let instant_string = map_temporal_result(cx, instant_string_result, NAME)?;

        Ok(cx.alloc_string(&instant_string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Instant.prototype.toJSON (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.tojson)
    fn to_json(cx, this_value, _) {
        const NAME: &str = "Instant.prototype.toJSON";

        let instant = this_instant(cx, this_value, NAME)?;

        let instant_string_result = instant.instant().to_ixdtf_string_with_provider(
            None,
            ToStringRoundingOptions::default(),
            cx.temporal_provider(),
        );
        let instant_string = map_temporal_result(cx, instant_string_result, NAME)?;

        Ok(cx.alloc_string(&instant_string)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Instant.prototype.valueOf (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.valueof)
    fn value_of(cx, _, _) {
        type_error(cx, "Instant.prototype.valueOf must not be called")
    }}

    runtime_fn! {
    /// Temporal.Instant.prototype.toZonedDateTimeISO (https://tc39.es/proposal-temporal/#sec-temporal.instant.prototype.tozoneddatetimeiso)
    fn to_zoned_date_time_iso(cx, this_value, arguments) {
        const NAME: &str = "Instant.prototype.toZonedDateTimeISO";

        let instant = this_instant(cx, this_value, NAME)?;

        let time_zone_arg = arguments.get(cx, 0);
        let time_zone = to_time_zone_identifier(cx, time_zone_arg, NAME)?;

        let zoned_date_time_result = instant
            .instant()
            .to_zoned_date_time_iso_with_provider(time_zone, cx.temporal_provider());
        let zoned_date_time = map_temporal_result(cx, zoned_date_time_result, NAME)?;

        Ok(ZonedDateTimeObject::new(cx, zoned_date_time)?.as_value())
    }}
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
