use num_bigint::BigInt;
use temporal_rs::Instant;

use crate::{
    common::constants::NANOSECONDS_IN_ONE_MILLISECOND,
    runtime::{
        Context, Handle, Realm, Value,
        alloc_error::AllocResult,
        builtin_function::BuiltinFunction,
        error::type_error,
        eval_result::EvalResult,
        function::get_argument,
        intrinsics::{
            bigint_constructor::number_to_bigint,
            intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
            temporal::{
                instant_object::InstantObject,
                utils::{clamp_epoch_nanos_to_i128, map_temporal_result},
            },
        },
        object_value::ObjectValue,
        type_utilities::{ToPrimitivePreferredType, to_bigint, to_number, to_primitive},
    },
};

pub struct InstantConstructor;

impl InstantConstructor {
    /// Temporal.Instant Constructor (https://tc39.es/proposal-temporal/#sec-temporal-instant-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            RuntimeFunction::InstantConstructor_construct,
            1,
            cx.names.instant(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::InstantPrototype).into(),
        )?;

        func.intrinsic_func(
            cx,
            cx.names.from(),
            RuntimeFunction::InstantConstructor_from,
            1,
            realm,
        )?;
        func.intrinsic_func(
            cx,
            cx.names.from_epoch_milliseconds(),
            RuntimeFunction::InstantConstructor_fromEpochMilliseconds,
            1,
            realm,
        )?;
        func.intrinsic_func(
            cx,
            cx.names.from_epoch_nanoseconds(),
            RuntimeFunction::InstantConstructor_fromEpochNanoseconds,
            1,
            realm,
        )?;
        func.intrinsic_func(
            cx,
            cx.names.compare(),
            RuntimeFunction::InstantConstructor_compare,
            2,
            realm,
        )?;

        Ok(func)
    }

    /// Temporal.Instant (https://tc39.es/proposal-temporal/#sec-temporal-instant)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let Some(new_target) = cx.current_new_target() else {
            return type_error(cx, "Temporal.Instant constructor must be called with new");
        };

        let epoch_nanos_arg = get_argument(cx, arguments, 0);
        let epoch_nanos = to_bigint(cx, epoch_nanos_arg)?;

        let instant = create_temporal_instant(
            cx,
            &epoch_nanos.bigint(),
            Some(new_target),
            "Temporal.Instant",
        )?;

        Ok(instant.as_value())
    }

    /// Temporal.Instant.from (https://tc39.es/proposal-temporal/#sec-temporal.instant.from)
    pub fn from(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let item_arg = get_argument(cx, arguments, 0);
        let instant = to_temporal_instant(cx, item_arg, "Instant.from")?;

        Ok(InstantObject::new(cx, instant)?.as_value())
    }

    /// Temporal.Instant.fromEpochMilliseconds (https://tc39.es/proposal-temporal/#sec-temporal.instant.fromepochmilliseconds)
    pub fn from_epoch_milliseconds(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Instant.fromEpochMilliseconds";
        let epoch_millis_arg = get_argument(cx, arguments, 0);
        let epoch_millis_number = to_number(cx, epoch_millis_arg)?;
        let epoch_millis_bigint = number_to_bigint(cx, *epoch_millis_number, NAME)?;

        let epoch_nanos = epoch_millis_bigint.bigint() * NANOSECONDS_IN_ONE_MILLISECOND;

        Ok(create_temporal_instant(cx, &epoch_nanos, None, NAME)?.as_value())
    }

    /// Temporal.Instant.fromEpochNanoseconds (https://tc39.es/proposal-temporal/#sec-temporal.instant.fromepochnanoseconds)
    pub fn from_epoch_nanoseconds(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let epoch_nanos_arg = get_argument(cx, arguments, 0);
        let epoch_nanos = to_bigint(cx, epoch_nanos_arg)?;

        let instant = create_temporal_instant(
            cx,
            &epoch_nanos.bigint(),
            None,
            "Instant.fromEpochNanoseconds",
        )?;

        Ok(instant.as_value())
    }

    /// Temporal.Instant.compare (https://tc39.es/proposal-temporal/#sec-temporal.instant.compare)
    pub fn compare(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        const NAME: &str = "Instant.compare";

        let arg_1 = get_argument(cx, arguments, 0);
        let arg_2 = get_argument(cx, arguments, 1);

        let instant_1 = to_temporal_instant(cx, arg_1, NAME)?;
        let instant_2 = to_temporal_instant(cx, arg_2, NAME)?;

        Ok(cx.smi(instant_1.cmp(&instant_2) as i32))
    }
}

/// CreateTemporalInstant (https://tc39.es/proposal-temporal/#sec-temporal-createtemporalinstant)
pub fn create_temporal_instant(
    cx: Context,
    epoch_nanos: &BigInt,
    new_target: Option<Handle<ObjectValue>>,
    method_name: &str,
) -> EvalResult<Handle<InstantObject>> {
    let instant_result = Instant::try_new(clamp_epoch_nanos_to_i128(epoch_nanos));
    let instant = map_temporal_result(cx, instant_result, method_name)?;

    match new_target {
        None => InstantObject::new(cx, instant),
        Some(new_target) => InstantObject::new_from_constructor(cx, new_target, instant),
    }
}

/// ToTemporalInstant (https://tc39.es/proposal-temporal/#sec-temporal-totemporalinstant)
pub fn to_temporal_instant(
    cx: Context,
    mut item: Handle<Value>,
    method_name: &str,
) -> EvalResult<Instant> {
    if item.is_object() {
        // Check if item is a Temporal object of some kind
        let item_object = item.as_object();
        if let Some(instant) = item_object.as_instant_object() {
            return Ok(instant.instant());
        } else if let Some(zoned_date_time) = item_object.as_zoned_date_time_object() {
            return Ok(zoned_date_time.zoned_date_time().to_instant());
        }

        // Otherwise will treat as a string
        item = to_primitive(cx, item, ToPrimitivePreferredType::String)?;
    }

    // Otherwise parse Instant from string
    if !item.is_string() {
        return type_error(cx, &format!("{method_name} instant must be a string"));
    }

    let item_string = item.as_string().to_wtf8_string()?;

    let parsed_instant_result = Instant::from_utf8(item_string.as_bytes());
    let parsed_instant = map_temporal_result(cx, parsed_instant_result, method_name)?;

    Ok(parsed_instant)
}
