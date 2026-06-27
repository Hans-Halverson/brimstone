use temporal_rs::{
    TemporalResult, TimeZone,
    host::{HostClock, HostHooks, HostTimeZone},
    now::Now,
    provider::TimeZoneProvider,
    unix_time::EpochNanoseconds,
};

use crate::{
    runtime::{
        Context, EvalResult, Handle, Realm, Value,
        alloc_error::AllocResult,
        intrinsics::{
            intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
            temporal::{
                instant_object::InstantObject,
                plain_date_object::PlainDateObject,
                plain_date_time_object::PlainDateTimeObject,
                plain_time_object::PlainTimeObject,
                utils::{map_temporal_result, to_time_zone_identifier},
                zoned_date_time_object::ZonedDateTimeObject,
            },
        },
        object_value::ObjectValue,
        property::Property,
    },
    runtime_fn,
};

/// The Temporal.Now Object (https://tc39.es/proposal-temporal/#sec-temporal-now-object)
pub struct TemporalNowObject;

impl TemporalNowObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Temporal.Now [ @@toStringTag ] (https://tc39.es/proposal-temporal/#sec-temporal-now-%symbol.tostringtag%)
        let to_string_tag_key = cx.symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.temporal_now().as_string().into(), false, false, true),
        )?;

        object.intrinsic_func(
            cx,
            cx.names.time_zone_id(),
            RuntimeFunction::TemporalNowObject_timeZoneId,
            0,
            realm,
        )?;

        object.intrinsic_func(
            cx,
            cx.names.instant_(),
            RuntimeFunction::TemporalNowObject_instant,
            0,
            realm,
        )?;

        object.intrinsic_func(
            cx,
            cx.names.plain_date_time_iso(),
            RuntimeFunction::TemporalNowObject_plainDateTimeISO,
            0,
            realm,
        )?;

        object.intrinsic_func(
            cx,
            cx.names.zoned_date_time_iso(),
            RuntimeFunction::TemporalNowObject_zonedDateTimeISO,
            0,
            realm,
        )?;

        object.intrinsic_func(
            cx,
            cx.names.plain_date_iso(),
            RuntimeFunction::TemporalNowObject_plainDateISO,
            0,
            realm,
        )?;

        object.intrinsic_func(
            cx,
            cx.names.plain_time_iso(),
            RuntimeFunction::TemporalNowObject_plainTimeISO,
            0,
            realm,
        )?;

        Ok(object)
    }

    runtime_fn! {
    /// Temporal.Now.timeZoneId (https://tc39.es/proposal-temporal/#sec-temporal.now.timezoneid)
    fn time_zone_id(cx, _, _) {
        const NAME: &str = "Now.timeZoneId";

        let time_zone_result = Self::now(cx).time_zone_with_provider(cx.temporal_provider());
        let time_zone = map_temporal_result(cx, time_zone_result, NAME)?;

        let time_zone_str_result = time_zone.identifier_with_provider(cx.temporal_provider());
        let time_zone_str = map_temporal_result(cx, time_zone_str_result, NAME)?;

        Ok(cx.alloc_string(&time_zone_str)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Now.instant (https://tc39.es/proposal-temporal/#sec-temporal.now.instant)
    fn instant(cx, _, _) {
        let instant_result = Self::now(cx).instant();
        let instant = map_temporal_result(cx, instant_result, "Now.instant")?;

        Ok(InstantObject::new(cx, instant)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Now.plainDateTimeISO (https://tc39.es/proposal-temporal/#sec-temporal.now.plaindatetimeiso)
    fn plain_date_time_iso(cx, _, arguments) {
        const NAME: &str = "Now.plainDateTimeISO";

        let time_zone_arg = arguments.get(cx, 0);
        let time_zone = Self::parse_time_zone_argument(cx, time_zone_arg, NAME)?;

        let date_time_result =
            Self::now(cx).plain_date_time_iso_with_provider(time_zone, cx.temporal_provider());
        let date_time = map_temporal_result(cx, date_time_result, NAME)?;

        Ok(PlainDateTimeObject::new(cx, date_time)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Now.zonedDateTimeISO (https://tc39.es/proposal-temporal/#sec-temporal.now.zoneddatetimeiso)
    fn zoned_date_time_iso(cx, _, arguments) {
        const NAME: &str = "Now.zonedDateTimeISO";

        let time_zone_arg = arguments.get(cx, 0);
        let time_zone = Self::parse_time_zone_argument(cx, time_zone_arg, NAME)?;

        let zoned_date_time_result =
            Self::now(cx).zoned_date_time_iso_with_provider(time_zone, cx.temporal_provider());
        let zoned_date_time = map_temporal_result(cx, zoned_date_time_result, NAME)?;

        Ok(ZonedDateTimeObject::new(cx, zoned_date_time)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Now.plainDateISO (https://tc39.es/proposal-temporal/#sec-temporal.now.plaindateiso)
    fn plain_date_iso(cx, _, arguments) {
        const NAME: &str = "Now.plainDateISO";

        let time_zone_arg = arguments.get(cx, 0);
        let time_zone = Self::parse_time_zone_argument(cx, time_zone_arg, NAME)?;

        let date_result =
            Self::now(cx).plain_date_iso_with_provider(time_zone, cx.temporal_provider());
        let date = map_temporal_result(cx, date_result, NAME)?;

        Ok(PlainDateObject::new(cx, date)?.as_value())
    }}

    runtime_fn! {
    /// Temporal.Now.plainTimeISO (https://tc39.es/proposal-temporal/#sec-temporal.now.plaintimeiso)
    fn plain_time_iso(cx, _, arguments) {
        const NAME: &str = "Now.plainTimeISO";

        let time_zone_arg = arguments.get(cx, 0);
        let time_zone = Self::parse_time_zone_argument(cx, time_zone_arg, NAME)?;

        let time_result =
            Self::now(cx).plain_time_iso_with_provider(time_zone, cx.temporal_provider());
        let time = map_temporal_result(cx, time_result, NAME)?;

        Ok(PlainTimeObject::new(cx, time)?.as_value())
    }}

    fn now(cx: Context) -> Now<NowImpl> {
        Now::new(NowImpl { cx })
    }

    fn parse_time_zone_argument(
        cx: Context,
        arg: Handle<Value>,
        method_name: &str,
    ) -> EvalResult<Option<TimeZone>> {
        if arg.is_undefined() {
            Ok(None)
        } else {
            Ok(Some(to_time_zone_identifier(cx, arg, method_name)?))
        }
    }
}

struct NowImpl {
    cx: Context,
}

impl HostClock for NowImpl {
    fn get_host_epoch_nanoseconds(&self) -> TemporalResult<EpochNanoseconds> {
        let nanos = self.cx.current_unix_time_nanos();
        Ok(EpochNanoseconds(nanos as i128))
    }
}

impl HostTimeZone for NowImpl {
    fn get_host_time_zone(
        &self,
        provider: &(impl TimeZoneProvider + ?Sized),
    ) -> TemporalResult<TimeZone> {
        // TODO: Handle system time zone
        Ok(TimeZone::utc_with_provider(provider))
    }
}

impl HostHooks for NowImpl {}
