use crate::runtime::{
    Context, Handle, Realm, alloc_error::AllocResult, intrinsics::intrinsics::Intrinsic,
    object_value::ObjectValue, property::Property,
};

/// The Temporal Object (https://tc39.es/proposal-temporal/#sec-temporal-objects)
pub struct TemporalObject;

impl TemporalObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        let now_object = realm.get_intrinsic(Intrinsic::TemporalNow);
        object.intrinsic_data_prop(cx, cx.names.now(), now_object.as_value())?;

        let duration_constructor = realm.get_intrinsic(Intrinsic::DurationConstructor);
        object.intrinsic_data_prop(cx, cx.names.duration(), duration_constructor.as_value())?;

        let instant_constructor = realm.get_intrinsic(Intrinsic::InstantConstructor);
        object.intrinsic_data_prop(cx, cx.names.instant(), instant_constructor.as_value())?;

        let plain_date_constructor = realm.get_intrinsic(Intrinsic::PlainDateConstructor);
        object.intrinsic_data_prop(cx, cx.names.plain_date(), plain_date_constructor.as_value())?;

        let plain_date_time_constructor = realm.get_intrinsic(Intrinsic::PlainDateTimeConstructor);
        object.intrinsic_data_prop(
            cx,
            cx.names.plain_date_time(),
            plain_date_time_constructor.as_value(),
        )?;

        let plain_month_day_constructor = realm.get_intrinsic(Intrinsic::PlainMonthDayConstructor);
        object.intrinsic_data_prop(
            cx,
            cx.names.plain_month_day(),
            plain_month_day_constructor.as_value(),
        )?;

        let plain_time_constructor = realm.get_intrinsic(Intrinsic::PlainTimeConstructor);
        object.intrinsic_data_prop(cx, cx.names.plain_time(), plain_time_constructor.as_value())?;

        let plain_year_month_constructor =
            realm.get_intrinsic(Intrinsic::PlainYearMonthConstructor);
        object.intrinsic_data_prop(
            cx,
            cx.names.plain_year_month(),
            plain_year_month_constructor.as_value(),
        )?;

        let zoned_date_time_constructor = realm.get_intrinsic(Intrinsic::ZonedDateTimeConstructor);
        object.intrinsic_data_prop(
            cx,
            cx.names.zoned_date_time(),
            zoned_date_time_constructor.as_value(),
        )?;

        // Temporal [ %Symbol.toStringTag% ] (https://tc39.es/proposal-temporal/#sec-temporal-%symbol.tostringtag%)
        let to_string_tag_key = cx.symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.temporal().as_string().into(), false, false, true),
        )?;

        Ok(object)
    }
}
