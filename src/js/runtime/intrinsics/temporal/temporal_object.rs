use crate::runtime::{
    Context, Handle, Realm, alloc_error::AllocResult, intrinsic_builder::IntrinsicBuilder,
    intrinsics::intrinsics::Intrinsic, object_value::ObjectValue,
};

/// The Temporal Object (https://tc39.es/proposal-temporal/#sec-temporal-objects)
pub struct TemporalObject;

impl TemporalObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::new_object(cx, realm, Intrinsic::ObjectPrototype)?;

        builder.data(cx.names.now(), realm.get_intrinsic(Intrinsic::TemporalNow).as_value())?;
        builder.data(
            cx.names.duration(),
            realm
                .get_intrinsic(Intrinsic::DurationConstructor)
                .as_value(),
        )?;
        builder.data(
            cx.names.instant(),
            realm
                .get_intrinsic(Intrinsic::InstantConstructor)
                .as_value(),
        )?;
        builder.data(
            cx.names.plain_date(),
            realm
                .get_intrinsic(Intrinsic::PlainDateConstructor)
                .as_value(),
        )?;
        builder.data(
            cx.names.plain_date_time(),
            realm
                .get_intrinsic(Intrinsic::PlainDateTimeConstructor)
                .as_value(),
        )?;
        builder.data(
            cx.names.plain_month_day(),
            realm
                .get_intrinsic(Intrinsic::PlainMonthDayConstructor)
                .as_value(),
        )?;
        builder.data(
            cx.names.plain_time(),
            realm
                .get_intrinsic(Intrinsic::PlainTimeConstructor)
                .as_value(),
        )?;
        builder.data(
            cx.names.plain_year_month(),
            realm
                .get_intrinsic(Intrinsic::PlainYearMonthConstructor)
                .as_value(),
        )?;
        builder.data(
            cx.names.zoned_date_time(),
            realm
                .get_intrinsic(Intrinsic::ZonedDateTimeConstructor)
                .as_value(),
        )?;

        // Temporal [ %Symbol.toStringTag% ] (https://tc39.es/proposal-temporal/#sec-temporal-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.temporal())?;

        builder.build()
    }
}
