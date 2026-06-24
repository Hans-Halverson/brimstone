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

        let plain_date_constructor = realm.get_intrinsic(Intrinsic::PlainDateConstructor);
        object.intrinsic_data_prop(cx, cx.names.plain_date(), plain_date_constructor.as_value())?;

        // Temporal [ %Symbol.toStringTag% ] (https://tc39.es/proposal-temporal/#sec-temporal-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.temporal().as_string().into(), false, false, true),
        )?;

        Ok(object)
    }
}
