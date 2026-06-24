use crate::runtime::{
    Context, Handle, Realm, alloc_error::AllocResult, intrinsics::intrinsics::Intrinsic,
    object_value::ObjectValue, property::Property,
};

pub struct PlainDatePrototype;

impl PlainDatePrototype {
    /// Properties of the Temporal.PlainDate Prototype Object (https://tc39.es/proposal-temporal/#sec-properties-of-the-temporal-plaindate-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once PlainDateConstructor has been created

        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.temporal_plain_date().as_string().into(), false, false, true),
        )?;

        Ok(object)
    }
}
