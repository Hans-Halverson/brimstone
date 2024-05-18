use crate::js::runtime::{
    object_value::ObjectValue, property::Property, realm::Realm, Context, Handle,
};

use super::intrinsics::Intrinsic;

pub struct PromisePrototype;

impl PromisePrototype {
    // 27.2.5 Properties of the Promise Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once PromiseConstructor has been created

        // 27.2.5.5 Promise.prototype [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.promise().as_string().into(), false, false, true),
        );

        object
    }
}
