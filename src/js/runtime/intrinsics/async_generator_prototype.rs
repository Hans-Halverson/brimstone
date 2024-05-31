use crate::js::runtime::{
    object_value::ObjectValue, property::Property, realm::Realm, Context, Handle,
};

use super::intrinsics::Intrinsic;

pub struct AsyncGeneratorPrototype;

impl AsyncGeneratorPrototype {
    // 27.6.1 The %AsyncGeneratorPrototype% Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object = ObjectValue::new(
            cx,
            Some(realm.get_intrinsic(Intrinsic::AsyncIteratorPrototype)),
            true,
        );

        // Constructor property is added once AsyncGeneratorFunctionPrototype is created

        // 27.6.1.5 %AsyncGeneratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.async_generator().as_string().into(), false, false, true),
        );

        object
    }
}
