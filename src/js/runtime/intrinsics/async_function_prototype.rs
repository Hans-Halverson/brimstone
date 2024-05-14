use crate::js::runtime::{
    object_value::ObjectValue, property::Property, realm::Realm, Context, Handle,
};

use super::intrinsics::Intrinsic;

pub struct AsyncFunctionPrototype;

impl AsyncFunctionPrototype {
    // 27.7.3 Properties of the AsyncFunction Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::FunctionPrototype)), true);

        // Constructor property is added once AsyncFunctionConstructor has been created

        // 27.7.3.2 AsyncFunction.prototype [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.async_function().as_string().into(), false, false, true),
        );

        object
    }
}
