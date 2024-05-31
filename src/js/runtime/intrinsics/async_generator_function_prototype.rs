use crate::js::runtime::{
    object_value::ObjectValue, property::Property, realm::Realm, Context, Handle,
};

use super::intrinsics::Intrinsic;

pub struct AsyncGeneratorFunctionPrototype;

impl AsyncGeneratorFunctionPrototype {
    // 27.4.3 Properties of the AsyncGeneratorFunction Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::FunctionPrototype)), true);

        // Constructor property is added once AsyncGeneratorFunctionConstructor has been created

        // 27.4.3.2 AsyncGeneratorFunction.prototype.prototype
        let proto = realm.get_intrinsic(Intrinsic::AsyncGeneratorPrototype);
        let proto_prop = Property::data(proto.into(), false, false, true);
        object.set_property(cx, cx.names.prototype(), proto_prop);

        // 27.4.3.3 AsyncGeneratorFunction.prototype [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(
                cx.names.async_generator_function().as_string().into(),
                false,
                false,
                true,
            ),
        );

        object
    }
}
