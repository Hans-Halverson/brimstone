use crate::js::runtime::{
    object_value::ObjectValue, property::Property, realm::Realm, Context, Handle,
};

use super::intrinsics::Intrinsic;

pub struct GeneratorFunctionPrototype;

impl GeneratorFunctionPrototype {
    // 27.3.3 Properties of the GeneratorFunction Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::FunctionPrototype)), true);

        // Constructor property is added once GeneratorFunctionConstructor has been created

        // 27.3.3.2 GeneratorFunction.prototype.prototype
        let proto = realm.get_intrinsic(Intrinsic::GeneratorPrototype);
        let proto_prop = Property::data(proto.into(), false, false, true);
        object.set_property(cx, cx.names.prototype(), proto_prop);

        // 27.3.3.3 GeneratorFunction.prototype [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.generator_function().as_string().into(), false, false, true),
        );

        object
    }
}
