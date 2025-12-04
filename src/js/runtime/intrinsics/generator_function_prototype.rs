use crate::runtime::{
    alloc_error::AllocResult, object_value::ObjectValue, property::Property, realm::Realm, Context,
    Handle,
};

use super::intrinsics::Intrinsic;

pub struct GeneratorFunctionPrototype;

impl GeneratorFunctionPrototype {
    /// Properties of the GeneratorFunction Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-generatorfunction-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::FunctionPrototype)), true)?;

        // Constructor property is added once GeneratorFunctionConstructor has been created

        // GeneratorFunction.prototype.prototype (https://tc39.es/ecma262/#sec-generatorfunction.prototype.prototype)
        let proto = realm.get_intrinsic(Intrinsic::GeneratorPrototype);
        let proto_prop = Property::data(proto.into(), false, false, true);
        object.set_property(cx, cx.names.prototype(), proto_prop)?;

        // GeneratorFunction.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-generatorfunction.prototype-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.generator_function().as_string().into(), false, false, true),
        )?;

        Ok(object)
    }
}
