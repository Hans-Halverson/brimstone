use crate::runtime::{
    Context, Handle, alloc_error::AllocResult, intrinsic_builder::IntrinsicBuilder,
    intrinsics::intrinsics::Intrinsic, object_value::ObjectValue, property::Property, realm::Realm,
};

pub struct GeneratorFunctionPrototype;

impl GeneratorFunctionPrototype {
    /// Properties of the GeneratorFunction Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-generatorfunction-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::FunctionPrototype)?;

        // Constructor property is added once GeneratorFunctionConstructor has been created

        // GeneratorFunction.prototype.prototype (https://tc39.es/ecma262/#sec-generatorfunction.prototype.prototype)
        let proto = realm.get_intrinsic(Intrinsic::GeneratorPrototype);
        builder.property(cx.names.prototype(), Property::data(proto.into(), false, false, true))?;

        // GeneratorFunction.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-generatorfunction.prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.generator_function())?;

        builder.build()
    }
}
