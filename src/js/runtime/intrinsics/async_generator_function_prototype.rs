use crate::runtime::{
    Context, Handle, alloc_error::AllocResult, intrinsic_builder::IntrinsicBuilder,
    intrinsics::intrinsics::Intrinsic, object_value::ObjectValue, property::Property, realm::Realm,
};

pub struct AsyncGeneratorFunctionPrototype;

impl AsyncGeneratorFunctionPrototype {
    /// Properties of the AsyncGeneratorFunction Prototype Object (https://tc39.es/ecma262/#sec-properties-of-asyncgeneratorfunction-prototype)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::new_object(cx, realm, Intrinsic::FunctionPrototype)?;

        // Constructor property is added once AsyncGeneratorFunctionConstructor has been created

        // AsyncGeneratorFunction.prototype.prototype (https://tc39.es/ecma262/#sec-asyncgeneratorfunction-prototype-prototype)
        let proto = realm.get_intrinsic(Intrinsic::AsyncGeneratorPrototype);
        builder.property(cx.names.prototype(), Property::data(proto.into(), false, false, true))?;

        // AsyncGeneratorFunction.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-asyncgeneratorfunction-prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.async_generator_function())?;

        builder.build()
    }
}
