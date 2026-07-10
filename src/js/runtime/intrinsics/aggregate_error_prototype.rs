use crate::runtime::{
    Context, Handle, alloc_error::AllocResult, intrinsic_builder::IntrinsicBuilder,
    intrinsics::intrinsics::Intrinsic, object_value::ObjectValue, realm::Realm,
};

pub struct AggregateErrorPrototype;

impl AggregateErrorPrototype {
    /// Properties of the AggregateError Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-aggregate-error-prototype-objects)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::new_object(cx, realm, Intrinsic::ErrorPrototype)?;

        // Constructor property is added once AggregateErrorConstructor has been created
        builder.data(cx.names.message(), cx.names.empty_string().as_string().into())?;
        builder.data(cx.names.name(), cx.names.aggregate_error().as_string().into())?;

        builder.build()
    }
}
