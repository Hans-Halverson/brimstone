use crate::runtime::{
    alloc_error::AllocResult, object_value::ObjectValue, property::Property, realm::Realm, Context,
    Handle,
};

use super::intrinsics::Intrinsic;

pub struct AsyncFunctionPrototype;

impl AsyncFunctionPrototype {
    /// Properties of the AsyncFunction Prototype Object (https://tc39.es/ecma262/#sec-async-function-prototype-properties)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::FunctionPrototype)), true)?;

        // Constructor property is added once AsyncFunctionConstructor has been created

        // AsyncFunction.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-async-function-prototype-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.async_function().as_string().into(), false, false, true),
        )?;

        Ok(object)
    }
}
