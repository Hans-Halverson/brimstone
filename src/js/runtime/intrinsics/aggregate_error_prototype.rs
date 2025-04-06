use crate::runtime::{object_value::ObjectValue, realm::Realm, Context, Handle};

use super::intrinsics::Intrinsic;

pub struct AggregateErrorPrototype;

impl AggregateErrorPrototype {
    /// Properties of the AggregateError Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-aggregate-error-prototype-objects)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ErrorPrototype)), true);

        // Constructor property is added once AggregateErrorConstructor has been created
        object.intrinsic_name_prop(cx, "AggregateError");
        object.intrinsic_data_prop(
            cx,
            cx.names.message(),
            cx.names.empty_string().as_string().into(),
        );
        object.intrinsic_data_prop(
            cx,
            cx.names.name(),
            cx.names.aggregate_error().as_string().into(),
        );

        object.to_handle()
    }
}
