use crate::js::runtime::{object_value::ObjectValue, realm::Realm, Context, Handle};

use super::intrinsics::Intrinsic;

pub struct AggregateErrorPrototype;

impl AggregateErrorPrototype {
    // 20.5.7.3 Properties of the AggregateError Prototype Object
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
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

        Handle::from_heap(object)
    }
}
