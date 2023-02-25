use crate::js::runtime::{
    gc::Gc, object_value::ObjectValue, ordinary_object::OrdinaryObject, realm::Realm, Context,
};

use super::intrinsics::Intrinsic;

pub struct AggregateErrorPrototype;

impl AggregateErrorPrototype {
    // 20.5.7.3 Properties of the AggregateError Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ErrorPrototype)), true);

        // Constructor property is added once AggregateErrorConstructor has been created
        object.intrinsic_name_prop(cx, "AggregateError");
        object.intrinsic_data_prop(&cx.names.message(), cx.names.empty_string().as_string().into());
        object.intrinsic_data_prop(&cx.names.name(), cx.names.aggregate_error().as_string().into());

        cx.heap.alloc(object).into()
    }
}
