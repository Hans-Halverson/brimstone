use std::mem::size_of;

use crate::{
    extend_object, must_a,
    runtime::{
        abstract_operations::{create_data_property_or_throw, set_integrity_level, IntegrityLevel},
        alloc_error::AllocResult,
        gc::{Handle, HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemKind,
        ordinary_object::object_create_with_optional_proto,
        string_value::StringValue,
        Context, HeapPtr,
    },
};

extend_object! {
    pub struct RawJSONObject {}
}

impl RawJSONObject {
    pub fn new(cx: Context, raw_json: Handle<StringValue>) -> AllocResult<Handle<RawJSONObject>> {
        let object = object_create_with_optional_proto::<RawJSONObject>(
            cx,
            HeapItemKind::RawJSONObject,
            None,
        )?
        .to_handle();

        must_a!(create_data_property_or_throw(
            cx,
            object.as_object(),
            cx.names.raw_json(),
            raw_json.as_value(),
        ));

        must_a!(set_integrity_level(cx, object.as_object(), IntegrityLevel::Frozen));

        Ok(object)
    }
}

impl HeapItem for HeapPtr<RawJSONObject> {
    fn byte_size(&self) -> usize {
        size_of::<RawJSONObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
    }
}
