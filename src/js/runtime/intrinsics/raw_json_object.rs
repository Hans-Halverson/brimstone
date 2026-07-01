use std::mem::size_of;

use crate::{
    extend_object, must_a,
    runtime::{
        Context, HeapItemKind, HeapPtr,
        abstract_operations::{IntegrityLevel, create_data_property_or_throw, set_integrity_level},
        alloc_error::AllocResult,
        gc::{Handle, HeapItem, HeapVisitor},
        ordinary_object::object_create_with_optional_proto,
        string_value::StringValue,
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

impl HeapItem for RawJSONObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<RawJSONObject>()
    }

    fn visit_pointers(mut raw_json_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        raw_json_object.visit_object_pointers(visitor);
    }
}
