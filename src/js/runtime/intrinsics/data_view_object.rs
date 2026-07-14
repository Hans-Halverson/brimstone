use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        Context, HeapPtr,
        eval_result::EvalResult,
        gc::{Handle, HeapItem, HeapVisitor},
        intrinsics::{array_buffer_object::ArrayBufferObject, intrinsics::Intrinsic},
        object_value::ObjectValue,
        ordinary_object::ObjectBuilder,
    },
};

extend_object! {
    /// DataView Objects (https://tc39.es/ecma262/#sec-dataview-objects)
    pub struct DataViewObject {
        viewed_array_buffer: HeapPtr<ArrayBufferObject>,
        /// Byte length of the DataView. None represents a value of AUTO.
        byte_length: Option<usize>,
        byte_offset: usize,
    }
}

impl DataViewObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        viewed_array_buffer: Handle<ArrayBufferObject>,
        byte_length: Option<usize>,
        byte_offset: usize,
    ) -> EvalResult<Handle<DataViewObject>> {
        let mut object = ObjectBuilder::<DataViewObject>::new(cx)
            .constructor_proto(constructor, Intrinsic::DataViewPrototype)?
            .build()?;

        let viewed_array_buffer = *viewed_array_buffer;

        object.viewed_array_buffer = viewed_array_buffer;
        object.byte_length = byte_length;
        object.byte_offset = byte_offset;

        Ok(object.to_handle())
    }

    pub fn viewed_array_buffer_ptr(&self) -> HeapPtr<ArrayBufferObject> {
        self.viewed_array_buffer
    }

    pub fn viewed_array_buffer(&self) -> Handle<ArrayBufferObject> {
        self.viewed_array_buffer.to_handle()
    }

    pub fn byte_length(&self) -> Option<usize> {
        self.byte_length
    }

    pub fn byte_offset(&self) -> usize {
        self.byte_offset
    }
}

impl HeapItem for DataViewObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<DataViewObject>()
    }

    fn visit_pointers(mut data_view_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        data_view_object.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut data_view_object.viewed_array_buffer);
    }
}
