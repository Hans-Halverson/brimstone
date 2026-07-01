use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        Context, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        gc::{Handle, HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::object_create_with_proto,
    },
    set_uninit,
};

// A WrappedValidIteratorObject wraps an iterator object and its next method.
extend_object! {
    pub struct WrappedValidIteratorObject {
        iterator: HeapPtr<ObjectValue>,
        next_method: Value,
    }
}

impl WrappedValidIteratorObject {
    pub fn new(
        cx: Context,
        iterator: Handle<ObjectValue>,
        next_method: Handle<Value>,
    ) -> AllocResult<Handle<ObjectValue>> {
        let mut object = object_create_with_proto::<WrappedValidIteratorObject>(
            cx,
            HeapItemKind::WrappedValidIteratorObject,
            cx.get_intrinsic(Intrinsic::WrapForValidIteratorPrototype),
        )?;

        set_uninit!(object.iterator, *iterator);
        set_uninit!(object.next_method, *next_method);

        Ok(object.as_object().to_handle())
    }

    pub fn iterator(&self) -> Handle<ObjectValue> {
        self.iterator.to_handle()
    }

    pub fn next_method(&self, cx: Context) -> Handle<Value> {
        self.next_method.to_handle(cx)
    }
}

impl HeapItem for WrappedValidIteratorObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<WrappedValidIteratorObject>()
    }

    fn visit_pointers(mut wrapped_valid_iterator: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        wrapped_valid_iterator.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut wrapped_valid_iterator.iterator);
        visitor.visit_value(&mut wrapped_valid_iterator.next_method);
    }
}
