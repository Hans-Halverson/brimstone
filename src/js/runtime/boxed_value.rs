use crate::{
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemDescriptor,
    },
    set_uninit,
};

/// A value that is allocated on the heap.
#[repr(C)]
pub struct BoxedValue {
    descriptor: HeapPtr<HeapItemDescriptor>,
    value: Value,
}

impl BoxedValue {
    pub fn new(cx: Context, value: Handle<Value>) -> AllocResult<HeapPtr<BoxedValue>> {
        let mut scope = cx.alloc_uninit::<BoxedValue>()?;

        set_uninit!(scope.descriptor, cx.descriptors.get(HeapItemKind::BoxedValue));
        set_uninit!(scope.value, *value);

        Ok(scope)
    }

    pub fn get(&self) -> Value {
        self.value
    }

    pub fn set(&mut self, value: Value) {
        self.value = value;
    }
}

impl HeapItem for BoxedValue {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        std::mem::size_of::<BoxedValue>()
    }

    fn visit_pointers(mut boxed_value: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut boxed_value.descriptor);
        visitor.visit_value(&mut boxed_value.value);
    }
}
