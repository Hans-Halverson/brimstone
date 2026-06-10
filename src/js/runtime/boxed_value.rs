use crate::{
    runtime::{
        Context, Handle, HeapPtr, Value,
        alloc_error::AllocResult,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
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

impl HeapItem for HeapPtr<BoxedValue> {
    fn byte_size(&self) -> usize {
        std::mem::size_of::<BoxedValue>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_value(&mut self.value);
    }
}
