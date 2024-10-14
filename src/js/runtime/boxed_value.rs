use crate::{js::runtime::object_descriptor::ObjectKind, set_uninit};

use super::{
    gc::{HeapObject, HeapVisitor},
    object_descriptor::ObjectDescriptor,
    Context, Handle, HeapPtr, Value,
};

/// A value that is allocated on the heap.
#[repr(C)]
pub struct BoxedValue {
    descriptor: HeapPtr<ObjectDescriptor>,
    value: Value,
}

impl BoxedValue {
    pub fn new(cx: Context, value: Handle<Value>) -> HeapPtr<BoxedValue> {
        let mut scope = cx.alloc_uninit::<BoxedValue>();

        set_uninit!(scope.descriptor, cx.base_descriptors.get(ObjectKind::BoxedValue));
        set_uninit!(scope.value, *value);

        scope
    }

    pub fn get(&self) -> Value {
        self.value
    }

    pub fn set(&mut self, value: Value) {
        self.value = value;
    }
}

impl HeapObject for HeapPtr<BoxedValue> {
    fn byte_size(&self) -> usize {
        std::mem::size_of::<BoxedValue>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_value(&mut self.value);
    }
}
