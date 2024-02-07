use crate::{field_offset, js::runtime::object_descriptor::ObjectKind, set_uninit};

use super::{
    collections::InlineArray,
    gc::{HeapObject, HeapVisitor},
    object_descriptor::ObjectDescriptor,
    object_value::ObjectValue,
    Context, Handle, HeapPtr, Value,
};

#[repr(C)]
pub struct Scope {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Parent scope, forming a chain of scopes up to the global scope.
    parent: Option<HeapPtr<Scope>>,
    /// Object containing scope bindings if this is a global or with scope.
    object: Option<HeapPtr<ObjectValue>>,
    /// Inline array of slots for variables in this scope.
    slots: InlineArray<Value>,
}

impl Scope {
    pub fn new_global(cx: Context, global_object: Handle<ObjectValue>) -> Handle<Scope> {
        let size = Self::calculate_size_in_bytes(0);
        let mut scope = cx.alloc_uninit_with_size::<Scope>(size);

        set_uninit!(scope.descriptor, cx.base_descriptors.get(ObjectKind::Scope));
        set_uninit!(scope.parent, None);
        set_uninit!(scope.object, Some(global_object.get_()));

        scope.slots.init_with_uninit(0);

        scope.to_handle()
    }

    const SLOTS_OFFSET: usize = field_offset!(Scope, slots);

    fn calculate_size_in_bytes(num_slots: usize) -> usize {
        Self::SLOTS_OFFSET + InlineArray::<Value>::calculate_size_in_bytes(num_slots)
    }

    #[inline]
    pub fn object_ptr(&self) -> HeapPtr<ObjectValue> {
        self.object.unwrap()
    }

    #[inline]
    pub fn object(&self) -> Handle<ObjectValue> {
        self.object_ptr().to_handle()
    }
}

impl HeapObject for HeapPtr<Scope> {
    fn byte_size(&self) -> usize {
        Scope::calculate_size_in_bytes(self.slots.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer_opt(&mut self.parent);
        visitor.visit_pointer_opt(&mut self.object);

        for slot in self.slots.as_mut_slice() {
            visitor.visit_value(slot);
        }
    }
}
