use crate::{field_offset, js::runtime::object_descriptor::ObjectKind, set_uninit};

use super::{
    collections::InlineArray,
    gc::{HeapObject, HeapVisitor},
    object_descriptor::ObjectDescriptor,
    string_value::{FlatString, StringValue},
    Context, Handle, HeapPtr,
};

#[repr(C)]
pub struct ScopeNames {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Inline array of names for the slots.
    names: InlineArray<HeapPtr<FlatString>>,
}

impl ScopeNames {
    pub fn new(cx: Context, names: &[Handle<FlatString>]) -> Handle<ScopeNames> {
        let size = Self::calculate_size_in_bytes(names.len());
        let mut scope_names = cx.alloc_uninit_with_size::<ScopeNames>(size);

        set_uninit!(scope_names.descriptor, cx.base_descriptors.get(ObjectKind::ScopeNames));

        scope_names.names.init_with_uninit(names.len());
        for (i, name) in names.iter().enumerate() {
            scope_names.names.set_unchecked(i, name.get_());
        }

        scope_names.to_handle()
    }

    const NAMES_OFFSET: usize = field_offset!(ScopeNames, names);

    fn calculate_size_in_bytes(num_slots: usize) -> usize {
        Self::NAMES_OFFSET + InlineArray::<StringValue>::calculate_size_in_bytes(num_slots)
    }

    pub fn len(&self) -> usize {
        self.names.len()
    }

    pub fn get_slot_name(&self, index: usize) -> HeapPtr<FlatString> {
        self.names.as_slice()[index]
    }
}

impl HeapObject for HeapPtr<ScopeNames> {
    fn byte_size(&self) -> usize {
        ScopeNames::calculate_size_in_bytes(self.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        for name in self.names.as_mut_slice() {
            visitor.visit_pointer(name);
        }
    }
}
