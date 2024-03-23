use bitflags::bitflags;

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
    /// Compressed flag data for each slot, with one byte per slot.
    _flags: [u64; 1],
}

bitflags! {
    #[derive(Clone, Copy)]
    pub struct ScopeNameFlags: u8 {
        const IS_CONST = 0x1;
    }
}

impl ScopeNames {
    pub fn new(
        cx: Context,
        names: &[Handle<FlatString>],
        flags: &[ScopeNameFlags],
    ) -> Handle<ScopeNames> {
        let size = Self::calculate_size_in_bytes(names.len());
        let mut scope_names = cx.alloc_uninit_with_size::<ScopeNames>(size);

        set_uninit!(scope_names.descriptor, cx.base_descriptors.get(ObjectKind::ScopeNames));

        // Copy names into inline names array
        scope_names.names.init_with_uninit(names.len());
        for (i, name) in names.iter().enumerate() {
            scope_names.names.set_unchecked(i, name.get_());
        }

        // Copy flags into flags section
        let flags_ptr = scope_names.get_flags_ptr() as *mut u8;
        unsafe {
            std::ptr::copy_nonoverlapping(flags.as_ptr().cast::<u8>(), flags_ptr, flags.len())
        };

        scope_names.to_handle()
    }

    const NAMES_OFFSET: usize = field_offset!(ScopeNames, names);

    fn calculate_size_in_bytes(num_slots: usize) -> usize {
        Self::flags_offset(num_slots) + Self::calculate_flags_size(num_slots)
    }

    fn flags_offset(num_slots: usize) -> usize {
        Self::NAMES_OFFSET + InlineArray::<StringValue>::calculate_size_in_bytes(num_slots)
    }

    pub fn calculate_flags_size(num_slots: usize) -> usize {
        // One byte per slot
        num_slots
    }

    fn get_flags_ptr(&self) -> *const u8 {
        let ptr = self as *const ScopeNames as *const u8;
        unsafe { ptr.add(Self::flags_offset(self.len())) }
    }

    pub fn len(&self) -> usize {
        self.names.len()
    }

    pub fn get_slot_name(&self, index: usize) -> HeapPtr<FlatString> {
        self.names.as_slice()[index]
    }

    /// Look up a name, returning its index in the scope if present. The name must be an interned
    /// string.
    pub fn lookup_name(&self, name: HeapPtr<FlatString>) -> Option<usize> {
        self.names.as_slice().iter().position(|n| n.ptr_eq(&name))
    }

    /// Return whether the binding at index is a const binding.
    pub fn is_const(&self, index: usize) -> bool {
        let flags = unsafe { *self.get_flags_ptr().add(index).cast::<ScopeNameFlags>() };
        flags.contains(ScopeNameFlags::IS_CONST)
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
