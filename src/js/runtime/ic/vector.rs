use crate::{
    field_offset,
    runtime::{
        alloc_error::AllocResult,
        collections::InlineArray,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
        ic::stubs::binary_arith::{
            AddICStub, BitAndICStub, BitOrICStub, BitXorICStub, DivICStub, ExpICStub, MulICStub,
            RemICStub, ShiftLeftICStub, ShiftRightArithICStub, ShiftRightLogicalICStub, SubICStub,
        },
        Context, Handle, HeapPtr,
    },
    set_uninit,
};

#[derive(Clone, Copy)]
pub enum ICEntry {
    Add(HeapPtr<AddICStub>),
    Sub(HeapPtr<SubICStub>),
    Mul(HeapPtr<MulICStub>),
    Div(HeapPtr<DivICStub>),
    Rem(HeapPtr<RemICStub>),
    Exp(HeapPtr<ExpICStub>),
    BitAnd(HeapPtr<BitAndICStub>),
    BitOr(HeapPtr<BitOrICStub>),
    BitXor(HeapPtr<BitXorICStub>),
    ShiftLeft(HeapPtr<ShiftLeftICStub>),
    ShiftRightArith(HeapPtr<ShiftRightArithICStub>),
    ShiftRightLogical(HeapPtr<ShiftRightLogicalICStub>),
}

#[repr(C)]
pub struct ICVector {
    descriptor: HeapPtr<HeapItemDescriptor>,
    /// Inline array of pointers to
    /// IC Stubs. Each element is a unique
    /// linked list of IC Stubs for an opcode
    slots: InlineArray<Option<ICEntry>>,
}

impl ICVector {
    /// The offset of the `slots` field in a ICVector
    const SLOTS_BYTE_OFFSET: usize = field_offset!(ICVector, slots);

    pub fn slots(&self) -> &InlineArray<Option<ICEntry>> {
        &self.slots
    }

    pub fn slots_mut(&mut self) -> &mut InlineArray<Option<ICEntry>> {
        &mut self.slots
    }

    /// Calculates the total size in bytes for a Feedback vector
    /// Note that the slots InlineArray can be variable length
    /// The size is the byte offset of `slots` + the numbers of bytes needed for the array
    /// of ic stubs
    fn calculate_size_in_bytes(num_slots: usize) -> usize {
        Self::SLOTS_BYTE_OFFSET + InlineArray::<Option<ICEntry>>::calculate_size_in_bytes(num_slots)
    }

    /// Create a new feedback vector with the passed feedback slots
    pub fn new(cx: Context, num_slots: usize) -> AllocResult<Handle<ICVector>> {
        // get the size of the the ICVector
        let size = Self::calculate_size_in_bytes(num_slots);
        // allocate the ICVector on the heap
        let mut object = cx.alloc_uninit_with_size::<ICVector>(size)?;

        // set the object's descriptor
        set_uninit!(object.descriptor, cx.base_descriptors.get(HeapItemKind::ICVector));

        // initialize the slots array
        object.slots.init_with_uninit(num_slots);

        // Initialize the ICStubs in the slots array to None
        for i in 0..num_slots {
            object.slots.set_unchecked(i, None);
        }

        Ok(object.to_handle())
    }
}

/// GC Integration for ICVector
impl HeapItem for HeapPtr<ICVector> {
    fn byte_size(&self) -> usize {
        ICVector::calculate_size_in_bytes(self.slots.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        // visit the descriptor
        visitor.visit_pointer(&mut self.descriptor);
        // visit each ICEntry's underlying Stub
        for stub in self.slots.as_mut_slice() {
            if let Some(ptr) = stub {
                match ptr {
                    ICEntry::Add(heap_ptr) => visitor.visit_pointer(heap_ptr),
                    ICEntry::Sub(heap_ptr) => visitor.visit_pointer(heap_ptr),
                    ICEntry::Mul(heap_ptr) => visitor.visit_pointer(heap_ptr),
                    ICEntry::Div(heap_ptr) => visitor.visit_pointer(heap_ptr),
                    ICEntry::Rem(heap_ptr) => visitor.visit_pointer(heap_ptr),
                    ICEntry::Exp(heap_ptr) => visitor.visit_pointer(heap_ptr),
                    ICEntry::BitAnd(heap_ptr) => visitor.visit_pointer(heap_ptr),
                    ICEntry::BitOr(heap_ptr) => visitor.visit_pointer(heap_ptr),
                    ICEntry::BitXor(heap_ptr) => visitor.visit_pointer(heap_ptr),
                    ICEntry::ShiftLeft(heap_ptr) => visitor.visit_pointer(heap_ptr),
                    ICEntry::ShiftRightArith(heap_ptr) => visitor.visit_pointer(heap_ptr),
                    ICEntry::ShiftRightLogical(heap_ptr) => visitor.visit_pointer(heap_ptr),
                }
            }
        }
    }
}
