use crate::{
    field_offset,
    runtime::{
        alloc_error::AllocResult,
        collections::InlineArray,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
        Context, Handle, HeapPtr,
    },
    set_uninit,
};

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum AddFeedbackState {
    /// No feedback collected yet.
    Uninitialized = 0,
    /// Both operands were smis, result was smi (no overflow).
    SmiSmi = 1,
    /// Both operands were numbers (smi or double), at least one was double.
    Number = 2,
    /// At least one operand was a string
    StringConcat = 3,
    /// Both operands were BigInts
    BigInt = 4,
    /// Mixed/incompatible types seen, or types changed across executions.
    Any = 5,
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FeedbackSlot {
    AddOp(AddFeedbackState),
    Unknown,
}

impl FeedbackSlot {
    pub fn add_uninit() -> Self {
        Self::AddOp(AddFeedbackState::Uninitialized)
    }

    pub fn add_string_concat() -> Self {
        Self::AddOp(AddFeedbackState::StringConcat)
    }
}

#[repr(C)]
pub struct FeedbackVector {
    descriptor: HeapPtr<HeapItemDescriptor>,
    /// Number of feedback slots
    num_slots: u32,
    /// Inline array of slot data.
    /// Note: the layout of FeedbackSlot
    /// needs to be very precise for this to
    /// work, hence the repr(u8)
    slots: InlineArray<FeedbackSlot>,
}

impl FeedbackVector {
    /// The offset of the `slots` field in a FeedbackVector
    const SLOTS_BYTE_OFFSET: usize = field_offset!(FeedbackVector, slots);

    pub fn slots(&self) -> &InlineArray<FeedbackSlot> {
        &self.slots
    }

    pub fn slots_mut(&mut self) -> &mut InlineArray<FeedbackSlot> {
        &mut self.slots
    }

    /// Calculates the total size in bytes for a Feedback vector
    /// Note that the slots InlineArray can be variable length (i.e., length `num_slots`)
    /// The size is the byte offset of `slots` + the numbers of bytes needed for the array
    /// of feedback slots
    fn calculate_size_in_bytes(num_slots: usize) -> usize {
        Self::SLOTS_BYTE_OFFSET + InlineArray::<FeedbackSlot>::calculate_size_in_bytes(num_slots)
    }

    /// Create a new feedback vector with the passed feedback slots
    pub fn new(cx: Context, initial_slots: &[FeedbackSlot]) -> AllocResult<Handle<FeedbackVector>> {
        // get the size of the the FeedbackVector
        let num_slots = initial_slots.len();
        let size = Self::calculate_size_in_bytes(num_slots);
        // allocate the FeedbackVector on the heap
        let mut object = cx.alloc_uninit_with_size::<FeedbackVector>(size)?;

        // set the object's descriptor
        set_uninit!(object.descriptor, cx.base_descriptors.get(HeapItemKind::FeedbackVector));
        // set the number of slots
        set_uninit!(object.num_slots, num_slots as u32);

        // Initialize the slots to the passed feedback slots
        object.slots.init_from_slice(initial_slots);

        Ok(object.to_handle())
    }
}

/// GC Integration for FeedbackVector
impl HeapItem for HeapPtr<FeedbackVector> {
    fn byte_size(&self) -> usize {
        FeedbackVector::calculate_size_in_bytes(self.num_slots as usize)
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        // For now, only visit the descriptor, and we do not
        // need to visit feedback slots as those have no pointers
        visitor.visit_pointer(&mut self.descriptor);
    }
}
