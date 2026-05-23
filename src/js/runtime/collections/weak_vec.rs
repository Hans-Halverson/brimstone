use crate::{
    field_offset,
    runtime::{
        alloc_error::AllocResult,
        collections::InlineArray,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
        Context, HeapPtr, Value,
    },
    set_uninit,
};

/// A growable array of weakly held values.
///
/// Garbage collector is aware of this type and compresses it in place during collection.
///
/// The intrusive `next_weak_vec` list used by the collector lives on the holder of the vec, not on
/// the vec itself.
#[repr(C)]
pub struct BsWeakVec {
    descriptor: HeapPtr<HeapItemDescriptor>,
    /// The number of elements stored in the array.
    length: usize,
    // Holds the address of the next weak list that has been visited during garbage collection.
    // Unused outside of garbage collection.
    next_weak_vec: Option<HeapPtr<BsWeakVec>>,
    /// The array along with its capacity, which is always a power of 2.
    array: InlineArray<Value>,
}

impl BsWeakVec {
    /// Create a new BsWeakVec with the given capacity.
    pub fn new(cx: Context, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        let size = Self::calculate_size_in_bytes(capacity);
        let mut vec = cx.alloc_uninit_with_size::<BsWeakVec>(size)?;

        set_uninit!(vec.descriptor, cx.base_descriptors.get(HeapItemKind::WeakVec));
        set_uninit!(vec.length, 0);
        set_uninit!(vec.next_weak_vec, None);
        vec.array.init_with_uninit(capacity);

        Ok(vec)
    }

    const ARRAY_FIELD_OFFSET: usize = field_offset!(BsWeakVec, array);

    #[inline]
    fn calculate_size_in_bytes(capacity: usize) -> usize {
        Self::ARRAY_FIELD_OFFSET + InlineArray::<Value>::calculate_size_in_bytes(capacity)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.length
    }

    #[inline]
    pub fn set_len(&mut self, len: usize) {
        self.length = len;
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.array.len()
    }

    pub fn next_weak_vec(&self) -> Option<HeapPtr<BsWeakVec>> {
        self.next_weak_vec
    }

    pub fn set_next_weak_vec(&mut self, next_weak_vec: Option<HeapPtr<BsWeakVec>>) {
        self.next_weak_vec = next_weak_vec;
    }

    #[inline]
    pub fn as_slice(&self) -> &[Value] {
        &self.array.as_slice()[..self.len()]
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [Value] {
        let len = self.len();
        &mut self.array.as_mut_slice()[..len]
    }

    /// Append an item to the BsWeakVec. Should only be called if there is room to append an item.
    pub fn push_without_growing(&mut self, item: Value) {
        let len = self.len();
        self.array.as_mut_slice()[len] = item;
        self.length += 1;
    }
}

impl HeapItem for HeapPtr<BsWeakVec> {
    fn byte_size(&self) -> usize {
        BsWeakVec::calculate_size_in_bytes(self.capacity())
    }

    /// Visit pointers intrinsic to all BsWeakVec. Do not visit elements as they could be of any type.
    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
    }
}

/// A BsWeakVec stored as the field of a heap item. Can create new BsWeakVec objects and set the
/// field to a new BsWeakVec.
#[allow(unused)]
pub trait BsWeakVecField {
    fn new_vec(cx: Context, capacity: usize) -> AllocResult<HeapPtr<BsWeakVec>>;

    fn get(&self) -> HeapPtr<BsWeakVec>;

    fn set(&mut self, vec: HeapPtr<BsWeakVec>);

    /// Prepare vec for appending a single item. This will grow the vec and update container to
    /// point to new vec if there is no room to append another item to the vec.
    #[inline]
    fn maybe_grow_for_push(&mut self, cx: Context) -> AllocResult<HeapPtr<BsWeakVec>> {
        let old_vec = self.get();

        // Check if we have room for another item in the vec
        let old_len = old_vec.len();
        let capacity = old_vec.capacity();
        if old_len < capacity {
            return Ok(old_vec);
        }

        // Save old vec behind handle before allocating
        let old_vec = old_vec.to_handle();

        // Double size of vector, starting at a length of 4
        let new_capacity = (capacity * 2).max(4);
        let mut new_vec = Self::new_vec(cx, new_capacity)?;

        // Update parent reference from old child to new child map
        self.set(new_vec);

        // Copy all values into new vec
        new_vec.length = old_len;
        new_vec.as_mut_slice().copy_from_slice(old_vec.as_slice());

        Ok(new_vec)
    }
}
