use crate::{
    field_offset,
    runtime::{
        alloc_error::AllocResult,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
        Context, HeapPtr, Value,
    },
    set_uninit,
};

use super::InlineArray;

/// A growable array of values.
#[repr(C)]
pub struct BsVec<T> {
    descriptor: HeapPtr<HeapItemDescriptor>,
    /// The number of elements stored in the array.
    length: usize,
    /// The array along with its capacity, which is always a power of 2.
    array: InlineArray<T>,
}

impl<T: Clone + Copy> BsVec<T> {
    /// Create a new BsVec with the given capacity.
    pub fn new(cx: Context, kind: HeapItemKind, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        let size = Self::calculate_size_in_bytes(capacity);
        let mut vec = cx.alloc_uninit_with_size::<BsVec<T>>(size)?;

        set_uninit!(vec.descriptor, cx.base_descriptors.get(kind));
        set_uninit!(vec.length, 0);
        vec.array.init_with_uninit(capacity);

        Ok(vec)
    }

    const ARRAY_FIELD_OFFSET: usize = field_offset!(BsVec<u8>, array);

    #[inline]
    fn calculate_size_in_bytes(capacity: usize) -> usize {
        Self::ARRAY_FIELD_OFFSET + InlineArray::<T>::calculate_size_in_bytes(capacity)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.length
    }

    #[inline]
    fn capacity(&self) -> usize {
        self.array.len()
    }

    #[inline]
    pub fn as_slice(&self) -> &[T] {
        &self.array.as_slice()[..self.len()]
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        let len = self.len();
        &mut self.array.as_mut_slice()[..len]
    }

    /// Append an item to the BsVec. Should only be called if there is room to append an item.
    pub fn push_without_growing(&mut self, item: T) {
        let len = self.len();
        self.array.as_mut_slice()[len] = item;
        self.length += 1;
    }
}

impl<T: Clone + Copy> HeapItem for HeapPtr<BsVec<T>> {
    fn byte_size(&self) -> usize {
        BsVec::<T>::calculate_size_in_bytes(self.capacity())
    }

    /// Visit pointers intrinsic to all BsVecs. Do not visit elements as they could be of any type.
    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
    }
}

/// A BsVec stored as the field of a heap item. Can create new BsVec objects and set the field to
/// a new BsVec.
pub trait BsVecField<T: Clone + Copy> {
    fn new_vec(cx: Context, capacity: usize) -> AllocResult<HeapPtr<BsVec<T>>>;

    fn get(&self) -> HeapPtr<BsVec<T>>;

    fn set(&mut self, vec: HeapPtr<BsVec<T>>);

    /// Prepare vec for appending a single item. This will grow the vec and update container to
    /// point to new vec if there is no room to append another item to the vec.
    #[inline]
    fn maybe_grow_for_push(&mut self, cx: Context) -> AllocResult<HeapPtr<BsVec<T>>> {
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

/// A generic vec of values. Corresponds to HeapItemKind::ValueVec.
type ValueVec = BsVec<Value>;

pub fn value_vec_byte_size(value_array: HeapPtr<ValueVec>) -> usize {
    BsVec::<Value>::calculate_size_in_bytes(value_array.capacity())
}

pub fn value_vec_visit_pointers(value_vec: &mut HeapPtr<ValueVec>, visitor: &mut impl HeapVisitor) {
    value_vec.visit_pointers(visitor);

    for value in value_vec.as_mut_slice() {
        visitor.visit_value(value);
    }
}
