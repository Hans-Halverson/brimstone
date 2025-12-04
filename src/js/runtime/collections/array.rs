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

/// A fixed size array of values.
#[repr(C)]
pub struct BsArray<T> {
    descriptor: HeapPtr<HeapItemDescriptor>,
    array: InlineArray<T>,
}

const ARRAY_BYTE_OFFSITE: usize = field_offset!(BsArray<u8>, array);

impl<T: Clone> BsArray<T> {
    pub fn new(
        cx: Context,
        kind: HeapItemKind,
        length: usize,
        initial: T,
    ) -> AllocResult<HeapPtr<Self>> {
        let size = Self::calculate_size_in_bytes(length);
        let mut array = cx.alloc_uninit_with_size::<BsArray<T>>(size)?;

        set_uninit!(array.descriptor, cx.base_descriptors.get(kind));
        array.array.init_with(length, initial);

        Ok(array)
    }

    pub fn new_from_slice(
        cx: Context,
        kind: HeapItemKind,
        slice: &[T],
    ) -> AllocResult<HeapPtr<Self>> {
        let size = Self::calculate_size_in_bytes(slice.len());
        let mut array = cx.alloc_uninit_with_size::<BsArray<T>>(size)?;

        set_uninit!(array.descriptor, cx.base_descriptors.get(kind));
        array.array.init_from_slice(slice);

        Ok(array)
    }
}

impl<T> BsArray<T> {
    pub fn new_uninit(
        cx: Context,
        kind: HeapItemKind,
        length: usize,
    ) -> AllocResult<HeapPtr<Self>> {
        let size = Self::calculate_size_in_bytes(length);
        let mut array = cx.alloc_uninit_with_size::<BsArray<T>>(size)?;

        set_uninit!(array.descriptor, cx.base_descriptors.get(kind));
        array.array.init_with_uninit(length);

        Ok(array)
    }

    #[inline]
    pub fn calculate_size_in_bytes(length: usize) -> usize {
        ARRAY_BYTE_OFFSITE + InlineArray::<T>::calculate_size_in_bytes(length)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.array.len()
    }

    #[inline]
    pub fn as_slice(&self) -> &[T] {
        self.array.as_slice()
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        self.array.as_mut_slice()
    }
}

impl<T> HeapItem for HeapPtr<BsArray<T>> {
    fn byte_size(&self) -> usize {
        BsArray::<T>::calculate_size_in_bytes(self.len())
    }

    /// Visit pointers intrinsic to all BsArrays. Do not visit elements as they could be of any type.
    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
    }
}

/// A generic array of values. Corresponds to HeapItemKind::ValueArray.
pub type ValueArray = BsArray<Value>;

pub fn value_array_byte_size(value_array: HeapPtr<ValueArray>) -> usize {
    ValueArray::calculate_size_in_bytes(value_array.len())
}

pub fn value_array_visit_pointers(
    value_array: &mut HeapPtr<ValueArray>,
    visitor: &mut impl HeapVisitor,
) {
    value_array.visit_pointers(visitor);

    for value in value_array.as_mut_slice() {
        visitor.visit_value(value);
    }
}

/// A generic array of bytes. Corresponds to HeapItemKind::ByteArray.
///
/// Can be used for any kind of opaque byte data.
pub type ByteArray = BsArray<u8>;

pub fn byte_array_byte_size(byte_array: HeapPtr<ByteArray>) -> usize {
    ByteArray::calculate_size_in_bytes(byte_array.len())
}

pub fn byte_array_visit_pointers(
    byte_array: &mut HeapPtr<ByteArray>,
    visitor: &mut impl HeapVisitor,
) {
    byte_array.visit_pointers(visitor);
}

/// A generic array of opaque 32-bit values. Corresponds to HeapItemKind::U32Array.
///
/// Can be used for any kind of opaque 32-bit data.
pub type U32Array = BsArray<u32>;

pub fn u32_array_byte_size(array: HeapPtr<U32Array>) -> usize {
    U32Array::calculate_size_in_bytes(array.len())
}

pub fn u32_array_visit_pointers(array: &mut HeapPtr<U32Array>, visitor: &mut impl HeapVisitor) {
    array.visit_pointers(visitor);
}
