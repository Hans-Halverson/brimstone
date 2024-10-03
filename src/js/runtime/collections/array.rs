use crate::{
    field_offset,
    js::runtime::{
        gc::{HeapObject, HeapVisitor},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        Context, HeapPtr, Value,
    },
    set_uninit,
};

use super::InlineArray;

/// A fixed size array of values.
#[repr(C)]
pub struct BsArray<T> {
    descriptor: HeapPtr<ObjectDescriptor>,
    array: InlineArray<T>,
}

const ARRAY_BYTE_OFFSITE: usize = field_offset!(BsArray<u8>, array);

impl<T: Clone> BsArray<T> {
    pub fn new(cx: Context, kind: ObjectKind, length: usize, initial: T) -> HeapPtr<Self> {
        let size = Self::calculate_size_in_bytes(length);
        let mut array = cx.alloc_uninit_with_size::<BsArray<T>>(size);

        set_uninit!(array.descriptor, cx.base_descriptors.get(kind));
        array.array.init_with(length, initial);

        array
    }

    pub fn new_from_slice(cx: Context, kind: ObjectKind, slice: &[T]) -> HeapPtr<Self> {
        let size = Self::calculate_size_in_bytes(slice.len());
        let mut array = cx.alloc_uninit_with_size::<BsArray<T>>(size);

        set_uninit!(array.descriptor, cx.base_descriptors.get(kind));
        array.array.init_from_slice(slice);

        array
    }

    pub fn new_uninit(cx: Context, kind: ObjectKind, length: usize) -> HeapPtr<Self> {
        let size = Self::calculate_size_in_bytes(length);
        let mut array = cx.alloc_uninit_with_size::<BsArray<T>>(size);

        set_uninit!(array.descriptor, cx.base_descriptors.get(kind));
        array.array.init_with_uninit(length);

        array
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

impl<T: Clone> HeapObject for HeapPtr<BsArray<T>> {
    fn byte_size(&self) -> usize {
        BsArray::<T>::calculate_size_in_bytes(self.len())
    }

    /// Visit pointers intrinsic to all BsArrays. Do not visit elements as they could be of any type.
    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
    }
}

/// A generic array of values. Corresponds to ObjectKind::ValueArray.
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

/// A generic array of bytes. Corresponds to ObjectKind::ByteArray.
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
