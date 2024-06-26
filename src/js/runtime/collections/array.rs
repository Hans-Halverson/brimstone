use crate::{
    field_offset,
    js::runtime::{
        gc::{HeapObject, HeapVisitor},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        Context, HeapPtr,
    },
    set_uninit,
};

use super::InlineArray;

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

    #[inline]
    pub fn calculate_size_in_bytes(length: usize) -> usize {
        ARRAY_BYTE_OFFSITE + InlineArray::<T>::calculate_size_in_bytes(length)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.array.len()
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
