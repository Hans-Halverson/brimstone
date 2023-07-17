use crate::{
    field_offset,
    js::runtime::{
        gc::IsHeapObject,
        object_descriptor::{ObjectDescriptor, ObjectKind},
        Context, HeapPtr,
    },
    set_uninit,
};

use super::InlineArray;

pub struct BsArray<T> {
    descriptor: HeapPtr<ObjectDescriptor>,
    array: InlineArray<T>,
}

impl<T> IsHeapObject for BsArray<T> {}

const ARRAY_BYTE_OFFSITE: usize = field_offset!(BsArray<u8>, array);

impl<T: Clone> BsArray<T> {
    pub fn new(cx: &mut Context, kind: ObjectKind, length: usize, initial: T) -> HeapPtr<Self> {
        let size = Self::calculate_size_in_bytes(length);
        let mut array = cx.heap.alloc_uninit_with_size::<BsArray<T>>(size);

        set_uninit!(array.descriptor, cx.base_descriptors.get(kind));
        array.array.init_with(length, initial);

        array
    }

    pub fn new_from_vec(cx: &mut Context, kind: ObjectKind, elements: Vec<T>) -> HeapPtr<Self> {
        let size = Self::calculate_size_in_bytes(elements.len());
        let mut array = cx.heap.alloc_uninit_with_size::<BsArray<T>>(size);

        set_uninit!(array.descriptor, cx.base_descriptors.get(kind));

        // Initialize entries array to empty
        array.array.init_with_uninit(elements.len());
        for (i, element) in elements.iter().enumerate() {
            array.array.set_unchecked(i, element.clone());
        }

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
