use crate::{
    field_offset,
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        collections::InlineArray,
        gc::{HeapItem, HeapVisitor, IsHeapItem, WithHeapItemKind},
        heap_item_descriptor::HeapItemDescriptor,
    },
    set_uninit,
};

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

        set_uninit!(array.descriptor, cx.descriptors.get(kind));
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

        set_uninit!(array.descriptor, cx.descriptors.get(kind));
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

        set_uninit!(array.descriptor, cx.descriptors.get(kind));
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

    /// Visit pointers intrinsic to all Arrays. Do not visit elements as they could be of any type.
    pub fn visit_array_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
    }
}

/// An instance of a BsArray with a specific element type. This has its own object descriptor
/// identifying the full BsArray<T>.
pub trait ArrayInstance:
    IsHeapItem
    + WithHeapItemKind
    + std::ops::Deref<Target = BsArray<Self::T>>
    + std::ops::DerefMut<Target = BsArray<Self::T>>
{
    type T;

    fn new(cx: Context, capacity: usize, initial: Self::T) -> AllocResult<HeapPtr<Self>>
    where
        Self::T: Clone,
    {
        Ok(BsArray::<Self::T>::new(cx, Self::KIND, capacity, initial)?.cast())
    }

    fn new_from_slice(cx: Context, slice: &[Self::T]) -> AllocResult<HeapPtr<Self>>
    where
        Self::T: Clone,
    {
        Ok(BsArray::<Self::T>::new_from_slice(cx, Self::KIND, slice)?.cast())
    }

    fn new_uninit(cx: Context, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        Ok(BsArray::<Self::T>::new_uninit(cx, Self::KIND, capacity)?.cast())
    }

    fn calculate_size_in_bytes(capacity: usize) -> usize {
        BsArray::<Self::T>::calculate_size_in_bytes(capacity)
    }
}

#[macro_export]
macro_rules! impl_array_instance {
    ($array_type:ident, $element_type:ty) => {
        #[repr(transparent)]
        pub struct $array_type($crate::runtime::collections::BsArray<$element_type>);

        impl $crate::runtime::collections::ArrayInstance for $array_type {
            type T = $element_type;
        }

        impl std::ops::Deref for $array_type {
            type Target = $crate::runtime::collections::BsArray<$element_type>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl std::ops::DerefMut for $array_type {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    };
}

impl_array_instance!(ValueArray, Value);

impl ValueArray {
    /// A ValueArray must be created from a slice of handles.
    ///
    /// It is not safe to use `ValueArray::new_from_slice`.
    pub fn new_from_handle_slice(
        cx: Context,
        slice: &[Handle<Value>],
    ) -> AllocResult<HeapPtr<ValueArray>> {
        let mut array = ValueArray::new_uninit(cx, slice.len())?;

        for (i, value) in slice.iter().enumerate() {
            array.as_mut_slice()[i] = **value;
        }

        Ok(array)
    }
}

impl HeapItem for ValueArray {
    fn byte_size(array: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(array.len())
    }

    fn visit_pointers(mut array: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        array.visit_array_pointers(visitor);

        for value in array.as_mut_slice() {
            visitor.visit_value(value);
        }
    }
}

impl_array_instance!(ByteArray, u8);

impl HeapItem for ByteArray {
    fn byte_size(array: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(array.len())
    }

    fn visit_pointers(mut array: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        array.visit_array_pointers(visitor);
    }
}

impl_array_instance!(U32Array, u32);

impl HeapItem for U32Array {
    fn byte_size(array: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(array.len())
    }

    fn visit_pointers(mut array: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        array.visit_array_pointers(visitor);
    }
}

// Only necessary so we get deref for HeapPtrs.
impl<T> IsHeapItem for BsArray<T> {}
