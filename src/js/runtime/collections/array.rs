use crate::{
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        collections::InlineArray,
        gc::{HeapItem, HeapVisitor, IsHeapItem, WithHeapItemKind},
        shape::Shape,
    },
    set_uninit,
};

/// A fixed size array of values.
///
/// May store extra data of type `E` in the header. Caller is responsible for initializing.
#[repr(C)]
pub struct BsArray<T, E = ()> {
    shape: HeapPtr<Shape>,
    /// Extra data stored in the header, if any.
    extra_data: E,
    /// The array along with its size.
    array: InlineArray<T>,
}

impl<T: Clone, E> BsArray<T, E> {
    pub fn new(
        cx: Context,
        kind: HeapItemKind,
        length: usize,
        initial: T,
    ) -> AllocResult<HeapPtr<Self>> {
        let size = Self::calculate_size_in_bytes(length);
        let mut array = cx.alloc_uninit_with_size::<Self>(size)?;

        set_uninit!(array.shape, cx.shapes.get(kind));
        array.array.init_with(length, initial);

        Ok(array)
    }

    pub fn new_from_slice(
        cx: Context,
        kind: HeapItemKind,
        slice: &[T],
    ) -> AllocResult<HeapPtr<Self>> {
        let size = Self::calculate_size_in_bytes(slice.len());
        let mut array = cx.alloc_uninit_with_size::<Self>(size)?;

        set_uninit!(array.shape, cx.shapes.get(kind));
        array.array.init_from_slice(slice);

        Ok(array)
    }
}

impl<T, E> BsArray<T, E> {
    pub fn new_uninit(
        cx: Context,
        kind: HeapItemKind,
        length: usize,
    ) -> AllocResult<HeapPtr<Self>> {
        let size = Self::calculate_size_in_bytes(length);
        let mut array = cx.alloc_uninit_with_size::<Self>(size)?;

        set_uninit!(array.shape, cx.shapes.get(kind));
        array.array.init_with_uninit(length);

        Ok(array)
    }

    #[inline]
    pub fn calculate_size_in_bytes(length: usize) -> usize {
        Self::array_byte_offset() + InlineArray::<T>::calculate_size_in_bytes(length)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.array.len()
    }

    /// The extra data stored in the header, if any.
    #[inline]
    pub fn extra_data(&self) -> &E {
        &self.extra_data
    }

    /// The extra data stored in the header, if any.
    #[inline]
    pub fn extra_data_mut(&mut self) -> &mut E {
        &mut self.extra_data
    }

    #[inline]
    pub fn as_slice(&self) -> &[T] {
        self.array.as_slice()
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        self.array.as_mut_slice()
    }

    /// Byte offset of the inline array.
    #[inline]
    pub const fn array_byte_offset() -> usize {
        std::mem::offset_of!(Self, array)
    }

    /// Visit pointers intrinsic to all Arrays. Do not visit elements as they could be of any type.
    pub fn visit_array_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.shape);
    }
}

/// An instance of a BsArray with a specific element type. This has its own object shape
/// identifying the full BsArray<T>.
pub trait ArrayInstance:
    IsHeapItem
    + WithHeapItemKind
    + std::ops::Deref<Target = BsArray<Self::T, Self::E>>
    + std::ops::DerefMut<Target = BsArray<Self::T, Self::E>>
{
    type T;
    type E;

    fn new(cx: Context, capacity: usize, initial: Self::T) -> AllocResult<HeapPtr<Self>>
    where
        Self::T: Clone,
    {
        Ok(BsArray::<Self::T, Self::E>::new(cx, Self::KIND, capacity, initial)?.cast())
    }

    fn new_from_slice(cx: Context, slice: &[Self::T]) -> AllocResult<HeapPtr<Self>>
    where
        Self::T: Clone,
    {
        Ok(BsArray::<Self::T, Self::E>::new_from_slice(cx, Self::KIND, slice)?.cast())
    }

    fn new_uninit(cx: Context, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        Ok(BsArray::<Self::T, Self::E>::new_uninit(cx, Self::KIND, capacity)?.cast())
    }

    fn calculate_size_in_bytes(capacity: usize) -> usize {
        BsArray::<Self::T, Self::E>::calculate_size_in_bytes(capacity)
    }
}

#[macro_export]
macro_rules! impl_array_instance {
    ($array_type:ident, $element_type:ty) => {
        $crate::impl_array_instance!($array_type, $element_type, ());
    };
    ($array_type:ident, $element_type:ty, $extra_data_type:ty) => {
        #[repr(transparent)]
        pub struct $array_type(
            $crate::runtime::collections::BsArray<$element_type, $extra_data_type>,
        );

        impl $crate::runtime::collections::ArrayInstance for $array_type {
            type T = $element_type;
            type E = $extra_data_type;
        }

        impl std::ops::Deref for $array_type {
            type Target = $crate::runtime::collections::BsArray<$element_type, $extra_data_type>;

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
impl<T, E> IsHeapItem for BsArray<T, E> {}
