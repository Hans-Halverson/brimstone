use crate::{
    field_offset,
    runtime::{
        Context, HeapItemKind, HeapPtr,
        alloc_error::AllocResult,
        collections::InlineArray,
        gc::{HeapVisitor, IsHeapItem},
        heap_item_descriptor::HeapItemDescriptor,
    },
    set_uninit,
};

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
    pub const MIN_CAPACITY: usize = 4;

    /// Create a new BsVec with the given capacity.
    pub fn new(cx: Context, kind: HeapItemKind, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        let size = Self::calculate_size_in_bytes(capacity);
        let mut vec = cx.alloc_uninit_with_size::<BsVec<T>>(size)?;

        set_uninit!(vec.descriptor, cx.descriptors.get(kind));
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
    pub fn set_len(&mut self, len: usize) {
        self.length = len;
    }

    #[inline]
    pub fn capacity(&self) -> usize {
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

    /// Visit pointers intrinsic to all Vecs. Do not visit elements as they could be of any type.
    pub fn visit_vec_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
    }
}

/// An instance of a BsVec with a specific element type. This has its own object descriptor
/// identifying the full BsVec<T>.
pub trait VecInstance:
    IsHeapItem + std::ops::Deref<Target = BsVec<Self::T>> + std::ops::DerefMut<Target = BsVec<Self::T>>
{
    type T: Clone + Copy;

    const KIND: HeapItemKind;

    const MIN_CAPACITY: usize = BsVec::<Self::T>::MIN_CAPACITY;

    fn new(cx: Context, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        Ok(BsVec::<Self::T>::new(cx, Self::KIND, capacity)?.cast())
    }

    fn new_initial(cx: Context) -> AllocResult<HeapPtr<Self>> {
        Ok(BsVec::<Self::T>::new(cx, Self::KIND, Self::MIN_CAPACITY)?.cast())
    }

    fn calculate_size_in_bytes(capacity: usize) -> usize {
        BsVec::<Self::T>::calculate_size_in_bytes(capacity)
    }
}

#[macro_export]
macro_rules! impl_vec_instance {
    ($vec_type:ident, $element_type:ty) => {
        #[repr(transparent)]
        pub struct $vec_type($crate::runtime::collections::BsVec<$element_type>);

        impl $crate::runtime::collections::VecInstance for $vec_type {
            type T = $element_type;

            const KIND: $crate::runtime::HeapItemKind = $crate::runtime::HeapItemKind::$vec_type;
        }

        impl std::ops::Deref for $vec_type {
            type Target = $crate::runtime::collections::BsVec<$element_type>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl std::ops::DerefMut for $vec_type {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    };
}

/// A BsVec stored as the field of a heap item. Can create new BsVec objects and set the field to
/// a new BsVec.
pub trait BsVecField<I: VecInstance> {
    fn get(&self) -> HeapPtr<I>;

    fn set_new(&mut self, cx: Context, capacity: usize) -> AllocResult<HeapPtr<I>>;

    /// Prepare vec for appending a single item. This will grow the vec and update container to
    /// point to new vec if there is no room to append another item to the vec.
    #[inline]
    fn maybe_grow_for_push(&mut self, cx: Context) -> AllocResult<HeapPtr<I>> {
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
        let new_capacity = (capacity * 2).max(I::MIN_CAPACITY);
        let mut new_vec = self.set_new(cx, new_capacity)?;

        // Copy all values into new vec
        new_vec.length = old_len;
        new_vec.as_mut_slice().copy_from_slice(old_vec.as_slice());

        Ok(new_vec)
    }
}

// Only necessary so we get deref for HeapPtrs.
impl<T> IsHeapItem for BsVec<T> {}
