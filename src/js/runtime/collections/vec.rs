use crate::{
    runtime::{
        Context, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        collections::InlineArray,
        gc::{HeapItem, HeapVisitor, IsHeapItem, WithHeapItemKind},
        shape::Shape,
    },
    set_uninit,
};

/// A growable array of values.
///
/// May store extra data of type `H` in the header. Caller is responsible for initializing.
#[repr(C)]
pub struct BsVec<T, H = ()> {
    shape: HeapPtr<Shape>,
    /// Extra data stored in the header, if any.
    extra_data: H,
    /// The number of elements stored in the array.
    length: usize,
    /// The array along with its capacity, which is always a power of 2.
    array: InlineArray<T>,
}

impl<T, H> BsVec<T, H> {
    /// Create a new BsVec with the given capacity.
    pub fn new(cx: Context, kind: HeapItemKind, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        let size = Self::calculate_size_in_bytes(capacity);
        let mut vec = cx.alloc_uninit_with_size::<BsVec<T, H>>(size)?;

        vec.init(cx, kind, capacity);

        Ok(vec)
    }

    pub fn init(&mut self, cx: Context, kind: HeapItemKind, capacity: usize) {
        set_uninit!(self.shape, cx.shapes.get(kind));
        set_uninit!(self.length, 0);
        self.array.init_with_uninit(capacity);

        // Note that extra data is uninitialized, caller must initialize it if needed.
    }

    const MIN_CAPACITY: usize = 4;

    #[inline]
    pub fn calculate_size_in_bytes(capacity: usize) -> usize {
        std::mem::offset_of!(Self, array) + InlineArray::<T>::calculate_size_in_bytes(capacity)
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

    /// The extra data stored in the header, if any.
    #[inline]
    pub fn extra_data(&self) -> &H {
        &self.extra_data
    }

    /// The extra data stored in the header, if any.
    #[inline]
    pub fn extra_data_mut(&mut self) -> &mut H {
        &mut self.extra_data
    }

    #[inline]
    pub fn get_unchecked(&self, index: usize) -> &T {
        self.array.get_unchecked(index)
    }

    #[inline]
    pub fn set_unchecked(&mut self, index: usize, value: T) {
        self.array.set_unchecked(index, value);
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
        visitor.visit_pointer(&mut self.shape);
    }
}

/// An instance of a BsVec with a specific element type. This has its own object shape
/// identifying the full BsVec<T>.
pub trait VecInstance:
    IsHeapItem
    + WithHeapItemKind
    + std::ops::Deref<Target = BsVec<Self::T, Self::H>>
    + std::ops::DerefMut<Target = BsVec<Self::T, Self::H>>
{
    type T: Clone + Copy;
    type H;

    const MIN_CAPACITY: usize = BsVec::<Self::T, Self::H>::MIN_CAPACITY;

    fn new(cx: Context, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        Ok(BsVec::<Self::T, Self::H>::new(cx, Self::KIND, capacity)?.cast())
    }

    fn new_initial(cx: Context) -> AllocResult<HeapPtr<Self>> {
        Ok(BsVec::<Self::T, Self::H>::new(cx, Self::KIND, Self::MIN_CAPACITY)?.cast())
    }

    fn calculate_size_in_bytes(capacity: usize) -> usize {
        BsVec::<Self::T, Self::H>::calculate_size_in_bytes(capacity)
    }
}

#[macro_export]
macro_rules! impl_vec_instance {
    ($vec_type:ident, $element_type:ty) => {
        $crate::impl_vec_instance!($vec_type, $element_type, ());
    };
    ($vec_type:ident, $element_type:ty, $extra_data_type:ty) => {
        #[repr(transparent)]
        pub struct $vec_type($crate::runtime::collections::BsVec<$element_type, $extra_data_type>);

        impl $crate::runtime::collections::VecInstance for $vec_type {
            type T = $element_type;
            type H = $extra_data_type;
        }

        impl std::ops::Deref for $vec_type {
            type Target = $crate::runtime::collections::BsVec<$element_type, $extra_data_type>;

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
impl<T, H> IsHeapItem for BsVec<T, H> {}

impl_vec_instance!(ValueVec, Value);

impl HeapItem for ValueVec {
    fn byte_size(vec: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(vec.capacity())
    }

    fn visit_pointers(mut vec: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        vec.visit_vec_pointers(visitor);

        for value in vec.as_mut_slice() {
            visitor.visit_value(value);
        }
    }
}
