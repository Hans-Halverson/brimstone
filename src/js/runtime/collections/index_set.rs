use std::hash::Hash;

use crate::runtime::{
    Context, Handle, HeapItemKind, HeapPtr,
    alloc_error::AllocResult,
    collections::{
        BsIndexMap,
        index_map::{GcSafeEntriesIter, GcUnsafeKeysIterMut, maybe_grow_for_insertion},
    },
    gc::{HeapVisitor, IsHeapItem, WithHeapItemKind},
};

/// Generic flat IndexSet implementation which is a simple wrapper over an IndexMap with unit values.
#[repr(transparent)]
pub struct BsIndexSet<T>(InnerMap<T>);

type InnerMap<T> = BsIndexMap<T, ()>;

impl<T: Eq + Hash + Clone> BsIndexSet<T> {
    pub const MIN_CAPACITY: usize = BsIndexMap::<T, ()>::MIN_CAPACITY;

    pub fn new(cx: Context, kind: HeapItemKind, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        Ok(InnerMap::<T>::new(cx, kind, capacity)?.cast())
    }

    pub fn calculate_size_in_bytes(capacity: usize) -> usize {
        InnerMap::<T>::calculate_size_in_bytes(capacity)
    }

    /// Create a new set whose entries are copied from the provided set.
    pub fn new_from_set(cx: Context, set: Handle<Self>) -> AllocResult<HeapPtr<Self>> {
        Ok(InnerMap::<T>::new_from_map(cx, set.cast())?.cast())
    }

    /// Number of elements inserted in the set.
    pub fn num_entries_occupied(&self) -> usize {
        self.0.num_entries_occupied()
    }

    /// Total number of elements that the IndexSet can hold.
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// Returns whether this set contains the given element.
    pub fn contains(&self, element: &T) -> bool {
        self.0.contains_key(element)
    }

    /// Remove an element from this set if the element is present. Return whether an element was removed.
    pub fn remove(&mut self, element: &T) -> bool {
        self.0.remove(element)
    }

    /// Remove all elements from this set.
    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// Return an iterator over the elements of the set. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn iter_mut_gc_unsafe(&mut self) -> GcUnsafeKeysIterMut<'_, T, ()> {
        self.0.keys_mut_gc_unsafe()
    }

    /// Insert an element into this set. Return whether the element was already present in the set.
    ///
    /// Assumes there is room to insert the element, silently fails to insert if set is full.
    pub fn insert_without_growing(&mut self, element: T) -> bool {
        self.0.insert_without_growing(element, ())
    }
}

impl<T: Eq + Hash + Clone> Handle<BsIndexSet<T>> {
    /// Return an iterator over the entries of the set. Iterator is GC-safe so it is safe to live
    /// across allocations.
    pub fn iter_gc_safe(&self) -> GcSafeEntriesIter<T, ()> {
        GcSafeEntriesIter::new(self.cast())
    }
}

/// An instance of a BsIndexSet with a specific element type. This has its own object
/// descriptor identifying the full BsIndexSet<T>.
pub trait IndexSetInstance:
    IsHeapItem
    + WithHeapItemKind
    + std::ops::Deref<Target = BsIndexSet<Self::T>>
    + std::ops::DerefMut<Target = BsIndexSet<Self::T>>
{
    type T: Eq + std::hash::Hash + Clone;

    const MIN_CAPACITY: usize = BsIndexSet::<Self::T>::MIN_CAPACITY;

    fn new(cx: Context, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        Ok(BsIndexSet::<Self::T>::new(cx, Self::KIND, capacity)?.cast())
    }

    fn new_from_set(cx: Context, set: Handle<Self>) -> AllocResult<HeapPtr<Self>> {
        Ok(BsIndexSet::<Self::T>::new_from_set(cx, set.cast())?.cast())
    }

    fn calculate_size_in_bytes(capacity: usize) -> usize {
        BsIndexSet::<Self::T>::calculate_size_in_bytes(capacity)
    }

    fn fix_iterator_for_resized_map(
        tombstone_map: HeapPtr<Self>,
        next_entry_index: &mut usize,
    ) -> HeapPtr<Self> {
        InnerMap::<Self::T>::fix_iterator_for_resized_map(tombstone_map.cast(), next_entry_index)
            .cast()
    }

    fn visit_pointers_impl<H: HeapVisitor>(
        map: HeapPtr<Self>,
        visitor: &mut H,
        entries_visitor: impl FnMut(HeapPtr<BsIndexSet<Self::T>>, &mut H),
    ) {
        BsIndexSet::<Self::T>::visit_pointers_impl(map.cast(), visitor, entries_visitor);
    }
}

#[macro_export]
macro_rules! impl_index_set_instance {
    ($set_type:ident, $element_type:ty) => {
        #[repr(transparent)]
        pub struct $set_type($crate::runtime::collections::BsIndexSet<$element_type>);

        impl $crate::runtime::collections::IndexSetInstance for $set_type {
            type T = $element_type;
        }

        impl std::ops::Deref for $set_type {
            type Target = $crate::runtime::collections::BsIndexSet<$element_type>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl std::ops::DerefMut for $set_type {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    };
}

/// A BsIndexSet stored as the field of a heap item. Can create new set and set the field to a
/// new set.
pub trait BsIndexSetField<I: IndexSetInstance>: Clone {
    fn get(&self) -> HeapPtr<I>;

    /// Create a new set with the given capacity and set the field to point to it.
    /// Return the new set.
    fn set_new(&mut self, cx: Context, capacity: usize) -> AllocResult<HeapPtr<I>>;

    /// Prepare set for insertion of a single element. This will grow the set and update container
    /// to point to new set if there is no room to insert another entry in the set.
    #[inline]
    fn maybe_grow_for_insertion(&mut self, cx: Context) -> AllocResult<HeapPtr<I>> {
        Ok(
            maybe_grow_for_insertion(cx, self.get().cast::<InnerMap<I::T>>(), |cx, capacity| {
                Ok(self.set_new(cx, capacity)?.cast())
            })?
            .cast(),
        )
    }
}

impl<T: Eq + Hash + Clone> BsIndexSet<T> {
    #[inline]
    pub fn visit_pointers_impl<H: HeapVisitor>(
        set: HeapPtr<Self>,
        visitor: &mut H,
        mut entries_visitor: impl FnMut(HeapPtr<Self>, &mut H),
    ) {
        BsIndexMap::<T, ()>::visit_pointers_impl(set.cast(), visitor, |set, visitor| {
            entries_visitor(set.cast(), visitor)
        })
    }
}

// Only necessary so we get deref for HeapPtrs.
impl<T> IsHeapItem for BsIndexSet<T> {}
