use std::hash::Hash;

use crate::runtime::{
    Context, HeapItemKind, HeapPtr,
    alloc_error::AllocResult,
    collections::{
        BsDefaultHasher, BsHashMap,
        hash_map::{
            BsBuildHasher, GcUnsafeEntryMutIter, GcUnsafeKeysIterMut, maybe_grow_for_insertion,
        },
    },
    gc::{HeapVisitor, IsHeapItem, WithHeapItemKind},
};

/// Generic flat HashSet implementation which is a simple wrapper over a HashMap with unit values.
#[repr(transparent)]
pub struct BsHashSet<T, H = BsDefaultHasher>(InnerMap<T, H>);

type InnerMap<T, H> = BsHashMap<T, (), H, ()>;

impl<T: Eq + Hash + Clone, H: BsBuildHasher> BsHashSet<T, H> {
    pub fn new(cx: Context, kind: HeapItemKind, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        Ok(InnerMap::<T, H>::new(cx, kind, capacity)?.cast())
    }

    pub fn new_initial(cx: Context, kind: HeapItemKind) -> AllocResult<HeapPtr<Self>> {
        Ok(InnerMap::<T, H>::new_initial(cx, kind)?.cast())
    }

    pub fn calculate_size_in_bytes(capacity: usize) -> usize {
        InnerMap::<T, H>::calculate_size_in_bytes(capacity)
    }

    /// Total number of elements in the backing array.
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// Returns whether this set contains the given element.
    pub fn contains(&self, element: &T) -> bool {
        self.0.contains_key(element)
    }

    /// Returns the key equal to the given key that is stored in the set.
    pub fn get(&self, key: &T) -> Option<&T> {
        self.0.get_entry(key).map(|(key, _)| key)
    }

    /// Remove an element from this set if the element is present. Return whether an element was removed.
    pub fn remove(&mut self, element: &T) -> bool {
        self.0.remove(element)
    }

    /// Insert an element into this set. Return whether the element was already present in the set.
    ///
    /// Assumes there is room to insert the element, silently fails to insert if set is full.
    pub fn insert_without_growing(&mut self, element: T) -> bool {
        self.0.insert_without_growing(element, ())
    }

    /// Return an iterator over the elements of the set. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn iter_mut_gc_unsafe(&mut self) -> GcUnsafeKeysIterMut<'_, T, ()> {
        self.0.keys_mut_gc_unsafe()
    }

    /// Return an iterator over the entries of the set, where each entry allows modifying the
    /// element or removing the entry in place. Iterator is not GC-safe, so make sure there are
    /// no allocations between construction and use.
    pub fn iter_entries_mut_gc_unsafe(&mut self) -> GcUnsafeEntryMutIter<'_, T, ()> {
        self.0.iter_entries_mut_gc_unsafe()
    }

    /// Visit pointers intrinsic to all HashSets. Do not visit entries as they could be of any type.
    pub fn visit_set_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.0.visit_map_pointers(visitor)
    }
}

/// An instance of a BsHashSet with a specific element type. This has its own object
/// shape identifying the full BsHashSet<T>.
pub trait HashSetInstance:
    IsHeapItem
    + WithHeapItemKind
    + std::ops::Deref<Target = BsHashSet<Self::T, Self::H>>
    + std::ops::DerefMut<Target = BsHashSet<Self::T, Self::H>>
{
    type T: Eq + std::hash::Hash + Clone;
    type H: BsBuildHasher;

    fn new(cx: Context, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        Ok(BsHashSet::<Self::T, Self::H>::new(cx, Self::KIND, capacity)?.cast())
    }

    fn new_initial(cx: Context) -> AllocResult<HeapPtr<Self>> {
        Ok(BsHashSet::<Self::T, Self::H>::new_initial(cx, Self::KIND)?.cast())
    }

    fn calculate_size_in_bytes(capacity: usize) -> usize {
        BsHashSet::<Self::T, Self::H>::calculate_size_in_bytes(capacity)
    }
}

#[macro_export]
macro_rules! impl_hash_set_instance {
    ($set_type:ident, $element_type:ty) => {
        $crate::impl_hash_set_instance!(
            $set_type,
            $element_type,
            $crate::runtime::collections::BsDefaultHasher
        );
    };
    ($set_type:ident, $element_type:ty, $hasher_type:ty) => {
        #[repr(transparent)]
        pub struct $set_type($crate::runtime::collections::BsHashSet<$element_type, $hasher_type>);

        impl $crate::runtime::collections::HashSetInstance for $set_type {
            type T = $element_type;
            type H = $hasher_type;
        }

        impl std::ops::Deref for $set_type {
            type Target = $crate::runtime::collections::BsHashSet<$element_type, $hasher_type>;

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

/// A BsHashSet stored as the field of a heap item. Can create new set and set the field to a
/// new set.
pub trait BsHashSetField<I: HashSetInstance>: Clone {
    fn get(&self, cx: Context) -> HeapPtr<I>;

    /// Create a new set with the given capacity and set the field to point to it.
    /// Return the new set.
    fn set_new(&mut self, cx: Context, capacity: usize) -> AllocResult<HeapPtr<I>>;

    /// Prepare set for insertion of a single element. This will grow the set and update container
    /// to point to new set if there is no room to insert another entry in the set.
    #[inline]
    fn maybe_grow_for_insertion(&mut self, cx: Context) -> AllocResult<HeapPtr<I>> {
        Ok(maybe_grow_for_insertion(
            cx,
            self.get(cx).cast::<InnerMap<I::T, I::H>>(),
            |cx, capacity| Ok(self.set_new(cx, capacity)?.cast()),
        )?
        .cast())
    }
}

// Only necessary so we get deref for HeapPtrs.
impl<T, H> IsHeapItem for BsHashSet<T, H> {}
