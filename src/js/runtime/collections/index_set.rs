use std::hash::Hash;

use crate::runtime::{
    gc::{HeapItem, HeapVisitor},
    heap_item_descriptor::HeapItemKind,
    Context, Handle, HeapPtr,
};

use super::{
    index_map::{GcSafeEntriesIter, GcUnsafeKeysIterMut},
    BsIndexMap, BsIndexMapField,
};

/// Generic flat IndexSet implementation which is a simple wrapper over a IndexMap with unit values.
#[repr(C)]
pub struct BsIndexSet<T>(BsIndexMap<T, ()>);

impl<T: Eq + Hash + Clone> BsIndexSet<T> {
    pub const MIN_CAPACITY: usize = BsIndexMap::<T, ()>::MIN_CAPACITY;

    pub fn new(cx: Context, kind: HeapItemKind, capacity: usize) -> HeapPtr<Self> {
        BsIndexMap::<T, ()>::new(cx, kind, capacity).cast()
    }

    pub fn calculate_size_in_bytes(capacity: usize) -> usize {
        BsIndexMap::<T, ()>::calculate_size_in_bytes(capacity)
    }

    /// Create a new set whose entries are copied from the provided set.
    pub fn new_from_set(cx: Context, set: Handle<Self>) -> HeapPtr<Self> {
        BsIndexMap::<T, ()>::new_from_map(cx, set.cast()).cast()
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

    /// Return iterator through the elements of the set. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn iter_mut_gc_unsafe(&mut self) -> GcUnsafeKeysIterMut<'_, T, ()> {
        self.0.keys_mut_gc_unsafe()
    }

    /// Insert an element into this set. Return whether the element was already present in the set.
    /// then overwrite the value. Return whether the key was already present in the map.
    ///
    /// Assumes there is room to insert the element, silently fails to insert if set is full.
    pub fn insert_without_growing(&mut self, element: T) -> bool {
        self.0.insert_without_growing(element, ())
    }
}

impl<T: Eq + Hash + Clone> Handle<BsIndexSet<T>> {
    /// Return iterator through the entries of the set. Iterator is GC-safe so it is safe to live
    /// across allocations.
    pub fn iter_gc_safe(&self) -> GcSafeEntriesIter<T, ()> {
        GcSafeEntriesIter::new(self.cast())
    }
}

/// A BsIndexSet stored as the field of a heap item. Can create new set and set the field to a
/// new set.
pub trait BsIndexSetField<T: Eq + Hash + Clone>: Clone {
    fn new(cx: Context, capacity: usize) -> HeapPtr<BsIndexSet<T>>;

    fn get(&self) -> HeapPtr<BsIndexSet<T>>;

    fn set(&mut self, set: HeapPtr<BsIndexSet<T>>);

    /// Prepare set for insertion of a single element. This will grow the set and update container
    /// to point to new set if there is no room to insert another entry in the set.
    #[inline]
    fn maybe_grow_for_insertion(&mut self, cx: Context) -> HeapPtr<BsIndexSet<T>> {
        let mut map_field = IndexMapField(self.clone());
        map_field.maybe_grow_for_insertion(cx).cast()
    }
}

/// Simple wrapper for IndexSet that allows treating it as a map field.
struct IndexMapField<T>(T);

impl<T: Eq + Hash + Clone, S: BsIndexSetField<T>> BsIndexMapField<T, ()> for IndexMapField<S> {
    fn new_map(&self, cx: Context, capacity: usize) -> HeapPtr<BsIndexMap<T, ()>> {
        S::new(cx, capacity).cast()
    }

    fn get(&self) -> HeapPtr<BsIndexMap<T, ()>> {
        self.0.get().cast()
    }

    fn set(&mut self, map: HeapPtr<BsIndexMap<T, ()>>) {
        self.0.set(map.cast())
    }
}

impl<T: Eq + Hash + Clone> HeapItem for HeapPtr<BsIndexSet<T>> {
    fn byte_size(&self) -> usize {
        BsIndexSet::<T>::calculate_size_in_bytes(self.capacity())
    }

    /// Visit pointers intrinsic to all IndexSets. Do not visit entries as they could be of any type.
    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast_mut::<BsIndexMap<T, ()>>()
            .visit_pointers_impl(visitor, |_, _| ())
    }
}

impl<T: Eq + Hash + Clone> HeapPtr<BsIndexSet<T>> {
    #[inline]
    pub fn visit_pointers_impl<H: HeapVisitor>(
        &mut self,
        visitor: &mut H,
        mut entries_visitor: impl FnMut(&mut Self, &mut H),
    ) {
        self.cast_mut::<BsIndexMap<T, ()>>()
            .visit_pointers_impl(visitor, |map, visitor| entries_visitor(&mut map.cast(), visitor))
    }
}
