use std::hash::Hash;

use crate::js::runtime::{
    gc::IsHeapObject, object_descriptor::ObjectKind, Context, Handle, HeapPtr,
};

use super::{
    index_map::{GcSafeEntriesIter, GcUnsafeKeysIter},
    BsIndexMap, BsIndexMapField,
};

/// Generic flat IndexSet implementation which is a simple wrapper over a IndexMap with unit values.
#[repr(C)]
pub struct BsIndexSet<T>(BsIndexMap<T, ()>);

impl<T> IsHeapObject for BsIndexSet<T> {}

impl<T: Eq + Hash + Clone> BsIndexSet<T> {
    pub const MIN_CAPACITY: usize = BsIndexMap::<T, ()>::MIN_CAPACITY;

    pub fn new(cx: &mut Context, kind: ObjectKind, capacity: usize) -> HeapPtr<Self> {
        BsIndexMap::<T, ()>::new(cx, kind, capacity).cast()
    }

    /// Number of elements inserted in the set.
    pub fn num_entries_occupied(&self) -> usize {
        self.0.num_entries_occupied()
    }

    /// Returns whether this set contains the given element.
    pub fn contains(&self, element: &T) -> bool {
        self.0.contains_key(element)
    }

    /// Remove an element from this set if the element is present. Return whether an element was removed.
    pub fn remove(&mut self, element: &T) -> bool {
        self.0.remove(element)
    }

    /// Return iterator through the elements of the set. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn iter_gc_unsafe(&self) -> GcUnsafeKeysIter<T, ()> {
        self.0.keys_gc_unsafe()
    }
}

impl<T: Eq + Hash + Clone> Handle<BsIndexSet<T>> {
    /// Return iterator through the entries of the set. Iterator is GC-safe so it is safe to live
    /// across allocations.
    pub fn iter_gc_safe(&self) -> GcSafeEntriesIter<T, ()> {
        GcSafeEntriesIter::new(self.cast())
    }
}

/// A BsIndexSet stored as the field of a heap object. Can create new set and set the field to a
/// new set.
pub trait BsIndexSetField<T: Eq + Hash + Clone>: Clone {
    fn new(cx: &mut Context, capacity: usize) -> HeapPtr<BsIndexSet<T>>;

    fn get(&self) -> HeapPtr<BsIndexSet<T>>;

    fn set(&mut self, set: HeapPtr<BsIndexSet<T>>);

    /// Insert an element into this set. Return whether the element was already present in the set.
    ///
    /// Insert may grow the set and update container to point to new set if there is no room to
    /// insert another element in the set.
    fn insert(&mut self, cx: &mut Context, element: T) -> bool {
        let mut map_field = IndexMapField(self.clone());
        map_field.insert(cx, element, ())
    }
}

/// Simple wrapper for IndexSet that allows treating it as a map field.
struct IndexMapField<T>(T);

impl<T: Eq + Hash + Clone, S: BsIndexSetField<T>> BsIndexMapField<T, ()> for IndexMapField<S> {
    fn new(&self, cx: &mut Context, capacity: usize) -> HeapPtr<BsIndexMap<T, ()>> {
        S::new(cx, capacity).cast()
    }

    fn get(&self) -> HeapPtr<BsIndexMap<T, ()>> {
        self.0.get().cast()
    }

    fn set(&mut self, map: HeapPtr<BsIndexMap<T, ()>>) {
        self.0.set(map.cast())
    }
}
