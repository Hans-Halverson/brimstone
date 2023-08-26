use std::hash::Hash;

use crate::js::runtime::{
    gc::{HeapObject, HeapVisitor},
    object_descriptor::ObjectKind,
    Context, HeapPtr,
};

use super::{hash_map::GcUnsafeKeysIterMut, BsHashMap, BsHashMapField};

/// Generic flat HashSet implementation which is a simple wrapper over a HashMap with unit values.
#[repr(C)]
pub struct BsHashSet<T>(BsHashMap<T, ()>);

impl<T: Eq + Hash + Clone> BsHashSet<T> {
    pub const MIN_CAPACITY: usize = BsHashMap::<T, ()>::MIN_CAPACITY;

    pub fn new(cx: Context, kind: ObjectKind, capacity: usize) -> HeapPtr<Self> {
        BsHashMap::<T, ()>::new(cx, kind, capacity).cast()
    }

    pub fn new_initial(cx: Context, kind: ObjectKind) -> HeapPtr<Self> {
        BsHashMap::<T, ()>::new_initial(cx, kind).cast()
    }

    pub fn calculate_size_in_bytes(capacity: usize) -> usize {
        BsHashMap::<T, ()>::calculate_size_in_bytes(capacity)
    }

    /// Number of elements inserted in the map.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Total number of elemets in the backing array.
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// Returns whether this map contains the given element.
    pub fn contains(&self, element: &T) -> bool {
        self.0.contains_key(element)
    }

    /// Returns the key equal to the given key that is stored in the set.
    pub fn get(&self, key: &T) -> Option<&T> {
        self.0.get_entry(key).map(|(key, _)| key)
    }

    /// Remove an element from this map if the element is present. Return whether an element was removed.
    pub fn remove(&mut self, element: &T) -> bool {
        self.0.remove(element)
    }

    /// Insert an element into this set. Return whether the element was already present in the set.
    /// then overwrite the value. Return whether the key was already present in the map.
    ///
    /// Assumes there is room to insert the element, silently fails to insert if set is full.
    pub fn insert_without_growing(&mut self, element: T) -> bool {
        self.0.insert_without_growing(element, ())
    }

    /// Return iterator through the elements of the set. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn iter_mut_gc_unsafe(&mut self) -> GcUnsafeKeysIterMut<T, ()> {
        self.0.keys_mut_gc_unsafe()
    }

    /// Visit pointers intrinsic to all HashSets. Do not visit entries as they could be of any type.
    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.0.visit_pointers(visitor)
    }
}

/// A BsHashSet stored as the field of a heap object. Can create new set and set the field to a
/// new set.
pub trait BsHashSetField<T: Eq + Hash + Clone>: Clone {
    fn new(cx: Context, capacity: usize) -> HeapPtr<BsHashSet<T>>;

    fn get(&self, cx: Context) -> HeapPtr<BsHashSet<T>>;

    fn set(&mut self, cx: Context, set: HeapPtr<BsHashSet<T>>);

    /// Prepare set for insertion of a single element. This will grow the set and update container
    /// to point to new set if there is no room to insert another entry in the set.
    #[inline]
    fn maybe_grow_for_insertion(&mut self, cx: Context) -> HeapPtr<BsHashSet<T>> {
        let mut map_field = HashMapField(self.clone());
        map_field.maybe_grow_for_insertion(cx).cast()
    }
}

/// Simple wrapper for HashSet that allows treating it as a map field.
struct HashMapField<T>(T);

impl<T: Eq + Hash + Clone, S: BsHashSetField<T>> BsHashMapField<T, ()> for HashMapField<S> {
    fn new(&self, cx: Context, capacity: usize) -> HeapPtr<BsHashMap<T, ()>> {
        S::new(cx, capacity).cast()
    }

    fn get(&self, cx: Context) -> HeapPtr<BsHashMap<T, ()>> {
        self.0.get(cx).cast()
    }

    fn set(&mut self, cx: Context, map: HeapPtr<BsHashMap<T, ()>>) {
        self.0.set(cx, map.cast())
    }
}

impl<T: Eq + Hash + Clone> HeapObject for HeapPtr<BsHashSet<T>> {
    fn byte_size(&self) -> usize {
        BsHashSet::<T>::calculate_size_in_bytes(self.capacity())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        BsHashSet::<T>::visit_pointers(self, visitor)
    }
}
