use std::hash::Hash;

use crate::js::runtime::{gc::IsHeapObject, object_descriptor::ObjectKind, Context, HeapPtr};

use super::{BsHashMap, BsHashMapField};

/// Generic flat HashSet implementation which is a simple wrapper over a HashMap with unit values.
#[repr(C)]
pub struct BsHashSet<T>(BsHashMap<T, ()>);

impl<T> IsHeapObject for BsHashSet<T> {}

impl<T: Eq + Hash + Clone> BsHashSet<T> {
    pub const MIN_CAPACITY: usize = BsHashMap::<T, ()>::MIN_CAPACITY;

    pub fn new(cx: &mut Context, kind: ObjectKind, capacity: usize) -> HeapPtr<Self> {
        BsHashMap::<T, ()>::new(cx, kind, capacity).cast()
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

    /// Remove an element from this map if the element is present. Return whether an element was removed.
    pub fn remove(&mut self, element: &T) -> bool {
        self.0.remove(element)
    }
}

/// A BsHashSet stored as the field of a heap object. Can create new set and set the field to a
/// new set.
pub trait BsHashSetField<T: Eq + Hash + Clone>: Clone {
    fn new(cx: &mut Context, capacity: usize) -> HeapPtr<BsHashSet<T>>;

    fn get(&self) -> HeapPtr<BsHashSet<T>>;

    fn set(&mut self, set: HeapPtr<BsHashSet<T>>);

    /// Insert an element into this set. Return whether the element was already present in the set.
    ///
    /// Insert may grow the set and update container to point to new set if there is no room to
    /// insert another element in the set.
    fn insert(&mut self, cx: &mut Context, element: T) -> bool {
        let mut map_field = HashMapField(self.clone());
        map_field.insert(cx, element, ())
    }
}

/// Simple wrapper for HashSet that allows treating it as a map field.
struct HashMapField<T>(T);

impl<T: Eq + Hash + Clone, S: BsHashSetField<T>> BsHashMapField<T, ()> for HashMapField<S> {
    fn new(cx: &mut Context, capacity: usize) -> HeapPtr<BsHashMap<T, ()>> {
        S::new(cx, capacity).cast()
    }

    fn get(&self) -> HeapPtr<BsHashMap<T, ()>> {
        self.0.get().cast()
    }

    fn set(&mut self, map: HeapPtr<BsHashMap<T, ()>>) {
        self.0.set(map.cast())
    }
}
