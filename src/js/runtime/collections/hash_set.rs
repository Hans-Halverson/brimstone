use std::hash::Hash;

use crate::js::runtime::{
    gc::IsHeapObject, object_descriptor::ObjectKind, Context, Handle, HeapPtr,
};

use super::{BsHashMap, BsHashMapContainer};

/// Generic flat HashSet implementation which is a simple wrapper over a HashMap with unit values.
#[repr(C)]
pub struct BsHashSet<T>(BsHashMap<T, ()>);

impl<T> IsHeapObject for BsHashSet<T> {}

impl<T: Eq + Hash + Clone> BsHashSet<T> {
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

impl<T: Eq + Hash + Clone> Handle<BsHashSet<T>> {
    /// Insert an element into this set. Return whether the element was already present in the set.
    ///
    /// Insert may grow the set and update container to point to new set if there is no room to
    /// insert another element in the set.
    pub fn insert(
        &mut self,
        cx: &mut Context,
        container: Handle<impl BsHashSetContainer<T>>,
        element: T,
    ) -> bool {
        self.cast::<BsHashMap<T, ()>>()
            .insert(cx, container, element, ())
    }
}

/// A heap object that holds a BsHashMap as a child. Specifies the exact kind of the map, and
/// provides the ability to update the pointer to the map.
pub trait BsHashSetContainer<T: Eq + Hash + Clone>: IsHeapObject {
    const KIND: ObjectKind;

    fn kind() -> ObjectKind {
        Self::KIND
    }

    fn new_set(cx: &mut Context) -> HeapPtr<BsHashSet<T>> {
        BsHashMap::<T, ()>::new(cx, Self::kind(), BsHashMap::<T, ()>::MIN_CAPACITY).cast()
    }

    fn set_set(&mut self, set: HeapPtr<BsHashSet<T>>);
}

impl<T: Eq + Hash + Clone, S: BsHashSetContainer<T>> BsHashMapContainer<T, ()> for S {
    const KIND: ObjectKind = S::KIND;

    fn set_map(&mut self, map: HeapPtr<BsHashMap<T, ()>>) {
        self.set_set(map.cast())
    }
}
