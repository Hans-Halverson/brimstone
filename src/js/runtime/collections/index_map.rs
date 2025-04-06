use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    mem::size_of,
    slice,
};

use crate::{
    field_offset,
    runtime::{
        gc::{HeapObject, HeapVisitor},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        Context, Handle, HeapPtr,
    },
    set_uninit,
};

use super::InlineArray;

/// Generic flat IndexMap implementation that tracks insertion order.
///
/// Based on the hash table described by Jason Orendorff at
/// https://wiki.mozilla.org/User:Jorend/Deterministic_hash_tables
#[repr(C)]
pub struct BsIndexMap<K, V> {
    descriptor: HeapPtr<ObjectDescriptor>,
    // Whether this is a tombstone object. Tombstone objects point to the new map that this map was
    // moved to during a resize. Tombstone objects are used to update iterators that point to the
    // tombstone.
    is_tombstone: bool,
    // Number of entries currently inserted, excluding deleted entries
    num_occupied: usize,
    // Number of deleted entries
    num_deleted: usize,
    // Inline array of indices into the entries array. The special index value EMPTY_INDEX signals
    // an empty index slot.
    //
    // Total capacity must be a power of 2.
    //
    // If this is a tombstone object then a pointer to the new map is stored at the first index.
    // This requires that the map is non-empty, so we must not create any empty maps.
    indices: InlineArray<usize>,
    // Array of entries in insertion order. Must have the same capacity as the indices array.
    _entries: [Entry<K, V>; 1],
}

const EMPTY_INDEX: usize = usize::MAX;

/// Entries hold the entry index of the next entry in the chain. The last entry in the chain is
/// marked by EMPTY_INDEX.
enum Entry<K, V> {
    Deleted { chain: usize },
    Occupied(OccupiedEntry<K, V>),
}

struct OccupiedEntry<K, V> {
    key: K,
    value: V,
    chain: usize,
}

const INDICES_BYTE_OFFSET: usize = field_offset!(BsIndexMap<String, String>, indices);

impl<K: Eq + Hash + Clone, V: Clone> BsIndexMap<K, V> {
    pub const MIN_CAPACITY: usize = 4;

    // Public interface

    pub fn new(cx: Context, kind: ObjectKind, capacity: usize) -> HeapPtr<Self> {
        // Size of a dense array with the given capacity, in bytes
        let size = Self::calculate_size_in_bytes(capacity);
        let mut hash_map = cx.alloc_uninit_with_size::<BsIndexMap<K, V>>(size);

        set_uninit!(hash_map.descriptor, cx.base_descriptors.get(kind));
        set_uninit!(hash_map.is_tombstone, false);
        set_uninit!(hash_map.num_occupied, 0);
        set_uninit!(hash_map.num_deleted, 0);

        // Initialize entries array to empty
        hash_map.indices.init_with(capacity, EMPTY_INDEX);

        // Leave entries uninitialized

        hash_map
    }

    /// Create a new map whose entries are copied from the provided map.
    pub fn new_from_map(cx: Context, map: Handle<Self>) -> HeapPtr<Self> {
        let size = Self::calculate_size_in_bytes(map.capacity());

        let copied_map = cx.alloc_uninit_with_size::<Self>(size);
        unsafe { std::ptr::copy(map.as_ptr(), copied_map.as_ptr(), size) };

        copied_map
    }

    /// Total number of entries that have been inserted, including those that have been deleted.
    #[inline]
    pub fn num_entries_used(&self) -> usize {
        self.num_occupied + self.num_deleted
    }

    #[inline]
    /// Total number of entries that have been inserted, excluding those that have been deleted.
    pub fn num_entries_occupied(&self) -> usize {
        self.num_occupied
    }

    /// Total number of entries that the IndexMap can hold.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.indices.len()
    }

    /// Whether this is a tombstone object that points to the new map that this map was moved to
    /// during a resize.
    #[inline]
    pub fn is_tombstone(&self) -> bool {
        self.is_tombstone
    }

    #[inline]
    pub fn calculate_size_in_bytes(capacity: usize) -> usize {
        INDICES_BYTE_OFFSET + Self::indices_byte_size(capacity) + Self::entries_byte_size(capacity)
    }

    /// Returns whether this map contains the given key.
    pub fn contains_key(&self, key: &K) -> bool {
        self.find_index(key).is_some()
    }

    /// Returns the value associated with the given key in this map, or None is the key is not present.
    pub fn get(&self, key: &K) -> Option<&V> {
        self.find_index(key)
            .map(|entry_index| &self.get_entry_unchecked(entry_index).as_occupied().value)
    }

    /// Remove an entry from this map if the key is present. Return whether an entry was removed.
    ///
    /// The deleted entry remains and becomes unusable.
    pub fn remove(&mut self, key: &K) -> bool {
        match self.find_index(key) {
            None => false,
            Some(entry_index) => {
                let entry = self.get_entry_unchecked_mut(entry_index);
                *entry = Entry::Deleted { chain: entry.as_occupied_mut().chain };

                self.num_occupied -= 1;
                self.num_deleted += 1;

                true
            }
        }
    }

    /// Remove all entries from this map.
    pub fn clear(&mut self) {
        // Clear indices array
        self.indices.init_with(self.capacity(), EMPTY_INDEX);

        // Entries array does not need to be changed, updating number of entires is sufficient
        self.num_occupied = 0;
        self.num_deleted = 0;
    }

    /// Return iterator through the entries of the map. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn iter_gc_unsafe(&self) -> GcUnsafeEntriesIter<K, V> {
        // Only iterate through a slice of the entries that have been used
        GcUnsafeEntriesIter(self.entries_as_slice()[..self.num_entries_used()].iter())
    }

    /// Return iterator through the entries of the map. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn iter_mut_gc_unsafe(&mut self) -> GcUnsafeEntriesIterMut<K, V> {
        // Only iterate through a slice of the entries that have been used
        let num_entries_used = self.num_entries_used();
        GcUnsafeEntriesIterMut(self.entries_as_slice_mut()[..num_entries_used].iter_mut())
    }

    /// Return iterator through the keys of the map. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn keys_gc_unsafe(&self) -> GcUnsafeKeysIter<K, V> {
        GcUnsafeKeysIter(self.entries_as_slice()[..self.num_entries_used()].iter())
    }

    /// Return iterator through the keys of the map. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn keys_mut_gc_unsafe(&mut self) -> GcUnsafeKeysIterMut<K, V> {
        let num_entries_used = self.num_entries_used();
        GcUnsafeKeysIterMut(self.entries_as_slice_mut()[..num_entries_used].iter_mut())
    }

    // Indices accessors

    #[inline]
    fn indices_byte_size(capacity: usize) -> usize {
        InlineArray::<usize>::calculate_size_in_bytes(capacity)
    }

    #[inline]
    fn get_index_unchecked(&self, hash_index: usize) -> usize {
        *self.indices.get_unchecked(hash_index)
    }

    #[inline]
    fn get_index_unchecked_mut(&mut self, hash_index: usize) -> &mut usize {
        self.indices.get_unchecked_mut(hash_index)
    }

    #[inline]
    fn set_index_unchecked(&mut self, hash_index: usize, entry_index: usize) {
        *self.get_index_unchecked_mut(hash_index) = entry_index;
    }

    #[inline]
    fn get_new_map_ptr(&self) -> HeapPtr<Self> {
        HeapPtr::from_ptr(self.get_index_unchecked(0) as *mut Self)
    }

    #[inline]
    fn get_new_map_ptr_mut(&mut self) -> &mut HeapPtr<Self> {
        unsafe {
            std::mem::transmute::<&mut usize, &mut HeapPtr<Self>>(self.get_index_unchecked_mut(0))
        }
    }

    #[inline]
    fn set_new_map_ptr(&mut self, new_map_ptr: HeapPtr<Self>) {
        *self.get_index_unchecked_mut(0) = new_map_ptr.as_ptr() as usize;
    }

    // Entries accessors

    #[inline]
    fn entries_as_ptr(&self) -> *const Entry<K, V> {
        let entries_byte_offset = Self::entries_byte_offset(self.capacity());
        unsafe {
            (self as *const _ as *const u8)
                .add(entries_byte_offset)
                .cast()
        }
    }

    #[inline]
    fn entries_byte_offset(capacity: usize) -> usize {
        INDICES_BYTE_OFFSET + Self::indices_byte_size(capacity)
    }

    #[inline]
    fn entries_byte_size(capacity: usize) -> usize {
        size_of::<Entry<K, V>>() * capacity
    }

    #[inline]
    fn entries_as_slice(&self) -> &[Entry<K, V>] {
        unsafe { std::slice::from_raw_parts(self.entries_as_ptr(), self.capacity()) }
    }

    #[inline]
    fn entries_as_slice_mut(&mut self) -> &mut [Entry<K, V>] {
        unsafe { std::slice::from_raw_parts_mut(self.entries_as_ptr().cast_mut(), self.capacity()) }
    }

    #[inline]
    fn get_entry_unchecked(&self, entry_index: usize) -> &Entry<K, V> {
        unsafe { self.entries_as_slice().get_unchecked(entry_index) }
    }

    #[inline]
    fn get_entry_unchecked_mut(&mut self, entry_index: usize) -> &mut Entry<K, V> {
        unsafe { self.entries_as_slice_mut().get_unchecked_mut(entry_index) }
    }

    #[inline]
    fn set_entry_unchecked(&mut self, entry_index: usize, entry: Entry<K, V>) {
        unsafe {
            *self.entries_as_slice_mut().get_unchecked_mut(entry_index) = entry;
        }
    }

    // Hashing helpers

    #[inline]
    fn mask(&self) -> usize {
        self.capacity() - 1
    }

    /// Map from a hash code to an index in the indices array.
    #[inline]
    fn hash_index(&self, hash_code: usize) -> usize {
        hash_code & self.mask()
    }

    #[inline]
    fn key_hash_code(key: &K) -> usize {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        hasher.finish() as usize
    }

    /// Return the index of the key if the key is present in the map, otherwise return None.
    /// Returns None for empty and deleted entries.
    #[inline]
    fn find_index(&self, key: &K) -> Option<usize> {
        // Short circuit on empty maps
        if self.capacity() == 0 {
            return None;
        }

        let hash_code = Self::key_hash_code(key);
        let hash_index = self.hash_index(hash_code);
        let mut current_entry_index = self.get_index_unchecked(hash_index);

        // Follow the chain until we hit the key or an empty index
        loop {
            if current_entry_index == EMPTY_INDEX {
                return None;
            }

            match self.get_entry_unchecked(current_entry_index) {
                Entry::Occupied(entry) => {
                    if entry.key == *key {
                        return Some(current_entry_index);
                    }

                    current_entry_index = entry.chain;
                }
                Entry::Deleted { chain } => {
                    current_entry_index = *chain;
                }
            }
        }
    }

    /// Insert the key value pair into this map. If there is already a value associated with the key
    /// then overwrite the value. Return whether the key was already present in the map.
    ///
    /// Assumes there is room to insert a key value pair, silently fails to insert if map is full.
    #[inline]
    pub fn insert_without_growing(&mut self, key: K, value: V) -> bool {
        let hash_code = Self::key_hash_code(&key);
        let hash_index = self.hash_index(hash_code);
        let mut current_entry_index = self.get_index_unchecked(hash_index);
        let next_empty_entry_index = self.num_entries_used();

        if current_entry_index == EMPTY_INDEX {
            // Very first entry with this hash index. Write index of the new entry as it will be the
            // start of the chain.
            self.set_index_unchecked(hash_index, next_empty_entry_index);
        } else {
            // Follow the chain until we hit the key or an empty index
            loop {
                match self.get_entry_unchecked_mut(current_entry_index) {
                    Entry::Deleted { chain } => {
                        // Found the end of the chain, so set old end to new entry
                        let next_entry_index = *chain;
                        if next_entry_index == EMPTY_INDEX {
                            *chain = next_empty_entry_index;
                            break;
                        }

                        current_entry_index = next_entry_index;
                    }
                    Entry::Occupied(entry) => {
                        // Key is already present, so overwrite it and don't change order
                        if entry.key == key {
                            entry.value = value;
                            return true;
                        }

                        // Found the end of the chain, so set old end to new entry
                        let next_entry_index = entry.chain;
                        if next_entry_index == EMPTY_INDEX {
                            entry.chain = next_empty_entry_index;
                            break;
                        }

                        current_entry_index = next_entry_index;
                    }
                }
            }
        }

        // Add new entry to end of entries array
        self.set_entry_unchecked(
            next_empty_entry_index,
            Entry::Occupied(OccupiedEntry { key, value, chain: EMPTY_INDEX }),
        );
        self.num_occupied += 1;

        // Did not overwrite an entry
        false
    }

    /// Given a tombstone and a next index for that tombstone, return the new map and the new next
    /// index.
    pub fn fix_iterator_for_resized_map(
        tombstone_map: HeapPtr<Self>,
        next_entry_index: &mut usize,
    ) -> HeapPtr<Self> {
        // Count the number of occupied entries in the tombstone map up to the next index. This is
        // the next index in the new map, since only occupied entries were kept.
        *next_entry_index = tombstone_map
            .entries_as_slice()
            .iter()
            .take(*next_entry_index)
            .filter(|entry| matches!(entry, Entry::Occupied(_)))
            .count();

        // Each tombstone contains a pointer to the new map
        tombstone_map.get_new_map_ptr()
    }
}

impl<K: Eq + Hash + Clone, V: Clone> Handle<BsIndexMap<K, V>> {
    /// Return iterator through the entries of the map. Iterator is GC-safe so it is safe to live
    /// across allocations.
    pub fn iter_gc_safe(&self) -> GcSafeEntriesIter<K, V> {
        GcSafeEntriesIter::new(*self)
    }
}

/// A BsHashMap stored as the field of a heap object. Can create new maps and set the field to a
/// new map.
pub trait BsIndexMapField<K: Eq + Hash + Clone, V: Clone> {
    fn new_map(&self, cx: Context, capacity: usize) -> HeapPtr<BsIndexMap<K, V>>;

    fn get(&self) -> HeapPtr<BsIndexMap<K, V>>;

    fn set(&mut self, map: HeapPtr<BsIndexMap<K, V>>);

    /// Prepare map for insertion of a single entry. This will grow the map and update container to
    /// point to new map if there is no room to insert another entry in the map.
    #[inline]
    fn maybe_grow_for_insertion(&mut self, cx: Context) -> HeapPtr<BsIndexMap<K, V>> {
        let old_map = self.get();
        let num_entries_used = old_map.num_entries_used();
        let capacity = old_map.capacity();

        if num_entries_used < capacity {
            return old_map;
        }

        // Save old map behind handle before allocating
        let mut old_map = old_map.to_handle();

        // Capacity jumps from 0 to min capacity
        let new_capacity;
        if capacity == 0 {
            new_capacity = BsIndexMap::<K, V>::MIN_CAPACITY;
        } else if old_map.num_deleted > (capacity / 2) {
            // New capacity stays the same if most elements are deleted
            new_capacity = capacity;
        } else {
            // Otherwise double capacity of map
            new_capacity = capacity * 2;
        }

        let mut new_map = self.new_map(cx, new_capacity);

        // Update parent reference from old child to new child map
        self.set(new_map);

        // Copy all values into new map. We have already guaranteed that there is enough room in map.
        for (key, value) in old_map.iter_gc_unsafe() {
            new_map.insert_without_growing(key, value);
        }

        // Empty maps should not become tombstones since there is nowhere to put the new map pointer
        // debug_assert!(capacity != 0);

        // Mark the old map as a tombstone object that points to the new map
        if capacity != 0 {
            old_map.is_tombstone = true;
            old_map.set_new_map_ptr(new_map);
        }

        new_map
    }
}

impl<K, V> Entry<K, V> {
    fn as_occupied(&self) -> &OccupiedEntry<K, V> {
        if let Entry::Occupied(entry) = self {
            entry
        } else {
            unreachable!()
        }
    }

    fn as_occupied_mut(&mut self) -> &mut OccupiedEntry<K, V> {
        if let Entry::Occupied(entry) = self {
            entry
        } else {
            unreachable!()
        }
    }
}

pub struct GcUnsafeEntriesIter<'a, K, V>(slice::Iter<'a, Entry<K, V>>);

impl<K: Clone, V: Clone> Iterator for GcUnsafeEntriesIter<'_, K, V> {
    type Item = (K, V);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.0.next() {
                // Found an entry
                Some(Entry::Occupied(entry)) => {
                    return Some((entry.key.clone(), entry.value.clone()));
                }
                // Reached the end of the entries slice
                None => return None,
                Some(Entry::Deleted { .. }) => {}
            }
        }
    }
}

pub struct GcUnsafeEntriesIterMut<'a, K, V>(slice::IterMut<'a, Entry<K, V>>);

impl<'a, K: Clone, V: Clone> Iterator for GcUnsafeEntriesIterMut<'a, K, V> {
    type Item = (&'a mut K, &'a mut V);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.0.next() {
                // Found an entry
                Some(Entry::Occupied(entry)) => {
                    return Some((&mut entry.key, &mut entry.value));
                }
                // Reached the end of the entries slice
                None => return None,
                Some(Entry::Deleted { .. }) => {}
            }
        }
    }
}

pub struct GcUnsafeKeysIter<'a, K, V>(slice::Iter<'a, Entry<K, V>>);

impl<K: Clone, V: Clone> Iterator for GcUnsafeKeysIter<'_, K, V> {
    type Item = K;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.0.next() {
                // Found an entry
                Some(Entry::Occupied(entry)) => {
                    return Some(entry.key.clone());
                }
                // Reached the end of the entries array
                None => return None,
                Some(Entry::Deleted { .. }) => {}
            }
        }
    }
}

pub struct GcUnsafeKeysIterMut<'a, K, V>(slice::IterMut<'a, Entry<K, V>>);

impl<'a, K: Clone, V: Clone> Iterator for GcUnsafeKeysIterMut<'a, K, V> {
    type Item = &'a mut K;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.0.next() {
                // Found an entry
                Some(Entry::Occupied(entry)) => {
                    return Some(&mut entry.key);
                }
                // Reached the end of the entries array
                None => return None,
                Some(Entry::Deleted { .. }) => {}
            }
        }
    }
}

#[repr(C)]
pub struct GcSafeEntriesIter<K, V> {
    map: Handle<BsIndexMap<K, V>>,
    // Index of the next entry to check
    next_entry_index: usize,
}

impl<K, V> GcSafeEntriesIter<K, V> {
    #[inline]
    pub fn new(map: Handle<BsIndexMap<K, V>>) -> Self {
        GcSafeEntriesIter { map, next_entry_index: 0 }
    }

    #[inline]
    pub fn from_parts(map: Handle<BsIndexMap<K, V>>, next_entry_index: usize) -> Self {
        GcSafeEntriesIter { map, next_entry_index }
    }

    #[inline]
    pub fn to_parts(&self) -> (Handle<BsIndexMap<K, V>>, usize) {
        (self.map, self.next_entry_index)
    }
}

impl<K: Eq + Hash + Clone, V: Clone> Iterator for GcSafeEntriesIter<K, V> {
    type Item = (K, V);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        // TODO: Handle when map data is moved to a new map e.g. when growing. Must write info for
        // switching iterator to point to new map in place of the old map.

        let mut current_entry_index = self.next_entry_index;

        while current_entry_index < self.map.num_entries_used() {
            match self.map.get_entry_unchecked(current_entry_index) {
                Entry::Deleted { .. } => {
                    current_entry_index += 1;
                }
                Entry::Occupied(entry) => {
                    self.next_entry_index = current_entry_index + 1;
                    return Some((entry.key.clone(), entry.value.clone()));
                }
            }
        }

        None
    }
}

impl<K: Eq + Hash + Clone, V: Clone> HeapObject for HeapPtr<BsIndexMap<K, V>> {
    fn byte_size(&self) -> usize {
        BsIndexMap::<K, V>::calculate_size_in_bytes(self.capacity())
    }

    /// Visit pointers intrinsic to all IndexMaps. Do not visit entries as they could be of any type.
    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        Self::visit_pointers_impl(self, visitor, |_, _| ());
    }
}

impl<K: Eq + Hash + Clone, V: Clone> HeapPtr<BsIndexMap<K, V>> {
    #[inline]
    pub fn visit_pointers_impl<H: HeapVisitor>(
        &mut self,
        visitor: &mut H,
        mut entries_visitor: impl FnMut(&mut Self, &mut H),
    ) {
        visitor.visit_pointer(&mut self.descriptor);

        if self.is_tombstone() {
            // Tombstones contain the new map but entries are not still live - we only need to know
            // if the entries are occupied.
            visitor.visit_pointer(self.get_new_map_ptr_mut());
        } else {
            // Otherwise visit entries since they are still live.
            entries_visitor(self, visitor);
        }
    }
}
