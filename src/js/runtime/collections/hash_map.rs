use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    slice,
};

use crate::{
    field_offset,
    js::runtime::{
        gc::IsHeapObject,
        object_descriptor::{ObjectDescriptor, ObjectKind},
        Context, HeapPtr,
    },
    set_uninit,
};

use super::InlineArray;

/// Generic flat HashMap implementation using quadratic probing.
#[repr(C)]
pub struct BsHashMap<K, V> {
    descriptor: HeapPtr<ObjectDescriptor>,
    // Number of kv pairs inserted in the map
    len: usize,
    // Inline array of entries which may be empty, occupied, or deleted. Total capacity must be
    // a power of 2.
    entries: InlineArray<Entry<K, V>>,
}

#[derive(Clone)]
#[repr(C)]
enum Entry<K, V> {
    Empty,
    Deleted,
    Occupied(KVPair<K, V>),
}

#[derive(Clone)]
#[repr(C)]
struct KVPair<K, V> {
    key: K,
    value: V,
}

const ENTRIES_BYTE_OFFSET: usize = field_offset!(BsHashMap<String, String>, entries);

impl<K, V> IsHeapObject for BsHashMap<K, V> {}

impl<K: Eq + Hash + Clone, V: Clone> BsHashMap<K, V> {
    pub const MIN_CAPACITY: usize = 4;

    pub fn new(cx: &mut Context, kind: ObjectKind, capacity: usize) -> HeapPtr<Self> {
        // Size of a dense array with the given capacity, in bytes
        let size = Self::calculate_size_in_bytes(capacity);
        let mut hash_map = cx.heap.alloc_uninit_with_size::<BsHashMap<K, V>>(size);

        hash_map.init(cx, kind, capacity);

        hash_map
    }

    pub fn init(&mut self, cx: &mut Context, kind: ObjectKind, capacity: usize) {
        set_uninit!(self.descriptor, cx.base_descriptors.get(kind));
        set_uninit!(self.len, 0);

        // Initialize entries array to empty
        self.entries.init_with(capacity, Entry::Empty);
    }

    pub fn new_initial(cx: &mut Context, kind: ObjectKind) -> HeapPtr<Self> {
        Self::new(cx, kind, Self::MIN_CAPACITY)
    }

    /// Number of kv pairs inserted in the map.
    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    /// Total number of entries in the backing array.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.entries.len()
    }

    #[inline]
    pub fn calculate_size_in_bytes(capacity: usize) -> usize {
        ENTRIES_BYTE_OFFSET + InlineArray::<Entry<K, V>>::calculate_size_in_bytes(capacity)
    }

    /// Return the minimum capacity needed to fit the given number of elements.
    #[inline]
    pub fn min_capacity_needed(num_elements: usize) -> usize {
        let capacity = num_elements.next_power_of_two();
        if num_elements > (capacity / 2) {
            capacity * 2
        } else {
            capacity
        }
    }

    /// Returns whether this map contains the given key.
    pub fn contains_key(&self, key: &K) -> bool {
        self.find_index(key).is_some()
    }

    /// Returns the value associated with the given key in this map, or None is the key is not present.
    pub fn get(&self, key: &K) -> Option<&V> {
        self.find_index(key)
            .map(|index| &self.entries.get_unchecked(index).as_occupied().value)
    }

    /// Returns the value associated with the given key in this map, or None is the key is not present.
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.find_index(key).map(move |index| {
            &mut self
                .entries
                .get_unchecked_mut(index)
                .as_occupied_mut()
                .value
        })
    }

    /// Remove an entry from this map if the key is present. Return whether an entry was removed.
    pub fn remove(&mut self, key: &K) -> bool {
        match self.find_index(key) {
            None => false,
            Some(index) => {
                self.entries.set_unchecked(index, Entry::Deleted);
                self.len -= 1;
                true
            }
        }
    }

    /// Return iterator through the entries of the map. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn iter_gc_unsafe(&self) -> GcUnsafeEntriesIter<K, V> {
        GcUnsafeEntriesIter(self.entries.as_slice().iter())
    }

    /// Return iterator through the keys of the map. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn keys_gc_unsafe(&self) -> GcUnsafeKeysIter<K, V> {
        GcUnsafeKeysIter(self.entries.as_slice().iter())
    }

    #[inline]
    fn mask(&self) -> usize {
        self.capacity() - 1
    }

    #[inline]
    fn initial_probe_index(&self, key_hash_code: usize) -> usize {
        key_hash_code & self.mask()
    }

    #[inline]
    fn next_probe_index(&self, prev_probe_index: usize, probe_stride: usize) -> usize {
        (prev_probe_index + probe_stride) & self.mask()
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
        let hash_code = Self::key_hash_code(key);
        let mut probe_index = self.initial_probe_index(hash_code);

        // Quadratic probing - probe stride increases by 1 each iteration
        for probe_stride in 1..=self.capacity() {
            match self.entries.get_unchecked(probe_index) {
                // If we encounter an empty entry we will not find the key
                Entry::Empty => return None,
                // Continue probing past deleted entries
                Entry::Deleted => {}
                Entry::Occupied(kv_pair) => {
                    if kv_pair.key == *key {
                        return Some(probe_index);
                    }

                    // Continue probing if key doesn't match
                }
            };

            probe_index = self.next_probe_index(probe_index, probe_stride);
        }

        None
    }

    /// Insert a key value pair into the map if thre is room. Silently fails to insert if map is
    /// already full.
    #[inline]
    pub fn insert_without_growing(&mut self, key: K, value: V) -> bool {
        let hash_code = Self::key_hash_code(&key);
        let mut probe_index = self.initial_probe_index(hash_code);

        // Quadratic probing - probe stride increases by 1 each iteration
        for probe_stride in 1..=self.capacity() {
            match self.entries.get_unchecked_mut(probe_index) {
                // First empty or deleted entry encountered is filled with the new kv pair
                Entry::Empty | Entry::Deleted => {
                    self.entries
                        .set_unchecked(probe_index, Entry::Occupied(KVPair { key, value }));
                    self.len += 1;

                    return false;
                }
                Entry::Occupied(kv_pair) => {
                    // Overwrite previous value if key is already present
                    if kv_pair.key == key {
                        kv_pair.value = value;
                        return true;
                    }

                    // Continue probing if key doesn't match
                }
            };

            probe_index = self.next_probe_index(probe_index, probe_stride);
        }

        false
    }
}

/// A BsHashMap stored as the field of a heap object. Can create new maps and set the field to a
/// new map.
pub trait BsHashMapField<K: Eq + Hash + Clone, V: Clone> {
    fn new(&self, cx: &mut Context, capacity: usize) -> HeapPtr<BsHashMap<K, V>>;

    fn get(&self) -> HeapPtr<BsHashMap<K, V>>;

    fn set(&mut self, map: HeapPtr<BsHashMap<K, V>>);

    /// Insert the key value pair into this map. If there is already a value associated with the key
    /// then overwrite the value. Return whether the key was already present in the map.
    ///
    /// Insert may grow the map and update container to point to new map if there is no room to
    /// insert another entry in the map.
    fn insert(&mut self, cx: &mut Context, key: K, value: V) -> bool {
        let mut map = self.maybe_grow_for_insertion(cx);
        map.insert_without_growing(key, value)
    }

    #[inline]
    fn maybe_grow_for_insertion(&mut self, cx: &mut Context) -> HeapPtr<BsHashMap<K, V>> {
        let old_map = self.get();

        // Keep at least half of the entries empty, otherwise grow
        let new_length = old_map.len() + 1;
        let capacity = old_map.capacity();
        if new_length <= capacity / 2 {
            return old_map;
        }

        // Save old map behind handle before allocating
        let old_map = old_map.to_handle();

        // Double size leaving map 1/4 full after growing
        let new_capacity = capacity * 2;
        let mut new_map = self.new(cx, new_capacity);

        // Update parent reference from old child to new child map
        self.set(new_map);

        // Copy all values into new map. We have already guaranteed that there is enough room in map.
        for (key, value) in old_map.iter_gc_unsafe() {
            new_map.insert_without_growing(key, value);
        }

        new_map
    }
}

impl<K, V> Entry<K, V> {
    fn as_occupied(&self) -> &KVPair<K, V> {
        if let Entry::Occupied(kv_pair) = self {
            kv_pair
        } else {
            unreachable!()
        }
    }

    fn as_occupied_mut(&mut self) -> &mut KVPair<K, V> {
        if let Entry::Occupied(kv_pair) = self {
            kv_pair
        } else {
            unreachable!()
        }
    }
}

pub struct GcUnsafeEntriesIter<'a, K, V>(slice::Iter<'a, Entry<K, V>>);

impl<'a, K: Clone, V: Clone> Iterator for GcUnsafeEntriesIter<'a, K, V> {
    type Item = (K, V);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.0.next() {
                // Found an entry
                Some(Entry::Occupied(entry)) => {
                    return Some((entry.key.clone(), entry.value.clone()));
                }
                // Reached the end of the entries array
                None => return None,
                Some(_) => {}
            }
        }
    }
}

pub struct GcUnsafeKeysIter<'a, K, V>(slice::Iter<'a, Entry<K, V>>);

impl<'a, K: Clone, V: Clone> Iterator for GcUnsafeKeysIter<'a, K, V> {
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
                Some(_) => {}
            }
        }
    }
}
