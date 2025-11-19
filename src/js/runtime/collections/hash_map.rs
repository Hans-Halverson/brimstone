use std::{
    borrow::Borrow,
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    slice,
};

use crate::{
    field_offset,
    runtime::{
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
        Context, HeapPtr,
    },
    set_uninit,
};

use super::InlineArray;

/// Generic flat HashMap implementation using quadratic probing.
#[repr(C)]
pub struct BsHashMap<K, V> {
    descriptor: HeapPtr<HeapItemDescriptor>,
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

impl<K: Eq + Hash + Clone, V: Clone> BsHashMap<K, V> {
    pub const MIN_CAPACITY: usize = 4;

    pub fn new(cx: Context, kind: HeapItemKind, capacity: usize) -> HeapPtr<Self> {
        // Size of a dense array with the given capacity, in bytes
        let size = Self::calculate_size_in_bytes(capacity);
        let mut hash_map = cx.alloc_uninit_with_size::<BsHashMap<K, V>>(size);

        hash_map.init(cx, kind, capacity);

        hash_map
    }

    pub fn init(&mut self, cx: Context, kind: HeapItemKind, capacity: usize) {
        set_uninit!(self.descriptor, cx.base_descriptors.get(kind));
        set_uninit!(self.len, 0);

        // Initialize entries array to empty
        self.entries.init_with(capacity, Entry::Empty);
    }

    pub fn new_initial(cx: Context, kind: HeapItemKind) -> HeapPtr<Self> {
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
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        self.find_index(key)
            .map(|index| &self.entries.get_unchecked(index).as_occupied().value)
    }

    /// Returns the key value pair associated with the given key in this map, or None is the key is not present.
    pub fn get_entry(&self, key: &K) -> Option<(&K, &V)> {
        self.find_index(key).map(|index| {
            let entry = &self.entries.get_unchecked(index).as_occupied();
            (&entry.key, &entry.value)
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
    pub fn iter_gc_unsafe(&self) -> GcUnsafeEntriesIter<'_, K, V> {
        GcUnsafeEntriesIter(self.entries.as_slice().iter())
    }

    /// Return iterator through the entries of the map. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn iter_mut_gc_unsafe(&mut self) -> GcUnsafeEntriesIterMut<'_, K, V> {
        GcUnsafeEntriesIterMut(self.entries.as_mut_slice().iter_mut())
    }

    /// Return iterator through the keys of the map. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn keys_gc_unsafe(&self) -> GcUnsafeKeysIter<'_, K, V> {
        GcUnsafeKeysIter(self.entries.as_slice().iter())
    }

    /// Return iterator through the keys of the map. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn keys_mut_gc_unsafe(&mut self) -> GcUnsafeKeysIterMut<'_, K, V> {
        GcUnsafeKeysIterMut(self.entries.as_mut_slice().iter_mut())
    }

    /// Visit pointers intrinsic to all HashMaps. Do not visit entries as they could be of any type.
    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
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
    fn key_hash_code<Q>(key: &Q) -> usize
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        let mut hasher = DefaultHasher::new();
        key.borrow().hash(&mut hasher);
        hasher.finish() as usize
    }

    /// Return the index of the key if the key is present in the map, otherwise return None.
    /// Returns None for empty and deleted entries.
    #[inline]
    fn find_index<Q>(&self, key: &Q) -> Option<usize>
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
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
                    if kv_pair.key.borrow() == key {
                        return Some(probe_index);
                    }

                    // Continue probing if key doesn't match
                }
            };

            probe_index = self.next_probe_index(probe_index, probe_stride);
        }

        None
    }

    /// Insert the key value pair into this map. If there is already a value associated with the key
    /// then overwrite the value. Return whether the key was already present in the map.
    ///
    /// Assumes there is room to insert a key value pair, silently fails to insert if map is full.
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

/// A BsHashMap stored as the field of a heap item. Can create new maps and set the field to a
/// new map.
pub trait BsHashMapField<K: Eq + Hash + Clone, V: Clone> {
    fn new_map(&self, cx: Context, capacity: usize) -> HeapPtr<BsHashMap<K, V>>;

    fn get(&self, cx: Context) -> HeapPtr<BsHashMap<K, V>>;

    fn set(&mut self, cx: Context, map: HeapPtr<BsHashMap<K, V>>);

    /// Prepare map for insertion of a single entry. This will grow the map and update container to
    /// point to new map if there is no room to insert another entry in the map.
    #[inline]
    fn maybe_grow_for_insertion(&mut self, cx: Context) -> HeapPtr<BsHashMap<K, V>> {
        let old_map = self.get(cx);

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
        let mut new_map = self.new_map(cx, new_capacity);

        // Update parent reference from old child to new child map
        self.set(cx, new_map);

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
                // Reached the end of the entries array
                None => return None,
                Some(_) => {}
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
                // Reached the end of the entries array
                None => return None,
                Some(_) => {}
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
                Some(_) => {}
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
                Some(_) => {}
            }
        }
    }
}

impl<K: Eq + Hash + Clone, V: Clone> HeapItem for HeapPtr<BsHashMap<K, V>> {
    fn byte_size(&self) -> usize {
        BsHashMap::<K, V>::calculate_size_in_bytes(self.capacity())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        BsHashMap::<K, V>::visit_pointers(self, visitor)
    }
}
