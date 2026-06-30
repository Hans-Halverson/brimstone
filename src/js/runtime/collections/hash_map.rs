use std::{
    borrow::Borrow,
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    slice,
};

use crate::{
    field_offset,
    runtime::{
        Context, HeapPtr,
        alloc_error::AllocResult,
        collections::InlineArray,
        gc::{HeapVisitor, IsHeapItem},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
    },
    set_uninit,
};

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

    pub fn new(cx: Context, kind: HeapItemKind, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        // Size of a dense array with the given capacity, in bytes
        let size = Self::calculate_size_in_bytes(capacity);
        let mut hash_map = cx.alloc_uninit_with_size::<BsHashMap<K, V>>(size)?;

        hash_map.init(cx, kind, capacity);

        Ok(hash_map)
    }

    pub fn init(&mut self, cx: Context, kind: HeapItemKind, capacity: usize) {
        set_uninit!(self.descriptor, cx.descriptors.get(kind));
        set_uninit!(self.len, 0);

        // Initialize entries array to empty
        self.entries.init_with(capacity, Entry::Empty);
    }

    pub fn new_initial(cx: Context, kind: HeapItemKind) -> AllocResult<HeapPtr<Self>> {
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

    /// Return an iterator over the entries of the map. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn iter_gc_unsafe(&self) -> GcUnsafeEntriesIter<'_, K, V> {
        GcUnsafeEntriesIter(self.entries.as_slice().iter())
    }

    /// Return an iterator over the entries of the map. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn iter_mut_gc_unsafe(&mut self) -> GcUnsafeEntriesIterMut<'_, K, V> {
        GcUnsafeEntriesIterMut(self.entries.as_mut_slice().iter_mut())
    }

    /// Return an iterator over the keys of the map. Iterator is not GC-safe, so make sure there
    /// are no allocations between construction and use.
    pub fn keys_gc_unsafe(&self) -> GcUnsafeKeysIter<'_, K, V> {
        GcUnsafeKeysIter(self.entries.as_slice().iter())
    }

    /// Return an iterator over the keys of the map. Iterator is not GC-safe, so make sure there
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

        let mut first_deleted_index = None;

        // Quadratic probing - probe stride increases by 1 each iteration
        for probe_stride in 1..=self.capacity() {
            match self.entries.get_unchecked_mut(probe_index) {
                // First empty entry encountered means we have not found an occupied entry with this
                // key. Must insert, preferring the first deleted entry if any were encountered.
                Entry::Empty => {
                    let insertion_index = first_deleted_index.unwrap_or(probe_index);
                    self.entries
                        .set_unchecked(insertion_index, Entry::Occupied(KVPair { key, value }));
                    self.len += 1;

                    return false;
                }
                // Mark the first deleted entry but keep going in case there is an occupied entry
                // with this key later on.
                Entry::Deleted => {
                    if first_deleted_index.is_none() {
                        first_deleted_index = Some(probe_index);
                    }
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

        // Only deleted entries left, insert into the first one
        if let Some(insertion_index) = first_deleted_index {
            self.entries
                .set_unchecked(insertion_index, Entry::Occupied(KVPair { key, value }));
            self.len += 1;
        }

        false
    }
}

/// An instance of a BsHashMap with a specific key and value type. This has its own object
/// descriptor identifying the full BsHashMap<K, V>.
pub trait HashMapInstance:
    IsHeapItem
    + std::ops::Deref<Target = BsHashMap<Self::K, Self::V>>
    + std::ops::DerefMut<Target = BsHashMap<Self::K, Self::V>>
{
    type K: Eq + std::hash::Hash + Clone;
    type V: Clone;

    const KIND: HeapItemKind;

    fn new(cx: Context, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        Ok(BsHashMap::<Self::K, Self::V>::new(cx, Self::KIND, capacity)?.cast())
    }

    fn new_initial(cx: Context) -> AllocResult<HeapPtr<Self>> {
        Ok(BsHashMap::<Self::K, Self::V>::new_initial(cx, Self::KIND)?.cast())
    }

    fn calculate_size_in_bytes(capacity: usize) -> usize {
        BsHashMap::<Self::K, Self::V>::calculate_size_in_bytes(capacity)
    }
}

#[macro_export]
macro_rules! impl_hash_map_instance {
    ($map_type:ident, $key_type:ty, $value_type:ty) => {
        #[repr(transparent)]
        pub struct $map_type($crate::runtime::collections::BsHashMap<$key_type, $value_type>);

        impl $crate::runtime::collections::HashMapInstance for $map_type {
            type K = $key_type;
            type V = $value_type;

            const KIND: $crate::runtime::heap_item_descriptor::HeapItemKind =
                $crate::runtime::heap_item_descriptor::HeapItemKind::$map_type;
        }

        impl $crate::runtime::gc::IsHeapItem for $map_type {}

        impl std::ops::Deref for $map_type {
            type Target = $crate::runtime::collections::BsHashMap<$key_type, $value_type>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl std::ops::DerefMut for $map_type {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    };
}

/// Prepare map for insertion of a single entry. This will grow the map and update container to
/// point to new map if there is no room to insert another entry in the map.
///
/// `set_new` callback creates a new map with the given capacity and uses it from then on.
#[inline]
pub fn maybe_grow_for_insertion<K, V>(
    cx: Context,
    map: HeapPtr<BsHashMap<K, V>>,
    mut set_new: impl FnMut(Context, usize) -> AllocResult<HeapPtr<BsHashMap<K, V>>>,
) -> AllocResult<HeapPtr<BsHashMap<K, V>>>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    // Keep at least half of the entries empty, otherwise grow
    let new_length = map.len() + 1;
    let capacity = map.capacity();
    if new_length <= capacity / 2 {
        return Ok(map);
    }

    // Save old map behind handle before allocating
    let old_map = map.to_handle();

    // Double size leaving map 1/4 full after growing
    let new_capacity = capacity * 2;
    let mut new_map = set_new(cx, new_capacity)?;

    // Copy all values into new map. We have already guaranteed that there is enough room in map.
    for (key, value) in old_map.iter_gc_unsafe() {
        new_map.insert_without_growing(key, value);
    }

    Ok(new_map)
}

/// A BsHashMap stored as the field of a heap item. Can create new maps and set the field to a
/// new map.
pub trait BsHashMapField<I: HashMapInstance> {
    fn get(&self, cx: Context) -> HeapPtr<I>;

    /// Create a new map with the given capacity and set the field to point to it.
    /// Return the new map.
    fn set_new(&mut self, cx: Context, capacity: usize) -> AllocResult<HeapPtr<I>>;

    /// Prepare map for insertion of a single entry. This will grow the map and update container to
    /// point to new map if there is no room to insert another entry in the map.
    #[inline]
    fn maybe_grow_for_insertion(&mut self, cx: Context) -> AllocResult<HeapPtr<I>> {
        Ok(maybe_grow_for_insertion(
            cx,
            self.get(cx).cast::<BsHashMap<I::K, I::V>>(),
            |cx, capacity| Ok(self.set_new(cx, capacity)?.cast()),
        )?
        .cast())
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

// Only necessary so we get deref for HeapPtrs.
impl<K, V> IsHeapItem for BsHashMap<K, V> {}
