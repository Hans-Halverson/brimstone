use std::mem::size_of;

use crate::{field_offset, set_uninit};

use super::{
    collections::{BsHashMap, BsHashMapField, InlineArray},
    gc::IsHeapObject,
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::ObjectValue,
    property::{HeapProperty, Property},
    Context, Handle, HeapPtr, Value,
};

// Properties keyed by array index. Back by dense array when possible, otherwise transition to
// sparse array represented by map.
#[repr(C)]
pub struct ArrayProperties {
    descriptor: HeapPtr<ObjectDescriptor>,
}

impl IsHeapObject for ArrayProperties {}

// Number of indices past the end of an array an access can occur before dense array is converted
// to a sparse array.
const SPARSE_ARRAY_THRESHOLD: u32 = 100;

impl HeapPtr<ArrayProperties> {
    #[inline]
    pub fn as_dense(&self) -> HeapPtr<DenseArrayProperties> {
        self.cast()
    }

    #[inline]
    pub fn as_sparse(&self) -> HeapPtr<SparseArrayProperties> {
        self.cast()
    }

    #[inline]
    pub fn as_dense_opt(&self) -> Option<HeapPtr<DenseArrayProperties>> {
        if self.descriptor.kind() == ObjectKind::DenseArrayProperties {
            Some(self.as_dense())
        } else {
            None
        }
    }

    pub fn array_length(&self) -> u32 {
        if let Some(dense_properties) = self.as_dense_opt() {
            dense_properties.len()
        } else {
            self.as_sparse().array_length()
        }
    }

    pub fn get_property(&self, cx: &mut Context, array_index: u32) -> Option<Property> {
        if let Some(dense_properties) = self.as_dense_opt() {
            if array_index >= dense_properties.len() {
                return None;
            }

            let value = dense_properties.get(array_index);
            if value.is_empty() {
                return None;
            }

            Some(Property::data(value.to_handle(cx), true, true, true))
        } else {
            let sparse_properties = self.as_sparse();
            sparse_properties
                .sparse_map
                .get(&array_index)
                .map(|property| Property::from_heap(cx, property))
        }
    }

    pub fn remove_property(&self, array_index: u32) {
        if let Some(mut dense_properties) = self.as_dense_opt() {
            dense_properties.set(array_index, Value::empty());
        } else {
            let mut sparse_properties = self.as_sparse();
            sparse_properties.sparse_map.remove(&array_index);
        }
    }
}

impl ArrayProperties {
    pub fn initial(cx: &mut Context) -> HeapPtr<ArrayProperties> {
        cx.default_array_properties
    }

    /// Expand an object's dense properties to have at least enough room for the new length.
    #[inline]
    fn grow_dense_properties(cx: &mut Context, mut object: Handle<ObjectValue>, new_length: u32) {
        let mut dense_properties = object.array_properties().as_dense();
        let old_length = dense_properties.len();

        // Only expand if new length doesn't fit in current capacity
        let old_capacity = dense_properties.capacity();
        if new_length <= old_capacity {
            // Fill added range with emptys
            dense_properties.set_len(new_length);
            dense_properties.set_empty_range(old_length, new_length);
            return;
        }

        // Capacity must be at least doubled
        let new_capacity = u32::max(old_capacity.saturating_mul(2), new_length);

        // Create new dense array properties, ensure that no allocation happens after this point
        // otherwise we could try to GC a partially initialized array.
        let mut new_dense_properties = DenseArrayProperties::new(cx, new_capacity);
        new_dense_properties.set_len(new_length);

        unsafe {
            // Copy data from old array to new array
            let new_data_ptr = new_dense_properties.array.data_mut_ptr();
            std::ptr::copy_nonoverlapping(
                dense_properties.array.data_ptr(),
                new_data_ptr,
                old_length as usize,
            );

            // Add empty values into rest of new buffer
            new_dense_properties.set_empty_range(old_length, new_capacity);
        }

        object.set_array_properties(new_dense_properties.cast());
    }

    #[inline]
    fn shrink_dense_properties(cx: &mut Context, mut object: Handle<ObjectValue>, new_length: u32) {
        let mut dense_properties = object.array_properties().as_dense();

        // Only shrink backing array if it would be less than one half filled
        if new_length >= (dense_properties.capacity() / 2) {
            dense_properties.set_len(new_length);
            return;
        }

        // Create new dense array properties, ensure that no allocation happens after this point
        // otherwise we could try to GC a partially initialized array.
        let mut new_dense_properties = DenseArrayProperties::new(cx, new_length);
        new_dense_properties.set_len(new_length);

        unsafe {
            // Copy data from old array to new array
            std::ptr::copy_nonoverlapping(
                dense_properties.array.data_ptr(),
                new_dense_properties.array.data_mut_ptr(),
                new_length as usize,
            );
        }

        object.set_array_properties(new_dense_properties.cast());
    }

    fn transition_to_sparse_properties(cx: &mut Context, mut object: Handle<ObjectValue>) {
        let dense_properties = object.array_properties().as_dense().to_handle();

        // Initial sparse map size is the number of non-empty properties
        let num_non_empty_properties = dense_properties
            .iter_gc_unsafe()
            .filter(|value| !value.is_empty())
            .count();
        let new_capacity = SparseMap::min_capacity_needed(num_non_empty_properties);
        let mut sparse_properties =
            SparseArrayProperties::new(cx, new_capacity, dense_properties.len());

        // Share handle across iterations
        let mut value_handle = Handle::<Value>::empty(cx);

        // Can reference internal pointer to map since loop does not allocate on managed heap,
        // map was initialized with the capacity to fit all elements.
        for (index, value) in dense_properties.iter_gc_unsafe().enumerate() {
            if !value.is_empty() {
                value_handle.replace(value);
                sparse_properties.sparse_map.insert_without_growing(
                    index as u32,
                    Property::data(value_handle, true, true, true).to_heap(),
                );
            }
        }

        object.set_array_properties(sparse_properties.cast());
    }

    // Resize array properties to match a new array length, potentially expanding and adding empty
    // values, or shrinking and removing existing values.
    //
    // Properties are removed in descending numerical order, and removal can fail if the property
    // is not configurable. In this case set the new length to have that property at the end of the
    // array, stop deleting other properties, and return false.
    //
    // Returns return true on success.
    pub fn set_len(cx: &mut Context, mut object: Handle<ObjectValue>, new_length: u32) -> bool {
        let array_properties = object.array_properties();
        if let Some(dense_properties) = array_properties.as_dense_opt() {
            let array_length = dense_properties.len();
            if new_length > array_length {
                if new_length >= array_length + SPARSE_ARRAY_THRESHOLD {
                    // First try falling back to sparse properties if this is an expanded dense array
                    Self::transition_to_sparse_properties(cx, object);
                    return Self::set_len(cx, object, new_length);
                } else {
                    // Otherwise stay dense but resize array with new empty elements
                    Self::grow_dense_properties(cx, object, new_length);
                    return true;
                }
            } else if new_length < array_length {
                // All properties are configurable so can always shrink directly to new length
                Self::shrink_dense_properties(cx, object, new_length);
                true
            } else {
                // Length is unchanged
                true
            }
        } else {
            let mut sparse_properties = array_properties.as_sparse();

            // Sparse expand case is easy, we simply set the new length
            if new_length >= sparse_properties.array_length() {
                sparse_properties.set_array_length(new_length);
                return true;
            }

            // Save behind handle before allocating
            let sparse_properties = sparse_properties.to_handle();

            // In sparse removal case we must first find the last non-configurable property.
            // Can use stored property directly since loop does not allocate.
            let mut last_non_configurable_index = None;
            for (index, stored_property) in sparse_properties.sparse_map.iter_gc_unsafe() {
                if (index as u32) < new_length {
                    continue;
                }
                if !stored_property.is_configurable() {
                    if index <= last_non_configurable_index.unwrap_or(index) {
                        last_non_configurable_index = Some(index);
                    }
                }
            }

            let new_length = if let Some(last_index) = last_non_configurable_index {
                last_index + 1
            } else {
                new_length
            };

            // Calculate number of properties that will be kept so that we can create new map with
            // appropriate size.
            let num_properties_left = sparse_properties
                .sparse_map
                .iter_gc_unsafe()
                .filter(|(index, _)| *index < new_length)
                .count();
            let new_capacity = SparseMap::min_capacity_needed(num_properties_left);
            let mut new_sparse_properties =
                SparseArrayProperties::new(cx, new_capacity, new_length);

            // Create a new map with non-truncated values. Can use stored property directly since
            // loop does not allocate on managed heap as map has capacity for all properties.
            for (index, stored_property) in sparse_properties.sparse_map.iter_gc_unsafe() {
                if index < new_length {
                    new_sparse_properties
                        .sparse_map
                        .insert_without_growing(index, stored_property.clone());
                }
            }

            object.set_array_properties(new_sparse_properties.cast());

            last_non_configurable_index.is_none()
        }
    }

    pub fn set_property(
        cx: &mut Context,
        object: Handle<ObjectValue>,
        array_index: u32,
        property: Property,
    ) {
        let array_properties = object.array_properties();
        if let Some(mut dense_properties) = array_properties.as_dense_opt() {
            if !property.is_allowed_as_dense_array_property() {
                // Property must have the correct attributes to be added as dense property.
                // Otherwise transition to sparse properties and add new property.
                Self::transition_to_sparse_properties(cx, object);
                Self::set_property(cx, object, array_index, property)
            } else if array_index >= dense_properties.len() {
                // Transition if property is added past the end of the array by a threshold
                if array_index >= dense_properties.len() + SPARSE_ARRAY_THRESHOLD {
                    Self::transition_to_sparse_properties(cx, object);
                } else {
                    Self::grow_dense_properties(cx, object, array_index + 1);
                }

                Self::set_property(cx, object, array_index, property);
            } else {
                dense_properties.set(array_index, property.value().get());
            }
        } else {
            let mut sparse_properties = array_properties.as_sparse();

            if array_index >= sparse_properties.array_length() {
                sparse_properties.set_array_length(array_index + 1);
            }

            let mut sparse_map_field = SparseMapField(object);
            sparse_map_field.insert(cx, array_index, property.to_heap());
        }
    }
}

#[repr(C)]
pub struct DenseArrayProperties {
    descriptor: HeapPtr<ObjectDescriptor>,
    // Number of elements stored in this array so far
    len: u32,
    // Array of elements, total size is the capacity of the array
    array: InlineArray<Value>,
}

impl IsHeapObject for DenseArrayProperties {}

const DENSE_ARRAY_DATA_OFFSET: usize = field_offset!(DenseArrayProperties, array);

impl DenseArrayProperties {
    pub fn new(cx: &mut Context, capacity: u32) -> HeapPtr<DenseArrayProperties> {
        // Size of a dense array with the given capacity, in bytes
        let size = DENSE_ARRAY_DATA_OFFSET
            + InlineArray::<Value>::calculate_size_in_bytes(capacity as usize);
        let mut object = cx.heap.alloc_uninit_with_size::<DenseArrayProperties>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::DenseArrayProperties));
        set_uninit!(object.len, 0);
        object.array.init(capacity as usize);

        object
    }

    #[inline]
    fn len(&self) -> u32 {
        self.len
    }

    #[inline]
    fn set_len(&mut self, len: u32) {
        self.len = len;
    }

    #[inline]
    fn capacity(&self) -> u32 {
        self.array.len() as u32
    }

    #[inline]
    fn get(&self, index: u32) -> Value {
        *(self.array.get_unchecked(index as usize))
    }

    #[inline]
    fn set(&mut self, index: u32, value: Value) {
        *(self.array.get_unchecked_mut(index as usize)) = value;
    }

    #[inline]
    fn set_empty_range(&mut self, start_index: u32, end_index: u32) {
        unsafe {
            let mut ptr = self.array.data_mut_ptr().add(start_index as usize);
            for _ in 0..(end_index - start_index) {
                ptr.write(Value::empty());
                ptr = ptr.add(1);
            }
        }
    }

    /// An iterator over the values of this dense array which is not GC safe. Caller must ensure
    /// that a GC cannot occur while the iterator is in use.
    #[inline]
    fn iter_gc_unsafe(&self) -> DenseArrayPropertiesGcUnsafeIter {
        let current = self.array.data_ptr();
        let end = unsafe { current.add(self.len() as usize) };

        DenseArrayPropertiesGcUnsafeIter { current, end }
    }
}

impl Handle<DenseArrayProperties> {
    #[inline]
    pub fn iter(&self) -> DenseArrayPropertiesIter {
        DenseArrayPropertiesIter { dense_array_properties: *self, current: 0, len: self.len() }
    }
}

pub struct DenseArrayPropertiesIter {
    dense_array_properties: Handle<DenseArrayProperties>,
    current: u32,
    len: u32,
}

impl Iterator for DenseArrayPropertiesIter {
    type Item = Value;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.current == self.len {
            None
        } else {
            let value = self.dense_array_properties.get(self.current);
            self.current += 1;
            Some(value)
        }
    }
}

struct DenseArrayPropertiesGcUnsafeIter {
    current: *const Value,
    end: *const Value,
}

impl Iterator for DenseArrayPropertiesGcUnsafeIter {
    type Item = Value;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.current == self.end {
            None
        } else {
            unsafe {
                let value = self.current.read();
                self.current = self.current.add(1);

                Some(value)
            }
        }
    }
}

#[repr(C)]
pub struct SparseArrayProperties {
    sparse_map: SparseMap,
    // One past the highest index in the map. Field should not be accessed directly since sparse_map
    // is variable sized.
    _array_length: u32,
}

type SparseMap = BsHashMap<u32, HeapProperty>;

impl IsHeapObject for SparseArrayProperties {}

impl SparseArrayProperties {
    fn new(cx: &mut Context, capacity: usize, array_length: u32) -> HeapPtr<SparseArrayProperties> {
        let byte_size = Self::calculate_size_in_bytes(capacity);
        let mut object = cx
            .heap
            .alloc_uninit_with_size::<SparseArrayProperties>(byte_size);

        object
            .sparse_map
            .init(cx, ObjectKind::SparseArrayProperties, capacity);

        // Set uninitialized array length
        object.set_array_length(array_length);

        object
    }

    #[inline]
    pub fn calculate_size_in_bytes(capacity: usize) -> usize {
        let sparse_map_byte_size = SparseMap::calculate_size_in_bytes(capacity);
        let array_length_byte_size = size_of::<u32>();

        sparse_map_byte_size + array_length_byte_size
    }

    fn array_length_ptr(&self) -> *const u32 {
        let array_length_byte_offset =
            SparseMap::calculate_size_in_bytes(self.sparse_map.capacity());
        unsafe {
            (self as *const _ as *const u8)
                .add(array_length_byte_offset)
                .cast()
        }
    }

    fn array_length(&self) -> u32 {
        unsafe { self.array_length_ptr().read() }
    }

    fn set_array_length(&mut self, array_length: u32) {
        unsafe {
            self.array_length_ptr().cast_mut().write(array_length);
        }
    }

    pub fn ordered_keys(&self) -> Vec<u32> {
        // Sparse map is unordered, so first extract and order keys
        let mut indexes_array = self.sparse_map.keys_gc_unsafe().collect::<Vec<_>>();
        indexes_array.sort();

        indexes_array
    }
}

impl HeapPtr<SparseArrayProperties> {
    pub fn sparse_map(&self) -> HeapPtr<SparseMap> {
        self.cast()
    }
}

struct SparseMapField(Handle<ObjectValue>);

impl BsHashMapField<u32, HeapProperty> for SparseMapField {
    fn new(&self, cx: &mut Context, capacity: usize) -> HeapPtr<SparseMap> {
        let array_length = self.0.array_properties_length();
        SparseArrayProperties::new(cx, capacity, array_length).cast()
    }

    #[inline]
    fn get(&self) -> HeapPtr<SparseMap> {
        self.0.array_properties().as_sparse().sparse_map()
    }

    #[inline]
    fn set(&mut self, map: HeapPtr<SparseMap>) {
        self.0.set_array_properties(map.cast())
    }
}
