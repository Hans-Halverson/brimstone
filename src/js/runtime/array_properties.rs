use std::{collections::HashMap, mem::size_of};

use crate::{field_offset, set_uninit};

use super::{
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

    pub fn len(&self) -> u32 {
        if let Some(dense_properties) = self.as_dense_opt() {
            dense_properties.length
        } else {
            self.as_sparse().length
        }
    }

    pub fn get_property(&self, cx: &mut Context, array_index: u32) -> Option<Property> {
        if let Some(dense_properties) = self.as_dense_opt() {
            if array_index >= dense_properties.length {
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
        let old_length = dense_properties.length;

        // Only expand if new length doesn't fit in current capacity
        let old_capacity = dense_properties.capacity;
        if new_length <= old_capacity {
            // Fill added range with emptys
            dense_properties.length = new_length;
            dense_properties.set_empty_range(old_length, new_length);
            return;
        }

        // Capacity must be at least doubled
        let new_capacity = u32::max(old_capacity.saturating_mul(2), new_length);

        // Create new dense array properties, ensure that no allocation happens after this point
        // otherwise we could try to GC a partially initialized array.
        let mut new_dense_properties = DenseArrayProperties::new(cx, new_capacity);
        new_dense_properties.length = new_length;

        unsafe {
            // Copy data from old array to new array
            let new_data_ptr = new_dense_properties.data.as_mut_ptr();
            std::ptr::copy_nonoverlapping(
                dense_properties.data.as_ptr(),
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
        if new_length >= (dense_properties.capacity / 2) {
            dense_properties.length = new_length;
            return;
        }

        // Create new dense array properties, ensure that no allocation happens after this point
        // otherwise we could try to GC a partially initialized array.
        let mut new_dense_properties = DenseArrayProperties::new(cx, new_length);
        new_dense_properties.length = new_length;

        unsafe {
            // Copy data from old array to new array
            std::ptr::copy_nonoverlapping(
                dense_properties.data.as_ptr(),
                new_dense_properties.data.as_mut_ptr(),
                new_length as usize,
            );
        }

        object.set_array_properties(new_dense_properties.cast());
    }

    fn transition_to_sparse_properties(cx: &mut Context, mut object: Handle<ObjectValue>) {
        let dense_properties = object.array_properties().as_dense();

        let mut sparse_properties = SparseArrayProperties::new(cx, dense_properties.length);

        // Share handle across iterations
        let mut value_handle = Handle::<Value>::empty(cx);

        // Can reference internal pointer to map since loop does not allocate on managed heap
        let sparse_map = &mut sparse_properties.sparse_map;
        for (index, value) in dense_properties.iter_gc_unsafe().enumerate() {
            if !value.is_empty() {
                value_handle.replace(value);
                sparse_map
                    .insert(index as u32, Property::data(value_handle, true, true, true).to_heap());
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
    pub fn set_len(cx: &mut Context, object: Handle<ObjectValue>, new_length: u32) -> bool {
        let array_properties = object.array_properties();
        if let Some(dense_properties) = array_properties.as_dense_opt() {
            let array_length = dense_properties.length;
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
            let sparse_map = &sparse_properties.sparse_map;

            // Sparse expand case is easy, we simply set the new length
            if new_length >= sparse_properties.length {
                sparse_properties.length = new_length;
                return true;
            }

            // In sparse removal case we must first find the last non-configurable property.
            // Can use stored property directly since loop does not allocate.
            let mut last_non_configurable_index = None;
            for (index, stored_property) in sparse_map.iter() {
                if (*index as u32) < new_length {
                    continue;
                }
                if !stored_property.is_configurable() {
                    if *index <= last_non_configurable_index.unwrap_or(*index) {
                        last_non_configurable_index = Some(*index);
                    }
                }
            }

            let new_length = if let Some(last_index) = last_non_configurable_index {
                last_index + 1
            } else {
                new_length
            };

            // Create a new map with non-truncated values. Can use stored property directly since
            // loop does not allocate on managed heap.
            let mut new_sparse_map = HashMap::new();
            for (index, stored_property) in sparse_map.iter() {
                if *index < new_length {
                    new_sparse_map.insert(*index, stored_property.clone());
                }
            }

            sparse_properties.sparse_map = new_sparse_map;
            sparse_properties.length = new_length;

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
            } else if array_index >= dense_properties.length {
                // Transition if property is added past the end of the array by a threshold
                if array_index >= dense_properties.length + SPARSE_ARRAY_THRESHOLD {
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

            sparse_properties
                .sparse_map
                .insert(array_index, property.to_heap());
            if array_index >= sparse_properties.length {
                sparse_properties.length = array_index + 1;
            }
        }
    }
}

#[repr(C)]
pub struct DenseArrayProperties {
    descriptor: HeapPtr<ObjectDescriptor>,
    // Number of elements stored in this array so far
    length: u32,
    // Total number of elements that can be stored in the array, may be larger than the length
    capacity: u32,
    // Variable sized array of elements. We must hardcode a constant number of elements, in this
    // cast 1, to avoid this becoming a DST while keeping correct alignment and offset of fields.
    data: [Value; 1],
}

impl IsHeapObject for DenseArrayProperties {}

const DENSE_ARRAY_DATA_OFFSET: usize = field_offset!(DenseArrayProperties, data);

impl DenseArrayProperties {
    pub fn new(cx: &mut Context, capacity: u32) -> HeapPtr<DenseArrayProperties> {
        // Size of a dense array with the given capacity, in bytes
        let size = DENSE_ARRAY_DATA_OFFSET + size_of::<Value>() * (capacity as usize);
        let mut object = cx.heap.alloc_uninit_with_size::<DenseArrayProperties>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::DenseArrayProperties));
        set_uninit!(object.length, 0);
        set_uninit!(object.capacity, capacity);

        object
    }

    fn get(&self, index: u32) -> Value {
        unsafe { (&self.data.as_ptr()).add(index as usize).read() }
    }

    fn set(&mut self, index: u32, value: Value) {
        unsafe { (&self.data.as_mut_ptr()).add(index as usize).write(value) }
    }

    #[inline]
    fn set_empty_range(&mut self, start_index: u32, end_index: u32) {
        unsafe {
            let mut ptr = self.data.as_mut_ptr().add(start_index as usize);
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
        let current = self.data.as_ptr();
        let end = unsafe { current.add(self.length as usize) };

        DenseArrayPropertiesGcUnsafeIter { current, end }
    }
}

impl Handle<DenseArrayProperties> {
    #[inline]
    pub fn iter(&self) -> DenseArrayPropertiesIter {
        DenseArrayPropertiesIter {
            dense_array_properties: *self,
            current: 0,
            length: self.length,
        }
    }
}

pub struct DenseArrayPropertiesIter {
    dense_array_properties: Handle<DenseArrayProperties>,
    current: u32,
    length: u32,
}

impl Iterator for DenseArrayPropertiesIter {
    type Item = Value;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.current == self.length {
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
    descriptor: HeapPtr<ObjectDescriptor>,
    sparse_map: HashMap<u32, HeapProperty>,
    length: u32,
}

impl IsHeapObject for SparseArrayProperties {}

impl SparseArrayProperties {
    fn new(cx: &mut Context, length: u32) -> HeapPtr<SparseArrayProperties> {
        let mut object = cx.heap.alloc_uninit::<SparseArrayProperties>();

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::SparseArrayProperties));
        set_uninit!(object.sparse_map, HashMap::new());
        set_uninit!(object.length, length);

        object
    }

    pub fn ordered_keys(&self) -> Vec<u32> {
        // Sparse map is unordered, so first extract and order keys
        let mut indexes_array = self.sparse_map.keys().map(|key| *key).collect::<Vec<_>>();
        indexes_array.sort();

        indexes_array
    }
}
