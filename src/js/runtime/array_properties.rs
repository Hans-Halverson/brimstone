use std::collections::HashMap;

use super::{property::Property, Value};

// Properties keyed by array index. Keep dense and backed by a true array if possible, otherwise
// switch to sparse array represented by map.
pub enum ArrayProperties {
    // Dense properties are all writable, enumerable, and configurable.
    Dense(Vec<Value>),
    Sparse { sparse_map: HashMap<u32, Property>, length: u32 },
}

// Number of indices past the end of an array an access can occur before dense array is converted
// to a sparse array.
pub const SPARSE_ARRAY_THRESHOLD: usize = 100;

impl ArrayProperties {
    pub const fn new() -> ArrayProperties {
        Self::Dense(vec![])
    }

    pub fn len(&self) -> u32 {
        match self {
            ArrayProperties::Dense(array) => array.len() as u32,
            ArrayProperties::Sparse { length, .. } => *length,
        }
    }

    #[inline]
    fn expand_dense_properties(&mut self, new_length: u32) {
        if let ArrayProperties::Dense(array) = self {
            let default_value = Value::empty();
            array.resize(new_length as usize, default_value);
        } else {
            unreachable!("expected dense properties");
        }
    }

    fn fall_back_to_sparse_properties(&mut self) {
        let array = if let ArrayProperties::Dense(array) = &self {
            array
        } else {
            unreachable!("expected dense properties");
        };

        let mut sparse_map = HashMap::new();

        for (index, value) in array.iter().enumerate() {
            if !value.is_empty() {
                sparse_map.insert(index as u32, Property::data(*value, true, true, true));
            }
        }

        *self = ArrayProperties::Sparse { sparse_map, length: array.len() as u32 }
    }

    // Resize array properties to match a new array length, potentially expanding and adding empty
    // values, or shrinking and removing existing values.
    //
    // Properties are removed in descending numerical order, and removal can fail if the property
    // is not configurable. In this case set the new length to have that property at the end of the
    // array, stop deleting other properties, and return false.
    //
    // Returns return true on success.
    pub fn set_len(&mut self, new_length: u32) -> bool {
        // First try falling back to sparse properties if this is an expanded dense array
        match &self {
            ArrayProperties::Dense(array)
                if new_length as usize >= array.len() + SPARSE_ARRAY_THRESHOLD =>
            {
                self.fall_back_to_sparse_properties();
            }
            _ => {}
        }

        match self {
            ArrayProperties::Dense(array) => {
                // In dense expansion case we resize array with new empty elements
                if new_length >= array.len() as u32 {
                    self.expand_dense_properties(new_length);
                    return true;
                }

                // In dense case all properties are configurable so can always truncate directly to
                // new length.
                array.truncate(new_length as usize);
                true
            }
            ArrayProperties::Sparse { sparse_map, length } => {
                // Sparse expand case is easy, we simply set the new length
                if new_length >= *length {
                    *length = new_length;
                    return true;
                }

                // In sparse removal case we must first find the last non-configurable property
                let mut last_non_configurable_index = None;
                for (index, value) in sparse_map.iter() {
                    if (*index as u32) < new_length {
                        continue;
                    }

                    if !value.is_configurable() {
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

                // Create a new map with non-truncated values
                let mut new_sparse_map = HashMap::new();
                for (index, property) in sparse_map.iter() {
                    if *index < new_length {
                        new_sparse_map.insert(*index, property.clone());
                    }
                }

                *sparse_map = new_sparse_map;
                *length = new_length;

                last_non_configurable_index.is_none()
            }
        }
    }

    pub fn get_property(&self, array_index: u32) -> Option<Property> {
        match self {
            ArrayProperties::Dense(array) => {
                if array_index as usize >= array.len() {
                    return None;
                }

                let value = &array[array_index as usize];
                if value.is_empty() {
                    return None;
                }

                Some(Property::data(*value, true, true, true))
            }
            ArrayProperties::Sparse { sparse_map, .. } => sparse_map.get(&array_index).cloned(),
        }
    }

    pub fn set_property(&mut self, array_index: u32, property: Property) {
        match self {
            ArrayProperties::Dense(array) => {
                // Property must have the correct attributes to be added as dense property.
                // Otherwise transition to sparse properties and add new property.
                if !property.is_allowed_as_dense_array_property() {
                    self.fall_back_to_sparse_properties();
                    self.set_property(array_index, property)
                } else if array_index as usize >= array.len() {
                    if array_index as usize >= array.len() + SPARSE_ARRAY_THRESHOLD {
                        self.fall_back_to_sparse_properties();
                    } else {
                        self.expand_dense_properties(array_index + 1);
                    }

                    self.set_property(array_index, property);
                } else {
                    array[array_index as usize] = property.value();
                }
            }
            ArrayProperties::Sparse { sparse_map, length } => {
                sparse_map.insert(array_index, property);
                if array_index >= *length {
                    *length = array_index + 1;
                }
            }
        }
    }

    pub fn remove_property(&mut self, array_index: u32) {
        match self {
            ArrayProperties::Dense(array) => {
                array[array_index as usize] = Value::empty();
            }
            ArrayProperties::Sparse { sparse_map, .. } => {
                sparse_map.remove(&array_index);
            }
        }
    }
}
