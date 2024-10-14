use std::collections::HashSet;

use crate::{field_offset, js::runtime::object_descriptor::ObjectKind, set_uninit};

use super::{
    collections::InlineArray,
    gc::{HeapObject, HeapVisitor},
    object_descriptor::ObjectDescriptor,
    object_value::ObjectValue,
    string_value::StringValue,
    Context, EvalResult, Handle, HeapPtr, PropertyKey, Value,
};

/// An iterator over the keys of an object, used in for-in loops.
///
/// All object keys are calculated when the iterator is first created (including walking the
/// prototype chain). This array of object keys is then iterated over, with the only catch being
/// that we need to check that the property has not yet been deleted by the time `next` is called.
///
/// This is a simple strategy but leaves much room for optimization.
///
/// Follows the guidance of EnumerateObjectProperties (https://tc39.es/ecma262/#sec-enumerate-object-properties)
/// and For-In Iterator Objects (https://tc39.es/ecma262/#sec-for-in-iterator-objects)
#[repr(C)]
pub struct ForInIterator {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// The target object that is being iterated over.
    object: HeapPtr<ObjectValue>,
    /// The next index in the keys array to be visited.
    index: usize,
    /// The set of all keys in the target object that are being iterated over.
    keys: InlineArray<HeapPtr<StringValue>>,
}

impl ForInIterator {
    fn new(
        cx: Context,
        object: Handle<ObjectValue>,
        keys: &[Handle<StringValue>],
    ) -> HeapPtr<ForInIterator> {
        let size = Self::calculate_size_in_bytes(keys.len());
        let mut iterator = cx.alloc_uninit_with_size::<ForInIterator>(size);

        set_uninit!(iterator.descriptor, cx.base_descriptors.get(ObjectKind::ForInIterator));
        set_uninit!(iterator.object, *object);
        set_uninit!(iterator.index, 0);

        iterator.keys.init_with_uninit(keys.len());
        for (i, key) in keys.iter().enumerate() {
            set_uninit!(iterator.keys.as_mut_slice()[i], **key);
        }

        iterator
    }

    /// Create a new for-in iterator for the given object. This function calculates all the keys
    /// up front and stores them in the iterator.
    pub fn new_for_object(
        cx: Context,
        object: Handle<ObjectValue>,
    ) -> EvalResult<HeapPtr<ForInIterator>> {
        // Walk prototype chain, collecting property keys that haven't already been visited
        let mut keys = vec![];
        let mut visited_keys = HashSet::new();
        let mut current_object = object;

        loop {
            let own_property_keys = current_object.own_property_keys(cx)?;
            for key in own_property_keys {
                if key.is_symbol() {
                    continue;
                }

                // Values returned from OwnPropertyKeys are either strings or symbols
                let key = key.as_string();

                // Skip keys that have already been seen, regardless of whether they are enumerable
                let property_key = PropertyKey::string(cx, key).to_handle(cx);
                if !visited_keys.insert(property_key) {
                    continue;
                }

                // Only collect enumerable keys
                if let Some(desc) = current_object.get_own_property(cx, property_key)? {
                    if desc.is_enumerable() {
                        keys.push(key);
                    }
                }
            }

            // Collect keys from the parent object, if there is one
            match current_object.get_prototype_of(cx)? {
                None => break,
                Some(proto_object) => current_object = proto_object,
            }
        }

        Ok(Self::new(cx, object, &keys))
    }

    const KEYS_OFFSET: usize = field_offset!(ForInIterator, keys);

    fn calculate_size_in_bytes(len: usize) -> usize {
        Self::KEYS_OFFSET + InlineArray::<HeapPtr<StringValue>>::calculate_size_in_bytes(len)
    }
}

impl Handle<ForInIterator> {
    /// Return the next string key in the iteration, or undefined if there are no more keys.
    pub fn next(&mut self, cx: Context) -> EvalResult<Value> {
        let object = self.object.to_handle();

        // Find the next property that has not been deleted
        loop {
            if self.index >= self.keys.len() {
                return Ok(Value::undefined());
            }

            let key = self.keys.get_unchecked(self.index).to_handle();
            let property_key = PropertyKey::string(cx, key).to_handle(cx);

            self.index += 1;

            // Check if property was deleted, otherwise skip it
            if object.has_property(cx, property_key)? {
                return Ok(*key.as_value());
            }
        }
    }
}

impl HeapObject for HeapPtr<ForInIterator> {
    fn byte_size(&self) -> usize {
        ForInIterator::calculate_size_in_bytes(self.keys.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.object);

        for key in self.keys.as_mut_slice() {
            visitor.visit_pointer(key);
        }
    }
}
