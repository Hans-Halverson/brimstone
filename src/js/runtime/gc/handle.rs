use crate::js::runtime::{Context, Value};

use super::Gc;

/// Preparation for handles refactor. For direct references to heap pointers, such as references to
/// other heap objects stored within a heap object.. May not be held on stack during a GC (which can
/// occur during any heap allocation).
pub type HeapPtr<T> = Gc<T>;

/// Preparation for handles refactor. For any heap pointers that may be held on stack during a
/// GC (which can occur during any heap allocation).
pub type Handle<T> = Gc<T>;

/// Preparation for handles refactor. For any values that may be held on stack during a GC (which
/// can occur during any heap allocation).
pub type HandleValue = Value;

impl HandleValue {
    /// Get the value stored behind the handle.
    #[inline]
    pub fn get(&self) -> Value {
        *self
    }

    #[inline]
    pub fn from_value(_: &mut Context, value: Value) -> HandleValue {
        value
    }

    /// Replace the value stored behind this handle with a new value. Note that all copies of this
    /// handle will also be changed.
    #[inline]
    pub fn replace(&mut self, value: Value) {
        *self = value
    }
}

impl<T> Handle<T> {
    /// Get the heap pointer stored behind the handle.
    #[inline]
    pub fn get_(&self) -> HeapPtr<T> {
        self.clone()
    }

    #[inline]
    pub fn from_heap(heap_ptr: HeapPtr<T>) -> Handle<T> {
        heap_ptr
    }
}
