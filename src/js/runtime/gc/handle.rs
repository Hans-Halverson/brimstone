use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use crate::js::runtime::{
    object_value::ObjectValue,
    string_value::StringValue,
    value::{AccessorValue, BigIntValue, SymbolValue},
    Context, Value,
};

use super::{HeapPtr, IsHeapObject};

/// Preparation for handles refactor. For any heap pointers that may be held on stack during a
/// GC (which can occur during any heap allocation).
pub struct Handle<T> {
    contents: Value,
    phantom_data: PhantomData<T>,
}

impl<T> Handle<T> {
    #[inline]
    pub const fn new(value: Value) -> Handle<T> {
        Handle { contents: value, phantom_data: PhantomData }
    }

    #[inline]
    pub const fn uninit() -> Handle<T> {
        Handle::new(Value::empty())
    }

    #[inline]
    pub fn cast<U>(&self) -> Handle<U> {
        Handle::new(self.contents)
    }
}

impl Handle<Value> {
    /// Get the value stored behind the handle.
    #[inline]
    pub fn get(&self) -> Value {
        self.contents
    }

    /// Replace the value stored behind this handle with a new value. Note that all copies of this
    /// handle will also be changed.
    #[inline]
    pub fn replace(&mut self, value: Value) {
        self.contents = value
    }

    #[inline]
    pub fn as_object(&self) -> Handle<ObjectValue> {
        self.cast()
    }

    #[inline]
    pub fn as_string(&self) -> Handle<StringValue> {
        self.cast()
    }

    #[inline]
    pub fn as_symbol(&self) -> Handle<SymbolValue> {
        self.cast()
    }

    #[inline]
    pub fn as_bigint(&self) -> Handle<BigIntValue> {
        self.cast()
    }

    #[inline]
    pub fn as_accessor(&self) -> Handle<AccessorValue> {
        self.cast()
    }

    #[inline]
    pub fn from_fixed_non_heap_ptr(value_ref: &Value) -> Handle<Value> {
        Handle::new(*value_ref)
    }
}

impl<T: IsHeapObject> Handle<T> {
    /// Get the heap pointer stored behind the handle.
    #[inline]
    pub fn get_(&self) -> HeapPtr<T> {
        self.contents.as_pointer().cast()
    }
}

impl Value {
    #[inline]
    pub fn to_handle(&self, _: &mut Context) -> Handle<Value> {
        Handle::new(*self)
    }
}

impl<T> HeapPtr<T> {
    #[inline]
    pub fn to_handle(&self) -> Handle<T> {
        Handle::new(self.cast::<ObjectValue>().into())
    }
}

impl Deref for Handle<Value> {
    type Target = Value;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}

impl DerefMut for Handle<Value> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.contents
    }
}

impl<T: IsHeapObject> Deref for Handle<T> {
    type Target = HeapPtr<T>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { std::mem::transmute::<&Handle<T>, &Self::Target>(self) }
    }
}

impl<T: IsHeapObject> DerefMut for Handle<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::mem::transmute::<&mut Handle<T>, &mut Self::Target>(self) }
    }
}

impl<T: IsHeapObject> From<Handle<T>> for Handle<Value> {
    #[inline]
    fn from(value: Handle<T>) -> Self {
        value.cast()
    }
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Handle<T> {}
