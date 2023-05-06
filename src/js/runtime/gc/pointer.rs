use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use crate::js::runtime::Value;

/// A pointer to a value in the GC heap.
pub struct Gc<T> {
    ptr: NonNull<T>,
}

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
    pub fn get(&self) -> Value {
        *self
    }
}

impl<T> Gc<T> {
    #[inline]
    pub const fn as_ptr(&self) -> *mut T {
        self.ptr.as_ptr()
    }

    #[inline]
    pub const fn as_non_null_ptr(&self) -> NonNull<T> {
        self.ptr
    }

    #[inline]
    pub const fn from_ptr(ptr: *mut T) -> Gc<T> {
        unsafe { Gc { ptr: NonNull::new_unchecked(ptr) } }
    }

    #[inline]
    pub const fn from_ref(reference: &T) -> Gc<T> {
        Gc::from_ptr(reference as *const T as *mut T)
    }

    #[inline]
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }

    #[inline]
    pub fn cast<U>(&self) -> Gc<U> {
        Gc::from_ptr(self.as_ptr() as *mut U)
    }

    #[inline]
    pub const fn uninit() -> Gc<T> {
        Gc { ptr: NonNull::dangling() }
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc { ptr: self.ptr }
    }
}

impl<T> Copy for Gc<T> {}

/// Marker trait to allow generic autoderef of a particular type.
pub trait GcDeref {}

impl<T: GcDeref> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: GcDeref> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }
}
