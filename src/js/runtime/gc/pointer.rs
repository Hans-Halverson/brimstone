use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

/// A pointer to a value in the GC heap.
pub struct Gc<T> {
    ptr: NonNull<T>,
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
