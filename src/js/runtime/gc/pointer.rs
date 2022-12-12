use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

/// A pointer to a value in the GC heap.
pub struct Gc<T: ?Sized> {
    ptr: NonNull<T>,
}

impl<T: ?Sized> Gc<T> {
    pub const fn as_ptr(&self) -> *mut T {
        self.ptr.as_ptr()
    }

    pub const fn from_ptr(ptr: *mut T) -> Gc<T> {
        unsafe {
            Gc {
                ptr: NonNull::new_unchecked(ptr),
            }
        }
    }
}

impl<T> Gc<T> {
    pub fn uninit() -> Gc<T> {
        Gc {
            ptr: NonNull::dangling(),
        }
    }
}

impl<T: ?Sized> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: ?Sized> AsMut<T> for Gc<T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { self.ptr.as_mut() }
    }
}

impl<T: ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc { ptr: self.ptr }
    }
}

impl<T: ?Sized> Copy for Gc<T> {}

/// Marker trait to allow generic autoderef of a particular type.
pub trait GcDeref {}

impl<T: GcDeref + ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T: GcDeref + ?Sized> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}
