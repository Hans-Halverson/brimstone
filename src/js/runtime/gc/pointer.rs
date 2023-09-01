use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use super::{HandleContents, IsHeapObject};

/// For direct references to heap pointers, such as references to other heap objects stored within a
/// heap object. May not be held on stack during a GC (which can occur during any heap allocation).
pub struct HeapPtr<T> {
    ptr: NonNull<T>,
}

impl<T> HeapPtr<T> {
    #[inline]
    pub const fn as_ptr(&self) -> *mut T {
        self.ptr.as_ptr()
    }

    #[inline]
    pub const fn from_ptr(ptr: *mut T) -> HeapPtr<T> {
        unsafe { HeapPtr { ptr: NonNull::new_unchecked(ptr) } }
    }

    #[inline]
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }

    #[inline]
    pub fn cast<U>(&self) -> HeapPtr<U> {
        HeapPtr::from_ptr(self.as_ptr() as *mut U)
    }

    #[inline]
    pub fn cast_mut<U>(&mut self) -> &mut HeapPtr<U> {
        unsafe { std::mem::transmute(self) }
    }

    #[inline]
    pub const fn uninit() -> HeapPtr<T> {
        HeapPtr { ptr: NonNull::dangling() }
    }

    #[inline]
    pub fn to_handle_contents(&self) -> HandleContents {
        self.ptr.as_ptr() as usize
    }
}

impl<T> Clone for HeapPtr<T> {
    fn clone(&self) -> Self {
        HeapPtr { ptr: self.ptr }
    }
}

impl<T> Copy for HeapPtr<T> {}

impl<T: IsHeapObject> Deref for HeapPtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: IsHeapObject> DerefMut for HeapPtr<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }
}
