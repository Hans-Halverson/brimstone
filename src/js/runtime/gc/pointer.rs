use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use super::{HandleContents, IsHeapItem, ToHandleContents};

/// For direct references to heap pointers, such as references to other heap items stored within a
/// heap item. May not be held on stack during a GC (which can occur during any heap allocation).
pub struct HeapPtr<T> {
    ptr: NonNull<T>,
}

impl<T> HeapPtr<T> {
    #[inline]
    pub const fn as_ptr(&self) -> *mut T {
        self.ptr.as_ptr()
    }

    #[inline]
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
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
}

impl<T: IsHeapItem> ToHandleContents for T {
    type Impl = HeapPtr<T>;

    #[inline]
    fn to_handle_contents(heap_ptr: HeapPtr<T>) -> HandleContents {
        heap_ptr.ptr.as_ptr() as usize
    }
}

impl<T> Clone for HeapPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for HeapPtr<T> {}

impl<T: IsHeapItem> Deref for HeapPtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: IsHeapItem> DerefMut for HeapPtr<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }
}
