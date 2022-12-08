use std::ptr::NonNull;

/// A pointer to a value in the GC heap.
pub struct Gc<T> {
    ptr: NonNull<T>,
}

impl<T> Gc<T> {
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

impl<T> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T> AsMut<T> for Gc<T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { self.ptr.as_mut() }
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc { ptr: self.ptr }
    }
}

impl<T> Copy for Gc<T> {}
