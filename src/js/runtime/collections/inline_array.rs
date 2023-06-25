use std::mem::size_of;

use crate::set_uninit;

// An inline collection of a fixed number of elements that lives on the managed heap. Length is
// stored inline with the elements themselves.
#[repr(C)]
pub struct InlineArray<T> {
    // Number of elements in the array
    len: usize,
    // Variable sized array of elements. We must hardcode a constant number of elements, in this
    // case 1, to avoid this becoming a DST while keeping correct alignment and offset of fields.
    data: [T; 1],
}

impl<T> InlineArray<T> {
    /// Initialize an uninitialized InlineArray.
    pub fn init(&mut self, len: usize) {
        set_uninit!(self.len, len);
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn data_ptr(&self) -> *const T {
        self.data.as_ptr()
    }

    #[inline]
    pub fn data_mut_ptr(&mut self) -> *mut T {
        self.data.as_mut_ptr()
    }

    #[inline]
    pub fn calculate_size_in_bytes(len: usize) -> usize {
        size_of::<usize>() + len * size_of::<T>()
    }

    #[inline]
    fn as_slice(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len()) }
    }

    #[inline]
    fn as_mut_slice(&mut self) -> &mut [T] {
        unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.len()) }
    }

    #[inline]
    pub fn get_unchecked(&self, index: usize) -> &T {
        unsafe { self.as_slice().get_unchecked(index) }
    }

    #[inline]
    pub fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        unsafe { self.as_mut_slice().get_unchecked_mut(index) }
    }
}
