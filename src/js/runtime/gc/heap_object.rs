use std::mem::transmute;

use crate::runtime::{PropertyKey, Value};

use super::{HeapItem, HeapPtr};

/// Trait implemented by all objects stored on the heap.
pub trait HeapObject {
    /// Size of this heap object in bytes. Not guaranteed to be aligned.
    fn byte_size(&self) -> usize;

    /// Call the provided visit function on all pointer fields in this object. Pass a mutable
    /// reference to the fields themselves so they can be updated in copying collection.
    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor);
}

/// Marker trait that denotes an object on the managed heap
pub trait IsHeapObject {}

impl<T> IsHeapObject for T where HeapPtr<T>: HeapObject {}

impl<T> HeapPtr<T>
where
    HeapPtr<T>: HeapObject,
{
    #[inline]
    pub fn as_heap_item(&self) -> HeapPtr<HeapItem> {
        self.cast()
    }
}

pub trait HeapVisitor {
    /// Visit a strongly held pointer.
    fn visit(&mut self, ptr: &mut HeapPtr<HeapItem>) {
        self.visit_common(ptr);
    }

    /// Visit a weakly held pointer.
    fn visit_weak(&mut self, ptr: &mut HeapPtr<HeapItem>) {
        self.visit_common(ptr);
    }

    /// Visit any pointer (which may be either strong or weak).
    fn visit_common(&mut self, _ptr: &mut HeapPtr<HeapItem>) {}

    /// Visit a pointer a Rust vtable.
    fn visit_rust_vtable_pointer(&mut self, _ptr: &mut *const ()) {}

    /// Visit a strongly held pointer of any type.
    #[inline]
    fn visit_pointer<T>(&mut self, ptr: &mut HeapPtr<T>) {
        unsafe { self.visit(transmute::<&mut HeapPtr<T>, &mut HeapPtr<HeapItem>>(ptr)) };
    }

    /// Visit a weakly held pointer of any type.
    #[inline]
    fn visit_weak_pointer<T>(&mut self, ptr: &mut HeapPtr<T>) {
        unsafe { self.visit_weak(transmute::<&mut HeapPtr<T>, &mut HeapPtr<HeapItem>>(ptr)) };
    }

    /// Visit an optional strongly held pointer.
    #[inline]
    fn visit_pointer_opt<T>(&mut self, ptr: &mut Option<HeapPtr<T>>) {
        if ptr.is_some() {
            unsafe {
                self.visit(transmute::<&mut Option<HeapPtr<T>>, &mut HeapPtr<HeapItem>>(ptr))
            };
        }
    }

    /// Visit a strongly held value.
    #[inline]
    fn visit_value(&mut self, value: &mut Value) {
        if value.is_pointer() {
            unsafe { self.visit(transmute::<&mut Value, &mut HeapPtr<HeapItem>>(value)) };
        }
    }

    /// Visit a weakly held value.
    #[inline]
    fn visit_weak_value(&mut self, value: &mut Value) {
        if value.is_pointer() {
            unsafe { self.visit_weak(transmute::<&mut Value, &mut HeapPtr<HeapItem>>(value)) };
        }
    }

    /// Visit a strongly held property key.
    #[inline]
    fn visit_property_key(&mut self, property_key: &mut PropertyKey) {
        unsafe { self.visit_value(transmute::<&mut PropertyKey, &mut Value>(property_key)) };
    }
}
