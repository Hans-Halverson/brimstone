use std::mem::transmute;

use crate::runtime::{PropertyKey, Value};

use super::{AnyHeapItem, HeapPtr};

pub trait HeapVisitor {
    /// Visit a strongly held pointer.
    fn visit(&mut self, ptr: &mut HeapPtr<AnyHeapItem>) {
        self.visit_common(ptr);
    }

    /// Visit a weakly held pointer.
    fn visit_weak(&mut self, ptr: &mut HeapPtr<AnyHeapItem>) {
        self.visit_common(ptr);
    }

    /// Visit any pointer (which may be either strong or weak).
    fn visit_common(&mut self, _ptr: &mut HeapPtr<AnyHeapItem>) {}

    /// Visit a pointer a Rust vtable.
    fn visit_rust_vtable_pointer(&mut self, _ptr: &mut *const ()) {}

    /// Visit a strongly held pointer of any type.
    #[inline]
    fn visit_pointer<T>(&mut self, ptr: &mut HeapPtr<T>) {
        unsafe { self.visit(transmute::<&mut HeapPtr<T>, &mut HeapPtr<AnyHeapItem>>(ptr)) };
    }

    /// Visit a weakly held pointer of any type.
    #[inline]
    fn visit_weak_pointer<T>(&mut self, ptr: &mut HeapPtr<T>) {
        unsafe { self.visit_weak(transmute::<&mut HeapPtr<T>, &mut HeapPtr<AnyHeapItem>>(ptr)) };
    }

    /// Visit an optional strongly held pointer.
    #[inline]
    fn visit_pointer_opt<T>(&mut self, ptr: &mut Option<HeapPtr<T>>) {
        if ptr.is_some() {
            unsafe {
                self.visit(transmute::<&mut Option<HeapPtr<T>>, &mut HeapPtr<AnyHeapItem>>(ptr))
            };
        }
    }

    /// Visit a strongly held value.
    #[inline]
    fn visit_value(&mut self, value: &mut Value) {
        if value.is_pointer() {
            unsafe { self.visit(transmute::<&mut Value, &mut HeapPtr<AnyHeapItem>>(value)) };
        }
    }

    /// Visit a weakly held value.
    #[inline]
    fn visit_weak_value(&mut self, value: &mut Value) {
        if value.is_pointer() {
            unsafe { self.visit_weak(transmute::<&mut Value, &mut HeapPtr<AnyHeapItem>>(value)) };
        }
    }

    /// Visit a strongly held property key.
    #[inline]
    fn visit_property_key(&mut self, property_key: &mut PropertyKey) {
        unsafe { self.visit_value(transmute::<&mut PropertyKey, &mut Value>(property_key)) };
    }
}
