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
    fn visit(&mut self, ptr: &mut HeapPtr<HeapItem>);

    #[inline]
    fn visit_pointer<T>(&mut self, ptr: &mut HeapPtr<T>) {
        unsafe { self.visit(transmute::<&mut HeapPtr<T>, &mut HeapPtr<HeapItem>>(ptr)) };
    }

    #[inline]
    fn visit_pointer_opt<T>(&mut self, ptr: &mut Option<HeapPtr<T>>) {
        if ptr.is_some() {
            unsafe {
                self.visit(transmute::<&mut Option<HeapPtr<T>>, &mut HeapPtr<HeapItem>>(ptr))
            };
        }
    }

    #[inline]
    fn visit_value(&mut self, value: &mut Value) {
        if value.is_pointer() {
            unsafe { self.visit(transmute::<&mut Value, &mut HeapPtr<HeapItem>>(value)) };
        }
    }

    #[inline]
    fn visit_property_key(&mut self, property_key: &mut PropertyKey) {
        unsafe { self.visit_value(transmute::<&mut PropertyKey, &mut Value>(property_key)) };
    }
}
