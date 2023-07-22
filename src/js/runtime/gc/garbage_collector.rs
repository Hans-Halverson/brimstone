use std::ptr::NonNull;

use crate::js::runtime::{gc::Heap, object_descriptor::ObjectDescriptor, Context};

use super::{HeapItem, HeapObject, HeapPtr, HeapVisitor};

/// A Cheney-style semispace garbage collector. Partitions heap into from-space and to-space, and
/// copies from one partition to the other on each GC.
pub struct GarbageCollector {
    // Bound of the space we are copying from
    from_space_start_ptr: *const u8,
    from_space_end_ptr: *const u8,

    // Pointer to the next to-space object to fix
    fix_ptr: *const u8,

    // Pointer to the end of the allocated objects in to-space. Is the address of the next allocated
    // object.
    alloc_ptr: *const u8,
}

impl GarbageCollector {
    fn new(cx: &mut Context) -> GarbageCollector {
        let (from_space_start_ptr, from_space_end_ptr) = cx.heap.current_heap_bounds();
        let (to_space_start_ptr, _) = cx.heap.next_heap_bounds();

        GarbageCollector {
            from_space_start_ptr,
            from_space_end_ptr,
            fix_ptr: to_space_start_ptr,
            alloc_ptr: to_space_start_ptr,
        }
    }

    pub fn run(cx: &mut Context) {
        let mut gc = Self::new(cx);

        // First visit roots and copy their objects to the from-heap
        cx.visit_roots(&mut gc);

        // Then start fixing all pointers in each new heap object until fix pointer catches up with
        // alloc pointer, meaning all live objects have been copied and pointers have been fixed.
        while gc.fix_ptr < gc.alloc_ptr {
            let mut new_heap_item = HeapPtr::from_ptr(gc.fix_ptr.cast_mut()).cast::<HeapItem>();
            new_heap_item.visit_pointers(&mut gc);

            // Increment fix pointer to point to next new heap object
            let alloc_size = Heap::alloc_size_for_request_size(new_heap_item.byte_size());
            unsafe { gc.fix_ptr = gc.fix_ptr.add(alloc_size) }
        }

        cx.heap.swap_heaps(gc.alloc_ptr);
    }

    #[inline]
    fn copy_or_fix_pointer(&mut self, heap_item: &mut HeapPtr<HeapItem>) {
        // Pointers may either point outside the heap or have already been copied
        if !self.is_in_from_space(heap_item.as_ptr() as *const _ as *const u8) {
            // The only other valid pointers are the null pointer and the const dangling pointer.
            debug_assert!(
                heap_item.as_ptr() == NonNull::dangling().as_ptr()
                    || heap_item.as_ptr() as usize == 0
            );

            return;
        }

        // Every heap item has a known descriptor
        let descriptor = heap_item.descriptor();

        // If descriptor is actually a forwarding pointer then simply rewrite pointer to old heap
        // object to instead point to new heap object.
        if let Some(forwarding_ptr) = decode_forwarding_pointer(descriptor) {
            *heap_item = forwarding_ptr;
            return;
        }

        // Calculate size of heap object. We are guaranteed to have enough space in the from-space
        // since all allocations fit into the to-space.
        let alloc_size = Heap::alloc_size_for_request_size(heap_item.byte_size());

        // Copy object from old to new heap, and bump alloc_ptr to point past new allocation
        let new_heap_item = HeapPtr::from_ptr(self.alloc_ptr.cast_mut()).cast::<HeapItem>();
        unsafe {
            self.alloc_ptr = self.alloc_ptr.add(alloc_size);
            std::ptr::copy_nonoverlapping::<u8>(
                heap_item.as_ptr().cast(),
                new_heap_item.as_ptr().cast(),
                alloc_size,
            );
        }

        // Overwrite the descriptor inside the old heap object to be a forwarding pointer to
        // new heap object.
        let forwarding_pointer = encode_forwarding_pointer(new_heap_item);
        heap_item.set_descriptor(forwarding_pointer);

        // Rewrite pointer to old heap object to instead point to new heap object
        *heap_item = new_heap_item;
    }

    #[inline]
    fn is_in_from_space(&self, ptr: *const u8) -> bool {
        self.from_space_start_ptr <= ptr && ptr < self.from_space_end_ptr
    }
}

impl HeapVisitor for GarbageCollector {
    fn visit(&mut self, ptr: &mut HeapPtr<HeapItem>) {
        self.copy_or_fix_pointer(ptr);
    }
}

// The first item in each heap object is a pointer, is either:
//   - A pointer to the object descriptor if this object has not yet been copied to the from space
//   - A forwarding pointer to the address in the from space the object has been copied to
//
// Tag lowest bit of pointer to signal a forwarding pointer
const FORWARDING_POINTER_TAG: usize = 0x1;

fn decode_forwarding_pointer(descriptor: HeapPtr<ObjectDescriptor>) -> Option<HeapPtr<HeapItem>> {
    let ptr_bits = descriptor.as_ptr() as usize;
    if ptr_bits & FORWARDING_POINTER_TAG == FORWARDING_POINTER_TAG {
        return Some(HeapPtr::from_ptr((ptr_bits ^ FORWARDING_POINTER_TAG) as *mut HeapItem));
    }

    None
}

fn encode_forwarding_pointer(heap_ptr: HeapPtr<HeapItem>) -> HeapPtr<ObjectDescriptor> {
    let ptr_bits = heap_ptr.as_ptr() as usize;
    HeapPtr::from_ptr((ptr_bits | FORWARDING_POINTER_TAG) as *mut ObjectDescriptor)
}
