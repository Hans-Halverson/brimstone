use std::ptr::NonNull;

use crate::js::runtime::{
    gc::Heap,
    intrinsics::{
        weak_map_object::WeakMapObject, weak_ref_constructor::WeakRefObject,
        weak_set_object::WeakSetObject,
    },
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::ObjectValue,
    Context, Value,
};

use super::{HeapItem, HeapObject, HeapPtr, HeapVisitor};

/// A Cheney-style semispace garbage collector. Partitions heap into from-space and to-space, and
/// copies from one partition to the other on each GC.
pub struct GarbageCollector {
    // Bound of the region we are copying from
    from_space_start_ptr: *const u8,
    from_space_end_ptr: *const u8,

    // Bounds of the region we are copying to
    to_space_start_ptr: *const u8,
    to_space_end_ptr: *const u8,

    // Pointer to the next to-space object to fix
    fix_ptr: *const u8,

    // Pointer to the end of the allocated objects in to-space. Is the address of the next allocated
    // object.
    alloc_ptr: *const u8,

    // Chain of pointers to all WeakRefs that have been visited during this gc cycle
    weak_ref_list: Option<HeapPtr<WeakRefObject>>,

    // Chain of pointers to all WeakSets that have been visited during this gc cycle
    weak_set_list: Option<HeapPtr<WeakSetObject>>,

    // Chain of pointers to all WeakMaps that have been visited during this gc cycle
    weak_map_list: Option<HeapPtr<WeakMapObject>>,
}

impl GarbageCollector {
    fn new(cx: &mut Context) -> GarbageCollector {
        let (from_space_start_ptr, from_space_end_ptr) = cx.heap.current_heap_bounds();
        let (to_space_start_ptr, to_space_end_ptr) = cx.heap.next_heap_bounds();

        GarbageCollector {
            from_space_start_ptr,
            from_space_end_ptr,
            to_space_start_ptr,
            to_space_end_ptr,
            fix_ptr: to_space_start_ptr,
            alloc_ptr: to_space_start_ptr,
            weak_ref_list: None,
            weak_set_list: None,
            weak_map_list: None,
        }
    }

    pub fn run(cx: &mut Context) {
        let mut gc = Self::new(cx);

        // First visit roots and copy their objects to the from-heap
        cx.visit_roots(&mut gc);

        loop {
            // Then start fixing all pointers in each new heap object until fix pointer catches up with
            // alloc pointer, meaning all live objects have been copied and pointers have been fixed.
            while gc.fix_ptr < gc.alloc_ptr {
                let mut new_heap_item = HeapPtr::from_ptr(gc.fix_ptr.cast_mut()).cast::<HeapItem>();
                new_heap_item.visit_pointers(&mut gc);

                // Increment fix pointer to point to next new heap object
                let alloc_size = Heap::alloc_size_for_request_size(new_heap_item.byte_size());
                unsafe { gc.fix_ptr = gc.fix_ptr.add(alloc_size) }
            }

            // Visit weak map values that are known to be live due to live keys. Keep iterating
            // until fixpoint where no new live keys are found.
            if !gc.visit_live_weak_map_entries() {
                break;
            }
        }

        // Fix the weak objects, handling objects that have been garbage collected
        gc.fix_weak_refs();
        gc.fix_weak_sets();
        gc.fix_weak_maps();

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

        // Type specific actions for live objects. All weak objects must be collected in lists so
        // they can be traversed later.
        match new_heap_item.descriptor().kind() {
            ObjectKind::WeakRefObject => {
                self.add_visited_weak_ref(new_heap_item.cast::<WeakRefObject>())
            }
            ObjectKind::WeakSetObject => {
                self.add_visited_weak_set(new_heap_item.cast::<WeakSetObject>())
            }
            ObjectKind::WeakMapObject => {
                self.add_visited_weak_map(new_heap_item.cast::<WeakMapObject>())
            }
            _ => {}
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

    #[inline]
    fn is_in_to_space(&self, ptr: *const u8) -> bool {
        self.to_space_start_ptr <= ptr && ptr < self.to_space_end_ptr
    }

    // Add a weak ref to the linked list of weak refs that are live during this garbage collection.
    fn add_visited_weak_ref(&mut self, mut weak_ref: HeapPtr<WeakRefObject>) {
        if let Some(next_weak_ref) = self.weak_ref_list {
            weak_ref.set_next_weak_ref(next_weak_ref);
        }

        self.weak_ref_list = Some(weak_ref);
    }

    // Add a weak set to the linked list of weak sets that are live during this garbage collection.
    fn add_visited_weak_set(&mut self, mut weak_set: HeapPtr<WeakSetObject>) {
        if let Some(next_weak_set) = self.weak_set_list {
            weak_set.set_next_weak_set(next_weak_set);
        }

        self.weak_set_list = Some(weak_set);
    }

    // Add a weak map to the linked list of weak map that are live during this garbage collection.
    fn add_visited_weak_map(&mut self, mut weak_map: HeapPtr<WeakMapObject>) {
        if let Some(next_weak_map) = self.weak_map_list {
            weak_map.set_next_weak_map(next_weak_map);
        }

        self.weak_map_list = Some(weak_map);
    }

    // Visit live weak refs and check if their targets are still live. Should only be called after
    // liveness of all objects have been determined.
    fn fix_weak_refs(&mut self) {
        // Walk all live weak refs in the list
        let mut next_weak_ref = self.weak_ref_list;
        while let Some(mut weak_ref) = next_weak_ref {
            let target = weak_ref.weak_ref_target();
            if target.is_pointer() {
                let target_descriptor = target.as_pointer().descriptor();

                // Target is known to be live if it has already moved (and left a forwarding pointer)
                if let Some(forwarding_ptr) = decode_forwarding_pointer(target_descriptor) {
                    weak_ref.set_weak_ref_target(forwarding_ptr.cast::<ObjectValue>().into());
                } else {
                    // Otherwise target was garbage collected so reset target to undefined
                    weak_ref.set_weak_ref_target(Value::undefined());
                }
            }

            next_weak_ref = weak_ref.next_weak_ref();
        }
    }

    // Visit live weak sets and check if their targets are still live. Should only be called after
    // liveness of all objects have been determined.
    fn fix_weak_sets(&mut self) {
        // Walk all live weak sets in the list
        let mut next_weak_set = self.weak_set_list;
        while let Some(weak_set) = next_weak_set {
            for weak_ref in weak_set.weak_set_data().iter_mut_gc_unsafe() {
                let weak_ref_value = weak_ref.value_mut();
                let weak_ref_descriptor = weak_ref_value.as_pointer().descriptor();

                // Value is known to be live if it has already moved (and left a forwarding pointer)
                if let Some(forwarding_ptr) = decode_forwarding_pointer(weak_ref_descriptor) {
                    *weak_ref_value = forwarding_ptr.cast::<ObjectValue>().into();
                } else {
                    // Otherwise value was garbage collected so remove value from set.
                    // It is safe to remove during iteration for a BsHashSet.
                    weak_set.weak_set_data().remove(weak_ref);
                }
            }

            next_weak_set = weak_set.next_weak_set();
        }
    }

    // Visit live weak maps and check if their keys are still live. Should only be called after
    // liveness of all objects have been determined.
    fn fix_weak_maps(&mut self) {
        // Walk all live weak maps in the list
        let mut next_weak_map = self.weak_map_list;
        while let Some(weak_map) = next_weak_map {
            for (weak_key, _) in weak_map.weak_map_data().iter_mut_gc_unsafe() {
                let weak_key_value = weak_key.value_mut();

                // Check if key has already been visited and moved to the to-space. If not, key
                // should be garbage collected so remove entry from map. It is sfae to remove
                // during iteration for a BsHashMap.
                if !self.is_in_to_space(weak_key_value.as_pointer().as_ptr().cast()) {
                    weak_map.weak_map_data().remove(weak_key);
                }
            }

            next_weak_map = weak_map.next_weak_map();
        }
    }

    // Visit live weak maps and check if their keys are still live. Visit the values associted with
    // all lives keys. Return whether any new live keys were found.
    fn visit_live_weak_map_entries(&mut self) -> bool {
        // Walk all live weak maps in the list
        let mut next_weak_map = self.weak_map_list;
        let mut found_new_live_key = false;

        while let Some(weak_map) = next_weak_map {
            for (weak_key, value) in weak_map.weak_map_data().iter_mut_gc_unsafe() {
                let weak_key_value = weak_key.value_mut();

                // Check if key has already been visited and moved to the to-space
                if !self.is_in_to_space(weak_key_value.as_pointer().as_ptr().cast()) {
                    let weak_key_descriptor = weak_key_value.as_pointer().descriptor();

                    // Key is known to be live if it has already moved and left a forwarding
                    // pointer. Visit the value associated with the key since we now know it is
                    // live. Also update key to point to to-space so we can tell it is visited.
                    if let Some(forwarding_ptr) = decode_forwarding_pointer(weak_key_descriptor) {
                        *weak_key_value = forwarding_ptr.cast::<ObjectValue>().into();
                        found_new_live_key = true;
                        self.visit_value(value);
                    }
                }
            }

            next_weak_map = weak_map.next_weak_map();
        }

        found_new_live_key
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
