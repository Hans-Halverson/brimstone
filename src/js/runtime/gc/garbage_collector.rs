use std::{ops::Range, ptr::NonNull};

use crate::{
    common::constants::MAX_HEAP_SIZE,
    runtime::{
        gc::Heap,
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
        interned_strings::InternedStrings,
        intrinsics::{
            finalization_registry_object::FinalizationRegistryObject,
            weak_map_object::WeakMapObject, weak_ref_constructor::WeakRefObject,
            weak_set_object::WeakSetObject,
        },
        string_value::FlatString,
        Context, Value,
    },
};

use super::{AnyHeapItem, HeapItem, HeapPtr, HeapVisitor};

/// A Cheney-style semispace garbage collector. Partitions heap into from-space and to-space, and
/// copies from one partition to the other on each GC.
pub struct GarbageCollector {
    // Type of GC to run. Normal or grow.
    type_: GcType,

    // Bounds of the space we are copying from
    from_space_bounds: Range<*const u8>,

    // Old bounds of the permanent space. For non-resizing GCs this is the same as the new bounds.
    old_permanent_space_bounds: Range<*const u8>,

    // New bounds of the permanent space. For non-resizing GCs this is the same as the old bounds.
    new_permanent_space_bounds: Range<*const u8>,

    // Pointer to the next to-space item to fix
    fix_ptr: *const u8,

    // Pointer to the end of the allocated items in to-space. Is the address of the next allocated
    // item.
    alloc_ptr: *const u8,

    // Chain of pointers to all WeakRefs that have been visited during this gc cycle
    weak_ref_list: Option<HeapPtr<WeakRefObject>>,

    // Chain of pointers to all WeakSets that have been visited during this gc cycle
    weak_set_list: Option<HeapPtr<WeakSetObject>>,

    // Chain of pointers to all WeakMaps that have been visited during this gc cycle
    weak_map_list: Option<HeapPtr<WeakMapObject>>,

    // Chain of pointers to all FinalizationRegistries that have been visited during this gc cycle
    finalization_registry_list: Option<HeapPtr<FinalizationRegistryObject>>,
}

#[derive(Clone, Debug)]
pub enum GcType {
    /// Normal GC, swap semispaces in the heap.
    Normal,
    /// Grow the heap during this GC. Optionally specify an allocation size that must be supported.
    Grow { alloc_size: Option<usize> },
}

impl GarbageCollector {
    fn new(cx: Context, type_: GcType, new_heap: Option<&Heap>) -> GarbageCollector {
        let dest_heap = new_heap.unwrap_or(&cx.heap);

        // The to-space is either the next heap for a non-resizing GC, or the new starting
        // (aka current) heap for a resizing GC.
        let to_space_start_ptr = match new_heap {
            None => cx.heap.next_heap_bounds().start,
            Some(new_heap) => new_heap.current_heap_bounds().start,
        };

        GarbageCollector {
            type_,
            from_space_bounds: cx.heap.current_heap_bounds(),
            old_permanent_space_bounds: cx.heap.permanent_heap_bounds(),
            new_permanent_space_bounds: dest_heap.permanent_heap_bounds(),
            fix_ptr: to_space_start_ptr,
            alloc_ptr: to_space_start_ptr,
            weak_ref_list: None,
            weak_set_list: None,
            weak_map_list: None,
            finalization_registry_list: None,
        }
    }

    pub fn run(mut cx: Context, type_: GcType) {
        // Create the new heap size if this is a resizing GC
        let new_heap_size = Self::calculate_new_heap_size(cx, &type_);
        let new_heap = new_heap_size.map(|size| Heap::new_for_resize(&cx.heap, size));

        let mut gc = Self::new(cx, type_, new_heap.as_ref());

        // If resizing first move the entire permanent space from the old to new heap
        if gc.is_resizing() {
            gc.move_permanent_heap();
        }

        // Visit all roots and move them to the to-heap
        gc.visit_all_roots(cx);

        // Then use set of moved roots to find all remaining live items and move them to the to-heap
        gc.move_all_live_items();

        // All live items have been identified and moved. We can now prune dead weak references and
        // handle finalizers.
        gc.prune_weak_reference_and_handle_finalizers(cx);

        // In GC stress test mode, overwrite the old heap with 0x01 bytes to try to catch reads from
        // pointers to the old heap.
        #[cfg(feature = "gc_stress_test")]
        {
            if cx.heap.gc_stress_test && matches!(gc.type_, GcType::Normal) {
                let start = cx.heap.current_heap_bounds().start;
                unsafe {
                    std::ptr::write_bytes(start.cast_mut(), 0x01, cx.heap.bytes_allocated());
                }
            }
        }

        match new_heap {
            // Normal GCs can simply swap the semispaces in the heap
            None => cx.heap.swap_heaps(gc.alloc_ptr),
            // If resizing then we must replace the old heap with the new heap
            Some(mut new_heap) => {
                new_heap.finish_resized_heap(gc.alloc_ptr);
                cx.heap = new_heap;
            }
        }
    }

    /// Whether this is a resizing GC.
    pub fn is_resizing(&self) -> bool {
        matches!(self.type_, GcType::Grow { .. })
    }

    /// Calculate the new heap size if this should be a resizing GC. Otherwise return None.
    fn calculate_new_heap_size(cx: Context, type_: &GcType) -> Option<usize> {
        if let GcType::Grow { alloc_size } = &type_ {
            // Double the size of the heap by default
            let mut new_heap_size = cx.heap.heap_size() * 2;

            if let Some(alloc_size) = alloc_size {
                if alloc_size * 2 > cx.heap.heap_size() {
                    new_heap_size = (alloc_size * 2).next_power_of_two();
                }
            }

            new_heap_size = new_heap_size.min(MAX_HEAP_SIZE);

            Some(new_heap_size)
        } else {
            None
        }
    }

    /// Visit all roots, moving roots to the to-space and fixing pointers to the root to reflect the
    /// new location.
    ///
    /// Roots may appear in the context or in the permanent space.
    fn visit_all_roots(&mut self, mut cx: Context) {
        cx.visit_roots_for_gc(self);
        self.visit_permanent_heap_roots();
    }

    /// Visit all the root items in the permanent heap, moving the roots to the to-heap and fixing
    /// pointers in the permanent heap to point to the new location of the root.
    ///
    /// The permanent heap may contain pointers into the semispaces if a permanent heap item was
    /// mutated.
    fn visit_permanent_heap_roots(&mut self) {
        let mut current_ptr = self.new_permanent_space_bounds.start;
        let end_ptr = self.new_permanent_space_bounds.end;

        while current_ptr < end_ptr {
            let mut permanent_heap_item =
                HeapPtr::from_ptr(current_ptr.cast_mut()).cast::<AnyHeapItem>();
            permanent_heap_item.visit_pointers(self);

            // Increment current pointer to point to next permanent heap item
            let alloc_size = Heap::alloc_size_for_request_size(permanent_heap_item.byte_size());
            unsafe { current_ptr = current_ptr.add(alloc_size) }
        }
    }

    /// Move all remaining live items from the from-heap to the to-heap, fixing pointers to point to
    /// the to-heap.
    ///
    /// Iterates until all live items have been discovered. Liveness includes ephemeron handling.
    fn move_all_live_items(&mut self) {
        loop {
            // Then start fixing all pointers in each new heap item until fix pointer catches up
            // with alloc pointer, meaning all live obwitemsjects have been copied and pointers have
            // been fixed.
            while self.fix_ptr < self.alloc_ptr {
                let mut new_heap_item =
                    HeapPtr::from_ptr(self.fix_ptr.cast_mut()).cast::<AnyHeapItem>();
                new_heap_item.visit_pointers(self);

                // Increment fix pointer to point to next new heap item
                let alloc_size = Heap::alloc_size_for_request_size(new_heap_item.byte_size());
                unsafe { self.fix_ptr = self.fix_ptr.add(alloc_size) }
            }

            // Visit ephemeron values that are known to be live due to live keys. Keep iterating
            // until fixpoint where no new live keys are found.
            if !self.visit_live_weak_map_entries() && !self.visit_live_finalization_registry_cells()
            {
                break;
            }
        }
    }

    /// Move the entire contents of the permanent heap from the old bounds to the new bounds.
    fn move_permanent_heap(&self) {
        let old_permanent_bounds = self.old_permanent_space_bounds.clone();
        let new_permanent_bounds = self.new_permanent_space_bounds.clone();

        let mut current_ptr = old_permanent_bounds.start;
        let mut dest_ptr = new_permanent_bounds.start;

        while current_ptr < old_permanent_bounds.end {
            // Move the old permanent heap item to the new permanent heap
            let mut heap_item = HeapPtr::from_ptr(current_ptr.cast_mut()).cast::<AnyHeapItem>();
            let (_, alloc_size) = Self::move_heap_item(&mut heap_item, &mut dest_ptr);

            // Increment pointer to point to next old permanent heap item
            current_ptr = unsafe { current_ptr.add(alloc_size) }
        }

        // Entire old permanent heap should have been copied 1:1 to the new permanent heap
        debug_assert!(dest_ptr == new_permanent_bounds.end);
    }

    #[inline]
    fn copy_or_fix_pointer(&mut self, heap_item: &mut HeapPtr<AnyHeapItem>) {
        let heap_item_ptr = heap_item.as_ptr() as *const _ as *const u8;

        // We only need to visit pointers in a space that has had its contents moved. This is
        // normally the to-space but may also include the permanent space if resizing.
        if !self.is_in_moved_space(heap_item_ptr) {
            // The only valid pointers are either null, uninitialized (aka NonNull::dangling()), or
            // pointers to the new permanent space.
            debug_assert!(
                self.is_in_new_permanent_space(heap_item_ptr)
                    || heap_item.as_ptr() == NonNull::dangling().as_ptr()
                    || heap_item.as_ptr() as usize == 0
            );

            return;
        }

        // Every heap item has a known descriptor
        let descriptor = heap_item.descriptor();

        // If descriptor is actually a forwarding pointer then simply rewrite pointer to old heap
        // item to instead point to new heap item.
        if let Some(forwarding_ptr) = decode_forwarding_pointer(descriptor) {
            *heap_item = forwarding_ptr;
            return;
        }

        // Move the heap item to the to-space. We are guaranteed to have enough space in the
        // to-space since all allocations fit into the from-space.
        let (new_heap_item, _) = Self::move_heap_item(heap_item, &mut self.alloc_ptr);

        // Rewrite pointer to old heap item to instead point to new heap item
        *heap_item = new_heap_item;

        // Type specific actions for live heap items. All weak objects must be collected in lists so
        // they can be traversed later.
        match new_heap_item.descriptor().kind() {
            HeapItemKind::WeakRefObject => {
                self.add_visited_weak_ref(new_heap_item.cast::<WeakRefObject>())
            }
            HeapItemKind::WeakSetObject => {
                self.add_visited_weak_set(new_heap_item.cast::<WeakSetObject>())
            }
            HeapItemKind::WeakMapObject => {
                self.add_visited_weak_map(new_heap_item.cast::<WeakMapObject>())
            }
            HeapItemKind::FinalizationRegistryObject => self.add_visited_finalization_registry(
                new_heap_item.cast::<FinalizationRegistryObject>(),
            ),
            _ => {}
        }
    }

    /// Move a heap item to a destination address. Leaves a forwarding pointer at the old heap item
    /// old heap item. Updates the destination address to point to the next heap-item-aligned
    /// address after the new heap item.
    ///
    /// Return the new heap item and the allocation size for the heap item.
    #[inline]
    fn move_heap_item(
        heap_item: &mut HeapPtr<AnyHeapItem>,
        dest_ptr: &mut *const u8,
    ) -> (HeapPtr<AnyHeapItem>, usize) {
        // Calculate size of heap item. Caller must ensure that there is enough space at the
        // destination to hold the moved item.
        let alloc_size = Heap::alloc_size_for_request_size(heap_item.byte_size());

        // Copy item from old to new heap, and bump alloc_ptr to point past new allocation
        let new_heap_item = HeapPtr::from_ptr(dest_ptr.cast_mut()).cast::<AnyHeapItem>();
        unsafe {
            std::ptr::copy_nonoverlapping::<u8>(
                heap_item.as_ptr().cast(),
                new_heap_item.as_ptr().cast(),
                alloc_size,
            );
        }

        // Overwrite the descriptor inside the old heap item to be a forwarding pointer to
        // new heap item.
        let forwarding_pointer = encode_forwarding_pointer(new_heap_item);
        heap_item.set_descriptor(forwarding_pointer);

        // Bump the `dest_ptr` to point to the next aligned address after the new heap item
        unsafe { *dest_ptr = dest_ptr.add(alloc_size) };

        (new_heap_item, alloc_size)
    }

    #[inline]
    fn is_in_from_space(&self, ptr: *const u8) -> bool {
        self.from_space_bounds.contains(&ptr)
    }

    #[inline]
    fn is_in_old_permanent_space(&self, ptr: *const u8) -> bool {
        self.old_permanent_space_bounds.contains(&ptr)
    }

    #[inline]
    fn is_in_new_permanent_space(&self, ptr: *const u8) -> bool {
        self.new_permanent_space_bounds.contains(&ptr)
    }

    /// Whether a pointer is in a space that was moved. This includes the from space, as well as the
    /// permanent space if the GC is a growing GC.
    #[inline]
    fn is_in_moved_space(&self, ptr: *const u8) -> bool {
        self.is_in_from_space(ptr) || (self.is_resizing() && self.is_in_old_permanent_space(ptr))
    }

    // Add a weak ref to the linked list of weak refs that are live during this garbage collection.
    fn add_visited_weak_ref(&mut self, mut weak_ref: HeapPtr<WeakRefObject>) {
        weak_ref.set_next_weak_ref(self.weak_ref_list);
        self.weak_ref_list = Some(weak_ref);
    }

    // Add a weak set to the linked list of weak sets that are live during this garbage collection.
    fn add_visited_weak_set(&mut self, mut weak_set: HeapPtr<WeakSetObject>) {
        weak_set.set_next_weak_set(self.weak_set_list);
        self.weak_set_list = Some(weak_set);
    }

    // Add a weak map to the linked list of weak map that are live during this garbage collection.
    fn add_visited_weak_map(&mut self, mut weak_map: HeapPtr<WeakMapObject>) {
        weak_map.set_next_weak_map(self.weak_map_list);
        self.weak_map_list = Some(weak_map);
    }

    // Add a finalization registry to the linked list of finalization registries that are live
    // during this garbage collection.
    fn add_visited_finalization_registry(
        &mut self,
        mut finalization_registry: HeapPtr<FinalizationRegistryObject>,
    ) {
        finalization_registry.set_next_finalization_registry(self.finalization_registry_list);
        self.finalization_registry_list = Some(finalization_registry);
    }

    fn prune_weak_reference_and_handle_finalizers(&mut self, cx: Context) {
        // Prune interned strings that are no longer referenced
        self.prune_weak_interned_strings(cx);

        // Fix the weak objects, handling objects that have been garbage collected
        self.fix_weak_refs();
        self.fix_weak_sets();
        self.fix_weak_maps();
        self.fix_finalization_registries(cx);
    }

    // Visit live weak refs and check if their targets are still live. Should only be called after
    // liveness of all items has been determined.
    fn fix_weak_refs(&mut self) {
        // Walk all live weak refs in the list
        let mut next_weak_ref = self.weak_ref_list;
        while let Some(mut weak_ref) = next_weak_ref {
            let target = weak_ref.weak_ref_target();

            debug_assert!(target.is_pointer());
            let target_ptr = target.as_pointer().as_ptr().cast();

            if self.is_in_moved_space(target_ptr) {
                let target_descriptor = target.as_pointer().descriptor();

                // Target is known to be live if it has already moved (and left a forwarding pointer)
                if let Some(forwarding_ptr) = decode_forwarding_pointer(target_descriptor) {
                    weak_ref.set_weak_ref_target(Value::heap_item(forwarding_ptr));
                } else {
                    // Otherwise target was garbage collected so reset target to undefined
                    weak_ref.set_weak_ref_target(Value::undefined());
                }
            }

            next_weak_ref = weak_ref.next_weak_ref();
        }
    }

    // Visit live weak sets and check if their targets are still live. Should only be called after
    // liveness of all items has been determined.
    fn fix_weak_sets(&mut self) {
        // Walk all live weak sets in the list
        let mut next_weak_set = self.weak_set_list;
        while let Some(weak_set) = next_weak_set {
            for weak_ref in weak_set.weak_set_data().iter_mut_gc_unsafe() {
                let weak_ref_value = weak_ref.value_mut();

                debug_assert!(weak_ref_value.is_pointer());
                let weak_ref_value_ptr = weak_ref_value.as_pointer().as_ptr().cast();

                if self.is_in_moved_space(weak_ref_value_ptr) {
                    let weak_ref_descriptor = weak_ref_value.as_pointer().descriptor();

                    // Value is known to be live if it has already moved (and left a forwarding pointer)
                    if let Some(forwarding_ptr) = decode_forwarding_pointer(weak_ref_descriptor) {
                        *weak_ref_value = Value::heap_item(forwarding_ptr);
                    } else {
                        // Otherwise value was garbage collected so remove value from set.
                        // It is safe to remove during iteration for a BsHashSet.
                        weak_set.weak_set_data().remove(weak_ref);
                    }
                }
            }

            next_weak_set = weak_set.next_weak_set();
        }
    }

    // Visit live weak maps and check if their keys are still live. Should only be called after
    // liveness of all items has been determined.
    fn fix_weak_maps(&mut self) {
        // Walk all live weak maps in the list
        let mut next_weak_map = self.weak_map_list;
        while let Some(weak_map) = next_weak_map {
            for (weak_key, _) in weak_map.weak_map_data().iter_mut_gc_unsafe() {
                let weak_key_value = weak_key.value_mut();

                debug_assert!(weak_key_value.is_pointer());
                let weak_key_ptr = weak_key_value.as_pointer().as_ptr().cast();

                // Check if key was not live. If not, key should be garbage collected so remove
                // entry from map. It is safe to remove during iteration for a BsHashMap.
                if self.is_in_moved_space(weak_key_ptr) {
                    weak_map.weak_map_data().remove(weak_key);
                }
            }

            next_weak_map = weak_map.next_weak_map();
        }
    }

    // Visit live weak maps and check if their keys are still live. Visit the values associated with
    // all lives keys. Return whether any new live keys were found.
    fn visit_live_weak_map_entries(&mut self) -> bool {
        // Walk all live weak maps in the list
        let mut next_weak_map = self.weak_map_list;
        let mut found_new_live_value = false;

        while let Some(weak_map) = next_weak_map {
            for (weak_key, value) in weak_map.weak_map_data().iter_mut_gc_unsafe() {
                let weak_key_value = weak_key.value_mut();

                debug_assert!(weak_key_value.is_pointer());
                let weak_key_ptr = weak_key_value.as_pointer().as_ptr().cast();

                // Check if key has not yet been visited and moved to the to-space
                if self.is_in_moved_space(weak_key_ptr) {
                    let weak_key_descriptor = weak_key_value.as_pointer().descriptor();

                    // Key is known to be live if it has already moved and left a forwarding
                    // pointer. Visit the value associated with the key since we now know it is
                    // live. Also update key to point to to-space so we can tell it is visited.
                    if let Some(forwarding_ptr) = decode_forwarding_pointer(weak_key_descriptor) {
                        *weak_key_value = Value::heap_item(forwarding_ptr);
                        found_new_live_value = true;
                        self.visit_value(value);
                    }
                } else if self.is_in_old_permanent_space(weak_key_ptr) {
                    // Check if key was in the permanent space. If so its value is live and will
                    // always be visited.
                    //
                    // Note that we only need to visit the value if it is a pointer. Make sure to
                    // only count this as a new live value if it was not already moved.
                    if value.is_pointer()
                        && self.is_in_moved_space(value.as_pointer().as_ptr().cast())
                    {
                        let value_descriptor = value.as_pointer().descriptor();
                        if decode_forwarding_pointer(value_descriptor).is_none() {
                            found_new_live_value = true;
                        }

                        self.visit_value(value);
                    }
                }
            }

            next_weak_map = weak_map.next_weak_map();
        }

        found_new_live_value
    }

    // Finalization registries are effectively a WeakMap from target value to unregister tokens.
    // Visit live finalization registries and check if their cells are still live. Visit the
    // weakly held unregister tokens in each cell where the target is live. Return whether any newly
    // live unregister tokens were found.
    fn visit_live_finalization_registry_cells(&mut self) -> bool {
        // Walk all live weak maps in the list
        let mut next_finalization_registry = self.finalization_registry_list;
        let mut found_new_live_unregister_token = false;

        while let Some(finalization_registry) = next_finalization_registry {
            for cell in finalization_registry.cells().iter_mut_gc_unsafe().flatten() {
                let target_value = cell.target;

                debug_assert!(target_value.is_pointer());
                let target_ptr = target_value.as_pointer().as_ptr().cast();

                // Check if key has not yet been visited and moved
                if self.is_in_moved_space(target_ptr) {
                    let target_descriptor = target_value.as_pointer().descriptor();

                    // Target is known to be live if it has already moved and left a forwarding
                    // pointer. Visit the unregister key associated with the target since we now
                    // know it is live. Also update target to point to to-space so we can tell it
                    // is visited.
                    if let Some(forwarding_ptr) = decode_forwarding_pointer(target_descriptor) {
                        cell.target = Value::heap_item(forwarding_ptr);

                        if let Some(ref mut unregister_token) = cell.unregister_token {
                            found_new_live_unregister_token = true;
                            self.visit_value(unregister_token);
                        }
                    }
                } else if self.is_in_old_permanent_space(target_ptr) {
                    // Check if target was in the permanent space. If so its unregister token is
                    // live and will always be visited.
                    //
                    // Note that we only need to visit the token if it is a pointer. Make sure
                    // to only count this as a new live token if it was not already moved.
                    if let Some(ref mut unregister_token) = cell.unregister_token {
                        if unregister_token.is_pointer()
                            && self.is_in_moved_space(unregister_token.as_pointer().as_ptr().cast())
                        {
                            let token_descriptor = unregister_token.as_pointer().descriptor();
                            if decode_forwarding_pointer(token_descriptor).is_none() {
                                found_new_live_unregister_token = true;
                            }

                            self.visit_value(unregister_token);
                        }
                    }
                }
            }

            next_finalization_registry = finalization_registry.next_finalization_registry();
        }

        found_new_live_unregister_token
    }

    fn fix_finalization_registries(&mut self, mut cx: Context) {
        // Walk all live finalization registries in the list
        let mut next_finalization_registry = self.finalization_registry_list;
        while let Some(finalization_registry) = next_finalization_registry {
            for cell_opt in finalization_registry.cells().iter_mut_gc_unsafe() {
                if let Some(cell) = cell_opt {
                    debug_assert!(cell.target.is_pointer());
                    let target_ptr = cell.target.as_pointer().as_ptr().cast();

                    // Check if target has already been visited and moved. If not, target should be
                    // garbage collected so remove cell from registry.
                    if self.is_in_moved_space(target_ptr) {
                        // Enqueue cleanup callback, passing in the held value
                        cx.task_queue().enqueue_callback_1_task(
                            finalization_registry.cleanup_callback().into(),
                            cell.held_value,
                        );

                        // Then remove cell from registry
                        finalization_registry.cells().remove_cell(cell_opt);
                    }
                }
            }

            next_finalization_registry = finalization_registry.next_finalization_registry();
        }
    }

    fn prune_weak_interned_strings(&mut self, cx: Context) {
        // First check interned strings set
        let mut string_set = InternedStrings::strings(cx);
        for string_ref in string_set.clone().iter_mut_gc_unsafe() {
            let string_descriptor = string_ref.descriptor();

            if self.is_in_moved_space(string_ref.as_ptr().cast()) {
                // String is known to be live if it has already moved (and left a forwarding pointer)
                if let Some(forwarding_ptr) = decode_forwarding_pointer(string_descriptor) {
                    *string_ref = forwarding_ptr.cast::<FlatString>();
                } else {
                    // Otherwise string was garbage collected so remove string from set.
                    // It is safe to remove during iteration for a BsHashSet.
                    string_set.remove(string_ref);
                }
            }
        }

        // Then check values of interned strings map
        let mut cloned_cx = cx;
        let string_map = cloned_cx.interned_strings.generator_cache_mut();
        string_map.retain(|_, string_ref| {
            let string_descriptor = string_ref.descriptor();

            if self.is_in_moved_space(string_ref.as_ptr().cast()) {
                // String is known to be live if it has already moved (and left a forwarding pointer)
                if let Some(forwarding_ptr) = decode_forwarding_pointer(string_descriptor) {
                    *string_ref = forwarding_ptr.cast::<FlatString>();
                } else {
                    // Otherwise string was garbage collected so remove string from map
                    return false;
                }
            }

            true
        });
    }
}

impl HeapVisitor for GarbageCollector {
    fn visit(&mut self, ptr: &mut HeapPtr<AnyHeapItem>) {
        self.copy_or_fix_pointer(ptr);
    }
}

// The first item in each heap item is a pointer, is either:
//   - A pointer to the item's descriptor if this item has not yet been copied to the from space
//   - A forwarding pointer to the address in the from space the item has been copied to
//
// Tag lowest bit of pointer to signal a forwarding pointer
const FORWARDING_POINTER_TAG: usize = 0x1;

fn decode_forwarding_pointer(
    descriptor: HeapPtr<HeapItemDescriptor>,
) -> Option<HeapPtr<AnyHeapItem>> {
    let ptr_bits = descriptor.as_ptr() as usize;
    if ptr_bits & FORWARDING_POINTER_TAG == FORWARDING_POINTER_TAG {
        return Some(HeapPtr::from_ptr((ptr_bits ^ FORWARDING_POINTER_TAG) as *mut AnyHeapItem));
    }

    None
}

fn encode_forwarding_pointer(heap_ptr: HeapPtr<AnyHeapItem>) -> HeapPtr<HeapItemDescriptor> {
    let ptr_bits = heap_ptr.as_ptr() as usize;
    HeapPtr::from_ptr((ptr_bits | FORWARDING_POINTER_TAG) as *mut HeapItemDescriptor)
}
