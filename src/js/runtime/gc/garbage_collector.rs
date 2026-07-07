use std::{ops::Range, ptr::NonNull};

use crate::runtime::{
    Context, HeapItemKind, PropertyKey, Value,
    gc::{
        AnyHeapItem, Heap, HeapItem, HeapPtr, HeapVisitor,
        heap_item::{for_each_heap_item, visit_pointers_for_kind},
    },
    interned_strings::InternedStrings,
    intrinsics::{
        finalization_registry_object::FinalizationRegistryObject, weak_map_object::WeakMapObject,
        weak_ref_object::WeakRefObject, weak_set_object::WeakSetObject,
    },
    shape::{PrototypeObjectChildrenShapesVec, Shape, TransitionVec},
    string_value::FlatString,
    transitions::TransitionKind,
};

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

    // Intrusive list of all WeakRefs that have been visited during this gc cycle
    weak_ref_list: Option<HeapPtr<WeakRefObject>>,

    // Intrusive list of all WeakSets that have been visited during this gc cycle
    weak_set_list: Option<HeapPtr<WeakSetObject>>,

    // Intrusive list of all WeakMaps that have been visited during this gc cycle
    weak_map_list: Option<HeapPtr<WeakMapObject>>,

    // Intrusive list of all FinalizationRegistries that have been visited during this gc cycle
    finalization_registry_list: Option<HeapPtr<FinalizationRegistryObject>>,

    // Intrusive list of all PrototypeObjectChildrenShapesVec that have been visited during this gc cycle
    weak_vec_list: Option<HeapPtr<PrototypeObjectChildrenShapesVec>>,
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
            weak_vec_list: None,
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

            new_heap_size = new_heap_size.min(cx.options.max_heap_size);

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
        for_each_heap_item(self.new_permanent_space_bounds.clone(), |item| {
            let kind = item.shape().kind();
            visit_pointers_for_kind(item, self, kind);
            kind
        });
    }

    /// Move all remaining live items from the from-heap to the to-heap, fixing pointers to point to
    /// the to-heap.
    ///
    /// Iterates until all live items have been discovered. Liveness includes ephemeron handling.
    fn move_all_live_items(&mut self) {
        loop {
            // Then start fixing all pointers in each new heap item until fix pointer catches up
            // with alloc pointer, meaning all live objects have been copied and pointers have
            // been fixed.
            while self.fix_ptr < self.alloc_ptr {
                let new_heap_item =
                    HeapPtr::from_ptr(self.fix_ptr.cast_mut()).cast::<AnyHeapItem>();
                AnyHeapItem::visit_pointers(new_heap_item, self);

                // Increment fix pointer to point to next new heap item
                let byte_size = AnyHeapItem::byte_size(new_heap_item);
                let alloc_size = Heap::alloc_size_for_request_size(byte_size);
                unsafe { self.fix_ptr = self.fix_ptr.add(alloc_size) }
            }

            // Visit ephemeron values that are known to be live due to live keys. Keep iterating
            // until fixpoint where no new live keys are found.
            let found_live_weak_map_value = self.visit_live_weak_map_entries();
            let found_live_unregister_token = self.visit_live_finalization_registry_cells();

            if !found_live_weak_map_value && !found_live_unregister_token {
                break;
            }
        }
    }

    /// Move the entire contents of the permanent heap from the old bounds to the new bounds.
    fn move_permanent_heap(&self) {
        let mut dest_ptr = self.new_permanent_space_bounds.start;

        for_each_heap_item(self.old_permanent_space_bounds.clone(), |mut item| {
            // Read the kind before the shape is overwritten with a forwarding pointer
            let kind = item.shape().kind();
            Self::move_heap_item(&mut item, &mut dest_ptr);
            kind
        });

        // Entire old permanent heap should have been copied 1:1 to the new permanent heap
        debug_assert!(dest_ptr == self.new_permanent_space_bounds.end);
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

        // Every heap item has a known shape
        let shape = heap_item.shape();

        // If shape is actually a forwarding pointer then simply rewrite pointer to old heap
        // item to instead point to new heap item.
        if let Some(forwarding_ptr) = decode_forwarding_pointer(shape) {
            *heap_item = forwarding_ptr;
            return;
        }

        // Move the heap item to the to-space. We are guaranteed to have enough space in the
        // to-space since all allocations fit into the from-space.
        let new_heap_item = Self::move_heap_item(heap_item, &mut self.alloc_ptr);

        // Rewrite pointer to old heap item to instead point to new heap item
        *heap_item = new_heap_item;

        // Type specific actions for live heap items. All weak objects must be collected in lists so
        // they can be traversed later.
        match new_heap_item.shape().kind() {
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
            HeapItemKind::PrototypeObjectChildrenShapesVec => {
                self.add_visited_weak_vec(new_heap_item.cast::<PrototypeObjectChildrenShapesVec>())
            }
            _ => {}
        }
    }

    /// Move a heap item to a destination address. Leaves a forwarding pointer at the old heap item
    /// old heap item. Updates the destination address to point to the next heap-item-aligned
    /// address after the new heap item.
    ///
    /// Return the new heap item.
    #[inline]
    fn move_heap_item(
        heap_item: &mut HeapPtr<AnyHeapItem>,
        dest_ptr: &mut *const u8,
    ) -> HeapPtr<AnyHeapItem> {
        // Calculate size of heap item. Caller must ensure that there is enough space at the
        // destination to hold the moved item.
        let alloc_size = Heap::alloc_size_for_request_size(AnyHeapItem::byte_size(*heap_item));

        // Copy item from old to new heap, and bump alloc_ptr to point past new allocation
        let new_heap_item = HeapPtr::from_ptr(dest_ptr.cast_mut()).cast::<AnyHeapItem>();
        unsafe {
            std::ptr::copy_nonoverlapping::<u8>(
                heap_item.as_ptr().cast(),
                new_heap_item.as_ptr().cast(),
                alloc_size,
            );
        }

        // Overwrite the shape inside the old heap item to be a forwarding pointer to
        // new heap item.
        let forwarding_pointer = encode_forwarding_pointer(new_heap_item);
        heap_item.set_shape(forwarding_pointer);

        // Bump the `dest_ptr` to point to the next aligned address after the new heap item
        unsafe { *dest_ptr = dest_ptr.add(alloc_size) };

        new_heap_item
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

    // Add a weak map to the linked list of weak maps that are live during this garbage collection.
    fn add_visited_weak_map(&mut self, mut weak_map: HeapPtr<WeakMapObject>) {
        weak_map.set_next_weak_map(self.weak_map_list);
        self.weak_map_list = Some(weak_map);
    }

    // Add a weak vec to the linked list of weak vecs that are live during this garbage collection.
    fn add_visited_weak_vec(&mut self, mut weak_vec: HeapPtr<PrototypeObjectChildrenShapesVec>) {
        weak_vec.set_next_weak_vec(self.weak_vec_list);
        self.weak_vec_list = Some(weak_vec);
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

    /// Resolve a weak pointer to a heap item, determining whether the target is live along with its
    /// current (possibly moved-to) location.
    ///
    /// Must only be called after all live items have been moved. A target in a moved space is
    /// live iff it left behind a forwarding pointer, and a target anywhere else (e.g. in the
    /// permanent space) was never moved and is always live.
    #[inline]
    fn resolve_weak(&self, target: HeapPtr<AnyHeapItem>) -> WeakResolution {
        if !self.is_in_moved_space(target.as_ptr().cast()) {
            return WeakResolution::Live(target);
        }

        match decode_forwarding_pointer(target.shape()) {
            Some(new_location) => WeakResolution::Live(new_location),
            None => WeakResolution::Dead,
        }
    }

    fn prune_weak_reference_and_handle_finalizers(&mut self, cx: Context) {
        // Prune interned strings that are no longer referenced
        self.prune_weak_interned_strings(cx);

        // Prune shapes in the transition tree that are no longer referenced from the heap
        self.prune_weak_transition_tree(cx);

        // Fix the weak objects, handling objects that have been garbage collected
        self.fix_weak_refs();
        self.fix_weak_sets();
        self.fix_weak_maps();
        self.fix_weak_vecs();
        self.fix_finalization_registries(cx);
    }

    // Visit live weak refs and check if their targets are still live. Should only be called after
    // liveness of all items has been determined.
    fn fix_weak_refs(&mut self) {
        // Walk all live weak refs in the list
        let mut next_weak_ref = self.weak_ref_list;
        while let Some(mut weak_ref) = next_weak_ref {
            next_weak_ref = weak_ref.next_weak_ref();

            // Target may have already been cleared to undefined by a prior GC
            let target = weak_ref.weak_ref_target();
            if !target.is_pointer() {
                continue;
            }

            match self.resolve_weak(target.as_pointer()) {
                // Live references are updated to point to the new location
                WeakResolution::Live(target) => {
                    weak_ref.set_weak_ref_target(Value::heap_item(target))
                }
                // Target was garbage collected so reset target to undefined
                WeakResolution::Dead => weak_ref.set_weak_ref_target(Value::undefined()),
            }
        }
    }

    // Visit live weak sets and check if their values are still live. Should only be called after
    // liveness of all items has been determined.
    fn fix_weak_sets(&mut self) {
        // Walk all live weak sets in the list
        let mut next_weak_set = self.weak_set_list;
        while let Some(weak_set) = next_weak_set {
            next_weak_set = weak_set.next_weak_set();

            for mut entry in weak_set.weak_set_data().iter_entries_mut_gc_unsafe() {
                let value = entry.key_mut().value_mut();
                debug_assert!(value.is_pointer());

                match self.resolve_weak(value.as_pointer()) {
                    // Live references are updated to point to the new location
                    WeakResolution::Live(new_location) => *value = Value::heap_item(new_location),
                    // Value was garbage collected so remove the entry from the set
                    WeakResolution::Dead => entry.remove(),
                }
            }
        }
    }

    // Visit live weak maps and check if their keys are still live. Should only be called after
    // liveness of all items has been determined.
    fn fix_weak_maps(&mut self) {
        // Walk all live weak maps in the list
        let mut next_weak_map = self.weak_map_list;
        while let Some(weak_map) = next_weak_map {
            next_weak_map = weak_map.next_weak_map();

            for mut entry in weak_map.weak_map_data().iter_entries_mut_gc_unsafe() {
                let key = entry.key_mut().value_mut();
                debug_assert!(key.is_pointer());

                // Key was garbage collected so remove the entry from the map
                if let WeakResolution::Dead = self.resolve_weak(key.as_pointer()) {
                    entry.remove();
                }

                // Note that live keys and values were already updated to point to their new
                // locations when ephemerons were processed.
            }
        }
    }

    // Visit live weak maps and check if their keys are still live. Visit the values associated with
    // all live keys. Return whether any new live values were found.
    fn visit_live_weak_map_entries(&mut self) -> bool {
        // Walk all live weak maps in the list
        let mut next_weak_map = self.weak_map_list;
        let mut found_new_live_value = false;

        while let Some(weak_map) = next_weak_map {
            next_weak_map = weak_map.next_weak_map();
            for (weak_key, value) in weak_map.weak_map_data().iter_mut_gc_unsafe() {
                found_new_live_value |= self.visit_ephemeron(weak_key.value_mut(), Some(value));
            }
        }

        found_new_live_value
    }

    // Finalization registries are effectively a WeakMap from target value to unregister token.
    // Visit live finalization registries and check if their cells' targets are still live. Visit
    // the weakly held unregister token in each cell where the target is live. Return whether any
    // newly live unregister tokens were found.
    fn visit_live_finalization_registry_cells(&mut self) -> bool {
        // Walk all live weak maps in the list
        let mut next_finalization_registry = self.finalization_registry_list;
        let mut found_new_live_unregister_token = false;

        while let Some(finalization_registry) = next_finalization_registry {
            next_finalization_registry = finalization_registry.next_finalization_registry();

            for cell in finalization_registry.cells().iter_mut_gc_unsafe().flatten() {
                found_new_live_unregister_token |=
                    self.visit_ephemeron(&mut cell.target, cell.unregister_token.as_mut());
            }
        }

        found_new_live_unregister_token
    }

    /// Visit a single ephemeron - a weak key and an optional dependent value that is only live
    /// if the key is live.
    ///
    /// If the key is newly discovered to be live then update the key to point to its new location
    /// and visit the value. Keys in the permanent space are always live so their values are always
    /// visited.
    ///
    /// Return whether a dependent value was newly discovered to be live.
    fn visit_ephemeron(&mut self, key: &mut Value, value: Option<&mut Value>) -> bool {
        debug_assert!(key.is_pointer());
        let key_ptr = key.as_pointer().as_ptr().cast();

        // Check if key has not yet been visited and moved to the to-space
        if self.is_in_moved_space(key_ptr) {
            // Key is known to be live if it has already moved and left a forwarding pointer.
            // Visit the value associated with the key since we now know it is live. Also update
            // key to point into to-space so we can tell it is visited.
            if let Some(forwarding_ptr) = decode_forwarding_pointer(key.as_pointer().shape()) {
                *key = Value::heap_item(forwarding_ptr);

                if let Some(value) = value {
                    self.visit_value(value);
                    return true;
                }
            }
        } else if self.is_in_old_permanent_space(key_ptr) {
            // Key was in the permanent space so it is always live and its value is always visited.
            //
            // Note that we only need to visit the value if it is a pointer. Make sure to only
            // count this as a new live value if it was not already moved.
            if let Some(value) = value {
                if value.is_pointer() && self.is_in_moved_space(value.as_pointer().as_ptr().cast())
                {
                    let is_newly_live =
                        decode_forwarding_pointer(value.as_pointer().shape()).is_none();
                    self.visit_value(value);
                    return is_newly_live;
                }
            }
        }

        false
    }

    // Visit live finalization registries and check if their cells' targets are still live. Should
    // only be called after liveness of all items has been determined.
    fn fix_finalization_registries(&mut self, mut cx: Context) {
        // Walk all live finalization registries in the list
        let mut next_finalization_registry = self.finalization_registry_list;
        while let Some(finalization_registry) = next_finalization_registry {
            next_finalization_registry = finalization_registry.next_finalization_registry();

            for cell_opt in finalization_registry.cells().iter_mut_gc_unsafe() {
                if let Some(cell) = cell_opt {
                    debug_assert!(cell.target.is_pointer());

                    // Target was garbage collected so enqueue the cleanup callback, passing in
                    // the held value, then remove the cell from the registry.
                    if let WeakResolution::Dead = self.resolve_weak(cell.target.as_pointer()) {
                        cx.task_queue().enqueue_callback_1_task(
                            finalization_registry.cleanup_callback().into(),
                            cell.held_value,
                        );

                        finalization_registry.cells().remove_cell(cell_opt);
                    }

                    // Note that live targets were already updated to point to their new locations
                    // when ephemerons were processed.
                }
            }
        }
    }

    /// Walk the intrusive list of live weak vecs and compress each one.
    fn fix_weak_vecs(&mut self) {
        // Walk all live weak vecs in the list
        let mut next_weak_vec = self.weak_vec_list;
        while let Some(weak_vec) = next_weak_vec {
            next_weak_vec = weak_vec.next_weak_vec();

            self.compress_weak_vec(weak_vec);
        }
    }

    /// Compress a `PrototypeObjectChildrenShapesVec` by removing elements that have been garbage
    /// collected.
    ///
    /// Vector is compressed in place by moving live elements down to fill the slots of dead
    /// elements. Note that backing array is never shrunk.
    fn compress_weak_vec(&self, mut weak_vec: HeapPtr<PrototypeObjectChildrenShapesVec>) {
        let len = weak_vec.len();
        let mut next_kept_index = 0;

        for index in 0..len {
            let element = weak_vec.as_mut_slice()[index];

            let kept_element = if let Some(element) = element {
                match self.resolve_weak(element.as_any()) {
                    WeakResolution::Live(new_location) => Some(Some(new_location.cast::<Shape>())),
                    WeakResolution::Dead => None,
                }
            } else {
                // Prune removed values
                None
            };

            if let Some(element) = kept_element {
                weak_vec.as_mut_slice()[next_kept_index] = element;
                next_kept_index += 1;
            }
        }

        weak_vec.set_len(next_kept_index);
    }

    fn prune_weak_interned_strings(&mut self, cx: Context) {
        // First remove dead strings from the set of interned strings
        let mut string_set = InternedStrings::strings(cx);
        for mut entry in string_set.iter_entries_mut_gc_unsafe() {
            if !self.resolve_weak_interned_string(entry.key_mut()) {
                entry.remove();
            }
        }

        // Then check values of interned strings map
        let mut cloned_cx = cx;
        let string_map = cloned_cx.interned_strings.generator_cache_mut();
        string_map.retain(|_, string_ref| self.resolve_weak_interned_string(string_ref));
    }

    /// Resolve a weak reference to an interned string in place. Return whether the string is
    /// still live.
    fn resolve_weak_interned_string(&self, string_ref: &mut HeapPtr<FlatString>) -> bool {
        match self.resolve_weak(string_ref.as_any()) {
            WeakResolution::Live(new_location) => {
                *string_ref = new_location.cast();
                true
            }
            WeakResolution::Dead => false,
        }
    }

    /// Traverse the transition tree and prune all shapes that are no longer live.
    fn prune_weak_transition_tree(&mut self, cx: Context) {
        // First prune the initial level of the transition tree
        let mut roots = cx.shapes.transition_tree_roots_ptr();
        for mut entry in roots.iter_entries_mut_gc_unsafe() {
            // Keep edge only if the shape is live
            let shape_ref = entry.value_mut();
            match self.resolve_weak(shape_ref.as_any()) {
                WeakResolution::Live(new_location) => {
                    *shape_ref = new_location.cast();

                    // Prototype guaranteed to be live since it is referenced by shape
                    if let Some(prototype) = &mut entry.key_mut().prototype {
                        if let WeakResolution::Live(new_location) =
                            self.resolve_weak(prototype.as_any())
                        {
                            *prototype = new_location.cast();
                        } else {
                            panic!("Live shape has a dead prototype in its transition tree");
                        }
                    }
                }
                // Prune dead shapes from the transition tree
                WeakResolution::Dead => entry.remove(),
            }
        }

        // Then prune the remaining branches of the transition tree via a DFS traversal
        let mut stack = vec![];
        for (_, shape) in roots.iter_gc_unsafe() {
            stack.push(shape);
        }

        while let Some(mut shape) = stack.pop() {
            if let Some(transitions_or_next_shape) = shape.transitions_or_next_shape() {
                if shape.has_transitions_vec() {
                    // TransitionVecs are held strongly but their contents are held weakly.
                    // Compress the transition vec in place and descend into live children.
                    let transitions_vec = transitions_or_next_shape.cast::<TransitionVec>();
                    self.compress_transition_vec(transitions_vec);

                    for transition in transitions_vec.as_slice() {
                        stack.push(transition.next_shape_ptr());
                    }
                } else {
                    // Shapes are held weakly
                    match self.resolve_weak(transitions_or_next_shape) {
                        WeakResolution::Live(new_location) => {
                            shape.set_transitions_or_next_shape(Some(new_location));
                            stack.push(new_location.cast::<Shape>());
                        }
                        WeakResolution::Dead => {
                            shape.set_transitions_or_next_shape(None);
                        }
                    }
                }
            }
        }
    }

    /// Compress a TransitionVec in place by removing transitions to shapes that are no longer live.
    fn compress_transition_vec(&self, mut transition_vec: HeapPtr<TransitionVec>) {
        let len = transition_vec.len();
        let mut next_kept_index = 0;

        for index in 0..len {
            let mut transition = transition_vec.as_mut_slice()[index];

            // Only transitions to live shapes are kept, compressing the TransitionVec in place
            if let WeakResolution::Live(new_shape_location) =
                self.resolve_weak(transition.next_shape_ptr().as_any())
            {
                let new_shape = new_shape_location.cast::<Shape>();
                transition.set_next_shape(new_shape);

                // Fix up weak references within the transition itself. These are all guaranteed to
                // be live since the live shape has a (transitive) strong reference to them.
                match *transition.kind_mut() {
                    TransitionKind::DefineProperty { ref mut key, .. }
                        if let Some(heap_key) = key.as_heap_item_opt() =>
                    {
                        if let WeakResolution::Live(new_key_location) = self.resolve_weak(heap_key)
                        {
                            *key = PropertyKey::new_from_heap_item_unchecked(new_key_location);
                        } else {
                            panic!("Live shape has a dead key in its transition tree");
                        }
                    }
                    TransitionKind::SetPrototype { prototype: Some(ref mut prototype) } => {
                        if let WeakResolution::Live(new_prototype_location) =
                            self.resolve_weak(prototype.as_any())
                        {
                            *prototype = new_prototype_location.cast();
                        } else {
                            panic!("Live shape has a dead prototype in its transition tree");
                        }
                    }
                    _ => {}
                }

                // Write to the next available slot in the TransitionVec
                transition_vec.as_mut_slice()[next_kept_index] = transition;
                next_kept_index += 1;
            }
        }

        transition_vec.set_len(next_kept_index);
    }
}

impl HeapVisitor for GarbageCollector {
    fn visit(&mut self, ptr: &mut HeapPtr<AnyHeapItem>) {
        self.copy_or_fix_pointer(ptr);
    }
}

/// The result of resolving a weak pointer once liveness of all items is known.
enum WeakResolution {
    /// The target is live, at this possibly-new location.
    Live(HeapPtr<AnyHeapItem>),
    /// The target was garbage collected.
    Dead,
}

// The first item in each heap item is a pointer, is either:
//   - A pointer to the item's shape if this item has not yet been copied to the from space
//   - A forwarding pointer to the address in the from space the item has been copied to
//
// Tag lowest bit of pointer to signal a forwarding pointer
const FORWARDING_POINTER_TAG: usize = 0x1;

fn decode_forwarding_pointer(shape: HeapPtr<Shape>) -> Option<HeapPtr<AnyHeapItem>> {
    let ptr_bits = shape.as_ptr() as usize;
    if ptr_bits & FORWARDING_POINTER_TAG == FORWARDING_POINTER_TAG {
        return Some(HeapPtr::from_ptr((ptr_bits ^ FORWARDING_POINTER_TAG) as *mut AnyHeapItem));
    }

    None
}

fn encode_forwarding_pointer(heap_ptr: HeapPtr<AnyHeapItem>) -> HeapPtr<Shape> {
    let ptr_bits = heap_ptr.as_ptr() as usize;
    HeapPtr::from_ptr((ptr_bits | FORWARDING_POINTER_TAG) as *mut Shape)
}
