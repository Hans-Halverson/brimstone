use std::{alloc::Layout, mem::size_of, ops::Range, ptr::NonNull};

use crate::{
    common::{constants::MAX_HEAP_SIZE, serialized_heap::SerializedHeap},
    runtime::{gc::garbage_collector::GarbageCollector, Context},
};

use super::{
    handle::HandleContext,
    heap_serializer::{calculate_extra_offset, HeapSpaceDeserializer},
    GcType, HeapPtr, HeapVisitor,
};

/// Heap Layout:
///
/// | HeapInfo | Permanent | Semispace 1 | Semispace 2 |
///
/// Permanent is an optional region used for objects that are never collected such as some builtins.
/// The rest of the heap is split into two semispaces which are used as the main heap.
///
/// Heap is aligned to a 1GB boundary and contains a reference to the context. This allows any heap
/// pointer to be masked to find the start of the heap along with its context.
pub struct Heap {
    /// Pointer to the start of the heap, where HeapInfo is stored
    heap_start: *const u8,
    /// Pointer to the end of the heap
    heap_end: *const u8,
    /// Pointer to the start of the permanent region
    permanent_start: *const u8,
    /// Pointer to the end of the permanent region
    permanent_end: *const u8,
    /// Pointer to the start of the current semispace
    start: *const u8,
    /// Pointer to where the next heap allocation will occur, grows as more allocations occur
    current: *const u8,
    /// Pointer to the end of the current semispace
    end: *const u8,
    // Pointer to the start of the next semispace
    next_heap_start: *const u8,
    // Pointer to the end of the next semispace
    next_heap_end: *const u8,
    layout: Layout,

    #[cfg(feature = "gc_stress_test")]
    pub gc_stress_test: bool,
}

/// The heap is always aligned to a 1GB boundary. Must be aligned to a power of two alignment
/// greater than the heap size so that we can mask heap pointers to find start of heap.
const HEAP_ALIGNMENT: usize = 1024 * 1024 * 1024;

/// All heap items are aligned to 8-byte boundaries
type HeapItemAlignmentType = u64;
const HEAP_ITEM_ALIGNMENT: usize = std::mem::size_of::<HeapItemAlignmentType>();

impl Heap {
    pub fn new(initial_size: usize) -> Heap {
        // Create uninitialized buffer of memory for heap
        unsafe {
            let layout = Layout::from_size_align(initial_size, HEAP_ALIGNMENT).unwrap();
            let heap_start = std::alloc::alloc(layout);
            let heap_end = heap_start.add(initial_size);

            let semispace_size = (initial_size - size_of::<HeapInfo>()) / 2;

            // Leave room for heap info struct at start of heap
            let start = heap_start.add(size_of::<HeapInfo>());
            let end = start.add(semispace_size);

            // Find bounds of other heap part
            let next_heap_start = end;
            let next_heap_end = end.add(semispace_size);

            HeapInfo::from_raw_heap_ptr(heap_start).init();

            Heap {
                heap_start,
                heap_end,
                // Permament region is empty to start
                permanent_start: NonNull::dangling().as_ptr(),
                permanent_end: NonNull::dangling().as_ptr(),
                start,
                current: start,
                end,
                next_heap_start,
                next_heap_end,
                layout,

                #[cfg(feature = "gc_stress_test")]
                gc_stress_test: false,
            }
        }
    }

    /// Initialize an uninitialized heap with a serialized heap. This will copy the serialized heap
    /// over and fix all pointers within the new heap.
    ///
    /// Pointers were encoded as offsets and can be restored by adding to the new heap base pointer.
    pub fn init_from_serialized(&mut self, cx: Context, serialized: &SerializedHeap) {
        // Size of the `HeapInfo` struct can be different from the serialized heap, e.g. due to
        // feature flags. We must apply an additional offset to account for this.
        let extra_offset = calculate_extra_offset(serialized);
        let new_permanent_start_offset = extra_offset + serialized.permanent_space.start_offset;
        let new_current_start_offset = extra_offset + serialized.current_space.start_offset;

        // Ensure actual heap has enough room for the serialized heap
        let permanent_size = serialized.permanent_space.bytes.len();
        let semispace_size = (self.heap_size() - new_current_start_offset) / 2;
        if serialized.current_space.bytes.len() > semispace_size {
            panic!("Serialized heap is larger than the actual heap");
        }

        // Find bounds of permanent space
        self.permanent_start = unsafe { self.heap_start.add(new_permanent_start_offset) };
        self.permanent_end = unsafe { self.permanent_start.add(permanent_size) };

        // Copy permanent semispace into the actual heap
        self.permanent_heap_mut()
            .copy_from_slice(serialized.permanent_space.bytes);

        // Rewrite offsets in the permanent space to pointers in the new heap
        HeapSpaceDeserializer::deserialize(cx, self.permanent_heap_mut(), extra_offset);

        // Find bounds of used part of current semispace
        self.start = unsafe { self.heap_start.add(new_current_start_offset) };
        self.current = unsafe { self.start.add(serialized.current_space.bytes.len()) };

        // Copy used portion of current semispace into the actual heap
        self.current_used_heap_mut()
            .copy_from_slice(serialized.current_space.bytes);

        // Rewrite offsets in the current semispace to pointers in the new heap
        HeapSpaceDeserializer::deserialize(cx, self.current_used_heap_mut(), extra_offset);

        // Write the end of the current semispace
        self.end = unsafe { self.start.add(semispace_size) };

        // Write the bounds of the next semispace, making sure it is aligned
        self.next_heap_start = align_pointer_up(self.end, HEAP_ITEM_ALIGNMENT);
        self.next_heap_end = unsafe { self.next_heap_start.add(semispace_size) };

        self.debug_assert_heap_well_formed();
    }

    /// Create a new heap for a resizing GC, provided the old heap and the new heap size.
    pub fn new_for_resize(prev_heap: &Heap, new_size: usize) -> Heap {
        debug_assert!(new_size.is_power_of_two());

        let mut new_heap = Self::new(new_size);

        // Copy the old HeapInfo into the new heap
        unsafe {
            std::ptr::copy_nonoverlapping(
                prev_heap.heap_start,
                new_heap.heap_start.cast_mut(),
                size_of::<HeapInfo>(),
            )
        };

        // Set up bounds of the new permanent semispace
        new_heap.permanent_start = unsafe {
            let offset = prev_heap.permanent_start.offset_from(prev_heap.heap_start);
            new_heap.heap_start.offset(offset)
        };
        new_heap.permanent_end = unsafe {
            new_heap
                .permanent_start
                .add(prev_heap.permanent_bytes_allocated())
        };

        // Calculate size of each new semispace. Make sure that we can evenly divide the remaining
        // heap into semispaces with the correct alignment and the exact same size.
        let semispaces_start = align_pointer_up(new_heap.permanent_end, HEAP_ITEM_ALIGNMENT * 2);
        let semispaces_start_offset = semispaces_start as usize - new_heap.heap_start as usize;
        let semispace_size = (new_heap.heap_size() - semispaces_start_offset) / 2;

        // Set up bounds of the new semispaces
        new_heap.start = semispaces_start;
        new_heap.current = new_heap.start;
        new_heap.end = unsafe { semispaces_start.add(semispace_size) };

        new_heap.next_heap_start = align_pointer_up(new_heap.end, HEAP_ITEM_ALIGNMENT);
        new_heap.next_heap_end = unsafe { new_heap.next_heap_start.add(semispace_size) };

        // Set additional GC flags
        #[cfg(feature = "gc_stress_test")]
        {
            new_heap.gc_stress_test = prev_heap.gc_stress_test;
        }

        new_heap.debug_assert_heap_well_formed();

        new_heap
    }

    fn debug_assert_heap_well_formed(&self) {
        // The semispaces must not extend beyond the end of the heap
        debug_assert!(self.end <= self.heap_end);
        debug_assert!(self.next_heap_end <= self.heap_end);

        // The semispaces must be aligned to the heap item alignment
        debug_assert!(is_heap_item_aligned(self.start));
        debug_assert!(is_heap_item_aligned(self.next_heap_start));

        // The semispaces must have the exact same size
        debug_assert_eq!(
            self.end as usize - self.start as usize,
            self.next_heap_end as usize - self.next_heap_start as usize
        );

        // The current pointer must be within the bounds of the current semispace
        debug_assert!(self.current >= self.start && self.current < self.end);
    }

    #[inline]
    pub fn info<'a>(&self) -> &'a mut HeapInfo {
        unsafe { &mut *(self.heap_start as *const _ as *mut HeapInfo) }
    }

    pub fn alloc_uninit<T>(cx: Context) -> HeapPtr<T> {
        Self::alloc_uninit_with_size::<T>(cx, size_of::<T>())
    }

    /// Allocate an object of a given type with the specified size in bytes. When called directly,
    /// is used to allocate dynamically sized objects.
    ///
    /// Allocation will have at least the given size and is guaranteed to have 8-byte alignment.
    #[inline]
    pub fn alloc_uninit_with_size<T>(mut cx: Context, size: usize) -> HeapPtr<T> {
        let alloc_size = Self::alloc_size_for_request_size(size);

        // Run a GC on every allocation in stress test mode
        #[cfg(feature = "gc_stress_test")]
        if cx.heap.gc_stress_test {
            Self::run_gc(cx, GcType::Normal);
        }

        // Ensure that this direct reference to heap is not used after a GC or allocation
        let heap = &mut cx.heap;

        unsafe {
            let start = heap.current;

            // Calculate where the current will be after this allocation, checking if there is room
            let next_current = start.add(alloc_size);
            if (next_current as usize) > (heap.end as usize) {
                // If there is not room run a gc cycle

                // Resize the heap
                if cx.heap.heap_size() < MAX_HEAP_SIZE {
                    Self::run_gc(cx, GcType::Grow { alloc_size: Some(alloc_size) });
                } else {
                    Self::run_gc(cx, GcType::Normal);
                }

                // Make sure there is enough space for allocation after gc, otherwise we are out of
                // heap memory.
                cx.heap.panic_if_out_of_memory(alloc_size);

                return Self::alloc_uninit_with_size(cx, alloc_size);
            }

            // Update end pointer and write into memory
            heap.current = next_current;
            let start = start.cast_mut().cast();

            HeapPtr::from_ptr(start)
        }
    }

    pub fn run_gc(cx: Context, type_: GcType) {
        GarbageCollector::run(cx, type_)
    }

    fn has_room_for_alloc(&self, alloc_size: usize) -> bool {
        unsafe { self.current.add(alloc_size) <= self.end }
    }

    fn panic_if_out_of_memory(&self, alloc_size: usize) {
        if !self.has_room_for_alloc(alloc_size) {
            panic!("Ran of out heap memory");
        }
    }

    pub fn heap_size(&self) -> usize {
        self.heap_end as usize - self.heap_start as usize
    }

    pub fn heap_start(&self) -> *const u8 {
        self.heap_start
    }

    pub fn current_heap_bounds(&self) -> Range<*const u8> {
        self.start..self.end
    }

    pub fn next_heap_bounds(&self) -> Range<*const u8> {
        self.next_heap_start..self.next_heap_end
    }

    pub fn permanent_heap_bounds(&self) -> Range<*const u8> {
        self.permanent_start..self.permanent_end
    }

    pub fn permanent_heap(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(self.permanent_start, self.permanent_bytes_allocated())
        }
    }

    pub fn permanent_heap_mut(&mut self) -> &mut [u8] {
        unsafe {
            std::slice::from_raw_parts_mut(
                self.permanent_start.cast_mut(),
                self.permanent_bytes_allocated(),
            )
        }
    }

    pub fn current_used_heap_bounds(&self) -> (*const u8, *const u8) {
        (self.start, self.current)
    }

    pub fn current_used_heap(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.start, self.bytes_allocated()) }
    }

    pub fn current_used_heap_mut(&mut self) -> &mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(self.start.cast_mut(), self.bytes_allocated()) }
    }

    pub fn bytes_allocated(&self) -> usize {
        self.current as usize - self.start as usize
    }

    pub fn permanent_bytes_allocated(&self) -> usize {
        self.permanent_end as usize - self.permanent_start as usize
    }

    /// Returns true if the current semispace is before the next semispace in memory.
    pub fn is_current_before_next(&self) -> bool {
        self.start < self.next_heap_start
    }

    pub fn swap_heaps(&mut self, free_space_start_ptr: *const u8) {
        let old_start = self.start;
        let old_end = self.end;

        self.start = self.next_heap_start;
        self.end = self.next_heap_end;

        self.next_heap_start = old_start;
        self.next_heap_end = old_end;

        self.current = free_space_start_ptr;
    }

    pub fn finish_resized_heap(&mut self, free_space_start_ptr: *const u8) {
        self.current = free_space_start_ptr;
        self.debug_assert_heap_well_formed();
    }

    pub fn alloc_size_for_request_size(request_byte_size: usize) -> usize {
        align_up(request_byte_size, HEAP_ITEM_ALIGNMENT)
    }

    pub fn visit_roots(&self, visitor: &mut impl HeapVisitor) {
        self.info().handle_context().visit_roots(visitor)
    }

    /// Mark the current semispace as permanent, claiming it for the permanent region. Redistribute
    /// the remaining heap space between the two semispaces.
    pub fn mark_current_semispace_as_permanent(&mut self) {
        // The permanent region is before the two semispaces, so if the current semispace is the
        // last one then copy its allocated contents to the start of the first semispace, where the
        // permanent region will start.
        if self.start < self.next_heap_start {
            self.permanent_start = self.start;
            self.permanent_end = self.current;
        } else {
            let bytes_allocated = self.bytes_allocated();
            unsafe {
                std::ptr::copy_nonoverlapping(
                    self.start,
                    self.next_heap_start.cast_mut(),
                    bytes_allocated,
                );
            }

            self.permanent_start = self.next_heap_start;
            self.permanent_end = unsafe { self.next_heap_start.add(bytes_allocated) };
        }

        // Make sure that we can evenly divide the remaining heap into semispaces with the correct
        // alignment and the exact same size.
        let semispaces_start_ptr = align_pointer_up(self.permanent_end, HEAP_ITEM_ALIGNMENT * 2);
        let semispaces_end_ptr = if self.end < self.next_heap_end {
            self.next_heap_end
        } else {
            self.end
        };

        // Calculate the remaining size for each semispace
        let available_size = semispaces_end_ptr as usize - semispaces_start_ptr as usize;
        let semispace_size = available_size / 2;

        // Write bounds of new semispaces
        self.start = semispaces_start_ptr;
        self.end = unsafe { semispaces_start_ptr.add(semispace_size) };

        self.next_heap_start = self.end;
        self.next_heap_end = semispaces_end_ptr;

        // Reset current pointer to start of newly empty start semispace
        self.current = self.start;

        self.debug_assert_heap_well_formed();
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe { std::alloc::dealloc(self.heap_start as *mut u8, self.layout) };
    }
}

// Align a number up, rounding down to zero
fn align_up(ptr_bits: usize, alignment: usize) -> usize {
    (ptr_bits + (alignment - 1)) & !(alignment - 1)
}

// Align a heap pointer, rounding up to infinity
fn align_pointer_up(ptr: *const u8, alignment: usize) -> *const u8 {
    align_up(ptr as usize, alignment) as *const u8
}

fn is_heap_item_aligned(ptr: *const u8) -> bool {
    ptr.cast::<HeapItemAlignmentType>().is_aligned()
}

/// Heap data stored at the beginning of the heap
pub struct HeapInfo {
    /// Reference to the context that holds this heap.
    context: Context,
    handle_context: HandleContext,
}

impl HeapInfo {
    pub fn init(&mut self) {
        self.handle_context.init();
    }

    pub fn set_context(&mut self, cx: Context) {
        self.context = cx;
    }

    #[inline]
    pub fn from_heap_ptr<'a, T>(heap_ptr: HeapPtr<T>) -> &'a mut HeapInfo {
        HeapInfo::from_raw_heap_ptr(heap_ptr.as_ptr())
    }

    #[inline]
    pub fn from_raw_heap_ptr<'a, T>(heap_ptr: *const T) -> &'a mut HeapInfo {
        const HEAP_BASE_MASK: usize = !(HEAP_ALIGNMENT - 1);
        let heap_base = ((heap_ptr as usize) & HEAP_BASE_MASK) as *mut HeapInfo;

        unsafe { &mut *heap_base }
    }

    pub fn cx(&self) -> Context {
        self.context
    }

    pub fn handle_context(&mut self) -> &mut HandleContext {
        &mut self.handle_context
    }
}
