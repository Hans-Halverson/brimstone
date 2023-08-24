use std::{alloc::Layout, mem::size_of, ptr::NonNull};

use crate::js::runtime::{gc::garbage_collector::GarbageCollector, Context};

use super::{handle::HandleContext, HeapPtr, HeapVisitor};

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
    /// Pointer to the heap info which is at the very start of the heap
    heap_info: *const u8,
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

/// Default size of the heap, in bytes
const DEFAULT_HEAP_SIZE: usize = 256 * 1024 * 1024;

// Amount of heap space that can be used for allocation
const USABLE_HEAP_SIZE: usize = (DEFAULT_HEAP_SIZE - size_of::<HeapInfo>()) / 2;

/// The heap is always aligned to a 1GB boundary. Must be aligned to a power of two alignment
/// greater than the heap size so that we can mask heap pointers to find start of heap.
const HEAP_ALIGNMENT: usize = 1024 * 1024 * 1024;

/// All heap objects are aligned to 8-byte boundaries
const HEAP_OBJECT_ALIGNMENT: usize = 8;

impl Heap {
    pub fn new() -> Heap {
        // Create uninitialized buffer of memory for heap
        unsafe {
            let layout = Layout::from_size_align(DEFAULT_HEAP_SIZE, HEAP_ALIGNMENT).unwrap();
            let heap_info = std::alloc::alloc(layout);

            // Leave room for heap info struct at start of heap
            let start = heap_info.add(size_of::<HeapInfo>());
            let end = start.add(USABLE_HEAP_SIZE);

            // Find bounds of other heap part
            let next_heap_start = end;
            let next_heap_end = end.add(USABLE_HEAP_SIZE);

            HeapInfo::from_raw_heap_ptr(heap_info).init();

            Heap {
                heap_info,
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

    #[inline]
    pub fn info<'a, 'b>(&'a self) -> &'b mut HeapInfo {
        unsafe { &mut *(self.heap_info as *const _ as *mut HeapInfo) }
    }

    pub fn alloc_uninit<T>(&mut self) -> HeapPtr<T> {
        self.alloc_uninit_with_size::<T>(size_of::<T>())
    }

    /// Allocate an object of a given type with the specified size in bytes. When called directly,
    /// is used to allocate dynamically sized objects.
    ///
    /// Allocation will have at least the given size and is guaranteed to have 8-byte alignment.
    #[inline]
    pub fn alloc_uninit_with_size<T>(&mut self, size: usize) -> HeapPtr<T> {
        let alloc_size = Self::alloc_size_for_request_size(size);

        // Run a GC on every allocation in stress test mode
        #[cfg(feature = "gc_stress_test")]
        if self.gc_stress_test {
            self.run_gc();
        }

        unsafe {
            let start = self.current;

            // Calculate where the current will be after this allocation, checking if there is room
            let next_current = start.add(alloc_size);
            if (next_current as usize) > (self.end as usize) {
                // If there is not room run a gc cycle
                self.run_gc();

                // Make sure there is enough space for allocation after gc, otherwise we are out of
                // heap memory.
                self.panic_if_out_of_memory(alloc_size);

                return self.alloc_uninit_with_size(alloc_size);
            }

            // Update end pointer and write into memory
            self.current = next_current;
            let start = start.cast_mut().cast();

            HeapPtr::from_ptr(start)
        }
    }

    pub fn run_gc(&mut self) {
        GarbageCollector::run(self.info().cx())
    }

    fn has_room_for_alloc(&self, alloc_size: usize) -> bool {
        unsafe { self.current.add(alloc_size) <= self.end }
    }

    fn panic_if_out_of_memory(&self, alloc_size: usize) {
        if !self.has_room_for_alloc(alloc_size) {
            panic!("Ran of out heap memory");
        }
    }

    pub fn current_heap_bounds(&self) -> (*const u8, *const u8) {
        (self.start, self.end)
    }

    pub fn next_heap_bounds(&self) -> (*const u8, *const u8) {
        (self.next_heap_start, self.next_heap_end)
    }

    pub fn permanent_heap_bounds(&self) -> (*const u8, *const u8) {
        (self.permanent_start, self.permanent_end)
    }

    pub fn bytes_allocated(&self) -> usize {
        self.current as usize - self.start as usize
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

    pub fn alloc_size_for_request_size(request_byte_size: usize) -> usize {
        align_up(request_byte_size, HEAP_OBJECT_ALIGNMENT)
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
        let semispaces_start_ptr = align_pointer_up(self.permanent_end, HEAP_OBJECT_ALIGNMENT * 2);
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
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe { std::alloc::dealloc(self.heap_info as *mut u8, self.layout) };
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

/// Heap data stored at the beginning of the heap
pub struct HeapInfo {
    /// Reference to the context that holds this heap.
    context: *mut Context,
    handle_context: HandleContext,
}

impl HeapInfo {
    pub fn init(&mut self) {
        self.handle_context.init();
    }

    pub fn set_context(&mut self, cx: &mut Context) {
        self.context = cx as *mut Context;
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

    pub fn cx(&self) -> &mut Context {
        unsafe { &mut *self.context }
    }

    pub fn handle_context(&mut self) -> &mut HandleContext {
        &mut self.handle_context
    }
}
