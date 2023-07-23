use std::{alloc::Layout, mem::size_of};

use crate::js::runtime::{gc::garbage_collector::GarbageCollector, Context};

use super::{handle::HandleContext, HeapPtr, HeapVisitor};

pub struct Heap {
    /// Pointer to the heap info which is at the very start of the heap
    heap_info: *const u8,
    /// Pointer to the start of the heap
    start: *const u8,
    /// Pointer to where the next heap allocation will occur, grows as more allocations occur
    current: *const u8,
    /// Pointer to the end of the heap
    end: *const u8,
    // Pointer to the start of the next heap
    next_heap_start: *const u8,
    // Pointer to the end of the next heap
    next_heap_end: *const u8,
    layout: Layout,
}

/// Default size of the heap, in bytes
const DEFAULT_HEAP_SIZE: usize = 256 * 1024 * 1024;

// Amount of heap space that can be used for allocation
const USABLE_HEAP_SIZE: usize = (DEFAULT_HEAP_SIZE - size_of::<HeapInfo>()) / 2;

/// The heap is always aligned to a 1GB boundary. Must be aligned to a power of two alignment
/// greater than the heap size so that we can mask heap pointers to find start of heap.
const HEAP_ALIGNMENT: usize = 1024 * 1024 * 1024;

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
                start,
                current: start,
                end,
                next_heap_start,
                next_heap_end,
                layout,
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
        GarbageCollector::run(self.info().cx());
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
        // All allocations must be 8-byte aligned so round up to nearest multiple of 8
        (request_byte_size + 7) & !7
    }

    pub fn visit_roots(&self, visitor: &mut impl HeapVisitor) {
        self.info().handle_context().visit_roots(visitor)
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe { std::alloc::dealloc(self.heap_info as *mut u8, self.layout) };
    }
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
