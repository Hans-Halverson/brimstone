use std::{
    alloc::Layout,
    mem::{align_of, size_of},
};

use crate::js::runtime::Context;

use super::{handle::HandleContext, HeapPtr};

pub struct Heap {
    /// Pointer to the start of the heap
    start: *const u8,
    /// Pointer to where the next heap allocation will occur, grows as more allocations occur
    current: *const u8,
    /// Pointer to the end of the heap
    end: *const u8,
    layout: Layout,
}

/// Marker trait that denotes an object on the managed heap
pub trait IsHeapObject {}

/// Default size of the heap, in bytes
const DEFAULT_HEAP_SIZE: usize = 256 * 1024 * 1024;

/// The heap is always aligned to a 1GB boundary. Must be aligned to a power of two alignment
/// greater than the heap size so that we can mask heap pointers to find start of heap.
const HEAP_ALIGNMENT: usize = 1024 * 1024 * 1024;

impl Heap {
    pub fn new() -> Heap {
        // Create uninitialized buffer of memory for heap
        unsafe {
            let layout = Layout::from_size_align(DEFAULT_HEAP_SIZE, HEAP_ALIGNMENT).unwrap();
            let start = std::alloc::alloc(layout);

            // Leave room for heap info struct at start of heap
            let current = start.add(size_of::<HeapInfo>());
            let end = start.add(DEFAULT_HEAP_SIZE);

            HeapInfo::from_raw_heap_ptr(start).init();

            Heap { start, current, end, layout }
        }
    }

    #[inline]
    pub fn info<'a, 'b>(&'a self) -> &'b mut HeapInfo {
        unsafe { &mut *(self.start as *const _ as *mut HeapInfo) }
    }

    pub fn alloc_uninit<T>(&mut self) -> HeapPtr<T> {
        self.alloc_uninit_with_size::<T>(size_of::<T>(), align_of::<T>())
    }

    /// Allocate an object of a given type with the specified size in bytes. When called directly,
    /// is used to allocate dynamically sized objects.
    #[inline]
    pub fn alloc_uninit_with_size<T>(&mut self, size: usize, align: usize) -> HeapPtr<T> {
        unsafe {
            // First align start offset to alignment of type
            let start = self.current.add(self.current.align_offset(align));

            // Calculate where the current will be after this allocation, checking if there is room
            let next_current = start.add(size);
            if (next_current as usize) > (self.end as usize) {
                panic!("Ran out of memory")
            }

            // Update end pointer and write into memory
            self.current = next_current;
            let start = start.cast_mut().cast();

            HeapPtr::from_ptr(start)
        }
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        unsafe { std::alloc::dealloc(self.start as *mut u8, self.layout) };
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
