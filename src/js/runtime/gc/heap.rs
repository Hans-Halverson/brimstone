use std::mem::{align_of, size_of};

use super::pointer::Gc;

// Simple bump allocator for now
pub struct Heap {
    memory: Vec<u8>,
    // Pointer to where the next heap allocation will occur, grows as more allocations occur
    current: *const u8,
    // Pointer to the end of the heap
    end: *const u8,
}

const DEFAULT_HEAP_SIZE: usize = 256 * 1024 * 1024;

impl Heap {
    pub fn new() -> Heap {
        // Create uninitialized buffer of memory for heap
        unsafe {
            let mut memory: Vec<u8> = Vec::with_capacity(DEFAULT_HEAP_SIZE);
            memory.set_len(DEFAULT_HEAP_SIZE);

            let start = memory.as_ptr();

            Heap { memory, current: start, end: start.add(DEFAULT_HEAP_SIZE) }
        }
    }

    pub fn alloc<T>(&mut self, value: T) -> Gc<T> {
        let uninit_value_ref = self.alloc_uninit::<T>();
        unsafe { uninit_value_ref.as_ptr().write(value) };
        uninit_value_ref
    }

    pub fn alloc_uninit<T>(&mut self) -> Gc<T> {
        unsafe {
            // First align start offset to alignment of type
            let start = self.current.add(self.current.align_offset(align_of::<T>()));

            // Calculate where the current will be after this allocation, checking if there is room
            let next_current = start.add(size_of::<T>());
            if (next_current as usize) > (self.end as usize) {
                panic!("Ran out of memory")
            }

            // Update end pointer and write into memory
            self.current = next_current;
            let start = start.cast_mut().cast::<T>();

            Gc::from_ptr(start)
        }
    }
}
