use std::mem::{align_of, size_of};

use crate::js::runtime::value::StringValue;

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

            Heap {
                memory,
                current: start,
                end: start.add(DEFAULT_HEAP_SIZE),
            }
        }
    }

    pub fn alloc<T>(&mut self, value: T) -> Gc<T> {
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
            start.write(value);

            Gc::from_ptr(start)
        }
    }

    pub fn alloc_string(&mut self, str: String) -> Gc<StringValue> {
        self.alloc(StringValue::new(str))
    }
}
