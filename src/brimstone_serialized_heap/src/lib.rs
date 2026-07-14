// Heap snapshots are enabled for 64-bit targets only.
#![cfg_attr(target_pointer_width = "32", allow(dead_code))]
mod generated_serialized_heap {
    include!(concat!(env!("OUT_DIR"), "/generated_serialized_heap.rs"));
}

#[cfg(target_pointer_width = "64")]
use brimstone_core::common::serialized_heap::set_default_serialized_heap;

/// Initialize the default serialized heap. Must be called before a Context is created.
///
/// Can be safely called any number of times.
pub fn init() {
    #[cfg(target_pointer_width = "64")]
    set_default_serialized_heap(generated_serialized_heap::SERIALIZED_HEAP);
}
