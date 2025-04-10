mod generated_serialized_heap {
    include!(concat!(env!("OUT_DIR"), "/generated_serialized_heap.rs"));
}

use brimstone_core::common::serialized_heap::set_default_serialized_heap;

/// Initialize the default serialized heap. Must be called before a Context is created.
///
/// Can be safely called any number of times.
pub fn init() {
    set_default_serialized_heap(generated_serialized_heap::SERIALIZED_HEAP);
}
