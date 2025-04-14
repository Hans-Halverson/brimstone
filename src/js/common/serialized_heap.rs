use std::sync::OnceLock;

/// A serialized version of the heap containing enough information to initialize a new heap from.
///
/// Non-owning, references a `HeapSerializer` with lifetime `'a`.
pub struct SerializedHeap<'a> {
    /// Copy of the heap's semispaces with pointers rewritten to offsets. Contains the full
    /// permanent semispace, and only the used portion of the current semispace.
    pub permanent_space: SerializedSemispace<'a>,
    pub current_space: SerializedSemispace<'a>,
    /// Offsets of all roots to the heap, in traversal order
    pub root_offsets: &'a [usize],
    /// Size of the HeapInfo struct
    pub heap_info_size: usize,
}

/// The serialized form of an individual semispace.
pub struct SerializedSemispace<'a> {
    /// Compressed bytes of the serialized semispace
    pub bytes: &'a [u8],
    /// Start offset of the semispace, relative to the heap start
    pub start_offset: usize,
}

static DEFAULT_SERIALIZED_HEAP: OnceLock<Option<SerializedHeap<'static>>> = OnceLock::new();

/// Get the current default serialized heap, if it has been set.
pub fn get_default_serialized_heap() -> Option<&'static SerializedHeap<'static>> {
    DEFAULT_SERIALIZED_HEAP.get().unwrap_or(&None).as_ref()
}

/// Set the heap to use as the default serialized heap.
pub fn set_default_serialized_heap(serialized_heap: SerializedHeap<'static>) {
    let _ = DEFAULT_SERIALIZED_HEAP.set(Some(serialized_heap));
}
