/// A Context along with its heap can be converted to a serialized form and then deserialized back
/// later. This allows for embedding an entire pre-initialized heap with all builtin objects into
/// the binary itself, then initializing a Context from the serialized heap at runtime.
///
/// The serialization strategy is described below:
///
/// The serialized form of the heap has all absolute pointers rewritten as offsets from the start of
/// the heap. In addition all root pointers in the Context must also have been rewritten as offsets.
/// To deserialize we simply traverse the heap and root pointers and rewrite back to an absolute
/// pointer by adding the base pointer of the heap. Roots offsets are saved in a vector following
/// traversal order which is the same during serialization and deserialization.
///
/// There are also some non-heap pointers contained within the heap, namely Rust vtable pointers
/// for dynamic dispatch. These are serialized to their RustVtable enum value and then
/// deserialized to the true value of the vtable pointer in the loaded program.
use crate::{
    common::serialized_heap::{SerializedHeap, SerializedSemispace},
    runtime::{
        object_descriptor::ObjectDescriptor,
        rust_vtables::{get_vtable, lookup_vtable_enum, RustVtable},
        Context, Value,
    },
};

use super::{GcType, Heap, HeapInfo, HeapItem, HeapPtr, HeapVisitor};

pub struct HeapSerializer {
    cx: Context,
    /// Copy of a slice of the heap's semispaces, will have pointers rewritten to offsets.
    permanent_space: Vec<u8>,
    current_space: Vec<u8>,
    /// Offsets of all roots to the heap, in traversal order.
    roots: Vec<usize>,
    /// Base pointer for the heap. Offset of all pointers is calculated relative to this.
    base: *const u8,
    /// Whether we are collecting roots, and should collect each offset in the roots list.
    is_collecting_roots: bool,
}

enum Space {
    Permanent,
    Current,
}

impl HeapSerializer {
    fn new(cx: Context) -> Self {
        // Copy the semispace slices to be serialized
        let permanent_space = cx.heap.permanent_heap().to_owned();
        let current_space = cx.heap.current_used_heap().to_owned();

        Self {
            cx,
            permanent_space,
            current_space,
            roots: vec![],
            base: cx.heap.heap_start(),
            is_collecting_roots: false,
        }
    }

    #[allow(unused)]
    pub fn serialize(mut cx: Context) -> Self {
        // Clear the WTF-8 to interned string cache, since it is transient
        cx.interned_strings.generator_cache_mut().clear();

        // First run a GC to only serialize live data in heap
        Heap::run_gc(cx, GcType::Normal);

        // Then must ensure that the current semispace is adjacent to the permanent semispace,
        // running another GC if necessary to swap the semispaces.
        if !cx.heap.is_current_before_next() {
            Heap::run_gc(cx, GcType::Normal);
        }

        let mut serializer = Self::new(cx);

        // Visit all roots in order, rewriting pointers to be offsets from the start of the heap
        // and collecting the offsets in traversal order.
        serializer.collect_roots();

        // Then rewrite pointers to offsets in all copied semispaces
        serializer.rewrite_pointers_to_offsets_in_space(Space::Permanent);
        serializer.rewrite_pointers_to_offsets_in_space(Space::Current);

        serializer
    }

    fn collect_roots(&mut self) {
        self.is_collecting_roots = true;
        self.cx.clone().visit_roots_for_serialization(self);
        self.is_collecting_roots = false;
    }

    /// Traverse a slice of a semispace, rewriting all pointers to be offsets from the heap start.
    fn rewrite_pointers_to_offsets_in_space(&mut self, space: Space) {
        let space = match space {
            Space::Permanent => &mut self.permanent_space,
            Space::Current => &mut self.current_space,
        };

        let space_len = space.len();
        let space_start_ptr = space.as_mut_ptr();

        let mut item_offset = 0;
        while item_offset < space_len {
            // Start of an object - cast to HeapItem and rewrite its pointers to be offsets from the
            // start of the heap.
            let item_ptr = unsafe { space_start_ptr.add(item_offset) };
            let mut heap_item = HeapPtr::from_ptr(item_ptr).cast::<HeapItem>();
            let kind = heap_item.descriptor().kind();

            heap_item.visit_pointers_for_kind(self, kind);

            // Increment fix pointer to point to next new heap object
            item_offset += Heap::alloc_size_for_request_size(heap_item.byte_size_for_kind(kind));
        }
    }

    pub fn as_serialized(&self) -> SerializedHeap {
        let permanent_start = self.cx.heap.permanent_heap_bounds().start;
        let current_start = self.cx.heap.current_used_heap_bounds().0;

        let permanent_offset = permanent_start as usize - self.base as usize;
        let current_offset = current_start as usize - self.base as usize;

        SerializedHeap {
            permanent_space: SerializedSemispace {
                bytes: self.permanent_space.as_slice(),
                start_offset: permanent_offset,
            },
            current_space: SerializedSemispace {
                bytes: self.current_space.as_slice(),
                start_offset: current_offset,
            },
            root_offsets: self.roots.as_slice(),
            heap_info_size: size_of::<HeapInfo>(),
        }
    }
}

impl HeapVisitor for HeapSerializer {
    fn visit_common(&mut self, ptr: &mut HeapPtr<HeapItem>) {
        // Find the pointer's offset from the start of the heap
        let offset = ptr.as_ptr() as usize - self.base as usize;

        if self.is_collecting_roots {
            // When collecting roots do not modify the Context's pointers
            self.roots.push(offset);
        } else {
            // Rewrite the pointer if within the heap
            *ptr = HeapPtr::from_ptr(offset as *mut HeapItem);
        }
    }

    fn visit_rust_vtable_pointer(&mut self, ptr: &mut *const ()) {
        // Rust vtable pointers are replaced with their enum value
        let enum_value = lookup_vtable_enum(*ptr).unwrap();
        *ptr = enum_value as usize as *const ();
    }
}

pub struct HeapSpaceDeserializer {
    /// Base pointer for the heap. Offset of all pointers is calculated relative to this.
    base: *const u8,
}

impl HeapSpaceDeserializer {
    fn new(cx: Context, extra_offset: usize) -> Self {
        // Apply extra offset directly to the base pointer to account for differences in HeapInfo
        // size when deserializing.
        let base = cx.heap.heap_start().wrapping_add(extra_offset);

        Self { base }
    }

    pub fn deserialize(cx: Context, bytes: &mut [u8], extra_offset: usize) {
        let mut deserializer = Self::new(cx, extra_offset);

        let mut item_offset = 0;
        while item_offset < bytes.len() {
            // Start of an object is a descriptor pointer, encoded as an offset;
            let heap_item_ptr = unsafe { bytes.as_mut_ptr().add(item_offset) };

            // Decode the descriptor and read its kind
            let descriptor_offset = unsafe { *heap_item_ptr.cast::<usize>() };
            let descriptor_ptr = unsafe {
                deserializer
                    .base
                    .add(descriptor_offset)
                    .cast::<ObjectDescriptor>()
            };
            let descriptor_kind = unsafe { (*descriptor_ptr).kind() };

            let mut heap_item = HeapPtr::from_ptr(heap_item_ptr).cast::<HeapItem>();
            heap_item.visit_pointers_for_kind(&mut deserializer, descriptor_kind);

            // Increment fix pointer to point to next new heap object
            let byte_size = heap_item.byte_size_for_kind(descriptor_kind);
            item_offset += Heap::alloc_size_for_request_size(byte_size);
        }
    }
}

impl HeapVisitor for HeapSpaceDeserializer {
    fn visit_common(&mut self, ptr: &mut HeapPtr<HeapItem>) {
        // Decode by adding the heap base pointer to each stored offset
        let decoded_ptr = unsafe { self.base.add(ptr.as_ptr() as usize) };
        *ptr = HeapPtr::from_ptr(decoded_ptr as *mut HeapItem);
    }

    fn visit_rust_vtable_pointer(&mut self, ptr: &mut *const ()) {
        // Rust vtable pointers were encoded as their enum value, so lookup the true vtable pointer
        let enum_value = unsafe { std::mem::transmute::<u8, RustVtable>(*ptr as usize as u8) };
        *ptr = get_vtable(enum_value);
    }
}

pub struct HeapRootsDeserializer<'a> {
    /// Base pointer for the heap. Offset of all pointers is calculated relative to this.
    base: *const u8,
    /// Index of the next root to deserialize.
    root_index: usize,
    /// Offsets of all roots to the heap, in traversal order.
    root_offsets: &'a [usize],
}

impl<'a> HeapRootsDeserializer<'a> {
    fn new(cx: Context, serialized: &'a SerializedHeap) -> Self {
        // Apply extra offset directly to the base pointer to account for differences in HeapInfo
        // size when deserializing.
        let extra_offset = calculate_extra_offset(serialized);
        let base = cx.heap.heap_start().wrapping_add(extra_offset);

        Self { base, root_index: 0, root_offsets: serialized.root_offsets }
    }

    pub fn deserialize(mut cx: Context, serialized: &'a SerializedHeap) {
        let mut deserializer = Self::new(cx, serialized);
        cx.visit_roots_for_serialization(&mut deserializer);

        // Make sure that the correct number of roots were deserialized
        debug_assert!(deserializer.root_index == serialized.root_offsets.len());
    }
}

impl HeapVisitor for HeapRootsDeserializer<'_> {
    fn visit_common(&mut self, ptr: &mut HeapPtr<HeapItem>) {
        // Find the next root offset in traversal order and decode it
        let root_offset = self.root_offsets[self.root_index];
        let root_ptr = unsafe { self.base.add(root_offset).cast::<HeapItem>() };

        *ptr = HeapPtr::from_ptr(root_ptr.cast_mut());

        self.root_index += 1;
    }

    fn visit_value(&mut self, value: &mut Value) {
        // All values should be rewritten regardless of whether they are pointers, since only
        // pointers were visited when serializing. This includes uninitialized values which may be
        // represented as the empty value.
        unsafe { self.visit(std::mem::transmute::<&mut Value, &mut HeapPtr<HeapItem>>(value)) };
    }
}
/// Size of the `HeapInfo` struct can be different from the serialized heap, e.g. due to
/// feature flags. We must apply an additional offset to account for this.
pub fn calculate_extra_offset(serialized: &SerializedHeap) -> usize {
    size_of::<HeapInfo>() - serialized.heap_info_size
}
