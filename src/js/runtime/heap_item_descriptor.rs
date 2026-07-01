use std::mem::size_of;

use bitflags::bitflags;

use crate::{
    runtime::{
        Context, HeapItemKind, Value,
        alloc_error::AllocResult,
        gc::{Handle, HeapItem, HeapPtr, HeapVisitor},
        object_value::{VirtualObject, VirtualObjectVtable},
        ordinary_object::OrdinaryObject,
        rust_vtables::extract_virtual_object_vtable,
    },
    set_uninit,
};

#[repr(C)]
pub struct HeapItemDescriptor {
    /// Always the singleton descriptor descriptor
    descriptor: HeapPtr<HeapItemDescriptor>,
    /// Rust VirtualObject vtable, used for dynamic dispatch to some object methods
    vtable: VirtualObjectVtable,
    /// Object's type
    kind: HeapItemKind,
    /// Bitflags for object
    flags: DescFlags,
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct DescFlags: u8 {
        /// Whether this heap item is an object value
        const IS_OBJECT = 1 << 0;
    }
}

impl HeapItemDescriptor {
    pub fn new<T>(
        cx: Context,
        descriptor: Handle<HeapItemDescriptor>,
        kind: HeapItemKind,
        flags: DescFlags,
    ) -> AllocResult<HeapPtr<HeapItemDescriptor>>
    where
        Handle<T>: VirtualObject,
    {
        let mut desc = cx.alloc_uninit::<HeapItemDescriptor>()?;

        set_uninit!(desc.descriptor, *descriptor);
        set_uninit!(desc.vtable, extract_virtual_object_vtable::<T>());
        set_uninit!(desc.kind, kind);
        set_uninit!(desc.flags, flags);

        Ok(desc)
    }

    /// Create the singleton descriptor descriptor, which all descriptors have in their
    /// `descriptor` field.
    pub fn new_descriptor_descriptor(cx: Context) -> AllocResult<HeapPtr<HeapItemDescriptor>> {
        // Create fake handle which will be read from, in order to initialize descriptor descriptor
        let value = Value::empty();
        let fake_descriptor_handle = Handle::<Value>::from_fixed_non_heap_ptr(&value).cast();

        // First set up the singleton descriptor descriptor, using an arbitrary vtable
        // (e.g. OrdinaryObject). Can only set self pointer after object initially created.
        let mut descriptor = HeapItemDescriptor::new::<OrdinaryObject>(
            cx,
            fake_descriptor_handle,
            HeapItemKind::HeapItemDescriptor,
            DescFlags::empty(),
        )?;

        // Descriptor descriptor points to itself, like all other descriptors
        descriptor.descriptor = descriptor;

        Ok(descriptor)
    }

    #[inline]
    pub const fn kind(&self) -> HeapItemKind {
        self.kind
    }

    #[inline]
    pub const fn vtable(&self) -> VirtualObjectVtable {
        self.vtable
    }

    #[inline]
    pub fn is_object(&self) -> bool {
        self.flags.contains(DescFlags::IS_OBJECT)
    }

    pub fn set_descriptor(&mut self, descriptor: HeapPtr<HeapItemDescriptor>) {
        self.descriptor = descriptor;
    }
}

impl HeapItem for HeapItemDescriptor {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<HeapItemDescriptor>()
    }

    fn visit_pointers(mut heap_item_descriptor: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut heap_item_descriptor.descriptor);
        visitor.visit_rust_vtable_pointer(&mut heap_item_descriptor.vtable);
    }
}
