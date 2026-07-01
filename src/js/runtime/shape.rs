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
pub struct Shape {
    /// Always the singleton shape shape
    shape: HeapPtr<Shape>,
    /// Rust VirtualObject vtable, used for dynamic dispatch to some object methods
    vtable: VirtualObjectVtable,
    /// Object's type
    kind: HeapItemKind,
    /// Bitflags for object
    flags: ShapeFlags,
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct ShapeFlags: u8 {
        /// Whether this heap item is an object value
        const IS_OBJECT = 1 << 0;
    }
}

impl Shape {
    pub fn new<T>(
        cx: Context,
        shape_shape: Handle<Shape>,
        kind: HeapItemKind,
        flags: ShapeFlags,
    ) -> AllocResult<HeapPtr<Shape>>
    where
        Handle<T>: VirtualObject,
    {
        let mut shape = cx.alloc_uninit::<Shape>()?;

        set_uninit!(shape.shape, *shape_shape);
        set_uninit!(shape.vtable, extract_virtual_object_vtable::<T>());
        set_uninit!(shape.kind, kind);
        set_uninit!(shape.flags, flags);

        Ok(shape)
    }

    /// Create the singleton shape shape, which all shapes have in their
    /// `shape` field.
    pub fn new_shape_shape(cx: Context) -> AllocResult<HeapPtr<Shape>> {
        // Create fake handle which will be read from, in order to initialize shape shape
        let value = Value::empty();
        let fake_shape_handle = Handle::<Value>::from_fixed_non_heap_ptr(&value).cast();

        // First set up the singleton shape shape, using an arbitrary vtable
        // (e.g. OrdinaryObject). Can only set self pointer after object initially created.
        let mut shape = Shape::new::<OrdinaryObject>(
            cx,
            fake_shape_handle,
            HeapItemKind::Shape,
            ShapeFlags::empty(),
        )?;

        // Shape shape points to itself, like all other shapes
        shape.shape = shape;

        Ok(shape)
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
        self.flags.contains(ShapeFlags::IS_OBJECT)
    }

    pub fn set_shape(&mut self, shape: HeapPtr<Shape>) {
        self.shape = shape;
    }
}

impl HeapItem for Shape {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<Shape>()
    }

    fn visit_pointers(mut shape: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut shape.shape);
        visitor.visit_rust_vtable_pointer(&mut shape.vtable);
    }
}
