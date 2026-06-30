use crate::{
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemDescriptor,
        object_value::ObjectValue,
    },
    set_uninit,
};

/// The value of an accessor property. May contain a getter and/or a setter.
#[repr(C)]
pub struct Accessor {
    descriptor: HeapPtr<HeapItemDescriptor>,
    pub get: Option<HeapPtr<ObjectValue>>,
    pub set: Option<HeapPtr<ObjectValue>>,
}

impl Accessor {
    pub fn new(
        cx: Context,
        get: Option<Handle<ObjectValue>>,
        set: Option<Handle<ObjectValue>>,
    ) -> AllocResult<Handle<Accessor>> {
        let mut accessor = cx.alloc_uninit::<Accessor>()?;

        set_uninit!(accessor.descriptor, cx.descriptors.get(HeapItemKind::Accessor));
        set_uninit!(accessor.get, get.map(|v| *v));
        set_uninit!(accessor.set, set.map(|v| *v));

        Ok(accessor.to_handle())
    }

    pub fn from_value(value: Handle<Value>) -> Handle<Accessor> {
        debug_assert!(value.is::<Accessor>());
        value.cast::<Accessor>()
    }
}

impl HeapItem for Accessor {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<Accessor>()
    }

    fn visit_pointers(mut accessor: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut accessor.descriptor);
        visitor.visit_pointer_opt(&mut accessor.get);
        visitor.visit_pointer_opt(&mut accessor.set);
    }
}
