use crate::{
    runtime::{alloc_error::AllocResult, heap_item_descriptor::HeapItemKind},
    set_uninit,
};

use super::{
    gc::{HeapItem, HeapVisitor},
    heap_item_descriptor::HeapItemDescriptor,
    object_value::ObjectValue,
    Context, Handle, HeapPtr, Value,
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

        set_uninit!(accessor.descriptor, cx.base_descriptors.get(HeapItemKind::Accessor));
        set_uninit!(accessor.get, get.map(|v| *v));
        set_uninit!(accessor.set, set.map(|v| *v));

        Ok(accessor.to_handle())
    }

    pub fn from_value(value: Handle<Value>) -> Handle<Accessor> {
        debug_assert!(
            value.is_pointer() && value.as_pointer().descriptor().kind() == HeapItemKind::Accessor
        );
        value.cast::<Accessor>()
    }
}

impl HeapItem for HeapPtr<Accessor> {
    fn byte_size(&self) -> usize {
        size_of::<Accessor>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer_opt(&mut self.get);
        visitor.visit_pointer_opt(&mut self.set);
    }
}
