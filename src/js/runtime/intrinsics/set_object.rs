use std::mem::size_of;

use crate::{
    extend_object, impl_index_set_instance,
    runtime::{
        Context, EvalResult, Handle, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        collections::{BsIndexSet, BsIndexSetField, IndexSetInstance},
        gc::{HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::{object_create, object_create_from_constructor},
        value::{ValueCollectionKey, ValueCollectionKeyHandle},
    },
    set_uninit,
};

extend_object! {
    /// Set Objects (https://tc39.es/ecma262/#sec-set-objects)
    pub struct SetObject {
        set_data: HeapPtr<ValueIndexSet>,
    }
}

impl SetObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<SetObject>> {
        // Allocate and place behind handle before allocating object
        let set_data = ValueIndexSet::new(cx, ValueIndexSet::MIN_CAPACITY)?.to_handle();

        let mut object = object_create_from_constructor::<SetObject>(
            cx,
            constructor,
            HeapItemKind::SetObject,
            Intrinsic::SetPrototype,
        )?;

        set_uninit!(object.set_data, *set_data);

        Ok(object.to_handle())
    }

    /// Create a new SetObject with the provided set data.
    pub fn new_from_set(
        cx: Context,
        set_data: Handle<ValueIndexSet>,
    ) -> AllocResult<Handle<SetObject>> {
        let mut object =
            object_create::<SetObject>(cx, HeapItemKind::SetObject, Intrinsic::SetPrototype)?;

        set_uninit!(object.set_data, *set_data);

        Ok(object.to_handle())
    }

    #[inline]
    pub fn set_data_ptr(&self) -> HeapPtr<ValueIndexSet> {
        self.set_data
    }

    #[inline]
    pub fn set_data(&self) -> Handle<ValueIndexSet> {
        self.set_data_ptr().to_handle()
    }

    #[inline]
    pub fn set_data_inner(&self) -> Handle<BsIndexSet<ValueCollectionKey>> {
        self.set_data().cast()
    }
}

impl Handle<SetObject> {
    fn set_data_field(&self) -> SetObjectSetField {
        SetObjectSetField(*self)
    }

    pub fn insert(&self, cx: Context, item: Handle<Value>) -> AllocResult<bool> {
        let item_handle = ValueCollectionKeyHandle::new(item)?;

        Ok(self
            .set_data_field()
            .maybe_grow_for_insertion(cx)?
            .insert_without_growing(item_handle.get()))
    }
}

impl_index_set_instance!(ValueIndexSet, ValueCollectionKey);

impl HeapItem for ValueIndexSet {
    fn byte_size(set: HeapPtr<ValueIndexSet>) -> usize {
        ValueIndexSet::calculate_size_in_bytes(set.capacity())
    }

    fn visit_pointers(set: HeapPtr<ValueIndexSet>, visitor: &mut impl HeapVisitor) {
        ValueIndexSet::visit_pointers_impl(set, visitor, |mut set, visitor| {
            for element in set.iter_mut_gc_unsafe() {
                element.visit_pointers(visitor);
            }
        });
    }
}

#[derive(Clone)]
pub struct SetObjectSetField(Handle<SetObject>);

impl BsIndexSetField<ValueIndexSet> for SetObjectSetField {
    fn get(&self) -> HeapPtr<ValueIndexSet> {
        self.0.set_data
    }

    fn set_new(&mut self, cx: Context, capacity: usize) -> AllocResult<HeapPtr<ValueIndexSet>> {
        let set = ValueIndexSet::new(cx, capacity)?;
        self.0.set_data = set;
        Ok(set)
    }
}

impl HeapItem for SetObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<SetObject>()
    }

    fn visit_pointers(mut set_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        set_object.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut set_object.set_data);
    }
}
