use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        collections::{BsIndexSet, BsIndexSetField},
        gc::{HeapObject, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{object_create, object_create_from_constructor},
        value::{ValueCollectionKey, ValueCollectionKeyHandle},
        Context, EvalResult, Handle, HeapPtr, Value,
    },
    set_uninit,
};

// Set Objects (https://tc39.es/ecma262/#sec-set-objects)
extend_object! {
    pub struct SetObject {
        set_data: HeapPtr<ValueSet>,
    }
}

pub type ValueSet = BsIndexSet<ValueCollectionKey>;

impl SetObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<SetObject>> {
        // Allocate and place behind handle before allocating object
        let set_data = SetObjectSetField::new(cx, ValueSet::MIN_CAPACITY).to_handle();

        let mut object = object_create_from_constructor::<SetObject>(
            cx,
            constructor,
            ObjectKind::SetObject,
            Intrinsic::SetPrototype,
        )?;

        set_uninit!(object.set_data, set_data.get_());

        Ok(object.to_handle())
    }

    /// Create a new SetObject with the provided set data.
    pub fn new_from_set(cx: Context, set_data: Handle<ValueSet>) -> Handle<SetObject> {
        let mut object =
            object_create::<SetObject>(cx, ObjectKind::SetObject, Intrinsic::SetPrototype);

        set_uninit!(object.set_data, set_data.get_());

        object.to_handle()
    }

    #[inline]
    pub fn set_data_ptr(&self) -> HeapPtr<ValueSet> {
        self.set_data
    }

    #[inline]
    pub fn set_data(&self) -> Handle<ValueSet> {
        self.set_data_ptr().to_handle()
    }
}

impl Handle<SetObject> {
    fn set_data_field(&self) -> SetObjectSetField {
        SetObjectSetField(*self)
    }

    pub fn insert(&self, cx: Context, item: Handle<Value>) -> bool {
        let item_handle = ValueCollectionKeyHandle::new(item);

        self.set_data_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(item_handle.get())
    }
}

#[derive(Clone)]
pub struct SetObjectSetField(Handle<SetObject>);

impl BsIndexSetField<ValueCollectionKey> for SetObjectSetField {
    fn new(cx: Context, capacity: usize) -> HeapPtr<ValueSet> {
        ValueSet::new(cx, ObjectKind::SetObjectValueSet, capacity)
    }

    fn get(&self) -> HeapPtr<ValueSet> {
        self.0.set_data
    }

    fn set(&mut self, set: HeapPtr<ValueSet>) {
        self.0.set_data = set;
    }
}

impl HeapObject for HeapPtr<SetObject> {
    fn byte_size(&self) -> usize {
        size_of::<SetObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.set_data);
    }
}

impl SetObjectSetField {
    pub fn byte_size(set: &HeapPtr<ValueSet>) -> usize {
        ValueSet::calculate_size_in_bytes(set.capacity())
    }

    pub fn visit_pointers(set: &mut HeapPtr<ValueSet>, visitor: &mut impl HeapVisitor) {
        set.visit_pointers_impl(visitor, |set, visitor| {
            for element in set.iter_mut_gc_unsafe() {
                element.visit_pointers(visitor);
            }
        });
    }
}
