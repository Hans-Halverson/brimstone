use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        collections::{BsHashSet, BsHashSetField},
        eval_result::EvalResult,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        value::{ValueCollectionKey, ValueCollectionKeyHandle},
        Context, Handle, HeapPtr, Value,
    },
    set_uninit,
};

use super::intrinsics::Intrinsic;

// WeakSet Objects (https://tc39.es/ecma262/#sec-weakset-objects)
extend_object! {
    pub struct WeakSetObject {
        // Set of weakly held references to values. Can only hold object and symbols.
        weak_set_data: HeapPtr<WeakValueSet>,
        // Holds the address of the next weak set that has been visited during garbage collection.
        // Unused outside of garbage collection.
        next_weak_set: Option<HeapPtr<WeakSetObject>>,
    }
}

type WeakValueSet = BsHashSet<ValueCollectionKey>;

impl WeakSetObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<WeakSetObject>> {
        let weak_set_data =
            WeakValueSet::new_initial(cx, ObjectKind::WeakSetObjectWeakValueSet).to_handle();

        let mut object = object_create_from_constructor::<WeakSetObject>(
            cx,
            constructor,
            ObjectKind::WeakSetObject,
            Intrinsic::WeakSetPrototype,
        )?;

        set_uninit!(object.weak_set_data, *weak_set_data);

        Ok(object.to_handle())
    }

    pub fn weak_set_data(&self) -> HeapPtr<WeakValueSet> {
        self.weak_set_data
    }

    pub fn next_weak_set(&self) -> Option<HeapPtr<WeakSetObject>> {
        self.next_weak_set
    }

    pub fn set_next_weak_set(&mut self, next_weak_set: Option<HeapPtr<WeakSetObject>>) {
        self.next_weak_set = next_weak_set;
    }
}

impl Handle<WeakSetObject> {
    pub fn weak_set_data_field(&self) -> WeakSetObjectSetField {
        WeakSetObjectSetField(*self)
    }

    pub fn insert(&self, cx: Context, item: Handle<Value>) -> bool {
        let item_handle = ValueCollectionKeyHandle::new(item);

        self.weak_set_data_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(item_handle.get())
    }
}

#[derive(Clone)]
pub struct WeakSetObjectSetField(Handle<WeakSetObject>);

impl BsHashSetField<ValueCollectionKey> for WeakSetObjectSetField {
    fn new(cx: Context, capacity: usize) -> HeapPtr<WeakValueSet> {
        WeakValueSet::new(cx, ObjectKind::WeakSetObjectWeakValueSet, capacity)
    }

    fn get(&self, _: Context) -> HeapPtr<WeakValueSet> {
        self.0.weak_set_data
    }

    fn set(&mut self, _: Context, set: HeapPtr<WeakValueSet>) {
        self.0.weak_set_data = set;
    }
}

impl HeapObject for HeapPtr<WeakSetObject> {
    fn byte_size(&self) -> usize {
        size_of::<WeakSetObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.weak_set_data);

        // Intentionally do not visit next_weak_set
    }
}

impl WeakSetObjectSetField {
    pub fn byte_size(map: &HeapPtr<WeakValueSet>) -> usize {
        WeakValueSet::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(set: &mut HeapPtr<WeakValueSet>, visitor: &mut impl HeapVisitor) {
        set.visit_pointers(visitor);

        for value in set.iter_mut_gc_unsafe() {
            visitor.visit_weak_value(value.value_mut());
        }
    }
}
