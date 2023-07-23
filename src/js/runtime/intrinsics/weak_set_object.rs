use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        collections::{BsHashSet, BsHashSetField},
        completion::EvalResult,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        value::ValueCollectionKey,
        Context, Handle, HeapPtr,
    },
    maybe, set_uninit,
};

use super::intrinsics::Intrinsic;

// 24.4 WeakSet Objects
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
        cx: &mut Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<WeakSetObject>> {
        let weak_set_data =
            WeakValueSet::new_initial(cx, ObjectKind::WeakSetObjectWeakValueSet).to_handle();

        let mut object = maybe!(object_create_from_constructor::<WeakSetObject>(
            cx,
            constructor,
            ObjectKind::WeakSetObject,
            Intrinsic::WeakSetPrototype
        ));

        set_uninit!(object.weak_set_data, weak_set_data.get_());

        object.to_handle().into()
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
}

#[derive(Clone)]
pub struct WeakSetObjectSetField(Handle<WeakSetObject>);

impl BsHashSetField<ValueCollectionKey> for WeakSetObjectSetField {
    fn new(cx: &mut Context, capacity: usize) -> HeapPtr<WeakValueSet> {
        WeakValueSet::new(cx, ObjectKind::WeakSetObjectWeakValueSet, capacity)
    }

    fn get(&self, _: &mut Context) -> HeapPtr<WeakValueSet> {
        self.0.weak_set_data
    }

    fn set(&mut self, _: &mut Context, set: HeapPtr<WeakValueSet>) {
        self.0.weak_set_data = set;
    }
}

impl HeapObject for HeapPtr<WeakSetObject> {
    fn byte_size(&self) -> usize {
        size_of::<WeakSetObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);

        // Intentionally do not visit next_weak_set
    }
}

impl WeakSetObjectSetField {
    pub fn byte_size(map: &HeapPtr<WeakValueSet>) -> usize {
        WeakValueSet::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(set: &mut HeapPtr<WeakValueSet>, visitor: &mut impl HeapVisitor) {
        set.visit_pointers(visitor);

        // Intentionally do not visit elements
    }
}
