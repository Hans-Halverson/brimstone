use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        collections::{BsHashMap, BsHashMapField},
        completion::EvalResult,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        value::ValueCollectionKey,
        Context, Handle, HeapPtr, Value,
    },
    maybe, set_uninit,
};

use super::intrinsics::Intrinsic;

// 24.3 WeakMap Objects
extend_object! {
    pub struct WeakMapObject {
        // Map of weakly held keys to values. Can only hold object and symbols as keys.
        weak_map_data: HeapPtr<ValueMap>,
        // Holds the address of the next weak map that has been visited during garbage collection.
        // Unused outside of garbage collection.
        next_weak_map: Option<HeapPtr<WeakMapObject>>,
    }
}

type ValueMap = BsHashMap<ValueCollectionKey, Value>;

impl WeakMapObject {
    pub fn new_from_constructor(
        cx: &mut Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<WeakMapObject>> {
        let mut object = maybe!(object_create_from_constructor::<WeakMapObject>(
            cx,
            constructor,
            ObjectKind::WeakMapObject,
            Intrinsic::WeakMapPrototype
        ));

        set_uninit!(object.weak_map_data, ValueMap::new_initial(cx, ObjectKind::MapObjectValueMap));

        object.to_handle().into()
    }

    pub fn weak_map_data(&self) -> HeapPtr<ValueMap> {
        self.weak_map_data
    }

    pub fn next_weak_map(&self) -> Option<HeapPtr<WeakMapObject>> {
        self.next_weak_map
    }

    pub fn set_next_weak_map(&mut self, next_weak_map: HeapPtr<WeakMapObject>) {
        self.next_weak_map = Some(next_weak_map);
    }
}

impl Handle<WeakMapObject> {
    pub fn weak_map_data_field(&self) -> WeakMapObjectMapField {
        WeakMapObjectMapField(*self)
    }
}

#[derive(Clone)]
pub struct WeakMapObjectMapField(Handle<WeakMapObject>);

impl BsHashMapField<ValueCollectionKey, Value> for WeakMapObjectMapField {
    fn new(&self, cx: &mut Context, capacity: usize) -> HeapPtr<ValueMap> {
        ValueMap::new(cx, ObjectKind::MapObjectValueMap, capacity)
    }

    fn get(&self, _: &mut Context) -> HeapPtr<ValueMap> {
        self.0.weak_map_data
    }

    fn set(&mut self, _: &mut Context, map: HeapPtr<ValueMap>) {
        self.0.weak_map_data = map;
    }
}

impl HeapObject for HeapPtr<WeakMapObject> {
    fn byte_size(&self) -> usize {
        size_of::<WeakMapObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);

        // Intentionally do not visit weak_map and next_weak_map
    }
}
