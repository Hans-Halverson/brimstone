use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        collections::{BsIndexMap, BsIndexMapField},
        gc::{HeapObject, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        value::{ValueCollectionKey, ValueCollectionKeyHandle},
        Context, EvalResult, Handle, HeapPtr, Value,
    },
    set_uninit,
};

// Map Objects (https://tc39.es/ecma262/#sec-map-objects)
extend_object! {
    pub struct MapObject {
        map_data: HeapPtr<ValueMap>,
    }
}

pub type ValueMap = BsIndexMap<ValueCollectionKey, Value>;

impl MapObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<MapObject>> {
        // Allocate and place behind handle before allocating environment
        let map_data =
            ValueMap::new(cx, ObjectKind::MapObjectValueMap, ValueMap::MIN_CAPACITY).to_handle();

        let mut object = object_create_from_constructor::<MapObject>(
            cx,
            constructor,
            ObjectKind::MapObject,
            Intrinsic::MapPrototype,
        )?;

        set_uninit!(object.map_data, map_data.get_());

        Ok(object.to_handle())
    }

    pub fn map_data(&self) -> HeapPtr<ValueMap> {
        self.map_data
    }
}

impl Handle<MapObject> {
    pub fn map_data_field(&self) -> MapObjectMapField {
        MapObjectMapField(*self)
    }

    pub fn insert(&self, cx: Context, key: Handle<Value>, value: Handle<Value>) -> bool {
        let key_handle = ValueCollectionKeyHandle::new(key);

        self.map_data_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(key_handle.get(), value.get())
    }
}

pub struct MapObjectMapField(Handle<MapObject>);

impl BsIndexMapField<ValueCollectionKey, Value> for MapObjectMapField {
    fn new_map(&self, cx: Context, capacity: usize) -> HeapPtr<ValueMap> {
        ValueMap::new(cx, ObjectKind::MapObjectValueMap, capacity)
    }

    fn get(&self) -> HeapPtr<ValueMap> {
        self.0.map_data
    }

    fn set(&mut self, map: HeapPtr<ValueMap>) {
        self.0.map_data = map;
    }
}

impl HeapObject for HeapPtr<MapObject> {
    fn byte_size(&self) -> usize {
        size_of::<MapObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.map_data);
    }
}

impl MapObjectMapField {
    pub fn byte_size(map: &HeapPtr<ValueMap>) -> usize {
        ValueMap::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(map: &mut HeapPtr<ValueMap>, visitor: &mut impl HeapVisitor) {
        map.visit_pointers_impl(visitor, |map, visitor| {
            for (key, value) in map.iter_mut_gc_unsafe() {
                key.visit_pointers(visitor);
                visitor.visit_value(value);
            }
        });
    }
}
