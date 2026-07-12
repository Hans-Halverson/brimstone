use std::mem::size_of;

use crate::{
    extend_object, impl_index_map_instance,
    runtime::{
        Context, EvalResult, Handle, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        collections::{
            BsIndexMap, BsIndexMapField, HashDosResistantHasher, index_map::IndexMapInstance,
        },
        gc::{HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        value::{ValueCollectionKey, ValueCollectionKeyHandle},
    },
    set_uninit,
};

extend_object! {
    /// Map Objects (https://tc39.es/ecma262/#sec-map-objects)
    pub struct MapObject {
        map_data: HeapPtr<ValueIndexMap>,
    }
}

impl MapObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<MapObject>> {
        // Allocate and place behind handle before allocating environment
        let map_data = ValueIndexMap::new(cx, ValueIndexMap::MIN_CAPACITY)?.to_handle();

        let mut object = object_create_from_constructor::<MapObject>(
            cx,
            constructor,
            HeapItemKind::MapObject,
            Intrinsic::MapPrototype,
        )?;

        set_uninit!(object.map_data, *map_data);

        Ok(object.to_handle())
    }

    pub fn map_data(&self) -> HeapPtr<ValueIndexMap> {
        self.map_data
    }

    pub fn map_data_inner(
        &self,
    ) -> Handle<BsIndexMap<ValueCollectionKey, Value, HashDosResistantHasher>> {
        self.map_data.to_handle().cast()
    }
}

impl Handle<MapObject> {
    pub fn map_data_field(&self) -> MapObjectMapField {
        MapObjectMapField(*self)
    }

    pub fn insert(
        &self,
        cx: Context,
        key: Handle<Value>,
        value: Handle<Value>,
    ) -> AllocResult<bool> {
        let key_handle = ValueCollectionKeyHandle::new(key)?;

        Ok(self
            .map_data_field()
            .maybe_grow_for_insertion(cx)?
            .insert_without_growing(key_handle.get(), *value))
    }
}

impl_index_map_instance!(ValueIndexMap, ValueCollectionKey, Value, HashDosResistantHasher);

impl HeapItem for ValueIndexMap {
    fn byte_size(map: HeapPtr<ValueIndexMap>) -> usize {
        ValueIndexMap::calculate_size_in_bytes(map.capacity())
    }

    fn visit_pointers(map: HeapPtr<ValueIndexMap>, visitor: &mut impl HeapVisitor) {
        ValueIndexMap::visit_pointers_impl(map, visitor, |mut map, visitor| {
            for (key, value) in map.iter_mut_gc_unsafe() {
                key.visit_pointers(visitor);
                visitor.visit_value(value);
            }
        });
    }
}

pub struct MapObjectMapField(Handle<MapObject>);

impl BsIndexMapField<ValueIndexMap> for MapObjectMapField {
    fn get(&self) -> HeapPtr<ValueIndexMap> {
        self.0.map_data
    }

    fn set_new(&mut self, cx: Context, capacity: usize) -> AllocResult<HeapPtr<ValueIndexMap>> {
        let map = ValueIndexMap::new(cx, capacity)?;
        self.0.map_data = map;
        Ok(map)
    }
}

impl HeapItem for MapObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<MapObject>()
    }

    fn visit_pointers(mut map_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        map_object.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut map_object.map_data);
    }
}
