use crate::{
    extend_object,
    js::runtime::{
        collections::{BsIndexMap, BsIndexMapField},
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        value::ValueCollectionKey,
        Context, EvalResult, Handle, HeapPtr, Value,
    },
    maybe, set_uninit,
};

// 24.1 Map Objects
extend_object! {
    pub struct MapObject {
        map_data: HeapPtr<ValueMap>,
    }
}

pub type ValueMap = BsIndexMap<ValueCollectionKey, Value>;

impl MapObject {
    pub fn new_from_constructor(
        cx: &mut Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<MapObject>> {
        // Allocate and place behind handle before allocating environment
        let map_data = ValueMap::new(cx, ObjectKind::MapObjectValueMap, ValueMap::MIN_CAPACITY);

        let mut object = maybe!(object_create_from_constructor::<MapObject>(
            cx,
            constructor,
            ObjectKind::MapObject,
            Intrinsic::MapPrototype
        ));

        set_uninit!(object.map_data, map_data);

        object.to_handle().into()
    }

    pub fn map_data(&self) -> HeapPtr<ValueMap> {
        self.map_data
    }
}

impl Handle<MapObject> {
    pub fn clear_map_data(&mut self, cx: &mut Context) {
        let new_map = ValueMap::new(cx, ObjectKind::MapObjectValueMap, ValueMap::MIN_CAPACITY);
        self.map_data = new_map;
    }

    pub fn map_data_field(&self) -> MapObjectMapField {
        MapObjectMapField(*self)
    }
}

pub struct MapObjectMapField(Handle<MapObject>);

impl BsIndexMapField<ValueCollectionKey, Value> for MapObjectMapField {
    fn new(&self, cx: &mut Context, capacity: usize) -> HeapPtr<ValueMap> {
        ValueMap::new(cx, ObjectKind::MapObjectValueMap, capacity)
    }

    fn get(&self) -> HeapPtr<ValueMap> {
        self.0.map_data
    }

    fn set(&mut self, map: HeapPtr<ValueMap>) {
        self.0.map_data = map;
    }
}
