use std::mem::size_of;

use crate::{
    extend_object, impl_hash_map_instance,
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        collections::{FastHasher, HashMapInstance, hash_map::BsHashMapField},
        eval_result::EvalResult,
        gc::{HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        value::{ValueCollectionKey, ValueCollectionKeyHandle},
    },
    set_uninit,
};

extend_object! {
    /// WeakMap Objects (https://tc39.es/ecma262/#sec-weakmap-objects)
    pub struct WeakMapObject {
        /// Map of weakly held keys to values. Can only hold object and symbols as keys.
        weak_map_data: HeapPtr<WeakValueMap>,
        /// Holds the address of the next weak map that has been visited during garbage collection.
        /// Unused outside of garbage collection.
        next_weak_map: Option<HeapPtr<WeakMapObject>>,
    }
}

impl WeakMapObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<WeakMapObject>> {
        let weak_map_data = WeakValueMap::new_initial(cx)?.to_handle();

        let mut object = object_create_from_constructor::<WeakMapObject>(
            cx,
            constructor,
            HeapItemKind::WeakMapObject,
            Intrinsic::WeakMapPrototype,
        )?;

        set_uninit!(object.weak_map_data, *weak_map_data);

        Ok(object.to_handle())
    }

    pub fn weak_map_data(&self) -> HeapPtr<WeakValueMap> {
        self.weak_map_data
    }

    pub fn next_weak_map(&self) -> Option<HeapPtr<WeakMapObject>> {
        self.next_weak_map
    }

    pub fn set_next_weak_map(&mut self, next_weak_map: Option<HeapPtr<WeakMapObject>>) {
        self.next_weak_map = next_weak_map;
    }
}

impl Handle<WeakMapObject> {
    pub fn weak_map_data_field(&self) -> WeakMapObjectMapField {
        WeakMapObjectMapField(*self)
    }

    pub fn insert(
        &self,
        cx: Context,
        key: Handle<Value>,
        value: Handle<Value>,
    ) -> AllocResult<bool> {
        let key_handle = ValueCollectionKeyHandle::new(key)?;

        Ok(self
            .weak_map_data_field()
            .maybe_grow_for_insertion(cx)?
            .insert_without_growing(key_handle.get(), *value))
    }
}

impl_hash_map_instance!(WeakValueMap, ValueCollectionKey, Value, FastHasher);

#[derive(Clone)]
pub struct WeakMapObjectMapField(Handle<WeakMapObject>);

impl BsHashMapField<WeakValueMap> for WeakMapObjectMapField {
    fn get(&self, _: Context) -> HeapPtr<WeakValueMap> {
        self.0.weak_map_data
    }

    fn set_new(&mut self, cx: Context, capacity: usize) -> AllocResult<HeapPtr<WeakValueMap>> {
        let map = WeakValueMap::new(cx, capacity)?;
        self.0.weak_map_data = map;
        Ok(map)
    }
}

impl HeapItem for WeakMapObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<WeakMapObject>()
    }

    fn visit_pointers(mut weak_map_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        weak_map_object.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut weak_map_object.weak_map_data);

        // Intentionally do not visit next_weak_map
    }
}

impl HeapItem for WeakValueMap {
    fn byte_size(map: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(map.capacity())
    }

    fn visit_pointers(mut map: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        map.visit_map_pointers(visitor);

        for (key, value) in map.iter_mut_gc_unsafe() {
            visitor.visit_weak_value(key.value_mut());
            visitor.visit_weak_value(value);
        }
    }
}
