use std::mem::size_of;

use crate::{
    cast_from_value_fn, extend_object,
    runtime::{
        array_object::create_array_from_list,
        collections::index_map::GcSafeEntriesIter,
        error::type_error,
        eval_result::EvalResult,
        gc::{HeapObject, HeapVisitor},
        iterator::create_iter_result_object,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        property::Property,
        realm::Realm,
        value::{Value, ValueCollectionKey},
        Context, Handle, HeapPtr,
    },
    set_uninit,
};

use super::{
    intrinsics::Intrinsic,
    map_object::{MapObject, ValueMap},
};

// Map Iterator Objects (https://tc39.es/ecma262/#sec-map-iterator-objects)
extend_object! {
    pub struct MapIterator {
        // Component parts of an index_map::GcSafeEntriesIter
        map: HeapPtr<ValueMap>,
        next_entry_index: usize,
        kind: MapIteratorKind,
        is_done: bool,
    }
}

pub enum MapIteratorKind {
    Key,
    Value,
    KeyAndValue,
}

impl MapIterator {
    pub fn new(cx: Context, map: Handle<MapObject>, kind: MapIteratorKind) -> Handle<MapIterator> {
        let mut object = object_create::<MapIterator>(
            cx,
            ObjectKind::MapIterator,
            Intrinsic::MapIteratorPrototype,
        );

        set_uninit!(object.map, map.map_data());
        set_uninit!(object.next_entry_index, 0);
        set_uninit!(object.kind, kind);
        set_uninit!(object.is_done, false);

        object.to_handle()
    }

    cast_from_value_fn!(MapIterator, "Map Iterator");

    fn get_iter(&self) -> GcSafeEntriesIter<ValueCollectionKey, Value> {
        GcSafeEntriesIter::<ValueCollectionKey, Value>::from_parts(
            self.map.to_handle(),
            self.next_entry_index,
        )
    }

    fn store_iter(&mut self, iter: GcSafeEntriesIter<ValueCollectionKey, Value>) {
        let (map, next_entry_index) = iter.to_parts();
        self.map = *map;
        self.next_entry_index = next_entry_index;
    }
}

/// The %MapIteratorPrototype% Object (https://tc39.es/ecma262/#sec-%mapiteratorprototype%-object)
pub struct MapIteratorPrototype;

impl MapIteratorPrototype {
    pub fn new(mut cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true);

        object.intrinsic_func(cx, cx.names.next(), Self::next, 0, realm);

        // %MapIteratorPrototype% [ @@toStringTag ] (https://tc39.es/ecma262/#sec-%mapiteratorprototype%-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let to_string_tag_value = cx.alloc_string("Map Iterator").into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(to_string_tag_value, false, false, true),
        );

        object
    }

    /// %MapIteratorPrototype%.next (https://tc39.es/ecma262/#sec-%mapiteratorprototype%.next)
    /// Adapted from the abstract closure in CreateMapIterator (https://tc39.es/ecma262/#sec-createmapiterator)
    pub fn next(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let mut map_iterator = MapIterator::cast_from_value(cx, this_value)?;

        // Check if iterator is already done
        if map_iterator.is_done {
            return Ok(create_iter_result_object(cx, cx.undefined(), true));
        }

        // Follow tombstone objects, fixing up iterator as needed. This may be a chain of tombstone
        // objects and we need to fix up the iterator at each step.
        while map_iterator.map.is_tombstone() {
            map_iterator.map = ValueMap::fix_iterator_for_resized_map(
                map_iterator.map,
                &mut map_iterator.next_entry_index,
            );
        }

        // Perform a single iteration, mutating iterator object
        let mut iter = map_iterator.get_iter();
        let iter_result = iter.next();
        map_iterator.store_iter(iter);

        match iter_result {
            None => {
                map_iterator.is_done = true;
                Ok(create_iter_result_object(cx, cx.undefined(), true))
            }
            Some((key, value)) => match map_iterator.kind {
                MapIteratorKind::Key => {
                    let key_value: Value = key.into();
                    let key_handle = key_value.to_handle(cx);

                    Ok(create_iter_result_object(cx, key_handle, false))
                }
                MapIteratorKind::Value => {
                    let value_handle = value.to_handle(cx);

                    Ok(create_iter_result_object(cx, value_handle, false))
                }
                MapIteratorKind::KeyAndValue => {
                    let key_value: Value = key.into();
                    let key_handle = key_value.to_handle(cx);
                    let value_handle = value.to_handle(cx);
                    let result_pair = create_array_from_list(cx, &[key_handle, value_handle]);

                    Ok(create_iter_result_object(cx, result_pair.into(), false))
                }
            },
        }
    }
}

impl HeapObject for HeapPtr<MapIterator> {
    fn byte_size(&self) -> usize {
        size_of::<MapIterator>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.map);
    }
}
