use std::mem::size_of;

use crate::{
    cast_from_value_fn, extend_object, intrinsic_methods,
    runtime::{
        Context, Handle, HeapPtr,
        alloc_error::AllocResult,
        array_object::create_array_from_list,
        collections::{
            HashDosResistantHasher,
            index_map::{GcSafeEntriesIter, IndexMapInstance},
        },
        error::type_error,
        eval_result::EvalResult,
        gc::{HeapItem, HeapVisitor},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            map_object::{MapObject, ValueIndexMap},
        },
        iterator::create_iter_result_object,
        object_value::ObjectValue,
        ordinary_object::ObjectBuilder,
        property::Property,
        realm::Realm,
        value::{Value, ValueCollectionKey},
    },
    runtime_fn, set_uninit,
};

extend_object! {
    /// Map Iterator Objects (https://tc39.es/ecma262/#sec-map-iterator-objects)
    pub struct MapIteratorObject {
        /// Component parts of an index_map::GcSafeEntriesIter
        map: HeapPtr<ValueIndexMap>,
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

type GcSafeMapEntriesIter = GcSafeEntriesIter<ValueCollectionKey, Value, HashDosResistantHasher>;

impl MapIteratorObject {
    pub fn new(
        cx: Context,
        map: Handle<MapObject>,
        kind: MapIteratorKind,
    ) -> AllocResult<Handle<MapIteratorObject>> {
        let mut object = ObjectBuilder::<MapIteratorObject>::new(cx)
            .intrinsic_proto(Intrinsic::MapIteratorPrototype)
            .build()?;

        set_uninit!(object.map, map.map_data());
        set_uninit!(object.next_entry_index, 0);
        set_uninit!(object.kind, kind);
        set_uninit!(object.is_done, false);

        Ok(object.to_handle())
    }

    cast_from_value_fn!(MapIteratorObject, "Map Iterator");

    fn get_iter(&self) -> GcSafeMapEntriesIter {
        GcSafeMapEntriesIter::from_parts(self.map.to_handle().cast(), self.next_entry_index)
    }

    fn store_iter(&mut self, iter: GcSafeMapEntriesIter) {
        let (map, next_entry_index) = iter.to_parts();
        self.map = (*map).cast();
        self.next_entry_index = next_entry_index;
    }
}

/// The %MapIteratorPrototype% Object (https://tc39.es/ecma262/#sec-%mapiteratorprototype%-object)
pub struct MapIteratorPrototype;

impl MapIteratorPrototype {
    pub fn new(mut cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::new_object(cx, realm, Intrinsic::IteratorPrototype)?;

        intrinsic_methods!(cx, builder, {
            next MapIteratorPrototype_next (0),
        });

        // %MapIteratorPrototype% [ @@toStringTag ] (https://tc39.es/ecma262/#sec-%mapiteratorprototype%-%symbol.tostringtag%)
        let to_string_tag_value = cx.alloc_static_string("Map Iterator")?.into();
        builder.property(
            cx.symbols.to_string_tag(),
            Property::data(to_string_tag_value, false, false, true),
        )?;

        builder.build()
    }

    runtime_fn! {
    /// %MapIteratorPrototype%.next (https://tc39.es/ecma262/#sec-%mapiteratorprototype%.next)
    /// Adapted from the abstract closure in CreateMapIterator (https://tc39.es/ecma262/#sec-createmapiterator)
    fn next(cx, this_value, _) {
        let mut map_iterator = MapIteratorObject::cast_from_value(cx, this_value)?;

        // Check if iterator is already done
        if map_iterator.is_done {
            return Ok(create_iter_result_object(cx, cx.undefined(), true)?);
        }

        // Follow tombstone objects, fixing up iterator as needed. This may be a chain of tombstone
        // objects and we need to fix up the iterator at each step.
        while map_iterator.map.is_tombstone() {
            map_iterator.map = ValueIndexMap::fix_iterator_for_resized_map(
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
                Ok(create_iter_result_object(cx, cx.undefined(), true)?)
            }
            Some((key, value)) => match map_iterator.kind {
                MapIteratorKind::Key => {
                    let key_value: Value = key.into();
                    let key_handle = key_value.to_handle(cx);

                    Ok(create_iter_result_object(cx, key_handle, false)?)
                }
                MapIteratorKind::Value => {
                    let value_handle = value.to_handle(cx);

                    Ok(create_iter_result_object(cx, value_handle, false)?)
                }
                MapIteratorKind::KeyAndValue => {
                    let key_value: Value = key.into();
                    let key_handle = key_value.to_handle(cx);
                    let value_handle = value.to_handle(cx);
                    let result_pair = create_array_from_list(cx, &[key_handle, value_handle])?;

                    Ok(create_iter_result_object(cx, result_pair.into(), false)?)
                }
            },
        }
    }}
}

impl HeapItem for MapIteratorObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<MapIteratorObject>()
    }

    fn visit_pointers(mut map_iterator: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        map_iterator.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut map_iterator.map);
    }
}
