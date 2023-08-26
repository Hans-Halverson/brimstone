use std::mem::size_of;

use crate::{
    cast_from_value_fn, extend_object,
    js::runtime::{
        array_object::create_array_from_list,
        collections::index_map::GcSafeEntriesIter,
        completion::EvalResult,
        error::type_error_,
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
    maybe, set_uninit,
};

use super::{
    intrinsics::Intrinsic,
    map_object::{MapObject, ValueMap},
};

// 24.1.5 Map Iterator Objects
extend_object! {
    pub struct MapIterator {
        // Component parts of an index_map::GcSafeEntriesIter
        map: HeapPtr<ValueMap>,
        next_entry_index: usize,
        kind: MapIteratorKind,
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
        self.map = map.get_();
        self.next_entry_index = next_entry_index;
    }
}

// 24.1.5.2 The %MapIteratorPrototype% Object
pub struct MapIteratorPrototype;

impl MapIteratorPrototype {
    pub fn new(mut cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true);

        object.intrinsic_func(cx, cx.names.next(), Self::next, 0, realm);

        // 24.1.5.2.2 %MapIteratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let to_string_tag_value = cx.alloc_string("Map Iterator").into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(to_string_tag_value, false, false, true),
        );

        object
    }

    // 24.1.5.2.1 %MapIteratorPrototype%.next
    // Adapted from the abstract closure in 24.1.5.1 CreateMapIterator
    fn next(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let mut map_iterator = maybe!(MapIterator::cast_from_value(cx, this_value));

        // Perform a single iteration, mutating iterator object
        let mut iter = map_iterator.get_iter();
        let iter_result = iter.next();
        map_iterator.store_iter(iter);

        match iter_result {
            None => create_iter_result_object(cx, cx.undefined(), true).into(),
            Some((key, value)) => match map_iterator.kind {
                MapIteratorKind::Key => {
                    let key_value: Value = key.into();
                    let key_handle = key_value.to_handle(cx);

                    create_iter_result_object(cx, key_handle, false).into()
                }
                MapIteratorKind::Value => {
                    let value_handle = value.to_handle(cx);

                    create_iter_result_object(cx, value_handle, false).into()
                }
                MapIteratorKind::KeyAndValue => {
                    let key_value: Value = key.into();
                    let key_handle = key_value.to_handle(cx);
                    let value_handle = value.to_handle(cx);
                    let result_pair = create_array_from_list(cx, &[key_handle, value_handle]);

                    create_iter_result_object(cx, result_pair.into(), false).into()
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
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.map);
    }
}
