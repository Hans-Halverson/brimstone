use crate::{
    cast_from_value_fn, extend_object,
    js::runtime::{
        array_object::create_array_from_list,
        completion::EvalResult,
        error::type_error_,
        gc::{Gc, HandleValue},
        iterator::create_iter_result_object,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        value::{Value, ValueMapIter},
        Context, Handle, HeapPtr,
    },
    maybe, set_uninit,
};

use super::{intrinsics::Intrinsic, map_constructor::MapObject};

// 24.1.5 Map Iterator Objects
extend_object! {
    pub struct MapIterator<'a> {
        // Map is not used directly, but is held so that it is not GC'd while iterator exists
        map: HeapPtr<MapObject>,
        iter: ValueMapIter<'a, Value>,
        kind: MapIteratorKind,
    }
}

pub enum MapIteratorKind {
    Key,
    Value,
    KeyAndValue,
}

impl<'a> MapIterator<'a> {
    pub fn new(
        cx: &mut Context,
        mut map: Handle<MapObject>,
        kind: MapIteratorKind,
    ) -> Handle<MapIterator> {
        let mut object = object_create::<MapIterator>(
            cx,
            ObjectKind::MapIterator,
            Intrinsic::MapIteratorPrototype,
        );

        set_uninit!(object.map, map.get_());
        set_uninit!(object.iter, map.map_data().iter());
        set_uninit!(object.kind, kind);

        Handle::from_heap(object)
    }

    cast_from_value_fn!(MapIterator, "Map Iterator");
}

// 24.1.5.2 The %MapIteratorPrototype% Object
pub struct MapIteratorPrototype;

impl MapIteratorPrototype {
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true);

        object.intrinsic_func(cx, cx.names.next(), Self::next, 0, realm);

        // 24.1.5.2.2 %MapIteratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        let to_string_tag_value = cx.alloc_string(String::from("Map Iterator")).into();
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
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let mut map_iterator = maybe!(MapIterator::cast_from_value(cx, this_value));

        match map_iterator.iter.next() {
            None => create_iter_result_object(cx, Value::undefined(), true).into(),
            Some((key, value)) => match map_iterator.kind {
                MapIteratorKind::Key => {
                    let key = (*key).into();
                    create_iter_result_object(cx, key, false).into()
                }
                MapIteratorKind::Value => {
                    let value = *value;
                    create_iter_result_object(cx, value, false).into()
                }
                MapIteratorKind::KeyAndValue => {
                    let key = (*key).into();
                    let value = *value;
                    let result_pair = create_array_from_list(cx, &[key, value]);

                    create_iter_result_object(cx, result_pair.into(), false).into()
                }
            },
        }
    }
}
