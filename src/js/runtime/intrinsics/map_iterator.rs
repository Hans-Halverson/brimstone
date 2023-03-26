use std::ops::Deref;

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    cast_from_value_fn, extend_object,
    js::runtime::{
        array_object::create_array_from_list,
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        error::type_error_,
        gc::{Gc, GcDeref},
        iterator::create_iter_result_object,
        object_value::{extract_object_vtable, Object, ObjectValue},
        ordinary_object::{object_ordinary_init, OrdinaryObject},
        property::{PrivateProperty, Property},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        realm::Realm,
        value::{Value, ValueMapIter},
        Context,
    },
    maybe,
};

use super::{intrinsics::Intrinsic, map_constructor::MapObject};

// 24.1.5 Map Iterator Objects
extend_object! {
    pub struct MapIterator<'a> {
        // Map is not used directly, but it held so that it is not GC'd while iterator exists
        map: Gc<MapObject>,
        iter: ValueMapIter<'a, Value>,
        kind: MapIteratorKind,
    }
}

pub enum MapIteratorKind {
    Key,
    Value,
    KeyAndValue,
}

impl<'a> GcDeref for MapIterator<'a> {}

impl<'a> Into<Gc<ObjectValue>> for Gc<MapIterator<'a>> {
    fn into(self) -> Gc<ObjectValue> {
        Gc::from_ptr(self.as_ref() as *const _ as *mut ObjectValue)
    }
}

impl<'a> Into<Gc<ObjectValue>> for &MapIterator<'a> {
    fn into(self) -> Gc<ObjectValue> {
        Gc::from_ptr(self as *const _ as *mut ObjectValue)
    }
}

impl<'a> Into<Gc<ObjectValue>> for &mut MapIterator<'a> {
    fn into(self) -> Gc<ObjectValue> {
        Gc::from_ptr(self as *const _ as *mut ObjectValue)
    }
}

impl<'a> MapIterator<'a> {
    const VTABLE: *const () = extract_object_vtable::<MapIterator>();

    pub fn new(cx: &mut Context, mut map: Gc<MapObject>, kind: MapIteratorKind) -> Gc<MapIterator> {
        let proto = cx
            .current_realm()
            .get_intrinsic(Intrinsic::MapIteratorPrototype);

        let mut object = cx.heap.alloc_uninit::<MapIterator>();
        object._vtable = Self::VTABLE;

        object_ordinary_init(object.object_mut(), proto);

        object.map = map;
        object.iter = map.map_data().iter();
        object.kind = kind;

        object
    }

    cast_from_value_fn!(MapIterator, "Map Iterator");
}

#[wrap_ordinary_object]
impl<'a> Object for MapIterator<'a> {}

// 24.1.5.2 The %MapIteratorPrototype% Object
pub struct MapIteratorPrototype;

impl MapIteratorPrototype {
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(cx, Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true);

        object.intrinsic_func(cx, &cx.names.next(), Self::next, 0, realm);

        // 24.1.5.2.2 %MapIteratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        let to_string_tag_value = cx.heap.alloc_string(String::from("Map Iterator")).into();
        object.set_property(
            &to_string_tag_key,
            Property::data(to_string_tag_value, false, false, true),
        );

        object.into()
    }

    // 24.1.5.2.1 %MapIteratorPrototype%.next
    // Adapted from the abstract closure in 24.1.5.1 CreateMapIterator
    fn next(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
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
