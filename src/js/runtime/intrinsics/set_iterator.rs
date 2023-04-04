use crate::{
    cast_from_value_fn, extend_object,
    js::runtime::{
        array_object::create_array_from_list,
        completion::EvalResult,
        error::type_error_,
        gc::Gc,
        iterator::create_iter_result_object,
        object_descriptor::ObjectKind,
        object_value::{HasObject, ObjectValue},
        ordinary_object::{object_ordinary_init, OrdinaryObject},
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        value::{Value, ValueSetIter},
        Context,
    },
    maybe,
};

use super::{intrinsics::Intrinsic, set_constructor::SetObject};

// 24.2.5 Set Iterator Objects
extend_object! {
    pub struct SetIterator<'a> {
        // Set is not used directly, but it held so that it is not GC'd while iterator exists
        set: Gc<SetObject>,
        iter: ValueSetIter<'a>,
        kind: SetIteratorKind,
    }
}

pub enum SetIteratorKind {
    Value,
    KeyAndValue,
}

impl<'a> SetIterator<'a> {
    pub fn new(cx: &mut Context, mut set: Gc<SetObject>, kind: SetIteratorKind) -> Gc<SetIterator> {
        let proto = cx
            .current_realm()
            .get_intrinsic(Intrinsic::SetIteratorPrototype);

        let mut object = cx.heap.alloc_uninit::<SetIterator>();
        object.descriptor = cx.base_descriptors.get(ObjectKind::SetIterator);

        object_ordinary_init(object.object_mut(), proto);

        object.set = set;
        object.iter = set.set_data().iter();
        object.kind = kind;

        object
    }

    cast_from_value_fn!(SetIterator, "Set Iterator");
}

// 24.2.5.2 The %SetIteratorPrototype% Object
pub struct SetIteratorPrototype;

impl SetIteratorPrototype {
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(cx, Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true);

        object.intrinsic_func(cx, &cx.names.next(), Self::next, 0, realm);

        // 24.2.5.2.2 %SetIteratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        let to_string_tag_value = cx.heap.alloc_string(String::from("Set Iterator")).into();
        object.set_property(
            &to_string_tag_key,
            Property::data(to_string_tag_value, false, false, true),
        );

        object.into()
    }

    // 24.2.5.2.1 %SetIteratorPrototype%.next
    // Adapted from the abstract closure in 24.2.5.1 CreateSetIterator
    fn next(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut set_iterator = maybe!(SetIterator::cast_from_value(cx, this_value));

        match set_iterator.iter.next() {
            None => create_iter_result_object(cx, Value::undefined(), true).into(),
            Some(value) => {
                let value = (*value).into();

                match set_iterator.kind {
                    SetIteratorKind::Value => create_iter_result_object(cx, value, false).into(),
                    SetIteratorKind::KeyAndValue => {
                        let result_pair = create_array_from_list(cx, &[value, value]);
                        create_iter_result_object(cx, result_pair.into(), false).into()
                    }
                }
            }
        }
    }
}
