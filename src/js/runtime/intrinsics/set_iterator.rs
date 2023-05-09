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
        realm::Realm,
        value::ValueSetIter,
        Context, Handle, HeapPtr,
    },
    maybe, set_uninit,
};

use super::{intrinsics::Intrinsic, set_constructor::SetObject};

// 24.2.5 Set Iterator Objects
extend_object! {
    pub struct SetIterator<'a> {
        // Set is not used directly, but it held so that it is not GC'd while iterator exists
        set: HeapPtr<SetObject>,
        iter: ValueSetIter<'a>,
        kind: SetIteratorKind,
    }
}

pub enum SetIteratorKind {
    Value,
    KeyAndValue,
}

impl<'a> SetIterator<'a> {
    pub fn new(
        cx: &mut Context,
        mut set: Handle<SetObject>,
        kind: SetIteratorKind,
    ) -> Handle<SetIterator> {
        let mut object = object_create::<SetIterator>(
            cx,
            ObjectKind::SetIterator,
            Intrinsic::SetIteratorPrototype,
        );

        set_uninit!(object.set, set.get_());
        set_uninit!(object.iter, set.set_data().iter());
        set_uninit!(object.kind, kind);

        object.to_handle()
    }

    cast_from_value_fn!(SetIterator, "Set Iterator");
}

// 24.2.5.2 The %SetIteratorPrototype% Object
pub struct SetIteratorPrototype;

impl SetIteratorPrototype {
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true);

        object.intrinsic_func(cx, cx.names.next(), Self::next, 0, realm);

        // 24.2.5.2.2 %SetIteratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let to_string_tag_value = cx.alloc_string(String::from("Set Iterator")).into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(to_string_tag_value, false, false, true),
        );

        object
    }

    // 24.2.5.2.1 %SetIteratorPrototype%.next
    // Adapted from the abstract closure in 24.2.5.1 CreateSetIterator
    fn next(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let mut set_iterator = maybe!(SetIterator::cast_from_value(cx, this_value));

        match set_iterator.iter.next() {
            None => create_iter_result_object(cx, cx.undefined(), true).into(),
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
