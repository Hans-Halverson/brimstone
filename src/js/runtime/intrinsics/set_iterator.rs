use std::mem::size_of;

use crate::{
    cast_from_value_fn, extend_object,
    js::runtime::{
        array_object::create_array_from_list,
        collections::{index_map::GcSafeEntriesIter, BsIndexMap},
        completion::EvalResult,
        error::type_error_,
        gc::{HeapObject, HeapVisitor},
        iterator::create_iter_result_object,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        property::Property,
        realm::Realm,
        value::ValueCollectionKey,
        Context, Handle, HeapPtr, Value,
    },
    maybe, set_uninit,
};

use super::{intrinsics::Intrinsic, set_object::SetObject};

// 24.2.5 Set Iterator Objects
extend_object! {
    pub struct SetIterator {
        // Component parts of an index_map::GcSafeEntriesIter
        set: HeapPtr<BsIndexMap<ValueCollectionKey, ()>>,
        next_entry_index: usize,
        kind: SetIteratorKind,
    }
}

pub enum SetIteratorKind {
    Value,
    KeyAndValue,
}

impl SetIterator {
    pub fn new(cx: Context, set: Handle<SetObject>, kind: SetIteratorKind) -> Handle<SetIterator> {
        let mut object = object_create::<SetIterator>(
            cx,
            ObjectKind::SetIterator,
            Intrinsic::SetIteratorPrototype,
        );

        set_uninit!(object.set, set.set_data().cast());
        set_uninit!(object.next_entry_index, 0);
        set_uninit!(object.kind, kind);

        object.to_handle()
    }

    cast_from_value_fn!(SetIterator, "Set Iterator");

    fn get_iter(&self) -> GcSafeEntriesIter<ValueCollectionKey, ()> {
        GcSafeEntriesIter::<ValueCollectionKey, ()>::from_parts(
            self.set.to_handle(),
            self.next_entry_index,
        )
    }

    fn store_iter(&mut self, iter: GcSafeEntriesIter<ValueCollectionKey, ()>) {
        let (set, next_entry_index) = iter.to_parts();
        self.set = set.get_();
        self.next_entry_index = next_entry_index;
    }
}

// 24.2.5.2 The %SetIteratorPrototype% Object
pub struct SetIteratorPrototype;

impl SetIteratorPrototype {
    pub fn new(mut cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true);

        object.intrinsic_func(cx, cx.names.next(), Self::next, 0, realm);

        // 24.2.5.2.2 %SetIteratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let to_string_tag_value = cx.alloc_string("Set Iterator").into();
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
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let mut set_iterator = maybe!(SetIterator::cast_from_value(cx, this_value));

        // Perform a single iteration, mutating iterator object
        let mut iter = set_iterator.get_iter();
        let iter_result = iter.next();
        set_iterator.store_iter(iter);

        match iter_result {
            None => create_iter_result_object(cx, cx.undefined(), true).into(),
            Some((value, _)) => {
                let value_value: Value = value.into();
                let value_handle = value_value.to_handle(cx);

                match set_iterator.kind {
                    SetIteratorKind::Value => {
                        create_iter_result_object(cx, value_handle, false).into()
                    }
                    SetIteratorKind::KeyAndValue => {
                        let result_pair = create_array_from_list(cx, &[value_handle, value_handle]);
                        create_iter_result_object(cx, result_pair.into(), false).into()
                    }
                }
            }
        }
    }
}

impl HeapObject for HeapPtr<SetIterator> {
    fn byte_size(&self) -> usize {
        size_of::<SetIterator>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.set);
    }
}
