use std::mem::size_of;

use crate::{
    cast_from_value_fn, extend_object,
    js::runtime::{
        abstract_operations::length_of_array_like,
        array_object::create_array_from_list,
        completion::EvalResult,
        error::type_error_,
        gc::{HeapObject, HeapVisitor},
        iterator::create_iter_result_object,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        value::Value,
        Context, Handle, HeapPtr,
    },
    maybe, set_uninit,
};

use super::intrinsics::Intrinsic;

// 23.1.5 Array Iterator Objects
extend_object! {
    pub struct ArrayIterator {
        array: HeapPtr<ObjectValue>,
        kind: ArrayIteratorKind,
        current_index: usize,
        get_length: fn(cx: &mut Context, array: Handle<ObjectValue>) -> EvalResult<u64>,
    }
}

pub enum ArrayIteratorKind {
    Key,
    Value,
    KeyAndValue,
}

impl ArrayIterator {
    pub fn new(
        cx: &mut Context,
        array: Handle<ObjectValue>,
        kind: ArrayIteratorKind,
    ) -> Handle<ArrayIterator> {
        let mut object = object_create::<ArrayIterator>(
            cx,
            ObjectKind::ArrayIterator,
            Intrinsic::ArrayIteratorPrototype,
        );

        // Only difference between array and typed array iterators is length getter, so calculate
        // on iterator start to avoid computing on every iteration.
        let get_length = if array.is_typed_array() {
            Self::get_typed_array_length
        } else {
            Self::get_array_like_length
        };

        set_uninit!(object.array, array.get_());
        set_uninit!(object.kind, kind);
        set_uninit!(object.current_index, 0);
        set_uninit!(object.get_length, get_length);

        object.to_handle()
    }

    fn array(&self) -> Handle<ObjectValue> {
        self.array.to_handle()
    }

    fn get_typed_array_length(cx: &mut Context, array: Handle<ObjectValue>) -> EvalResult<u64> {
        let typed_array = array.as_typed_array();
        if typed_array.viewed_array_buffer().is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        (typed_array.array_length() as u64).into()
    }

    fn get_array_like_length(cx: &mut Context, array: Handle<ObjectValue>) -> EvalResult<u64> {
        length_of_array_like(cx, array)
    }

    cast_from_value_fn!(ArrayIterator, "Array Iterator");
}

// 23.1.5.2 The %ArrayIteratorPrototype% Object
pub struct ArrayIteratorPrototype;

impl ArrayIteratorPrototype {
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let proto = realm.get_intrinsic(Intrinsic::IteratorPrototype);
        let mut object = ObjectValue::new(cx, Some(proto), true);

        object.intrinsic_func(cx, cx.names.next(), Self::next, 0, realm);

        // 23.1.5.2.2 %ArrayIteratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let to_string_tag_value = cx.alloc_string("Array Iterator").into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(to_string_tag_value, false, false, true),
        );

        object
    }

    // 23.1.5.2.1 %ArrayIteratorPrototype%.next
    // Adapted from the abstract closure in 23.1.5.1 CreateArrayIterator
    fn next(
        cx: &mut Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let mut array_iterator = maybe!(ArrayIterator::cast_from_value(cx, this_value));
        let array = array_iterator.array();

        // Dispatches based on whether this is array or typed array
        let length = maybe!((array_iterator.get_length)(cx, array));

        let current_index = array_iterator.current_index as u64;
        if current_index >= length {
            return create_iter_result_object(cx, cx.undefined(), true).into();
        }

        array_iterator.current_index += 1;

        match array_iterator.kind {
            ArrayIteratorKind::Key => {
                let key = Value::from(current_index).to_handle(cx);
                create_iter_result_object(cx, key, false).into()
            }
            ArrayIteratorKind::Value => {
                let property_key = PropertyKey::from_u64(cx, current_index).to_handle(cx);
                let value = maybe!(array.get(cx, property_key, array.into()));
                create_iter_result_object(cx, value, false).into()
            }
            ArrayIteratorKind::KeyAndValue => {
                let key = Value::from(current_index).to_handle(cx);
                let property_key = PropertyKey::from_u64(cx, current_index).to_handle(cx);
                let value = maybe!(array.get(cx, property_key, array.into()));

                let result_pair = create_array_from_list(cx, &[key, value]);
                create_iter_result_object(cx, result_pair.into(), false).into()
            }
        }
    }
}

impl HeapObject for HeapPtr<ArrayIterator> {
    fn byte_size(&self) -> usize {
        size_of::<ArrayIterator>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.array);
    }
}
