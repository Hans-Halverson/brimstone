use std::ops::Deref;

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    cast_from_value_fn, impl_gc_into,
    js::runtime::{
        abstract_operations::length_of_array_like,
        array_object::create_array_from_list,
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        error::type_error_,
        gc::{Gc, GcDeref},
        iterator::create_iter_result_object,
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::{ordinary_object_create, OrdinaryObject},
        property::{PrivateProperty, Property},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        realm::Realm,
        value::Value,
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 23.1.5 Array Iterator Objects
#[repr(C)]
pub struct ArrayIterator {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    array: Gc<ObjectValue>,
    kind: ArrayIteratorKind,
    current_index: usize,
    get_length: fn(array: Gc<ObjectValue>, cx: &mut Context) -> EvalResult<u64>,
}

pub enum ArrayIteratorKind {
    Key,
    Value,
    KeyAndValue,
}

impl GcDeref for ArrayIterator {}

impl_gc_into!(ArrayIterator, ObjectValue);

impl ArrayIterator {
    const VTABLE: *const () = extract_object_vtable::<ArrayIterator>();

    pub fn new(
        cx: &mut Context,
        array: Gc<ObjectValue>,
        kind: ArrayIteratorKind,
    ) -> Gc<ArrayIterator> {
        let proto = cx
            .current_realm()
            .get_intrinsic(Intrinsic::ArrayIteratorPrototype);
        let object = ordinary_object_create(proto);

        // Only difference between array and typed array iterators is length getter, so calculate
        // on iterator start to avoid computing on every iteration.
        let get_length = if array.is_typed_array() {
            Self::get_typed_array_length
        } else {
            Self::get_array_like_length
        };

        cx.heap.alloc(ArrayIterator {
            _vtable: Self::VTABLE,
            object,
            array,
            kind,
            current_index: 0,
            get_length,
        })
    }

    fn get_typed_array_length(array: Gc<ObjectValue>, cx: &mut Context) -> EvalResult<u64> {
        let typed_array = array.as_typed_array();
        if typed_array.viewed_array_buffer().is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        (typed_array.array_length() as u64).into()
    }

    fn get_array_like_length(array: Gc<ObjectValue>, cx: &mut Context) -> EvalResult<u64> {
        length_of_array_like(cx, array)
    }

    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }

    cast_from_value_fn!(ArrayIterator, "Array Iterator");
}

#[wrap_ordinary_object]
impl Object for ArrayIterator {}

// 23.1.5.2 The %ArrayIteratorPrototype% Object
pub struct ArrayIteratorPrototype;

impl ArrayIteratorPrototype {
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true);

        object.intrinsic_func(cx, &cx.names.next(), Self::next, 0, realm);

        // 23.1.5.2.2 %ArrayIteratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        let to_string_tag_value = cx.heap.alloc_string(String::from("Array Iterator")).into();
        object.set_property(
            &to_string_tag_key,
            Property::data(to_string_tag_value, false, false, true),
        );

        cx.heap.alloc(object).into()
    }

    // 23.1.5.2.1 %ArrayIteratorPrototype%.next
    // Adapted from the abstract closure in 23.1.5.1 CreateArrayIterator
    fn next(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut array_iterator = maybe!(ArrayIterator::cast_from_value(cx, this_value));
        let array = array_iterator.array;

        // Dispatches based on whether this is array or typed array
        let length = maybe!((array_iterator.get_length)(array, cx));

        let current_index = array_iterator.current_index as u64;
        if current_index >= length {
            return create_iter_result_object(cx, Value::undefined(), true).into();
        }

        array_iterator.current_index += 1;

        match array_iterator.kind {
            ArrayIteratorKind::Key => {
                create_iter_result_object(cx, Value::from(current_index), false).into()
            }
            ArrayIteratorKind::Value => {
                let property_key = PropertyKey::from_u64(cx, current_index);
                let value = maybe!(array.get(cx, &property_key, array.into()));
                create_iter_result_object(cx, value, false).into()
            }
            ArrayIteratorKind::KeyAndValue => {
                let key = Value::from(current_index);
                let property_key = PropertyKey::from_u64(cx, current_index);
                let value = maybe!(array.get(cx, &property_key, array.into()));

                let result_pair = create_array_from_list(cx, &[key, value]);
                create_iter_result_object(cx, result_pair.into(), false).into()
            }
        }
    }
}
