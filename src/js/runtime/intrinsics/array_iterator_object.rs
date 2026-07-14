use std::mem::size_of;

use crate::{
    cast_from_value_fn, extend_object, intrinsic_methods,
    runtime::{
        Context, Handle, HeapPtr,
        abstract_operations::length_of_array_like,
        alloc_error::AllocResult,
        array_object::create_array_from_list,
        error::type_error,
        eval_result::EvalResult,
        gc::{HeapItem, HeapVisitor},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic,
            typed_array_prototype::{
                is_typed_array_out_of_bounds, make_typed_array_with_buffer_witness_record,
                typed_array_length,
            },
        },
        iterator::create_iter_result_object,
        object_value::ObjectValue,
        ordinary_object::ObjectBuilder,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        value::Value,
    },
    runtime_fn, set_uninit,
};

extend_object! {
    /// Array Iterator Objects (https://tc39.es/ecma262/#sec-array-iterator-objects)
    pub struct ArrayIteratorObject {
        array: HeapPtr<ObjectValue>,
        kind: ArrayIteratorKind,
        is_done: bool,
        current_index: usize,
    }
}

pub enum ArrayIteratorKind {
    Key,
    Value,
    KeyAndValue,
}

impl ArrayIteratorObject {
    pub fn new(
        cx: Context,
        array: Handle<ObjectValue>,
        kind: ArrayIteratorKind,
    ) -> AllocResult<Handle<ArrayIteratorObject>> {
        let mut object = ObjectBuilder::<ArrayIteratorObject>::new(cx)
            .intrinsic_proto(Intrinsic::ArrayIteratorPrototype)
            .build()?;

        set_uninit!(object.array, *array);
        set_uninit!(object.is_done, false);
        set_uninit!(object.kind, kind);
        set_uninit!(object.current_index, 0);

        Ok(object.to_handle())
    }

    fn array(&self) -> Handle<ObjectValue> {
        self.array.to_handle()
    }

    cast_from_value_fn!(ArrayIteratorObject, "Array Iterator");
}

/// The %ArrayIteratorPrototype% Object (https://tc39.es/ecma262/#sec-%arrayiteratorprototype%-object)
pub struct ArrayIteratorPrototype;

impl ArrayIteratorPrototype {
    pub fn new(mut cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::new_object(cx, realm, Intrinsic::IteratorPrototype)?;

        intrinsic_methods!(cx, builder, {
            next ArrayIteratorPrototype_next (0),
        });

        // %ArrayIteratorPrototype% [ @@toStringTag ] (https://tc39.es/ecma262/#sec-%arrayiteratorprototype%-%symbol.tostringtag%)
        let to_string_tag_value = cx.alloc_static_string("Array Iterator")?.into();
        builder.property(
            cx.symbols.to_string_tag(),
            Property::data(to_string_tag_value, false, false, true),
        )?;

        builder.build()
    }

    runtime_fn! {
    /// %ArrayIteratorPrototype%.next (https://tc39.es/ecma262/#sec-%arrayiteratorprototype%.next)
    /// Adapted from the abstract closure in CreateArrayIterator (https://tc39.es/ecma262/#sec-createarrayiterator)
    fn next(cx, this_value, _) {
        let mut array_iterator = ArrayIteratorObject::cast_from_value(cx, this_value)?;
        let array = array_iterator.array();

        // Early return if iterator is already done, before potential failure when checking length
        if array_iterator.is_done {
            return Ok(create_iter_result_object(cx, cx.undefined(), true)?);
        }

        // Get the length of the underlying array-like or typed array
        let length = if array.is_typed_array() {
            let typed_array = array.as_typed_array();

            let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
            if is_typed_array_out_of_bounds(&typed_array_record) {
                return type_error(cx, "typed array is out of bounds");
            }

            typed_array_length(&typed_array_record) as u64
        } else {
            length_of_array_like(cx, array)?
        };

        let current_index = array_iterator.current_index as u64;
        if array_iterator.is_done || current_index >= length {
            array_iterator.is_done = true;
            return Ok(create_iter_result_object(cx, cx.undefined(), true)?);
        }

        array_iterator.current_index += 1;

        match array_iterator.kind {
            ArrayIteratorKind::Key => {
                let key = cx.number(current_index);
                Ok(create_iter_result_object(cx, key, false)?)
            }
            ArrayIteratorKind::Value => {
                let property_key = PropertyKey::from_u64_handle(cx, current_index)?;
                let value = array.get(cx, property_key, array.into())?;
                Ok(create_iter_result_object(cx, value, false)?)
            }
            ArrayIteratorKind::KeyAndValue => {
                let key = cx.number(current_index);
                let property_key = PropertyKey::from_u64_handle(cx, current_index)?;
                let value = array.get(cx, property_key, array.into())?;

                let result_pair = create_array_from_list(cx, &[key, value])?;
                Ok(create_iter_result_object(cx, result_pair.into(), false)?)
            }
        }
    }}
}

impl HeapItem for ArrayIteratorObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<ArrayIteratorObject>()
    }

    fn visit_pointers(mut array_iterator: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        array_iterator.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut array_iterator.array);
    }
}
