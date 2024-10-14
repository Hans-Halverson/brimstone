use std::mem::size_of;

use crate::{
    cast_from_value_fn, extend_object,
    js::runtime::{
        error::type_error,
        eval_result::EvalResult,
        gc::{HeapObject, HeapVisitor},
        iterator::create_iter_result_object,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        property::Property,
        realm::Realm,
        string_value::{FlatString, SafeCodePointIterator},
        Context, Handle, HeapPtr, Value,
    },
    set_uninit,
};

use super::intrinsics::Intrinsic;

// String Iterator Objects (https://tc39.es/ecma262/#sec-string-iterator-objects)
extend_object! {
    pub struct StringIterator {
        iter: SafeCodePointIterator,
    }
}

impl StringIterator {
    pub fn new(cx: Context, string: Handle<FlatString>) -> Handle<StringIterator> {
        let mut object = object_create::<StringIterator>(
            cx,
            ObjectKind::StringIterator,
            Intrinsic::StringIteratorPrototype,
        );

        set_uninit!(object.iter, string.iter_code_points_safe());

        object.to_handle()
    }

    cast_from_value_fn!(StringIterator, "String Iterator");
}

/// The %StringIteratorPrototype% Object (https://tc39.es/ecma262/#sec-%stringiteratorprototype%-object)
pub struct StringIteratorPrototype;

impl StringIteratorPrototype {
    pub fn new(mut cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let proto = realm.get_intrinsic(Intrinsic::IteratorPrototype);
        let mut object = ObjectValue::new(cx, Some(proto), true);

        object.intrinsic_func(cx, cx.names.next(), Self::next, 0, realm);

        // %StringIteratorPrototype% [ @@toStringTag ] (https://tc39.es/ecma262/#sec-%stringiteratorprototype%-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let to_string_tag_value = cx.alloc_string("String Iterator").into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(to_string_tag_value, false, false, true),
        );

        object
    }

    /// %StringIteratorPrototype%.next (https://tc39.es/ecma262/#sec-%stringiteratorprototype%.next)
    pub fn next(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let mut string_iterator = StringIterator::cast_from_value(cx, this_value)?;

        match string_iterator.iter.next() {
            None => Ok(create_iter_result_object(cx, cx.undefined(), true)),
            Some(next_code_point) => {
                let code_point_string =
                    FlatString::from_code_point(cx, next_code_point).as_string();
                Ok(create_iter_result_object(cx, code_point_string.into(), false))
            }
        }
    }
}

impl HeapObject for HeapPtr<StringIterator> {
    fn byte_size(&self) -> usize {
        size_of::<StringIterator>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        self.iter.visit_pointers(visitor);
    }
}
