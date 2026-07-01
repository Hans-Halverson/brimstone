use std::mem::size_of;

use crate::{
    cast_from_value_fn, extend_object, intrinsic_methods,
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        error::type_error,
        eval_result::EvalResult,
        gc::{HeapItem, HeapVisitor},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::intrinsics::Intrinsic,
        iterator::create_iter_result_object,
        object_value::ObjectValue,
        ordinary_object::object_create,
        property::Property,
        realm::Realm,
        string_value::{FlatString, SafeCodePointIterator},
    },
    runtime_fn, set_uninit,
};

// String Iterator Objects (https://tc39.es/ecma262/#sec-string-iterator-objects)
extend_object! {
    pub struct StringIteratorObject {
        iter: SafeCodePointIterator,
    }
}

impl StringIteratorObject {
    pub fn new(
        cx: Context,
        string: Handle<FlatString>,
    ) -> AllocResult<Handle<StringIteratorObject>> {
        let mut object = object_create::<StringIteratorObject>(
            cx,
            HeapItemKind::StringIteratorObject,
            Intrinsic::StringIteratorPrototype,
        )?;

        set_uninit!(object.iter, string.iter_code_points_safe());

        Ok(object.to_handle())
    }

    cast_from_value_fn!(StringIteratorObject, "String Iterator");
}

/// The %StringIteratorPrototype% Object (https://tc39.es/ecma262/#sec-%stringiteratorprototype%-object)
pub struct StringIteratorPrototype;

impl StringIteratorPrototype {
    pub fn new(mut cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::IteratorPrototype)?;

        intrinsic_methods!(cx, builder, {
            next StringIteratorPrototype_next (0),
        });

        // %StringIteratorPrototype% [ @@toStringTag ] (https://tc39.es/ecma262/#sec-%stringiteratorprototype%-%symbol.tostringtag%)
        let to_string_tag_value = cx.alloc_static_string("String Iterator")?.into();
        builder.property(
            cx.symbols.to_string_tag(),
            Property::data(to_string_tag_value, false, false, true),
        )?;

        builder.build()
    }

    runtime_fn! {
    /// %StringIteratorPrototype%.next (https://tc39.es/ecma262/#sec-%stringiteratorprototype%.next)
    fn next(cx, this_value, _) {
        let mut string_iterator = StringIteratorObject::cast_from_value(cx, this_value)?;

        match string_iterator.iter.next() {
            None => Ok(create_iter_result_object(cx, cx.undefined(), true)?),
            Some(next_code_point) => {
                let code_point_string =
                    FlatString::from_code_point(cx, next_code_point)?.as_string();
                Ok(create_iter_result_object(cx, code_point_string.into(), false)?)
            }
        }
    }}
}

impl HeapItem for StringIteratorObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<StringIteratorObject>()
    }

    fn visit_pointers(mut string_iterator: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        string_iterator.visit_object_pointers(visitor);
        string_iterator.iter.visit_pointers(visitor);
    }
}
