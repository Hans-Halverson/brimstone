use std::{ops::Deref, str::Chars};

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    cast_from_value_fn,
    js::runtime::{
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
        value::{StringValue, Value},
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 22.1.5 String Iterator Objects
#[repr(C)]
pub struct StringIterator<'a> {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    // String is not used directly, but it held so that it is not GC'd while iterator exists
    string: Gc<StringValue>,
    chars_iter: Chars<'a>,
}

impl<'a> GcDeref for StringIterator<'a> {}

impl<'a> Into<Gc<ObjectValue>> for Gc<StringIterator<'a>> {
    fn into(self) -> Gc<ObjectValue> {
        Gc::from_ptr(self.as_ref() as *const _ as *mut ObjectValue)
    }
}

impl<'a> Into<Gc<ObjectValue>> for &StringIterator<'a> {
    fn into(self) -> Gc<ObjectValue> {
        Gc::from_ptr(self as *const _ as *mut ObjectValue)
    }
}

impl<'a> Into<Gc<ObjectValue>> for &mut StringIterator<'a> {
    fn into(self) -> Gc<ObjectValue> {
        Gc::from_ptr(self as *const _ as *mut ObjectValue)
    }
}

impl<'a> StringIterator<'a> {
    const VTABLE: *const () = extract_object_vtable::<StringIterator>();

    pub fn new(cx: &mut Context, string: Gc<StringValue>) -> Gc<StringIterator> {
        let proto = cx
            .current_realm()
            .get_intrinsic(Intrinsic::StringIteratorPrototype);
        let object = ordinary_object_create(proto);

        cx.heap.alloc(StringIterator {
            _vtable: Self::VTABLE,
            object,
            string,
            chars_iter: string.str().chars(),
        })
    }

    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }

    cast_from_value_fn!(StringIterator, "String Iterator");
}

#[wrap_ordinary_object]
impl<'a> Object for StringIterator<'a> {}

// 22.1.5.1 The %StringIteratorPrototype% Object
pub struct StringIteratorPrototype;

impl StringIteratorPrototype {
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true);

        object.intrinsic_func(cx, &cx.names.next(), Self::next, 0, realm);

        // 22.1.5.1.2 %StringIteratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        let to_string_tag_value = cx.heap.alloc_string(String::from("String Iterator")).into();
        object.set_property(
            &to_string_tag_key,
            Property::data(to_string_tag_value, false, false, true),
        );

        cx.heap.alloc(object).into()
    }

    // 22.1.5.1.1 %StringIteratorPrototype%.next
    fn next(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut string_iterator = maybe!(StringIterator::cast_from_value(cx, this_value));

        match string_iterator.chars_iter.next() {
            None => create_iter_result_object(cx, Value::undefined(), true).into(),
            Some(next_char) => {
                let char_string = cx.heap.alloc_string(String::from(next_char));
                create_iter_result_object(cx, char_string.into(), false).into()
            }
        }
    }
}
