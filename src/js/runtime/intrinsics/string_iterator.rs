use crate::{
    cast_from_value_fn, extend_object,
    js::runtime::{
        completion::EvalResult,
        error::type_error_,
        gc::Gc,
        iterator::create_iter_result_object,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_ordinary_init,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        string_value::{CodePointIterator, StringValue},
        value::Value,
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 22.1.5 String Iterator Objects
extend_object! {
    pub struct StringIterator {
        // String is not used directly, but it held so that it is not GC'd while iterator exists
        string: Gc<StringValue>,
        code_points_iter: CodePointIterator,
    }
}

impl StringIterator {
    pub fn new(cx: &mut Context, string: Gc<StringValue>) -> Gc<StringIterator> {
        let proto = cx.get_intrinsic(Intrinsic::StringIteratorPrototype);

        let mut object = cx.heap.alloc_uninit::<StringIterator>();
        object_ordinary_init(cx, object.object(), ObjectKind::StringIterator, proto);

        object.string = string;
        object.code_points_iter = string.iter_code_points();

        object
    }

    cast_from_value_fn!(StringIterator, "String Iterator");
}

// 22.1.5.1 The %StringIteratorPrototype% Object
pub struct StringIteratorPrototype;

impl StringIteratorPrototype {
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let proto = realm.get_intrinsic(Intrinsic::IteratorPrototype);
        let mut object = ObjectValue::new(cx, Some(proto), true);

        object.intrinsic_func(cx, cx.names.next(), Self::next, 0, realm);

        // 22.1.5.1.2 %StringIteratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        let to_string_tag_value = cx.alloc_string(String::from("String Iterator")).into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(to_string_tag_value, false, false, true),
        );

        object.into()
    }

    // 22.1.5.1.1 %StringIteratorPrototype%.next
    fn next(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut string_iterator = maybe!(StringIterator::cast_from_value(cx, this_value));

        match string_iterator.code_points_iter.next() {
            None => create_iter_result_object(cx, Value::undefined(), true).into(),
            Some(next_code_point) => {
                let code_point_string = StringValue::from_code_point(cx, next_code_point);
                create_iter_result_object(cx, code_point_string.into(), false).into()
            }
        }
    }
}
