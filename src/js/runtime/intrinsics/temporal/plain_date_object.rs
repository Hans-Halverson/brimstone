use temporal_rs::PlainDate;

use crate::{
    extend_object,
    runtime::{
        Context, EvalResult, Handle, HeapItemKind, HeapPtr, Value,
        gc::{HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        value::RawBytesEncoding,
    },
    set_uninit,
};

// PlainDate Objects (https://tc39.es/proposal-temporal/#sec-temporal-plaindate-objects)
extend_object! {
    pub struct PlainDateObject {
        date: [Value; RawBytesEncoding::num_values::<PlainDate>()],
    }
}

impl PlainDateObject {
    pub fn new(cx: Context, date: PlainDate) -> EvalResult<Handle<PlainDateObject>> {
        let constructor = cx.get_intrinsic(Intrinsic::PlainDateConstructor);
        Self::new_from_constructor(cx, constructor, date)
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        date: PlainDate,
    ) -> EvalResult<Handle<PlainDateObject>> {
        let mut object = object_create_from_constructor::<PlainDateObject>(
            cx,
            constructor,
            HeapItemKind::PlainDateObject,
            Intrinsic::PlainDatePrototype,
        )?;

        set_uninit!(object.date, RawBytesEncoding::encode(&date));

        Ok(object.to_handle())
    }

    pub fn date(&self) -> PlainDate {
        RawBytesEncoding::decode(&self.date)
    }
}

impl HeapItem for PlainDateObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<PlainDateObject>()
    }

    fn visit_pointers(plain_date_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        plain_date_object.visit_object_pointers(visitor);
    }
}
