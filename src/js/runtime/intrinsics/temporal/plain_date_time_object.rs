use temporal_rs::PlainDateTime;

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

// PlainDateTime Objects (https://tc39.es/proposal-temporal/#sec-temporal-plaindatetime-objects)
extend_object! {
    pub struct PlainDateTimeObject {
        date_time: [Value; RawBytesEncoding::num_values::<PlainDateTime>()],
    }
}

impl PlainDateTimeObject {
    pub fn new(cx: Context, date_time: PlainDateTime) -> EvalResult<Handle<PlainDateTimeObject>> {
        let constructor = cx.get_intrinsic(Intrinsic::PlainDateTimeConstructor);
        Self::new_from_constructor(cx, constructor, date_time)
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        date_time: PlainDateTime,
    ) -> EvalResult<Handle<PlainDateTimeObject>> {
        let mut object = object_create_from_constructor::<PlainDateTimeObject>(
            cx,
            constructor,
            HeapItemKind::PlainDateTimeObject,
            Intrinsic::PlainDateTimePrototype,
        )?;

        set_uninit!(object.date_time, RawBytesEncoding::encode(&date_time));

        Ok(object.to_handle())
    }

    pub fn date_time(&self) -> PlainDateTime {
        RawBytesEncoding::decode(&self.date_time)
    }
}

impl HeapItem for PlainDateTimeObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<PlainDateTimeObject>()
    }

    fn visit_pointers(plain_date_time_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        plain_date_time_object.visit_object_pointers(visitor);
    }
}
