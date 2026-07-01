use temporal_rs::PlainTime;

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

// Temporal.PlainTime Objects (https://tc39.es/proposal-temporal/#sec-temporal-plaintime-objects)
extend_object! {
    pub struct PlainTimeObject {
        time: [Value; RawBytesEncoding::num_values::<PlainTime>()],
    }
}

impl PlainTimeObject {
    pub fn new(cx: Context, time: PlainTime) -> EvalResult<Handle<PlainTimeObject>> {
        let constructor = cx.get_intrinsic(Intrinsic::PlainTimeConstructor);
        Self::new_from_constructor(cx, constructor, time)
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        time: PlainTime,
    ) -> EvalResult<Handle<PlainTimeObject>> {
        let mut object = object_create_from_constructor::<PlainTimeObject>(
            cx,
            constructor,
            HeapItemKind::PlainTimeObject,
            Intrinsic::PlainTimePrototype,
        )?;

        set_uninit!(object.time, RawBytesEncoding::encode(&time));

        Ok(object.to_handle())
    }

    pub fn time(&self) -> PlainTime {
        RawBytesEncoding::decode(&self.time)
    }
}

impl HeapItem for PlainTimeObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<PlainTimeObject>()
    }

    fn visit_pointers(mut plain_time_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        plain_time_object.visit_object_pointers(visitor);
    }
}
