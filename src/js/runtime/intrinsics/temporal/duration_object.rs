use temporal_rs::Duration;

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

// Temporal.Duration Objects (https://tc39.es/proposal-temporal/#sec-temporal-duration-objects)
extend_object! {
    pub struct DurationObject {
        duration: [Value; RawBytesEncoding::num_values::<Duration>()],
    }
}

impl DurationObject {
    pub fn new(cx: Context, duration: Duration) -> EvalResult<Handle<DurationObject>> {
        let constructor = cx.get_intrinsic(Intrinsic::DurationConstructor);
        Self::new_from_constructor(cx, constructor, duration)
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        duration: Duration,
    ) -> EvalResult<Handle<DurationObject>> {
        let mut object = object_create_from_constructor::<DurationObject>(
            cx,
            constructor,
            HeapItemKind::DurationObject,
            Intrinsic::DurationPrototype,
        )?;

        set_uninit!(object.duration, RawBytesEncoding::encode(&duration));

        Ok(object.to_handle())
    }

    pub fn duration(&self) -> Duration {
        RawBytesEncoding::decode(&self.duration)
    }
}

impl HeapItem for DurationObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<DurationObject>()
    }

    fn visit_pointers(duration_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        duration_object.visit_object_pointers(visitor);
    }
}
