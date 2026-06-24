use temporal_rs::Duration;

use crate::{
    extend_object,
    runtime::{
        Context, EvalResult, Handle, HeapPtr,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemKind,
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
    },
    set_uninit,
};

// Temporal.Duration Objects (https://tc39.es/proposal-temporal/#sec-temporal-duration-objects)
extend_object! {
    pub struct DurationObject {
        duration: Duration,
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

        set_uninit!(object.duration, duration);

        Ok(object.to_handle())
    }

    pub fn duration(&self) -> Duration {
        self.duration
    }
}

impl HeapItem for HeapPtr<DurationObject> {
    fn byte_size(&self) -> usize {
        size_of::<DurationObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
    }
}
