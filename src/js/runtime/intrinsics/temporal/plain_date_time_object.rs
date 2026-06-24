use temporal_rs::PlainDateTime;

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

// PlainDateTime Objects (https://tc39.es/proposal-temporal/#sec-temporal-plaindatetime-objects)
extend_object! {
    pub struct PlainDateTimeObject {
        date_time: PlainDateTime,
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

        set_uninit!(object.date_time, date_time);

        Ok(object.to_handle())
    }

    pub fn date_time(&self) -> &PlainDateTime {
        &self.date_time
    }
}

impl HeapItem for HeapPtr<PlainDateTimeObject> {
    fn byte_size(&self) -> usize {
        size_of::<PlainDateTimeObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
    }
}
