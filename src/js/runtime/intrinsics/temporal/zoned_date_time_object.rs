use temporal_rs::ZonedDateTime;

use crate::{
    extend_object,
    runtime::{
        Context, EvalResult, Handle, HeapItemKind, HeapPtr,
        gc::{HeapItem, HeapUnaligned, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
    },
    set_uninit,
};

// ZonedDateTime Objects (https://tc39.es/proposal-temporal/#sec-temporal-zoneddatetime-objects)
extend_object! {
    pub struct ZonedDateTimeObject {
        // Contains an `i128` field and so is 16-byte aligned. Must only access through the
        // alignment wrapper.
        zoned_date_time: HeapUnaligned<ZonedDateTime>,
    }
}

impl ZonedDateTimeObject {
    pub fn new(
        cx: Context,
        zoned_date_time: ZonedDateTime,
    ) -> EvalResult<Handle<ZonedDateTimeObject>> {
        let constructor = cx.get_intrinsic(Intrinsic::ZonedDateTimeConstructor);
        Self::new_from_constructor(cx, constructor, zoned_date_time)
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        zoned_date_time: ZonedDateTime,
    ) -> EvalResult<Handle<ZonedDateTimeObject>> {
        let mut object = object_create_from_constructor::<ZonedDateTimeObject>(
            cx,
            constructor,
            HeapItemKind::ZonedDateTimeObject,
            Intrinsic::ZonedDateTimePrototype,
        )?;

        set_uninit!(object.zoned_date_time, HeapUnaligned::new(zoned_date_time));

        Ok(object.to_handle())
    }

    pub fn zoned_date_time(&self) -> ZonedDateTime {
        self.zoned_date_time.get()
    }
}

impl HeapItem for ZonedDateTimeObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<ZonedDateTimeObject>()
    }

    fn visit_pointers(mut zoned_date_time_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        zoned_date_time_object.visit_object_pointers(visitor);
    }
}
