use temporal_rs::ZonedDateTime;

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

// ZonedDateTime Objects (https://tc39.es/proposal-temporal/#sec-temporal-zoneddatetime-objects)
extend_object! {
    pub struct ZonedDateTimeObject {
        zoned_date_time: [Value; RawBytesEncoding::num_values::<ZonedDateTime>()],
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

        set_uninit!(object.zoned_date_time, RawBytesEncoding::encode(&zoned_date_time));

        Ok(object.to_handle())
    }

    pub fn zoned_date_time(&self) -> ZonedDateTime {
        RawBytesEncoding::decode(&self.zoned_date_time)
    }
}

impl HeapItem for ZonedDateTimeObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<ZonedDateTimeObject>()
    }

    fn visit_pointers(zoned_date_time_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        zoned_date_time_object.visit_object_pointers(visitor);
    }
}
