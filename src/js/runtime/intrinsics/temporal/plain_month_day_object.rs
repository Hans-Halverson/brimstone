use temporal_rs::PlainMonthDay;

use crate::{
    extend_object,
    runtime::{
        Context, EvalResult, Handle, HeapItemKind, HeapPtr,
        gc::{HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
    },
    set_uninit,
};

// PlainMonthDay Objects (https://tc39.es/proposal-temporal/#sec-temporal-plainmonthday-objects)
extend_object! {
    pub struct PlainMonthDayObject {
        month_day: PlainMonthDay,
    }
}

impl PlainMonthDayObject {
    pub fn new(cx: Context, month_day: PlainMonthDay) -> EvalResult<Handle<PlainMonthDayObject>> {
        let constructor = cx.get_intrinsic(Intrinsic::PlainMonthDayConstructor);
        Self::new_from_constructor(cx, constructor, month_day)
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        month_day: PlainMonthDay,
    ) -> EvalResult<Handle<PlainMonthDayObject>> {
        let mut object = object_create_from_constructor::<PlainMonthDayObject>(
            cx,
            constructor,
            HeapItemKind::PlainMonthDayObject,
            Intrinsic::PlainMonthDayPrototype,
        )?;

        set_uninit!(object.month_day, month_day);

        Ok(object.to_handle())
    }

    pub fn month_day(&self) -> &PlainMonthDay {
        &self.month_day
    }
}

impl HeapItem for PlainMonthDayObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<PlainMonthDayObject>()
    }

    fn visit_pointers(mut plain_month_day_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        plain_month_day_object.visit_object_pointers(visitor);
    }
}
