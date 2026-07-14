use temporal_rs::PlainMonthDay;

use crate::{
    extend_object,
    runtime::{
        Context, EvalResult, Handle, HeapPtr,
        gc::{HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::ObjectBuilder,
    },
    set_uninit,
};

extend_object! {
    /// PlainMonthDay Objects (https://tc39.es/proposal-temporal/#sec-temporal-plainmonthday-objects)
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
        let mut object = ObjectBuilder::<PlainMonthDayObject>::new(cx)
            .constructor_proto(constructor, Intrinsic::PlainMonthDayPrototype)?
            .build()?;

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
