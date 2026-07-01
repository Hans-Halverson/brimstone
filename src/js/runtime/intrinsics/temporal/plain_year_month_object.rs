use temporal_rs::PlainYearMonth;

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

// PlainYearMonth Objects (https://tc39.es/proposal-temporal/#sec-temporal-plainyearmonth-objects)
extend_object! {
    pub struct PlainYearMonthObject {
        year_month: PlainYearMonth,
    }
}

impl PlainYearMonthObject {
    pub fn new(
        cx: Context,
        year_month: PlainYearMonth,
    ) -> EvalResult<Handle<PlainYearMonthObject>> {
        let constructor = cx.get_intrinsic(Intrinsic::PlainYearMonthConstructor);
        Self::new_from_constructor(cx, constructor, year_month)
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        year_month: PlainYearMonth,
    ) -> EvalResult<Handle<PlainYearMonthObject>> {
        let mut object = object_create_from_constructor::<PlainYearMonthObject>(
            cx,
            constructor,
            HeapItemKind::PlainYearMonthObject,
            Intrinsic::PlainYearMonthPrototype,
        )?;

        set_uninit!(object.year_month, year_month);

        Ok(object.to_handle())
    }

    pub fn year_month(&self) -> &PlainYearMonth {
        &self.year_month
    }
}

impl HeapItem for PlainYearMonthObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<PlainYearMonthObject>()
    }

    fn visit_pointers(mut plain_year_month_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        plain_year_month_object.visit_object_pointers(visitor);
    }
}
