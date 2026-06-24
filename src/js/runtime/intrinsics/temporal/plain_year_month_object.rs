use temporal_rs::PlainYearMonth;

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

impl HeapItem for HeapPtr<PlainYearMonthObject> {
    fn byte_size(&self) -> usize {
        size_of::<PlainYearMonthObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
    }
}
