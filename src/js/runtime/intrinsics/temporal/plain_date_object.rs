use temporal_rs::PlainDate;

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

// DataView Objects (https://tc39.es/ecma262/#sec-dataview-objects)
extend_object! {
    pub struct PlainDateObject {
        date: PlainDate,
    }
}

impl PlainDateObject {
    pub fn new(cx: Context, date: PlainDate) -> EvalResult<Handle<PlainDateObject>> {
        let constructor = cx.get_intrinsic(Intrinsic::PlainDateConstructor);
        Self::new_from_constructor(cx, constructor, date)
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        date: PlainDate,
    ) -> EvalResult<Handle<PlainDateObject>> {
        let mut object = object_create_from_constructor::<PlainDateObject>(
            cx,
            constructor,
            HeapItemKind::PlainDateObject,
            Intrinsic::PlainDatePrototype,
        )?;

        set_uninit!(object.date, date);

        Ok(object.to_handle())
    }

    pub fn date(&self) -> &PlainDate {
        &self.date
    }
}

impl HeapItem for HeapPtr<PlainDateObject> {
    fn byte_size(&self) -> usize {
        size_of::<PlainDateObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
    }
}
