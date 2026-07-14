use temporal_rs::PlainDate;

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
    /// PlainDate Objects (https://tc39.es/proposal-temporal/#sec-temporal-plaindate-objects)
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
        let mut object = ObjectBuilder::<PlainDateObject>::new(cx)
            .constructor_proto(constructor, Intrinsic::PlainDatePrototype)?
            .build()?;

        set_uninit!(object.date, date);

        Ok(object.to_handle())
    }

    pub fn date(&self) -> &PlainDate {
        &self.date
    }
}

impl HeapItem for PlainDateObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<PlainDateObject>()
    }

    fn visit_pointers(mut plain_date_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        plain_date_object.visit_object_pointers(visitor);
    }
}
