use temporal_rs::Instant;

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

// Temporal.Instant Objects (https://tc39.es/proposal-temporal/#sec-temporal-instant-objects)
extend_object! {
    pub struct InstantObject {
        // Contains an `i128` field and so is 16-byte aligned. Must only access through the
        // alignment wrapper.
        instant: HeapUnaligned<Instant>,
    }
}

impl InstantObject {
    pub fn new(cx: Context, instant: Instant) -> EvalResult<Handle<InstantObject>> {
        let constructor = cx.get_intrinsic(Intrinsic::InstantConstructor);
        Self::new_from_constructor(cx, constructor, instant)
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        instant: Instant,
    ) -> EvalResult<Handle<InstantObject>> {
        let mut object = object_create_from_constructor::<InstantObject>(
            cx,
            constructor,
            HeapItemKind::InstantObject,
            Intrinsic::InstantPrototype,
        )?;

        set_uninit!(object.instant, HeapUnaligned::new(instant));

        Ok(object.to_handle())
    }

    pub fn instant(&self) -> Instant {
        self.instant.get()
    }
}

impl HeapItem for InstantObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<InstantObject>()
    }

    fn visit_pointers(instant_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        instant_object.visit_object_pointers(visitor);
    }
}
