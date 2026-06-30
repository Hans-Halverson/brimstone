use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        Context, HeapItemKind, HeapPtr,
        alloc_error::AllocResult,
        eval_result::EvalResult,
        gc::{Handle, HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::{
            object_create, object_create_from_constructor, object_create_with_proto,
        },
    },
    set_uninit,
};

// Boolean Objects (https://tc39.es/ecma262/#sec-boolean-objects)
extend_object! {
    pub struct BooleanObject {
        // The boolean value wrapped by this object
        boolean_data: bool,
    }
}

impl BooleanObject {
    pub fn new(cx: Context, boolean_data: bool) -> AllocResult<Handle<BooleanObject>> {
        let mut object = object_create::<BooleanObject>(
            cx,
            HeapItemKind::BooleanObject,
            Intrinsic::BooleanPrototype,
        )?;

        set_uninit!(object.boolean_data, boolean_data);

        Ok(object.to_handle())
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        boolean_data: bool,
    ) -> EvalResult<Handle<BooleanObject>> {
        let mut object = object_create_from_constructor::<BooleanObject>(
            cx,
            constructor,
            HeapItemKind::BooleanObject,
            Intrinsic::BooleanPrototype,
        )?;

        set_uninit!(object.boolean_data, boolean_data);

        Ok(object.to_handle())
    }

    pub fn new_with_proto(
        cx: Context,
        proto: Handle<ObjectValue>,
        boolean_data: bool,
    ) -> AllocResult<Handle<BooleanObject>> {
        let mut object =
            object_create_with_proto::<BooleanObject>(cx, HeapItemKind::BooleanObject, proto)?;

        set_uninit!(object.boolean_data, boolean_data);

        Ok(object.to_handle())
    }

    pub fn boolean_data(&self) -> bool {
        self.boolean_data
    }

    pub fn set_boolean_data(&mut self, boolean_data: bool) {
        self.boolean_data = boolean_data;
    }
}

impl HeapItem for BooleanObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<BooleanObject>()
    }

    fn visit_pointers(boolean_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        boolean_object.visit_object_pointers(visitor);
    }
}
