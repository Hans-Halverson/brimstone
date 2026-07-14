use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        Context, HeapPtr,
        alloc_error::AllocResult,
        eval_result::EvalResult,
        gc::{Handle, HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::ObjectBuilder,
    },
    set_uninit,
};

extend_object! {
    /// Boolean Objects (https://tc39.es/ecma262/#sec-boolean-objects)
    pub struct BooleanObject {
        /// The boolean value wrapped by this object
        boolean_data: bool,
    }
}

impl BooleanObject {
    pub fn new(cx: Context, boolean_data: bool) -> AllocResult<Handle<BooleanObject>> {
        let mut object = ObjectBuilder::<BooleanObject>::new(cx)
            .intrinsic_proto(Intrinsic::BooleanPrototype)
            .build()?;

        set_uninit!(object.boolean_data, boolean_data);

        Ok(object.to_handle())
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        boolean_data: bool,
    ) -> EvalResult<Handle<BooleanObject>> {
        let mut object = ObjectBuilder::<BooleanObject>::new(cx)
            .constructor_proto(constructor, Intrinsic::BooleanPrototype)?
            .build()?;

        set_uninit!(object.boolean_data, boolean_data);

        Ok(object.to_handle())
    }

    pub fn new_with_proto(
        cx: Context,
        proto: Handle<ObjectValue>,
        boolean_data: bool,
    ) -> AllocResult<Handle<BooleanObject>> {
        let mut object = ObjectBuilder::<BooleanObject>::new(cx)
            .proto(proto)
            .build()?;

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

    fn visit_pointers(mut boolean_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        boolean_object.visit_object_pointers(visitor);
    }
}
