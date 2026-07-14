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
    /// Number Objects (https://tc39.es/ecma262/#sec-number-objects)
    pub struct NumberObject {
        /// The number value wrapped by this object
        number_data: f64,
    }
}

impl NumberObject {
    pub fn new(cx: Context, number_data: f64) -> AllocResult<Handle<NumberObject>> {
        let mut object = ObjectBuilder::<NumberObject>::new(cx)
            .intrinsic_proto(Intrinsic::NumberPrototype)
            .build()?;

        set_uninit!(object.number_data, number_data);

        Ok(object.to_handle())
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        number_data: f64,
    ) -> EvalResult<Handle<NumberObject>> {
        let mut object = ObjectBuilder::<NumberObject>::new(cx)
            .constructor_proto(constructor, Intrinsic::NumberPrototype)?
            .build()?;

        set_uninit!(object.number_data, number_data);

        Ok(object.to_handle())
    }

    pub fn new_with_proto(
        cx: Context,
        proto: Handle<ObjectValue>,
        number_data: f64,
    ) -> AllocResult<Handle<NumberObject>> {
        let mut object = ObjectBuilder::<NumberObject>::new(cx)
            .proto(proto)
            .build()?;

        set_uninit!(object.number_data, number_data);

        Ok(object.to_handle())
    }

    pub fn number_data(&self) -> f64 {
        self.number_data
    }

    pub fn set_number_data(&mut self, number_data: f64) {
        self.number_data = number_data;
    }
}

impl HeapItem for NumberObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<NumberObject>()
    }

    fn visit_pointers(mut number_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        number_object.visit_object_pointers(visitor);
    }
}
