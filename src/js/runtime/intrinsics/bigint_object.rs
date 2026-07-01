use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        BigIntValue, Context, HeapItemKind, HeapPtr,
        alloc_error::AllocResult,
        gc::{Handle, HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        ordinary_object::object_create,
    },
    set_uninit,
};

// BigInt Objects (https://tc39.es/ecma262/#sec-bigint-objects)
extend_object! {
    pub struct BigIntObject {
        // The BigInt value wrapped by this object
        bigint_data: HeapPtr<BigIntValue>,
    }
}

impl BigIntObject {
    pub fn new_from_value(
        cx: Context,
        bigint_data: Handle<BigIntValue>,
    ) -> AllocResult<Handle<BigIntObject>> {
        let mut object = object_create::<BigIntObject>(
            cx,
            HeapItemKind::BigIntObject,
            Intrinsic::BigIntPrototype,
        )?;

        set_uninit!(object.bigint_data, *bigint_data);

        Ok(object.to_handle())
    }

    pub fn bigint_data(&self) -> Handle<BigIntValue> {
        self.bigint_data.to_handle()
    }
}

impl HeapItem for BigIntObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<BigIntObject>()
    }

    fn visit_pointers(mut bigint_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        bigint_object.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut bigint_object.bigint_data);
    }
}
