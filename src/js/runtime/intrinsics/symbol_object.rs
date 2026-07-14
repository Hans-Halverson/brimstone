use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        Context, HeapPtr, SymbolValue,
        alloc_error::AllocResult,
        gc::{Handle, HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        ordinary_object::ObjectBuilder,
    },
    set_uninit,
};

extend_object! {
    /// Symbol Objects (https://tc39.es/ecma262/#sec-symbol-objects)
    pub struct SymbolObject {
        /// The symbol value wrapped by this object
        symbol_data: HeapPtr<SymbolValue>,
    }
}

impl SymbolObject {
    pub fn new_from_value(
        cx: Context,
        symbol_data: Handle<SymbolValue>,
    ) -> AllocResult<Handle<SymbolObject>> {
        let mut object = ObjectBuilder::<SymbolObject>::new(cx)
            .intrinsic_proto(Intrinsic::SymbolPrototype)
            .build()?;

        set_uninit!(object.symbol_data, *symbol_data);

        Ok(object.to_handle())
    }

    pub fn symbol_data(&self) -> Handle<SymbolValue> {
        self.symbol_data.to_handle()
    }
}

impl HeapItem for SymbolObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<SymbolObject>()
    }

    fn visit_pointers(mut symbol_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        symbol_object.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut symbol_object.symbol_data);
    }
}
