use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        Context, Handle, HeapPtr, Value,
        eval_result::EvalResult,
        gc::{HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::ObjectBuilder,
    },
    set_uninit,
};

extend_object! {
    /// WeakRef Objects (https://tc39.es/ecma262/#sec-weak-ref-objects)
    pub struct WeakRefObject {
        /// Weakly held reference to a value. Can only be an object, symbol, or undefined.
        weak_ref_target: Value,
        /// Holds the address of the next weak ref that has been visited during garbage collection.
        /// Unused outside of garbage collection.
        next_weak_ref: Option<HeapPtr<WeakRefObject>>,
    }
}

impl WeakRefObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        value: Handle<Value>,
    ) -> EvalResult<Handle<WeakRefObject>> {
        let mut object = ObjectBuilder::<WeakRefObject>::new(cx)
            .constructor_proto(constructor, Intrinsic::WeakRefPrototype)?
            .build()?;

        set_uninit!(object.weak_ref_target, *value);

        Ok(object.to_handle())
    }

    pub fn weak_ref_target(&self) -> Value {
        self.weak_ref_target
    }

    pub fn set_weak_ref_target(&mut self, weak_ref_target: Value) {
        self.weak_ref_target = weak_ref_target;
    }

    pub fn next_weak_ref(&self) -> Option<HeapPtr<WeakRefObject>> {
        self.next_weak_ref
    }

    pub fn set_next_weak_ref(&mut self, next_weak_ref: Option<HeapPtr<WeakRefObject>>) {
        self.next_weak_ref = next_weak_ref;
    }
}

impl HeapItem for WeakRefObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<WeakRefObject>()
    }

    fn visit_pointers(mut weak_ref_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        weak_ref_object.visit_object_pointers(visitor);
        visitor.visit_weak_value(&mut weak_ref_object.weak_ref_target);

        // Intentionally do not visit next_weak_ref
    }
}
