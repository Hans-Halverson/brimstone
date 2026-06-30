use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        error::type_error,
        eval_result::EvalResult,
        gc::{HeapItem, HeapVisitor},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction},
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        realm::Realm,
    },
    runtime_fn, set_uninit,
};

// WeakRef Objects (https://tc39.es/ecma262/#sec-weak-ref-objects)
extend_object! {
    pub struct WeakRefObject {
        // Weakly held reference to a value. Can only be an object, symbol, or undefined.
        weak_ref_target: Value,
        // Holds the address of the next weak ref that has been visited during garbage collection.
        // Unused outside of garbage collection.
        next_weak_ref: Option<HeapPtr<WeakRefObject>>,
    }
}

impl WeakRefObject {
    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        value: Handle<Value>,
    ) -> EvalResult<Handle<WeakRefObject>> {
        let mut object = object_create_from_constructor::<WeakRefObject>(
            cx,
            constructor,
            HeapItemKind::WeakRefObject,
            Intrinsic::WeakRefPrototype,
        )?;

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

pub struct WeakRefConstructor;

impl WeakRefConstructor {
    /// Properties of the WeakRef Constructor (https://tc39.es/ecma262/#sec-properties-of-the-weak-ref-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::WeakRefConstructor_construct,
            1,
            cx.names.weak_ref(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::WeakRefPrototype)?;

        builder.build()
    }

    runtime_fn! {
    /// WeakRef (https://tc39.es/ecma262/#sec-weak-ref-target)
    fn construct(cx, _, arguments) {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            return type_error(cx, "WeakRef constructor must be called with new");
        };

        let target_value = arguments.get(cx, 0);
        if !can_be_held_weakly(cx, *target_value) {
            return type_error(
                cx,
                "WeakRef constructor argument must be an object or an unregistered symbol",
            );
        }

        Ok(WeakRefObject::new_from_constructor(cx, new_target, target_value)?.as_value())
    }}
}

/// CanBeHeldWeakly (https://tc39.es/ecma262/#sec-canbeheldweakly)
pub fn can_be_held_weakly(cx: Context, value: Value) -> bool {
    if value.is_object() {
        true
    } else if value.is_symbol() {
        // Only unregistered symbols are allowed to be held weakly. Registered symbols are never
        // garbage collected.
        let symbol_value = value.as_symbol();
        !cx.global_symbol_registry()
            .iter_gc_unsafe()
            .any(|(_, symbol)| symbol.ptr_eq(&symbol_value))
    } else {
        false
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
