use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error,
        function::get_argument,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        realm::Realm,
        Context, Handle, HeapPtr, Value,
    },
    set_uninit,
};

use super::intrinsics::Intrinsic;

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
            ObjectKind::WeakRefObject,
            Intrinsic::WeakRefPrototype,
        )?;

        set_uninit!(object.weak_ref_target, value.get());

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
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.weak_ref(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::WeakRefPrototype).into(),
        );

        func
    }

    /// WeakRef (https://tc39.es/ecma262/#sec-weak-ref-target)
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            return type_error(cx, "WeakRef constructor must be called with new");
        };

        let target_value = get_argument(cx, arguments, 0);
        if !can_be_held_weakly(cx, target_value.get()) {
            return type_error(cx, "WeakRef only holds objects and symbols");
        }

        Ok(WeakRefObject::new_from_constructor(cx, new_target, target_value)?.as_value())
    }
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

impl HeapObject for HeapPtr<WeakRefObject> {
    fn byte_size(&self) -> usize {
        size_of::<WeakRefObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);

        // Intentionally do not visit weak_ref_target and next_weak_ref
    }
}
