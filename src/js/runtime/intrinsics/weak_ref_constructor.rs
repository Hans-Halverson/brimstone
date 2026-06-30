use crate::{
    runtime::{
        Context, Handle, Value,
        alloc_error::AllocResult,
        error::type_error,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic, rust_runtime::RuntimeFunction, weak_ref_object::WeakRefObject,
        },
        object_value::ObjectValue,
        realm::Realm,
    },
    runtime_fn,
};

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
