use crate::{
    intrinsic_methods,
    runtime::{
        Context, Handle, Value,
        alloc_error::AllocResult,
        error::type_error,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{intrinsics::Intrinsic, weak_ref_constructor::WeakRefObject},
        object_value::ObjectValue,
        realm::Realm,
    },
    runtime_fn,
};

pub struct WeakRefPrototype;

impl WeakRefPrototype {
    /// Properties of the WeakRef Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-weak-ref-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once WeakRefConstructor has been created
        intrinsic_methods!(cx, builder, {
            deref WeakRefPrototype_deref (0),
        });

        // WeakRef.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-weak-ref.prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.weak_ref())?;

        builder.build()
    }

    runtime_fn! {
    /// WeakRef.prototype.deref (https://tc39.es/ecma262/#sec-weak-ref.prototype.deref)
    fn deref(cx, this_value, _) {
        if let Some(weak_ref_object) = this_weak_ref_value(this_value) {
            Ok(weak_ref_object.weak_ref_target().to_handle(cx))
        } else {
            type_error(cx, "WeakRef.prototype.deref must be called on a WeakRef")
        }
    }}
}

fn this_weak_ref_value(value: Handle<Value>) -> Option<Handle<WeakRefObject>> {
    if !value.is_object() {
        return None;
    }

    value.as_object().as_weak_ref_object()
}
