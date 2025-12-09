use crate::{
    handle_scope, must_a,
    runtime::{
        abstract_operations::define_property_or_throw, alloc_error::AllocResult, PropertyDescriptor,
    },
};

use super::{
    eval_result::EvalResult,
    gc::{GcType, Heap},
    intrinsics::intrinsics::Intrinsic,
    object_value::ObjectValue,
    realm::Realm,
    Context, Handle, Value,
};

pub struct GcObject;

impl GcObject {
    fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        object.intrinsic_func(cx, cx.names.run(), Self::run, 0, realm)?;

        Ok(object.to_handle())
    }

    /// Install the GC object on the realm's global object.
    pub fn install(cx: Context, realm: Handle<Realm>) -> AllocResult<()> {
        handle_scope!(cx, {
            let gc_object = GcObject::new(cx, realm)?;
            let desc = PropertyDescriptor::data(gc_object.as_value(), true, false, true);
            must_a!(define_property_or_throw(cx, realm.global_object(), cx.names.gc(), desc));

            Ok(())
        })
    }

    pub fn run(cx: Context, _: Handle<Value>, _: &[Handle<Value>]) -> EvalResult<Handle<Value>> {
        Heap::run_gc(cx, GcType::Normal);
        Ok(cx.undefined())
    }
}
