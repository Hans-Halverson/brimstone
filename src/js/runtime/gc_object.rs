use crate::{
    handle_scope,
    js::runtime::{abstract_operations::define_property_or_throw, PropertyDescriptor},
    must,
};

use super::{
    eval_result::EvalResult, gc::Heap, intrinsics::intrinsics::Intrinsic,
    object_value::ObjectValue, realm::Realm, Context, Handle, Value,
};

pub struct GcObject;

impl GcObject {
    fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        object.intrinsic_func(cx, cx.names.run(), Self::run, 0, realm);

        object.to_handle()
    }

    /// Install the GC object on the realm's global object.
    #[allow(unused)]
    pub fn install(cx: Context, realm: Handle<Realm>) {
        handle_scope!(cx, {
            let gc_object = GcObject::new(cx, realm);
            let desc = PropertyDescriptor::data(gc_object.as_value(), true, false, true);
            must!(define_property_or_throw(cx, realm.global_object(), cx.names.gc(), desc))
        });
    }

    pub fn run(cx: Context, _: Handle<Value>, _: &[Handle<Value>]) -> EvalResult<Handle<Value>> {
        Heap::run_gc(cx);
        Ok(cx.undefined())
    }
}
