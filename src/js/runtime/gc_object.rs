use crate::{
    handle_scope, intrinsic_methods, must_a,
    runtime::{
        Context, Handle, PropertyDescriptor,
        abstract_operations::define_property_or_throw,
        alloc_error::AllocResult,
        gc::{GcType, Heap},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        realm::Realm,
    },
    runtime_fn,
};

pub struct GcObject;

impl GcObject {
    fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        intrinsic_methods!(cx, builder, {
            run GcObject_run (0),
        });

        builder.build()
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

    runtime_fn! {
    fn run(cx, _, _) {
        Heap::run_gc(cx, GcType::Normal);
        Ok(cx.undefined())
    }}
}
