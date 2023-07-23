use super::{
    completion::EvalResult, intrinsics::intrinsics::Intrinsic, object_value::ObjectValue,
    realm::Realm, Context, Handle, Value,
};

pub struct GcObject;

impl GcObject {
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        object.intrinsic_func(cx, cx.names.run(), Self::run, 0, realm);

        object.to_handle()
    }

    fn run(
        cx: &mut Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        cx.heap.run_gc();
        cx.undefined().into()
    }
}
