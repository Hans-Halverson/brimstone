use crate::js::runtime::{
    builtin_function::BuiltinFunction, completion::EvalResult, object_value::ObjectValue,
    realm::Realm, Context, Handle, Value,
};

use super::intrinsics::Intrinsic;

pub struct PromiseConstructor;

impl PromiseConstructor {
    // 27.2.4 Properties of the Promise Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.promise(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::PromisePrototype).into(),
        );

        func
    }

    // 27.2.3.1 Promise
    pub fn construct(
        _: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        unimplemented!("Promise constructor")
    }
}
