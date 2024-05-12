use crate::js::runtime::{
    builtin_function::BuiltinFunction, completion::EvalResult, object_value::ObjectValue,
    realm::Realm, Context, Handle, Value,
};

use super::intrinsics::Intrinsic;

pub struct GeneratorFunctionConstructor;

impl GeneratorFunctionConstructor {
    // 27.3.2 Properties of the GeneratorFunction Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let proto = realm.get_intrinsic(Intrinsic::FunctionConstructor);
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.generator_function(),
            realm,
            Some(proto),
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm
                .get_intrinsic(Intrinsic::GeneratorFunctionPrototype)
                .into(),
        );

        func
    }

    // 27.3.1.1 GeneratorFunction
    pub fn construct(
        _: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        unimplemented!("GeneratorFunction constructor")
    }
}
