use crate::js::runtime::{
    builtin_function::BuiltinFunction, completion::EvalResult,
    eval::create_dynamic_function::create_dynamic_function, object_value::ObjectValue,
    realm::Realm, Context, Handle, Value,
};

use super::intrinsics::Intrinsic;

pub struct AsyncGeneratorFunctionConstructor;

impl AsyncGeneratorFunctionConstructor {
    /// Properties of the AsyncGeneratorFunction Constructor (https://tc39.es/ecma262/#sec-properties-of-asyncgeneratorfunction)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let proto = realm.get_intrinsic(Intrinsic::FunctionConstructor);
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.async_generator_function(),
            realm,
            Some(proto),
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm
                .get_intrinsic(Intrinsic::AsyncGeneratorFunctionPrototype)
                .into(),
        );

        func
    }

    /// AsyncGeneratorFunction (https://tc39.es/ecma262/#sec-asyncgeneratorfunction)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let constructor = cx.current_function();
        Ok(create_dynamic_function(
            cx,
            constructor,
            new_target,
            arguments,
            /* is_async */ true,
            /* is_generator */ true,
        )?
        .as_value())
    }
}
