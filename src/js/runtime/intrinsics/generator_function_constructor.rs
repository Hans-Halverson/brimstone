use crate::js::runtime::{
    builtin_function::BuiltinFunction, completion::EvalResult,
    eval::create_dynamic_function::create_dynamic_function, object_value::ObjectValue,
    realm::Realm, Context, Handle, Value,
};

use super::intrinsics::Intrinsic;

pub struct GeneratorFunctionConstructor;

impl GeneratorFunctionConstructor {
    /// Properties of the GeneratorFunction Constructor (https://tc39.es/ecma262/#sec-properties-of-the-generatorfunction-constructor)
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

    /// GeneratorFunction (https://tc39.es/ecma262/#sec-generatorfunction)
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
            /* is_async */ false,
            /* is_generator */ true,
        )?
        .as_value())
    }
}
