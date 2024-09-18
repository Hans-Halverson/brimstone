use crate::{
    js::runtime::{
        builtin_function::BuiltinFunction, completion::EvalResult,
        eval::create_dynamic_function::create_dynamic_function, object_value::ObjectValue,
        realm::Realm, Context, Handle, Value,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

pub struct FunctionConstructor;

impl FunctionConstructor {
    /// Properties of the Function Constructor, https://tc39.es/ecma262/#sec-properties-of-the-function-constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.function(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::FunctionPrototype).into(),
        );

        func
    }

    /// Function, https://tc39.es/ecma262/#sec-function-p1-p2-pn-body
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let constructor = cx.current_function();
        maybe!(create_dynamic_function(
            cx,
            constructor,
            new_target,
            arguments,
            /* is_async */ false,
            /* is_generator */ false
        ))
        .into()
    }
}
