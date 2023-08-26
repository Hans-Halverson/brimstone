use crate::{
    js::runtime::{
        builtin_function::BuiltinFunction, completion::EvalResult,
        eval::function::create_dynamic_function, object_value::ObjectValue, property::Property,
        realm::Realm, Context, Handle, Value,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

pub struct FunctionConstructor;

impl FunctionConstructor {
    // 20.2.2 Properties of the Function Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            cx.names.function(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            cx,
            cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::FunctionPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func
    }

    // 20.2.1.1 Function
    fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let constructor = cx.current_execution_context_ptr().function();
        maybe!(create_dynamic_function(cx, constructor, new_target, arguments)).into()
    }
}
