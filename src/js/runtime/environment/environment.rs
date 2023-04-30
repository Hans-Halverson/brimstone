use crate::{
    heap_trait_object,
    js::runtime::{
        completion::EvalResult, gc::Gc, object_value::ObjectValue, reference::Reference,
        string_value::StringValue, value::Value, Context,
    },
    maybe,
};

use super::{
    function_environment::FunctionEnvironment, global_environment::GlobalEnvironment,
    object_environment::ObjectEnvironment,
};

// 9.1 Environment Record
pub trait Environment {
    // Environment functions from spec
    fn has_binding(&self, cx: &mut Context, name: Gc<StringValue>) -> EvalResult<bool>;
    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()>;
    fn create_immutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        is_strict: bool,
    ) -> EvalResult<()>;
    fn initialize_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        value: Value,
    ) -> EvalResult<()>;
    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        value: Value,
        is_strict: bool,
    ) -> EvalResult<()>;
    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: Gc<StringValue>,
        _is_strict: bool,
    ) -> EvalResult<Value>;
    fn delete_binding(&mut self, cx: &mut Context, name: Gc<StringValue>) -> EvalResult<bool>;
    fn has_this_binding(&self) -> bool;
    fn has_super_binding(&self) -> bool;
    fn with_base_object(&self) -> Option<Gc<ObjectValue>>;

    fn get_this_binding(&self, cx: &mut Context) -> EvalResult<Value>;

    // Optional reference to the outer (parent) environment. If None this is the global environment.
    // Implements section 8.1 Lexical Environment, but embedded in each environment record.
    fn outer(&self) -> Option<DynEnvironment>;

    // Downcasts
    fn as_function_environment(&mut self) -> Option<Gc<FunctionEnvironment>> {
        None
    }

    fn as_global_environment(&mut self) -> Option<Gc<GlobalEnvironment>> {
        None
    }

    fn as_object_environment(&mut self) -> Option<Gc<ObjectEnvironment>> {
        None
    }
}

// 8.1.2.1 GetIdentifierReference
pub fn get_identifier_reference(
    cx: &mut Context,
    env: Option<DynEnvironment>,
    name: Gc<StringValue>,
    is_strict: bool,
) -> EvalResult<Reference> {
    match env {
        None => Reference::new_unresolvable(name, is_strict).into(),
        Some(env) => {
            if maybe!(env.has_binding(cx, name)) {
                Reference::new_env(env, name, is_strict).into()
            } else {
                get_identifier_reference(cx, env.outer(), name, is_strict)
            }
        }
    }
}

heap_trait_object!(Environment, DynEnvironment, HeapDynEnvironment, into_dyn_env);
