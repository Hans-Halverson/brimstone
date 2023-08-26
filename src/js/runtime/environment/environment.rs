use crate::{
    heap_trait_object,
    js::runtime::{
        completion::EvalResult, object_value::ObjectValue, reference::Reference,
        string_value::StringValue, Context, Handle, Value,
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
    fn has_binding(&self, cx: Context, name: Handle<StringValue>) -> EvalResult<bool>;
    fn create_mutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()>;
    fn create_immutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        is_strict: bool,
    ) -> EvalResult<()>;
    fn initialize_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
    ) -> EvalResult<()>;
    fn set_mutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
        is_strict: bool,
    ) -> EvalResult<()>;
    fn get_binding_value(
        &self,
        cx: Context,
        name: Handle<StringValue>,
        _is_strict: bool,
    ) -> EvalResult<Handle<Value>>;
    fn delete_binding(&mut self, cx: Context, name: Handle<StringValue>) -> EvalResult<bool>;
    fn has_this_binding(&self) -> bool;
    fn has_super_binding(&self) -> bool;
    fn with_base_object(&self) -> Option<Handle<ObjectValue>>;

    fn get_this_binding(&self, cx: Context) -> EvalResult<Handle<Value>>;

    // Optional reference to the outer (parent) environment. If None this is the global environment.
    // Implements section 8.1 Lexical Environment, but embedded in each environment record.
    fn outer(&self) -> Option<DynEnvironment>;

    // Downcasts
    fn as_function_environment(&mut self) -> Option<Handle<FunctionEnvironment>> {
        None
    }

    fn as_global_environment(&mut self) -> Option<Handle<GlobalEnvironment>> {
        None
    }

    fn as_object_environment(&mut self) -> Option<Handle<ObjectEnvironment>> {
        None
    }
}

// 8.1.2.1 GetIdentifierReference
pub fn get_identifier_reference(
    cx: Context,
    env: Option<DynEnvironment>,
    name: Handle<StringValue>,
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
