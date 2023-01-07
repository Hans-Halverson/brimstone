use crate::{
    js::runtime::{
        completion::EvalResult,
        gc::{Gc, GcDeref},
        object_value::ObjectValue,
        reference::Reference,
        value::Value,
        Context,
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
    fn has_binding(&self, cx: &mut Context, name: &str) -> EvalResult<bool>;
    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        can_delete: bool,
    ) -> EvalResult<()>;
    fn create_immutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        is_strict: bool,
    ) -> EvalResult<()>;
    fn initialize_binding(&mut self, cx: &mut Context, name: &str, value: Value) -> EvalResult<()>;
    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        value: Value,
        is_strict: bool,
    ) -> EvalResult<()>;
    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: &str,
        _is_strict: bool,
    ) -> EvalResult<Value>;
    fn delete_binding(&mut self, cx: &mut Context, name: &str) -> EvalResult<bool>;
    fn has_this_binding(&self) -> bool;
    fn has_super_binding(&self) -> bool;
    fn with_base_object(&self) -> Option<Gc<ObjectValue>>;

    fn get_this_binding(&self, cx: &mut Context) -> EvalResult<Value>;

    // Optional reference to the outer (parent) environment. If None this is the global environment.
    // Implements section 8.1 Lexical Environment, but embedded in each environment record.
    fn outer(&self) -> Option<Gc<dyn Environment>>;

    // Downcasts
    fn as_function_environment(&mut self) -> Option<&mut FunctionEnvironment> {
        None
    }

    fn as_global_environment(&mut self) -> Option<&mut GlobalEnvironment> {
        None
    }

    fn as_object_environment(&mut self) -> Option<&mut ObjectEnvironment> {
        None
    }
}

impl GcDeref for dyn Environment {}

// 8.1.2.1 GetIdentifierReference
pub fn get_identifier_reference(
    cx: &mut Context,
    env: Option<Gc<dyn Environment>>,
    name: &str,
    is_strict: bool,
) -> EvalResult<Reference> {
    match env {
        None => Reference::new_unresolvable(name.to_string(), is_strict).into(),
        Some(env) => {
            if maybe!(env.has_binding(cx, name)) {
                Reference::new_env(env, name.to_string(), is_strict).into()
            } else {
                get_identifier_reference(cx, env.outer(), name, is_strict)
            }
        }
    }
}

// Convert an environment GC reference to a trait object
pub fn to_trait_object<'a, T: Environment + 'a>(env: Gc<T>) -> Gc<dyn Environment + 'a> {
    Gc::from_ptr(env.as_ptr() as *mut dyn Environment)
}
