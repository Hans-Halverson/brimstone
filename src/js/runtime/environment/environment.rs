use crate::{
    js::runtime::{
        completion::AbstractResult,
        gc::{Gc, GcDeref},
        reference::Reference,
        value::Value,
        Context,
    },
    maybe_,
};

use super::{
    declarative_environment::DeclarativeEnvironment, function_environment::FunctionEnvironment,
};

// 8.1.1 Environment Record
pub trait Environment {
    // Environment functions from spec
    fn has_binding(&self, cx: &mut Context, name: &str) -> AbstractResult<bool>;
    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        can_delete: bool,
    ) -> AbstractResult<()>;
    fn create_immutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        is_strict: bool,
    ) -> AbstractResult<()>;
    fn initialize_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        value: Value,
    ) -> AbstractResult<()>;
    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()>;
    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: &str,
        _is_strict: bool,
    ) -> AbstractResult<Value>;
    fn delete_binding(&mut self, cx: &mut Context, name: &str) -> AbstractResult<bool>;
    fn has_this_binding(&self) -> bool;
    fn has_super_binding(&self) -> bool;
    fn with_base_object(&self) -> Value;

    fn get_this_binding(&self, cx: &mut Context) -> AbstractResult<Value>;

    // Optional reference to the outer (parent) environment. If None this is the global environment.
    // Implements section 8.1 Lexical Environment, but embedded in each environment record.
    fn outer(&self) -> Option<Gc<dyn Environment>>;

    // Downcasts
    fn as_function_environment(&mut self) -> Option<&mut FunctionEnvironment> {
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
) -> AbstractResult<Reference> {
    match env {
        None => Reference::new_value(Value::undefined(), name.to_string(), is_strict).into(),
        Some(env) => {
            if maybe_!(env.has_binding(cx, name)) {
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

pub fn placeholder_environment(cx: &mut Context) -> Gc<dyn Environment> {
    let env = cx.heap.alloc(DeclarativeEnvironment::new(None));
    to_trait_object(env)
}
