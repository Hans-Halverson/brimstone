use std::{collections::HashMap, rc::Rc};

use crate::{
    js::runtime::{completion::AbstractResult, value::Value, Context},
    maybe_,
};

use super::declarative_environment::DeclarativeEnvironment;

// 8.1 Lexical Environment
pub struct LexicalEnvironment {
    pub env: Rc<dyn Environment>,
    // Optional reference to the outer (parent) environment. If None this is the global environment.
    pub outer: Option<Rc<LexicalEnvironment>>,
}

impl LexicalEnvironment {
    /// Create a placeholder, "undefined" value that should not be used
    pub fn placeholder() -> LexicalEnvironment {
        LexicalEnvironment {
            env: Rc::new(DeclarativeEnvironment {
                bindings: HashMap::new(),
            }),
            outer: None,
        }
    }
}

// 8.1.1 Environment Record
pub trait Environment {
    fn has_binding(&self, name: &str) -> AbstractResult<bool>;
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
    fn delete_binding(&mut self, name: &str) -> AbstractResult<bool>;
    fn has_this_binding(&self) -> bool;
    fn has_super_binding(&self) -> bool;
    fn with_base_object(&self) -> Value;
}

pub struct Reference {
    base: ReferenceBase,
    name: String,
    is_strict: bool,
}

enum ReferenceBase {
    // Can only be undefined, an Object, a Boolean, a String, a Symbol, a Number, or a BigInt
    Value(Value),
    Env(Rc<dyn Environment>),
}

// 8.1.2.1 GetIdentifierReference
pub fn get_identifier_reference(
    env: Option<&LexicalEnvironment>,
    name: &str,
    is_strict: bool,
) -> AbstractResult<Reference> {
    match env {
        None => Reference {
            base: ReferenceBase::Value(Value::undefined()),
            name: name.to_string(),
            is_strict,
        }
        .into(),
        Some(env) => {
            if maybe_!(env.env.has_binding(name)) {
                Reference {
                    base: ReferenceBase::Env(env.env.clone()),
                    name: name.to_string(),
                    is_strict,
                }
                .into()
            } else {
                get_identifier_reference(env.outer.as_deref(), name, is_strict)
            }
        }
    }
}
