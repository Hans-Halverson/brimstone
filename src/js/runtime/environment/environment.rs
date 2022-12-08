use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::js::runtime::{
    completion::AbstractResult,
    gc::Gc,
    value::{ObjectValue, Value},
    Context,
};

use super::{
    declarative_environment::DeclarativeEnvironment, global_environment::GlobalEnvironment,
    object_environment::ObjectEnvironment,
};

// 8.1 Lexical Environment
pub struct LexicalEnvironment {
    env: Rc<dyn Environment>,
    // Optional reference to the outer (parent) environment. If None this is the global environment.
    outer: Option<Rc<LexicalEnvironment>>,
}

impl LexicalEnvironment {
    // 8.1.2.5 NewGlobalEnvironment
    pub fn new_global_environment(
        global_obj: Gc<ObjectValue>,
        global_this_val: Gc<ObjectValue>,
    ) -> LexicalEnvironment {
        let object_env = ObjectEnvironment {
            bindings: HashMap::new(),
            binding_obj: global_obj,
            with_environment: false,
        };
        let decl_env = DeclarativeEnvironment {
            bindings: HashMap::new(),
        };

        let global_env = GlobalEnvironment {
            object_env,
            global_this_val,
            decl_env,
            var_names: HashSet::new(),
        };

        let env = LexicalEnvironment {
            env: Rc::new(global_env),
            outer: None,
        };

        env
    }

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
