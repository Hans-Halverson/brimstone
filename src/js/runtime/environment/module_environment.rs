use std::{collections::HashMap, rc::Rc};

use crate::js::runtime::{completion::AbstractResult, value::Value, Context};

use super::{
    declarative_environment::DeclarativeEnvironment,
    environment::{Environment, LexicalEnvironment},
};

pub struct ModuleEnvironment {
    env: DeclarativeEnvironment,
}

impl ModuleEnvironment {
    // 8.1.2.6 NewModuleEnvironment
    fn new(outer: Rc<LexicalEnvironment>) -> LexicalEnvironment {
        let module_env = ModuleEnvironment {
            env: DeclarativeEnvironment {
                bindings: HashMap::new(),
            },
        };

        LexicalEnvironment {
            env: Rc::new(module_env),
            outer: Some(outer),
        }
    }
}

impl Environment for ModuleEnvironment {
    // 8.1.1.5.1 GetBindingValue
    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: &str,
        is_strict: bool,
    ) -> AbstractResult<Value> {
        unimplemented!()
    }

    // 8.1.1.5.2 DeleteBinding
    fn delete_binding(&mut self, _name: &str) -> AbstractResult<bool> {
        unreachable!("ModuleEnvironment::delete_binding is never called according to the spec")
    }

    // 8.1.1.5.3 HasThisBinding
    fn has_this_binding(&self) -> bool {
        true
    }

    // All other methods inherited from DeclarativeEnvironment

    fn has_binding(&self, name: &str) -> AbstractResult<bool> {
        self.env.has_binding(name)
    }

    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        can_delete: bool,
    ) -> AbstractResult<()> {
        self.env.create_mutable_binding(cx, name, can_delete)
    }

    fn create_immutable_binding(
        &mut self,
        cx: &mut Context,
        name: String,
        is_strict: bool,
    ) -> AbstractResult<()> {
        self.env.create_immutable_binding(cx, name, is_strict)
    }

    fn initialize_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        value: Value,
    ) -> AbstractResult<()> {
        self.env.initialize_binding(cx, name, value)
    }

    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: &str,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()> {
        self.env.set_mutable_binding(cx, name, value, is_strict)
    }

    fn has_super_binding(&self) -> bool {
        self.env.has_super_binding()
    }

    fn with_base_object(&self) -> Value {
        self.env.with_base_object()
    }
}

impl ModuleEnvironment {
    // 8.1.1.5.4 GetThisBinding
    fn get_this_binding(&self) -> AbstractResult<Value> {
        Value::undefined().into()
    }

    // 8.1.1.5.5 CreateImportBinding
    fn create_import_binding(&self) -> AbstractResult<Value> {
        unimplemented!()
    }
}
