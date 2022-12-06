use std::{cell::RefCell, rc::Rc};

use crate::js::runtime::{
    completion::AbstractResult,
    error::reference_error_,
    value::{ObjectValue, Value},
};

use super::{declarative_environment::DeclarativeEnvironment, environment::Environment};

pub struct FunctionEnvironment {
    env: DeclarativeEnvironment,
    this_value: Value,
    this_binding_status: ThisBindingStatus,
    function_object: Rc<RefCell<ObjectValue>>,
    home_object: Option<Rc<RefCell<ObjectValue>>>,
    new_target: Option<Rc<RefCell<ObjectValue>>>,
}

#[derive(PartialEq)]
pub enum ThisBindingStatus {
    // This is an arrow function
    Lexical,
    Initialized,
    Uninitialized,
}

impl Environment for FunctionEnvironment {
    // 8.1.1.3.2 HasThisBinding
    fn has_this_binding(&self) -> bool {
        self.this_binding_status != ThisBindingStatus::Lexical
    }

    // 8.1.1.3.3 HasSuperBinding
    fn has_super_binding(&self) -> bool {
        if self.this_binding_status == ThisBindingStatus::Lexical {
            return false;
        }

        !self.home_object.is_none()
    }

    // All other methods inherited from DeclarativeEnvironment

    fn has_binding(&self, name: &str) -> AbstractResult<bool> {
        self.env.has_binding(name)
    }

    fn create_mutable_binding(&mut self, name: String, can_delete: bool) -> AbstractResult<()> {
        self.env.create_mutable_binding(name, can_delete)
    }

    fn create_immutable_binding(&mut self, name: String, is_strict: bool) -> AbstractResult<()> {
        self.env.create_immutable_binding(name, is_strict)
    }

    fn initialize_binding(&mut self, name: &str, value: Value) -> AbstractResult<()> {
        self.env.initialize_binding(name, value)
    }

    fn set_mutable_binding(
        &mut self,
        name: &str,
        value: Value,
        is_strict: bool,
    ) -> AbstractResult<()> {
        self.env.set_mutable_binding(name, value, is_strict)
    }

    fn get_binding_value(&self, name: &str, is_strict: bool) -> AbstractResult<Value> {
        self.env.get_binding_value(name, is_strict)
    }

    fn delete_binding(&mut self, name: &str) -> AbstractResult<bool> {
        self.env.delete_binding(name)
    }

    fn with_base_object(&self) -> Value {
        self.env.with_base_object()
    }
}

impl FunctionEnvironment {
    // 8.1.1.3.1 BindThisValue
    fn bind_this_value(&mut self, value: Value) -> AbstractResult<Value> {
        if self.this_binding_status == ThisBindingStatus::Initialized {
            return reference_error_("this is already initialized");
        }

        self.this_value = value.clone();
        self.this_binding_status = ThisBindingStatus::Initialized;

        value.into()
    }

    // 8.1.1.3.4 GetThisBinding
    fn get_this_binding(&self) -> AbstractResult<Value> {
        if self.this_binding_status == ThisBindingStatus::Uninitialized {
            return reference_error_("this is not initialized");
        }

        return self.this_value.clone().into();
    }

    // 8.1.1.3.5 GetSuperBase
    fn get_super_base(&self) -> AbstractResult<Value> {
        match &self.home_object {
            None => Value::Undefined.into(),
            Some(home) => home.borrow().get_prototype_of(),
        }
    }
}
