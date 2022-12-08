use crate::js::runtime::{
    completion::AbstractResult,
    error::reference_error_,
    gc::Gc,
    value::{ObjectValue, Value},
    Context,
};

use super::{declarative_environment::DeclarativeEnvironment, environment::Environment};

pub struct FunctionEnvironment {
    env: DeclarativeEnvironment,
    this_value: Value,
    this_binding_status: ThisBindingStatus,
    function_object: Gc<ObjectValue>,
    home_object: Option<Gc<ObjectValue>>,
    new_target: Option<Gc<ObjectValue>>,
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

    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: &str,
        is_strict: bool,
    ) -> AbstractResult<Value> {
        self.env.get_binding_value(cx, name, is_strict)
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
    fn bind_this_value(&mut self, cx: &mut Context, value: Value) -> AbstractResult<Value> {
        if self.this_binding_status == ThisBindingStatus::Initialized {
            return reference_error_(cx, "this is already initialized");
        }

        self.this_value = value.clone();
        self.this_binding_status = ThisBindingStatus::Initialized;

        value.into()
    }

    // 8.1.1.3.4 GetThisBinding
    fn get_this_binding(&self, cx: &mut Context) -> AbstractResult<Value> {
        if self.this_binding_status == ThisBindingStatus::Uninitialized {
            return reference_error_(cx, "this is not initialized");
        }

        return self.this_value.clone().into();
    }

    // 8.1.1.3.5 GetSuperBase
    fn get_super_base(&self) -> AbstractResult<Value> {
        match &self.home_object {
            None => Value::undefined().into(),
            Some(home) => home.as_ref().get_prototype_of(),
        }
    }
}
