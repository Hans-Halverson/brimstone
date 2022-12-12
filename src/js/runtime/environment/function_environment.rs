use crate::js::runtime::{
    completion::AbstractResult,
    error::reference_error_,
    function::{Function, ThisMode},
    gc::Gc,
    object_value::ObjectValue,
    value::Value,
    Context,
};

use super::{declarative_environment::DeclarativeEnvironment, environment::Environment};

pub struct FunctionEnvironment {
    env: DeclarativeEnvironment,
    this_value: Value,
    this_binding_status: ThisBindingStatus,
    function_object: Gc<Function>,
    home_object: Option<Gc<ObjectValue>>,
    pub new_target: Value,
}

#[derive(PartialEq)]
pub enum ThisBindingStatus {
    // This is an arrow function
    Lexical,
    Initialized,
    Uninitialized,
}

impl FunctionEnvironment {
    // 8.1.2.4 NewFunctionEnvironment
    fn new(
        cx: &mut Context,
        function_object: Gc<Function>,
        new_target: Value,
    ) -> Gc<FunctionEnvironment> {
        let this_binding_status = if function_object.as_ref().this_mode == ThisMode::Lexical {
            ThisBindingStatus::Lexical
        } else {
            ThisBindingStatus::Uninitialized
        };

        // Inner decl env contains the outer environment pointer
        let decl_env =
            DeclarativeEnvironment::new(Some(function_object.as_ref().environment.clone()));

        cx.heap.alloc(FunctionEnvironment {
            env: decl_env,
            // This value is uninitialized on creation
            this_value: Value::undefined(),
            function_object,
            this_binding_status,
            home_object: function_object.as_ref().home_object,
            new_target,
        })
    }

    fn as_function_environment(&self) -> Option<&FunctionEnvironment> {
        Some(self)
    }
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

    // 8.1.1.3.4 GetThisBinding
    fn get_this_binding(&self, cx: &mut Context) -> AbstractResult<Value> {
        if self.this_binding_status == ThisBindingStatus::Uninitialized {
            return reference_error_(cx, "this is not initialized");
        }

        return self.this_value.clone().into();
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

    fn outer(&self) -> Option<Gc<dyn Environment>> {
        self.env.outer()
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

    // 8.1.1.3.5 GetSuperBase
    fn get_super_base(&self) -> AbstractResult<Value> {
        match &self.home_object {
            None => Value::undefined().into(),
            Some(home) => home.get_prototype_of(),
        }
    }
}
