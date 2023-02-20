use crate::{
    js::runtime::{
        completion::EvalResult,
        error::reference_error_,
        function::{Function, ThisMode},
        gc::Gc,
        object_value::ObjectValue,
        string_value::StringValue,
        value::Value,
        Context,
    },
    maybe,
};

use super::{declarative_environment::DeclarativeEnvironment, environment::Environment};

// 9.1.1.3 Function Environment Record
pub struct FunctionEnvironment {
    env: DeclarativeEnvironment,
    this_value: Value,
    this_binding_status: ThisBindingStatus,
    pub function_object: Gc<Function>,
    pub new_target: Option<Gc<ObjectValue>>,
}

#[derive(PartialEq)]
pub enum ThisBindingStatus {
    // This is an arrow function
    Lexical,
    Initialized,
    Uninitialized,
}

impl FunctionEnvironment {
    // 9.1.2.4 NewFunctionEnvironment
    pub fn new(
        cx: &mut Context,
        function_object: Gc<Function>,
        new_target: Option<Gc<ObjectValue>>,
    ) -> Gc<FunctionEnvironment> {
        let this_binding_status = if function_object.this_mode == ThisMode::Lexical {
            ThisBindingStatus::Lexical
        } else {
            ThisBindingStatus::Uninitialized
        };

        // Inner decl env contains the outer environment pointer
        let decl_env = DeclarativeEnvironment::new(Some(function_object.environment));

        cx.heap.alloc(FunctionEnvironment {
            env: decl_env,
            // This value is uninitialized on creation
            this_value: Value::undefined(),
            function_object,
            this_binding_status,
            new_target,
        })
    }
}

impl Environment for FunctionEnvironment {
    fn as_function_environment(&mut self) -> Option<&mut FunctionEnvironment> {
        Some(self)
    }

    // 9.1.1.3.2 HasThisBinding
    fn has_this_binding(&self) -> bool {
        self.this_binding_status != ThisBindingStatus::Lexical
    }

    // 9.1.1.3.3 HasSuperBinding
    fn has_super_binding(&self) -> bool {
        if self.this_binding_status == ThisBindingStatus::Lexical {
            return false;
        }

        self.function_object.home_object.is_some()
    }

    // 9.1.1.3.4 GetThisBinding
    fn get_this_binding(&self, cx: &mut Context) -> EvalResult<Value> {
        if self.this_binding_status == ThisBindingStatus::Uninitialized {
            return reference_error_(cx, "this is not initialized");
        }

        return self.this_value.clone().into();
    }

    // All other methods inherited from DeclarativeEnvironment

    fn has_binding(&self, cx: &mut Context, name: Gc<StringValue>) -> EvalResult<bool> {
        self.env.has_binding(cx, name)
    }

    fn create_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()> {
        self.env.create_mutable_binding(cx, name, can_delete)
    }

    fn create_immutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        is_strict: bool,
    ) -> EvalResult<()> {
        self.env.create_immutable_binding(cx, name, is_strict)
    }

    fn initialize_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        value: Value,
    ) -> EvalResult<()> {
        self.env.initialize_binding(cx, name, value)
    }

    fn set_mutable_binding(
        &mut self,
        cx: &mut Context,
        name: Gc<StringValue>,
        value: Value,
        is_strict: bool,
    ) -> EvalResult<()> {
        self.env.set_mutable_binding(cx, name, value, is_strict)
    }

    fn get_binding_value(
        &self,
        cx: &mut Context,
        name: Gc<StringValue>,
        is_strict: bool,
    ) -> EvalResult<Value> {
        self.env.get_binding_value(cx, name, is_strict)
    }

    fn delete_binding(&mut self, cx: &mut Context, name: Gc<StringValue>) -> EvalResult<bool> {
        self.env.delete_binding(cx, name)
    }

    fn with_base_object(&self) -> Option<Gc<ObjectValue>> {
        self.env.with_base_object()
    }

    fn outer(&self) -> Option<Gc<dyn Environment>> {
        self.env.outer()
    }
}

impl FunctionEnvironment {
    // 9.1.1.3.1 BindThisValue
    pub fn bind_this_value(&mut self, cx: &mut Context, value: Value) -> EvalResult<Value> {
        if self.this_binding_status == ThisBindingStatus::Initialized {
            return reference_error_(cx, "this is already initialized");
        }

        self.this_value = value;
        self.this_binding_status = ThisBindingStatus::Initialized;

        value.into()
    }

    // 9.1.1.3.5 GetSuperBase
    pub fn get_super_base(&self, cx: &mut Context) -> EvalResult<Value> {
        // Note that we can return either an object, undefined, or null, so we must convert from
        // options to the correct undefined vs null value.
        match &self.function_object.home_object {
            None => Value::undefined().into(),
            Some(home) => {
                let prototype = maybe!(home.get_prototype_of(cx));
                match prototype {
                    None => Value::null().into(),
                    Some(prototype) => prototype.into(),
                }
            }
        }
    }
}
