use std::mem::size_of;

use crate::{
    js::runtime::{
        completion::EvalResult,
        error::reference_error_,
        function::Function,
        gc::{Handle, HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        string_value::StringValue,
        value::Value,
        Context, HeapPtr,
    },
    maybe, set_uninit,
};

use super::{
    declarative_environment::DeclarativeEnvironment,
    environment::{DynEnvironment, Environment},
};

#[repr(C)]
// 9.1.1.3 Function Environment Record
pub struct FunctionEnvironment {
    env: DeclarativeEnvironment,
    this_value: Value,
    function_object: HeapPtr<Function>,
    new_target: Option<HeapPtr<ObjectValue>>,
    this_binding_status: ThisBindingStatus,
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
        cx: Context,
        function_object: Handle<Function>,
        new_target: Option<Handle<ObjectValue>>,
    ) -> Handle<FunctionEnvironment> {
        let this_binding_status = if function_object.is_lexical_this_mode() {
            ThisBindingStatus::Lexical
        } else {
            ThisBindingStatus::Uninitialized
        };

        // Allocate and put behind handle before allocating function environment
        let bindings = DeclarativeEnvironment::new_bindings_map(cx);

        let mut env = cx.alloc_uninit::<FunctionEnvironment>();

        DeclarativeEnvironment::init_as_base(
            cx,
            &mut env.env,
            ObjectKind::FunctionEnvironment,
            bindings,
            Some(function_object.environment()),
        );

        set_uninit!(env.this_value, Value::undefined());
        set_uninit!(env.function_object, function_object.get_());
        set_uninit!(env.new_target, new_target.map(|h| h.get_()));
        set_uninit!(env.this_binding_status, this_binding_status);

        env.to_handle()
    }

    fn this_value(&self, cx: Context) -> Handle<Value> {
        self.this_value.to_handle(cx)
    }

    pub fn function_object(&self) -> Handle<Function> {
        self.function_object.to_handle()
    }

    pub fn new_target(&self) -> Option<Handle<ObjectValue>> {
        self.new_target.map(|v| v.to_handle())
    }
}

impl Handle<FunctionEnvironment> {
    #[inline]
    fn env(&self) -> Handle<DeclarativeEnvironment> {
        self.cast()
    }
}

impl Environment for Handle<FunctionEnvironment> {
    fn as_function_environment(&mut self) -> Option<Handle<FunctionEnvironment>> {
        Some(*self)
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

        self.function_object.has_home_object()
    }

    // 9.1.1.3.4 GetThisBinding
    fn get_this_binding(&self, cx: Context) -> EvalResult<Handle<Value>> {
        if self.this_binding_status == ThisBindingStatus::Uninitialized {
            return reference_error_(cx, "this is not initialized");
        }

        return self.this_value(cx).into();
    }

    // All other methods inherited from DeclarativeEnvironment

    fn has_binding(&self, cx: Context, name: Handle<StringValue>) -> EvalResult<bool> {
        self.env().has_binding(cx, name)
    }

    fn create_mutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        can_delete: bool,
    ) -> EvalResult<()> {
        self.env().create_mutable_binding(cx, name, can_delete)
    }

    fn create_immutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        is_strict: bool,
    ) -> EvalResult<()> {
        self.env().create_immutable_binding(cx, name, is_strict)
    }

    fn initialize_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
    ) -> EvalResult<()> {
        self.env().initialize_binding(cx, name, value)
    }

    fn set_mutable_binding(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
        is_strict: bool,
    ) -> EvalResult<()> {
        self.env().set_mutable_binding(cx, name, value, is_strict)
    }

    fn get_binding_value(
        &self,
        cx: Context,
        name: Handle<StringValue>,
        is_strict: bool,
    ) -> EvalResult<Handle<Value>> {
        self.env().get_binding_value(cx, name, is_strict)
    }

    fn delete_binding(&mut self, cx: Context, name: Handle<StringValue>) -> EvalResult<bool> {
        self.env().delete_binding(cx, name)
    }

    fn with_base_object(&self) -> Option<Handle<ObjectValue>> {
        self.env().with_base_object()
    }

    fn outer(&self) -> Option<DynEnvironment> {
        self.env().outer()
    }
}

impl FunctionEnvironment {
    // 9.1.1.3.1 BindThisValue
    pub fn bind_this_value(&mut self, cx: Context, value: Handle<Value>) -> EvalResult<()> {
        if self.this_binding_status == ThisBindingStatus::Initialized {
            return reference_error_(cx, "this is already initialized");
        }

        self.this_value = value.get();
        self.this_binding_status = ThisBindingStatus::Initialized;

        ().into()
    }

    // 9.1.1.3.5 GetSuperBase
    pub fn get_super_base(&self, cx: Context) -> EvalResult<Handle<Value>> {
        // Note that we can return either an object, undefined, or null, so we must convert from
        // options to the correct undefined vs null value.
        match self.function_object.home_object() {
            None => cx.undefined().into(),
            Some(home) => {
                let prototype = maybe!(home.get_prototype_of(cx));
                match prototype {
                    None => cx.null().into(),
                    Some(prototype) => prototype.into(),
                }
            }
        }
    }
}

impl HeapObject for HeapPtr<FunctionEnvironment> {
    fn byte_size(&self) -> usize {
        size_of::<FunctionEnvironment>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<DeclarativeEnvironment>()
            .visit_pointers(visitor);
        visitor.visit_value(&mut self.this_value);
        visitor.visit_pointer(&mut self.function_object);
        visitor.visit_pointer_opt(&mut self.new_target);
    }
}
