use std::mem::size_of;

use crate::set_uninit;

use super::{
    completion::EvalResult,
    environment::{
        environment::{get_identifier_reference, DynEnvironment, HeapDynEnvironment},
        private_environment::PrivateEnvironment,
    },
    eval::script::Script,
    gc::{Handle, HeapObject, HeapVisitor},
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::ObjectValue,
    realm::Realm,
    reference::Reference,
    string_value::StringValue,
    value::Value,
    Context, HeapPtr,
};

// 9.4 Execution Context
#[repr(C)]
pub struct ExecutionContext {
    descriptor: HeapPtr<ObjectDescriptor>,
    function: Option<HeapPtr<ObjectValue>>,
    realm: HeapPtr<Realm>,
    script_or_module: Option<HeapScriptOrModule>,
    lexical_env: HeapDynEnvironment,
    variable_env: HeapDynEnvironment,
    private_env: Option<HeapPtr<PrivateEnvironment>>,
    is_strict_mode: bool,
}

impl ExecutionContext {
    pub fn new(
        cx: Context,
        function: Option<Handle<ObjectValue>>,
        realm: Handle<Realm>,
        script_or_module: Option<ScriptOrModule>,
        lexical_env: Option<DynEnvironment>,
        variable_env: Option<DynEnvironment>,
        private_env: Option<Handle<PrivateEnvironment>>,
        is_strict_mode: bool,
    ) -> Handle<ExecutionContext> {
        let mut exec_context = cx.alloc_uninit::<ExecutionContext>();

        let lex_env = lexical_env
            .map(|e| e.to_heap())
            .unwrap_or(cx.uninit_environment);
        let var_env = variable_env
            .map(|e| e.to_heap())
            .unwrap_or(cx.uninit_environment);

        set_uninit!(exec_context.descriptor, cx.base_descriptors.get(ObjectKind::ExecutionContext));
        set_uninit!(exec_context.function, function.map(|f| f.get_()));
        set_uninit!(exec_context.realm, realm.get_());
        set_uninit!(exec_context.script_or_module, script_or_module.map(|s| s.to_heap()));
        set_uninit!(exec_context.lexical_env, lex_env);
        set_uninit!(exec_context.variable_env, var_env);
        set_uninit!(exec_context.private_env, private_env.map(|p| p.get_()));
        set_uninit!(exec_context.is_strict_mode, is_strict_mode);

        exec_context.to_handle()
    }

    #[inline]
    pub fn function(&self) -> Handle<ObjectValue> {
        self.function.unwrap().to_handle()
    }

    #[inline]
    pub fn realm_ptr(&self) -> HeapPtr<Realm> {
        self.realm
    }

    #[inline]
    pub fn realm(&self) -> Handle<Realm> {
        self.realm.to_handle()
    }

    #[inline]
    pub fn script_or_module(&self) -> Option<ScriptOrModule> {
        self.script_or_module
            .as_ref()
            .map(ScriptOrModule::from_heap)
    }

    #[inline]
    pub fn lexical_env(&self) -> DynEnvironment {
        DynEnvironment::from_heap(&self.lexical_env)
    }

    #[inline]
    pub fn variable_env(&self) -> DynEnvironment {
        DynEnvironment::from_heap(&self.variable_env)
    }

    #[inline]
    pub fn private_env_ptr(&self) -> Option<HeapPtr<PrivateEnvironment>> {
        self.private_env
    }

    #[inline]
    pub fn private_env(&self) -> Option<Handle<PrivateEnvironment>> {
        self.private_env.map(|p| p.to_handle())
    }

    #[inline]
    pub fn is_strict_mode(&self) -> bool {
        self.is_strict_mode
    }

    #[inline]
    pub fn set_lexical_env(&mut self, env: DynEnvironment) {
        self.lexical_env = env.to_heap()
    }

    #[inline]
    pub fn set_variable_env(&mut self, env: DynEnvironment) {
        self.variable_env = env.to_heap()
    }

    #[inline]
    pub fn set_private_env(&mut self, private_env: Option<Handle<PrivateEnvironment>>) {
        self.private_env = private_env.map(|p| p.get_());
    }

    #[inline]
    pub fn get_intrinsic_ptr(&self, intrinsic: Intrinsic) -> HeapPtr<ObjectValue> {
        self.realm.get_intrinsic_ptr(intrinsic)
    }

    #[inline]
    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> Handle<ObjectValue> {
        self.realm.get_intrinsic(intrinsic)
    }

    #[inline]
    pub fn global_object(&self) -> Handle<ObjectValue> {
        self.realm.global_object()
    }
}

// 9.4.2 ResolveBinding
pub fn resolve_binding(
    cx: Context,
    name: Handle<StringValue>,
    env: Option<DynEnvironment>,
) -> EvalResult<Reference> {
    let env = match env {
        Some(env) => env,
        None => cx.current_execution_context_ptr().lexical_env(),
    };

    let is_strict = cx.current_execution_context_ptr().is_strict_mode();

    get_identifier_reference(cx, Some(env), name, is_strict)
}

// 9.4.3 GetThisEnvironment
pub fn get_this_environment(cx: Context) -> DynEnvironment {
    let mut current_env = cx.current_execution_context_ptr().lexical_env();
    loop {
        if current_env.has_this_binding() {
            return current_env;
        }

        // Guaranteed to not be None as because the top level environment is always the global
        // environment, which as "this" defined.
        current_env = current_env.outer().unwrap();
    }
}

// 9.4.4 ResolveThisBinding
pub fn resolve_this_binding(cx: Context) -> EvalResult<Handle<Value>> {
    get_this_environment(cx).get_this_binding(cx)
}

// 9.4.5 GetNewTarget
pub fn get_new_target(cx: Context) -> Option<Handle<ObjectValue>> {
    let mut this_env = get_this_environment(cx);
    let func_env = this_env.as_function_environment().unwrap();
    func_env.new_target()
}

/// ScriptOrModule that is stored on the stack.
#[derive(Clone)]
pub enum ScriptOrModule {
    Script(Handle<Script>),
}

/// ScriptOrModule that is stored on the managed heap.
pub enum HeapScriptOrModule {
    Script(HeapPtr<Script>),
}

impl ScriptOrModule {
    pub fn to_heap(&self) -> HeapScriptOrModule {
        match self {
            ScriptOrModule::Script(script) => HeapScriptOrModule::Script(script.get_()),
        }
    }

    pub fn from_heap(heap_script_or_module: &HeapScriptOrModule) -> ScriptOrModule {
        match heap_script_or_module {
            HeapScriptOrModule::Script(script) => ScriptOrModule::Script(script.to_handle()),
        }
    }
}

impl HeapScriptOrModule {
    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        match self {
            HeapScriptOrModule::Script(script) => visitor.visit_pointer(script),
        }
    }
}

impl HeapObject for HeapPtr<ExecutionContext> {
    fn byte_size(&self) -> usize {
        size_of::<ExecutionContext>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer_opt(&mut self.function);
        visitor.visit_pointer(&mut self.realm);
        self.script_or_module
            .as_mut()
            .map(|sm| sm.visit_pointers(visitor));
        self.lexical_env.visit_pointers(visitor);
        self.variable_env.visit_pointers(visitor);
        visitor.visit_pointer_opt(&mut self.private_env);
    }
}
