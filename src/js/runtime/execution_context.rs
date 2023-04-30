use super::{
    completion::EvalResult,
    environment::{
        environment::{get_identifier_reference, DynEnvironment, HeapDynEnvironment},
        private_environment::PrivateEnvironment,
    },
    eval::script::Script,
    gc::{Gc, GcDeref},
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::ObjectValue,
    realm::Realm,
    reference::Reference,
    string_value::StringValue,
    value::Value,
    Context,
};

// 9.4 Execution Context
#[repr(C)]
pub struct ExecutionContext {
    descriptor: Gc<ObjectDescriptor>,
    function: Option<Gc<ObjectValue>>,
    realm: Gc<Realm>,
    script_or_module: Option<HeapScriptOrModule>,
    lexical_env: HeapDynEnvironment,
    variable_env: HeapDynEnvironment,
    private_env: Option<Gc<PrivateEnvironment>>,
    is_strict_mode: bool,
}

impl GcDeref for ExecutionContext {}

impl ExecutionContext {
    pub fn new(
        cx: &mut Context,
        function: Option<Gc<ObjectValue>>,
        realm: Gc<Realm>,
        script_or_module: Option<ScriptOrModule>,
        lexical_env: DynEnvironment,
        variable_env: DynEnvironment,
        private_env: Option<Gc<PrivateEnvironment>>,
        is_strict_mode: bool,
    ) -> Gc<ExecutionContext> {
        let descriptor = cx.base_descriptors.get(ObjectKind::ExecutionContext);
        cx.heap.alloc(ExecutionContext {
            descriptor,
            function,
            realm,
            script_or_module: script_or_module.as_ref().map(ScriptOrModule::to_heap),
            lexical_env: lexical_env.to_heap(),
            variable_env: variable_env.to_heap(),
            private_env,
            is_strict_mode,
        })
    }

    #[inline]
    pub fn function(&self) -> Option<Gc<ObjectValue>> {
        self.function
    }

    #[inline]
    pub fn realm(&self) -> Gc<Realm> {
        self.realm
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
    pub fn private_env(&self) -> Option<Gc<PrivateEnvironment>> {
        self.private_env
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
    pub fn set_private_env(&mut self, private_env: Option<Gc<PrivateEnvironment>>) {
        self.private_env = private_env;
    }

    #[inline]
    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> Gc<ObjectValue> {
        self.realm.get_intrinsic(intrinsic)
    }

    #[inline]
    pub fn global_object(&self) -> Gc<ObjectValue> {
        self.realm.global_object()
    }
}

impl Gc<ExecutionContext> {
    pub fn script_or_module(&self) -> Option<ScriptOrModule> {
        self.script_or_module
            .as_ref()
            .map(ScriptOrModule::from_heap)
    }
}

// 9.4.2 ResolveBinding
pub fn resolve_binding(
    cx: &mut Context,
    name: Gc<StringValue>,
    env: Option<DynEnvironment>,
) -> EvalResult<Reference> {
    let env = match env {
        Some(env) => env,
        None => cx.current_execution_context().lexical_env(),
    };

    let is_strict = cx.current_execution_context().is_strict_mode;

    get_identifier_reference(cx, Some(env), name, is_strict)
}

// 9.4.3 GetThisEnvironment
pub fn get_this_environment(cx: &mut Context) -> DynEnvironment {
    let mut current_env = cx.current_execution_context().lexical_env();
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
pub fn resolve_this_binding(cx: &mut Context) -> EvalResult<Value> {
    get_this_environment(cx).get_this_binding(cx)
}

// 9.4.5 GetNewTarget
pub fn get_new_target(cx: &mut Context) -> Option<Gc<ObjectValue>> {
    let mut this_env = get_this_environment(cx);
    let func_env = this_env.as_function_environment().unwrap();
    func_env.new_target
}

/// ScriptOrModule that is stored on the stack.
#[derive(Clone)]
pub enum ScriptOrModule {
    Script(Gc<Script>),
}

/// ScriptOrModule that is stored on the managed heap.
pub struct HeapScriptOrModule {
    inner: ScriptOrModule,
}

impl ScriptOrModule {
    pub fn to_heap(&self) -> HeapScriptOrModule {
        HeapScriptOrModule { inner: self.clone() }
    }

    pub fn from_heap(heap_script_or_module: &HeapScriptOrModule) -> ScriptOrModule {
        heap_script_or_module.inner.clone()
    }
}
