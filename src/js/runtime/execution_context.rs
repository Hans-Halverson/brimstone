use super::{
    completion::EvalResult,
    environment::{
        environment::{get_identifier_reference, DynEnvironment},
        private_environment::PrivateEnvironment,
    },
    eval::script::Script,
    gc::{Gc, GcDeref},
    object_value::ObjectValue,
    realm::Realm,
    reference::Reference,
    string_value::StringValue,
    value::Value,
    Context,
};

// 9.4 Execution Context
pub struct ExecutionContext {
    pub function: Option<Gc<ObjectValue>>,
    pub realm: Gc<Realm>,
    pub script_or_module: Option<ScriptOrModule>,
    pub lexical_env: DynEnvironment,
    pub variable_env: DynEnvironment,
    pub private_env: Option<Gc<PrivateEnvironment>>,
    pub is_strict_mode: bool,
}

impl GcDeref for ExecutionContext {}

#[derive(Clone, Copy)]
pub enum ScriptOrModule {
    Script(Gc<Script>),
}

// 9.4.2 ResolveBinding
pub fn resolve_binding(
    cx: &mut Context,
    name: Gc<StringValue>,
    env: Option<DynEnvironment>,
) -> EvalResult<Reference> {
    let env = match env {
        Some(env) => env,
        None => cx.current_execution_context().lexical_env,
    };

    let is_strict = cx.current_execution_context().is_strict_mode;

    get_identifier_reference(cx, Some(env), name, is_strict)
}

// 9.4.3 GetThisEnvironment
pub fn get_this_environment(cx: &mut Context) -> DynEnvironment {
    let mut current_env = cx.current_execution_context().lexical_env;
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

// 9.4.6 GetGlobalObject
pub fn get_global_object(cx: &mut Context) -> Gc<ObjectValue> {
    cx.current_realm().global_object
}
