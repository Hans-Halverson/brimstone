use super::{
    completion::AbstractResult,
    environment::{
        environment::{get_identifier_reference, Environment},
        private_environment::PrivateEnvironment,
    },
    eval::script::Script,
    gc::{Gc, GcDeref},
    object_value::ObjectValue,
    realm::Realm,
    reference::Reference,
    value::Value,
    Context,
};

// 9.4 Execution Context
pub struct ExecutionContext {
    pub function: Option<Gc<ObjectValue>>,
    pub realm: Gc<Realm>,
    pub script_or_module: Option<ScriptOrModule>,
    pub lexical_env: Gc<dyn Environment>,
    pub variable_env: Gc<dyn Environment>,
    pub private_env: Option<Gc<PrivateEnvironment>>,
}

impl GcDeref for ExecutionContext {}

#[derive(Clone, Copy)]
pub enum ScriptOrModule {
    Script(Gc<Script>),
    Module,
}

// 9.4.2 ResolveBinding
fn resolve_binding(
    cx: &mut Context,
    name: &str,
    env: Option<Gc<dyn Environment>>,
) -> AbstractResult<Reference> {
    let env = match env {
        Some(env) => env,
        None => cx.current_execution_context().lexical_env,
    };

    // TODO: Check if currently evaluating strict mode code
    let is_strict = false;

    get_identifier_reference(cx, Some(env), name, is_strict)
}

// 9.4.3 GetThisEnvironment
fn get_this_environment(cx: &mut Context) -> Gc<dyn Environment> {
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
fn resolve_this_binding(cx: &mut Context) -> AbstractResult<Value> {
    get_this_environment(cx).get_this_binding(cx)
}

// 9.4.5 GetNewTarget
fn get_new_target(cx: &mut Context) -> Option<Gc<ObjectValue>> {
    let mut this_env = get_this_environment(cx);
    let func_env = this_env.as_function_environment().unwrap();
    func_env.new_target
}

// 9.4.6 GetGlobalObject
pub fn get_global_object(cx: &mut Context) -> Gc<ObjectValue> {
    cx.current_realm().global_object
}
