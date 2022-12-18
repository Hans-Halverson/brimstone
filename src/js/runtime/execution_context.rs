use std::{cell::RefCell, rc::Rc};

use crate::js::parser::ast;

use super::{
    completion::AbstractResult,
    environment::environment::{get_identifier_reference, Environment},
    gc::{Gc, GcDeref},
    object_value::ObjectValue,
    realm::Realm,
    reference::Reference,
    value::Value,
    Context,
};

// 8.3 Execution Context
pub struct ExecutionContext {
    pub function: Option<Value>,
    pub realm: Rc<RefCell<Realm>>,
    pub script_or_module: Option<ScriptOrModule>,
    pub lexical_env: Gc<dyn Environment>,
    pub variable_env: Gc<dyn Environment>,
}

impl GcDeref for ExecutionContext {}

#[derive(Clone, Copy)]
pub enum ScriptOrModule {
    Script(Gc<Script>),
    Module,
}

// 15.1.8 Script Record
pub struct Script {
    realm: Rc<RefCell<Realm>>,
    env: Option<Gc<dyn Environment>>,
    script_node: Rc<ast::Program>,
}

impl Script {
    pub fn new(script_node: Rc<ast::Program>, realm: Rc<RefCell<Realm>>) -> Script {
        Script {
            script_node,
            realm,
            env: None,
        }
    }
}

// 8.3.2 ResolveBinding
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

// 8.3.2 GetThisEnvironment
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

// 8.3.4 ResolveThisBinding
fn resolve_this_binding(cx: &mut Context) -> AbstractResult<Value> {
    get_this_environment(cx).get_this_binding(cx)
}

// 8.3.5 GetNewTarget
fn get_new_target(cx: &mut Context) -> Option<Gc<ObjectValue>> {
    let mut this_env = get_this_environment(cx);
    let func_env = this_env.as_function_environment().unwrap();
    func_env.new_target
}

// 8.3.6 GetGlobalObject
pub fn get_global_object(cx: &mut Context) -> Gc<ObjectValue> {
    cx.current_execution_context().realm.borrow().global_object
}
