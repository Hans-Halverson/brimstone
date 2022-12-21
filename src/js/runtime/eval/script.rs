use std::rc::Rc;

use crate::js::{
    parser::ast,
    runtime::{
        completion::Completion,
        environment::{environment::to_trait_object, global_environment::GlobalEnvironment},
        execution_context::{ExecutionContext, Script, ScriptOrModule},
        gc::Gc,
        realm::Realm,
        value::Value,
        Context,
    },
};

/// 15.1.10 ScriptEvaluation
pub fn evaluate_script(
    cx: &mut Context,
    program: Rc<ast::Program>,
    realm: Gc<Realm>,
) -> Completion {
    let script = cx.heap.alloc(Script::new(program.clone(), realm.clone()));

    let global_env = realm.global_env.clone();
    let global_env_object = to_trait_object(global_env);

    let script_ctx = cx.heap.alloc(ExecutionContext {
        function: None,
        realm,
        script_or_module: Some(ScriptOrModule::Script(script)),
        lexical_env: global_env_object,
        variable_env: global_env_object,
    });

    cx.push_execution_context(script_ctx);

    let mut result = global_declaration_instantiation(&program, global_env);

    if let Completion::Normal(_) = result {
        result = evaluate_program(&program);
    }

    if let Completion::Normal(None) = result {
        result = Completion::Normal(Some(Value::undefined()));
    }

    cx.pop_execution_context();

    return result;
}

/// 15.1.11 GlobalDeclarationInstantiation
fn global_declaration_instantiation(
    script: &ast::Program,
    env: Gc<GlobalEnvironment>,
) -> Completion {
    // TODO: Implement GlobalDeclarationInstantiation
    Completion::empty()
}

fn evaluate_program(program: &ast::Program) -> Completion {
    // TODO: Evaluate program
    Completion::empty()
}
