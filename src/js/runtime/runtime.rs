use std::cell::RefCell;
use std::rc::Rc;

use super::environment::environment::to_trait_object;
use super::environment::global_environment::GlobalEnvironment;
use super::execution_context::{Script, ScriptOrModule};
use super::gc::Gc;
use super::{
    completion::Completion, execution_context::ExecutionContext, realm::Realm, value::Value,
    Context,
};

use crate::js::parser::ast;

/// 15.1.10 ScriptEvaluation
pub fn evaluate_script(
    cx: &mut Context,
    program: Rc<ast::Program>,
    realm: Rc<RefCell<Realm>>,
) -> Completion {
    let script = cx.heap.alloc(Script::new(program.clone(), realm.clone()));

    let global_env = realm.borrow().global_env.clone();
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
pub fn global_declaration_instantiation(
    script: &ast::Program,
    env: Gc<GlobalEnvironment>,
) -> Completion {
    // First gather all toplevel names
    // let mut lex_names = HashSet::new();
    // let mut var_names = HashSet::new();

    // for toplevel in script.toplevels.iter() {
    //     match toplevel {
    //         ast::Toplevel::Statement(ast::Statement::VarDecl(ast::VariableDeclaration {
    //             ..
    //         })) => {}
    //         ast::Toplevel::Statement(ast::Statement::FuncDecl(ast::Function { .. })) => {}
    //         _ => {}
    //     }
    // }

    // TODO: Implement
    Completion::empty()
}

pub fn evaluate_program(program: &ast::Program) -> Completion {
    // TODO: Evaluate program
    Completion::empty()
}
