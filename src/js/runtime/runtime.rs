use crate::js::parser::ast;

use std::{collections::HashSet, rc::Rc};

use super::{completion::Completion, environment::LexicalEnvironment, realm::Realm, value::Value};

// 8.3 Execution Context
pub struct ExecutionContext {
    // Root AST node. Called script_or_module in spec.
    pub program: Option<Rc<ast::Program>>,
    pub realm: Realm,
    pub function: Option<Value>,
    pub lexical_env: Rc<LexicalEnvironment>,
    pub variable_env: Rc<LexicalEnvironment>,
}

/// 8.6 Agent
pub struct Agent {
    execution_context_stack: Vec<ExecutionContext>,
}

impl Agent {
    pub fn new() -> Agent {
        Agent {
            execution_context_stack: vec![],
        }
    }

    pub fn push_execution_context(&mut self, exec_ctx: ExecutionContext) {
        self.execution_context_stack.push(exec_ctx)
    }

    pub fn pop_execution_context(&mut self) {
        self.execution_context_stack.pop();
    }

    pub fn current_execution_context(&mut self) -> &mut ExecutionContext {
        self.execution_context_stack.last_mut().unwrap()
    }
}

/// 15.1.10 ScriptEvaluation
pub fn evaluate(agent: &mut Agent, program: Rc<ast::Program>) -> Completion {
    // TODO: Figure out realm creation, create initial realm and use by default
    let realm = Realm::new();

    let global_env = realm.global_env.clone();

    let script_ctx = ExecutionContext {
        function: None,
        realm,
        program: Some(program.clone()),
        lexical_env: global_env.clone(),
        variable_env: global_env.clone(),
    };

    agent.push_execution_context(script_ctx);

    let mut result = global_declaration_initialization(&program, &global_env);

    if let Completion::Normal(_) = result {
        result = evaluate_program(&program);
    }

    if let Completion::Normal(None) = result {
        result = Completion::Normal(Some(Value::Undefined));
    }

    agent.pop_execution_context();

    return result;
}

/// 15.1.11 GlobalDeclarationInitialization
pub fn global_declaration_initialization(
    script: &ast::Program,
    env: &LexicalEnvironment,
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
