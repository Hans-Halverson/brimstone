use crate::js::parser::ast;

use std::rc::Rc;

use super::realm::Realm;

/// 6.2.3 Completion Record
pub enum Completion {
    Normal(Option<Value>),
    Return(Option<Value>),
    Throw(Value),
    Break,
    Continue,
}

pub enum Value {
    Undefined,
    Null,
    Bool(bool),
    Number(f64),
    Object(ObjectValue),
}

pub struct ObjectValue {}

impl ObjectValue {
    pub const EMPTY: ObjectValue = ObjectValue {};
}

// 8.1 Lexical Environment
pub struct LexicalEnvironment {
    env: Environment,
    // Optional reference to the outer (parent) environment. If None this is the global environment.
    outer: Option<Rc<LexicalEnvironment>>,
}

impl LexicalEnvironment {
    pub const EMPTY: LexicalEnvironment = LexicalEnvironment {
        env: Environment::Declarative(DeclarativeEnvironment {}),
        outer: None,
    };

    // 8.1.2.5 NewGlobalEnvironment
    pub fn new_global_environment(
        global_obj: Rc<ObjectValue>,
        global_this_val: Rc<ObjectValue>,
    ) -> LexicalEnvironment {
        let obj_env = ObjectEnvironment {
            binding_obj: global_obj,
        };
        let decl_env = DeclarativeEnvironment {};

        let global_env = GlobalEnvironment {
            object_env: Rc::new(obj_env),
            global_this_val,
            decl_env: Rc::new(decl_env),
            var_names: vec![],
        };

        let env = LexicalEnvironment {
            env: Environment::Global(global_env),
            outer: None,
        };

        env
    }
}

// 8.1.1 Environment Record
pub enum Environment {
    Global(GlobalEnvironment),
    Declarative(DeclarativeEnvironment),
    Object(ObjectEnvironment),
}

// 8.1.1.1 Declarative Environment Record
pub struct DeclarativeEnvironment {}

// 8.1.1.2 Declarative Environment Record
pub struct ObjectEnvironment {
    binding_obj: Rc<ObjectValue>,
}

// 8.1.1.4 Global Environment Record
pub struct GlobalEnvironment {
    object_env: Rc<ObjectEnvironment>,
    global_this_val: Rc<ObjectValue>,
    decl_env: Rc<DeclarativeEnvironment>,
    var_names: Vec<String>,
}

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
    // TODO: Implement
    Completion::Normal(None)
}

pub fn evaluate_program(program: &ast::Program) -> Completion {
    // TODO: Evaluate program
    Completion::Normal(None)
}
