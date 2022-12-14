use crate::js::parser::ast;

use super::{
    environment::{
        declarative_environment::DeclarativeEnvironment,
        environment::{to_trait_object, Environment},
    },
    execution_context::{ExecutionContext, ScriptOrModule},
    gc::{Gc, Heap},
    realm::Realm,
};

/// Top level context for the JS runtime. Contains the heap, execution contexts, etc.
///
/// Includes properties from section 8.6 Agent.
pub struct Context {
    execution_context_stack: Vec<Gc<ExecutionContext>>,
    pub heap: Heap,

    // An empty environment to be used as an uninitialized value
    pub uninit_environment: Gc<dyn Environment>,

    // All ASTs produced by eval in this context. Saved here so that they are not freed while the
    // context is still running, as they may be needed e.g. due to functions returned from an eval.
    pub eval_asts: Vec<ast::Program>,
}

impl Context {
    pub fn new() -> Context {
        let mut heap = Heap::new();
        let uninit_environment = to_trait_object(heap.alloc(DeclarativeEnvironment::new(None)));

        Context {
            execution_context_stack: vec![],
            heap,
            uninit_environment,
            eval_asts: vec![],
        }
    }

    pub fn push_execution_context(&mut self, exec_ctx: Gc<ExecutionContext>) {
        self.execution_context_stack.push(exec_ctx)
    }

    pub fn pop_execution_context(&mut self) {
        self.execution_context_stack.pop();
    }

    pub fn current_execution_context(&mut self) -> Gc<ExecutionContext> {
        self.execution_context_stack.last_mut().unwrap().clone()
    }

    pub fn current_realm(&mut self) -> Gc<Realm> {
        self.current_execution_context().realm
    }

    // 9.4.1 GetActiveScriptOrModule
    pub fn get_active_script_or_module(&self) -> Option<ScriptOrModule> {
        self.execution_context_stack
            .iter()
            .rev()
            .find_map(|exec_ctx| exec_ctx.script_or_module)
    }
}
