use std::{cell::RefCell, rc::Rc};

use super::{
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
}

impl Context {
    pub fn new() -> Context {
        Context {
            execution_context_stack: vec![],
            heap: Heap::new(),
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

    pub fn current_realm(&mut self) -> Rc<RefCell<Realm>> {
        self.current_execution_context().realm.clone()
    }

    // 8.3.1 GetActiveScriptOrModule
    pub fn get_active_script_or_module(&self) -> Option<ScriptOrModule> {
        self.execution_context_stack
            .iter()
            .rev()
            .find_map(|exec_ctx| exec_ctx.script_or_module)
    }
}
