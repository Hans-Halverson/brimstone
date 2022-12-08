use super::{
    gc::{Gc, Heap},
    runtime::ExecutionContext,
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
}
