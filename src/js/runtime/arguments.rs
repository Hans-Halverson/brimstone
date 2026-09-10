use crate::runtime::{Context, Handle, Value, bytecode::stack_frame::StackFrame};

/// Arguments to a runtime function.
#[derive(Clone, Copy)]
pub struct Arguments {
    stack_frame: StackFrame,
}

impl Arguments {
    #[inline]
    pub fn new(stack_frame: StackFrame) -> Self {
        Self { stack_frame }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.stack_frame.argc()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.stack_frame.argc() == 0
    }

    /// Return the value of a particular argument, or undefined if the argument was not provided.
    #[inline]
    pub fn get(&self, cx: Context, i: usize) -> Handle<Value> {
        match self.stack_frame.args().get(i) {
            Some(arg) => arg.to_handle(cx),
            None => cx.undefined(),
        }
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &Value> {
        self.stack_frame.args().iter()
    }
}
