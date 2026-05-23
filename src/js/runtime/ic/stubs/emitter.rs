use crate::runtime::ic::stubs::{ICBackend, ICGuard, ICInstruction, ICOp};

pub struct Emitter {
    ops: Vec<ICInstruction>,
}

impl Emitter {
    pub fn new() -> Self {
        Emitter { ops: Vec::new() }
    }

    pub fn into_ops(self) -> Vec<ICInstruction> {
        self.ops
    }
}

impl ICBackend for Emitter {
    type Output = Emitter;

    fn guard(mut self, guard: ICGuard) -> Self {
        self.ops.push(ICInstruction::Guard(guard));
        self
    }

    fn exec(mut self, op: ICOp) -> Emitter {
        self.ops.push(ICInstruction::Exec(op));
        self
    }
}
