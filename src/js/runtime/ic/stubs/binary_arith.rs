use std::marker::PhantomData;

use crate::{
    runtime::{
        alloc_error::AllocResult,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
        ic::{
            fgc::FGC,
            stubs::{emitter::Emitter, executor::BinaryExecutor},
        },
        Context, EvalResult, Handle, HeapPtr, Value,
    },
    set_uninit,
};

pub enum BailReason {
    ExpectedSmi,
    ExpectedNumber,
    ExpectedBigInt,
    ExpectedString,
}

pub type BinaryArithICStubExecutor =
    fn(BinaryExecutor) -> FGC<EvalResult<Handle<Value>>, BailReason>;

pub type BinaryArithICStubEmitter = fn(Emitter) -> Emitter;

pub type ICStubExecutionResult = Result<EvalResult<Handle<Value>>, BailReason>;

macro_rules! def_op {
    ($(($op_name:ident, $ic_name:ident)),*) => {
        $(
            pub struct $op_name;
            pub type $ic_name = BinaryArithICStub<$op_name>;
            impl BinaryArithOp for $op_name {
                fn heap_item_kind() -> HeapItemKind { HeapItemKind::$ic_name }
            }
        )*
    }
}

// Zero sized types marking ops and IC Stubs aliases
def_op!(
    (AddOp, AddICStub),
    (SubOp, SubICStub),
    (MulOp, MulICStub),
    (DivOp, DivICStub),
    (RemOp, RemICStub),
    (ExpOp, ExpICStub),
    (BitAndOp, BitAndICStub),
    (BitOrOp, BitOrICStub),
    (BitXorOp, BitXorICStub),
    (ShiftLeftOp, ShiftLeftICStub),
    (ShiftRightArithOp, ShiftRightArithICStub),
    (ShiftRightLogicalOp, ShiftRightLogicalICStub),
    (LtOp, LtICStub),
    (LteOp, LteICStub),
    (GtOp, GtICStub),
    (GteOp, GteICStub),
    (StrictEqOp, StrictEqICStub),
    (StrictNeqOp, StrictNeqICStub),
    (LooseEqOp, LooseEqICStub),
    (LooseNeqOp, LooseNeqICStub)
);

pub trait BinaryArithOp {
    fn heap_item_kind() -> HeapItemKind;
}

#[repr(C)]
pub struct BinaryArithICStub<Op: BinaryArithOp> {
    descriptor: HeapPtr<HeapItemDescriptor>,
    /// pointer to the next stub
    next: Option<HeapPtr<BinaryArithICStub<Op>>>,
    /// The actual fast path computation
    executor: BinaryArithICStubExecutor,
    /// The emitter for this stub
    emitter: BinaryArithICStubEmitter,
    /// A zero sized marker that makes the IC stub chain typesafe
    _phantom: PhantomData<Op>,
}

impl<Op: BinaryArithOp> BinaryArithICStub<Op> {
    fn execute_code(
        &self,
        cx: Context,
        left: Handle<Value>,
        right: Handle<Value>,
    ) -> ICStubExecutionResult {
        let intermediate = (self.executor)(BinaryExecutor::new(cx, left, right));
        match intermediate {
            FGC::Ok(r) => Ok(r),
            FGC::Bail(br) => Err(br),
        }
    }

    // New always makes the next head `None`
    pub fn new(
        cx: &Context,
        executor: BinaryArithICStubExecutor,
        emitter: BinaryArithICStubEmitter,
    ) -> AllocResult<Handle<BinaryArithICStub<Op>>> {
        let mut object = cx.alloc_uninit::<BinaryArithICStub<Op>>()?;

        set_uninit!(object.descriptor, cx.base_descriptors.get(Op::heap_item_kind()));
        set_uninit!(object.next, None);
        set_uninit!(object.executor, executor);
        set_uninit!(object.emitter, emitter);
        set_uninit!(object._phantom, PhantomData);

        Ok(object.to_handle())
    }

    /// Set the next one in the chain - note that it's impossible to
    /// add two ICStubs for different ops because of the `Op` type marker
    pub fn set_next(&mut self, val: Option<HeapPtr<BinaryArithICStub<Op>>>) {
        self.next = val;
    }

    pub fn try_execute(
        &self,
        cx: Context,
        left: Handle<Value>,
        right: Handle<Value>,
    ) -> Option<EvalResult<Handle<Value>>> {
        // try to execute each individual stub in the linked list
        let mut curr = Some(self);
        while let Some(stub) = curr {
            match stub.execute_code(cx, left, right) {
                Ok(r) => return Some(r),
                // continue to next stub
                Err(_e) => {}
            }
            curr = stub.next.as_deref();
        }
        // fall through - none of the stubs were a hit
        None
    }
}

/// GC Integration for AddICStub
impl<Op: BinaryArithOp> HeapItem for HeapPtr<BinaryArithICStub<Op>> {
    fn byte_size(&self) -> usize {
        size_of::<BinaryArithICStub<Op>>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer_opt(&mut self.next);
    }
}
