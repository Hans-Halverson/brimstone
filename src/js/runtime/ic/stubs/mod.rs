use crate::runtime::ic::stubs::binary_arith::BailReason;

pub mod binary_arith;
pub mod emitter;
pub mod executor;

pub enum Operand {
    Left,
    Right,
}

pub enum ICGuard {
    Smi(Operand, BailReason),
    Number(Operand, BailReason),
    String(Operand, BailReason),
    BigInt(Operand, BailReason),
}

pub enum ICOp {
    AddSmi,
    AddNumber,
    AddBigInt,
    ConcatString,
    SubSmi,
    SubNumber,
    SubBigInt,
    MulSmi,
    MulNumber,
    MulBigInt,
    DivSmi,
    DivNumber,
    DivBigInt,
    RemSmi,
    RemNumber,
    RemBigInt,
    ExpSmi,
    ExpNumber,
    ExpBigInt,
    BitAndSmi,
    BitAndNumber,
    BitAndBigInt,
    BitOrSmi,
    BitOrNumber,
    BitOrBigInt,
    BitXorSmi,
    BitXorNumber,
    BitXorBigInt,
    ShiftLeftSmi,
    ShiftLeftNumber,
    ShiftLeftBigInt,
    ShiftRightArithSmi,
    ShiftRightArithNumber,
    ShiftRightArithBigInt,
    ShiftRightLogicalSmi,
    ShiftRightLogicalNumber,
    StrictEqSmi,
    StrictEqNumber,
    StrictEqBigInt,
    StrictEqString,
    StrictNeqSmi,
    StrictNeqNumber,
    StrictNeqBigInt,
    StrictNeqString,
    LooseEqSmi,
    LooseEqNumber,
    LooseEqBigInt,
    LooseEqString,
    LooseNeqSmi,
    LooseNeqNumber,
    LooseNeqBigInt,
    LooseNeqString,
    LtSmi,
    LtNumber,
    LtBigInt,
    LtString,
    LteSmi,
    LteNumber,
    LteBigInt,
    LteString,
    GtSmi,
    GtNumber,
    GtBigInt,
    GtString,
    GteSmi,
    GteNumber,
    GteBigInt,
    GteString,
}

pub enum ICInstruction {
    Guard(ICGuard),
    Exec(ICOp),
}

/// A trait used to generalize ICStubs to
/// something that can both be used to
/// execute IC Stubs (highly optimized)
/// by the compiler or to emit an IR
/// representing  
pub trait ICBackend: Sized {
    type Output;
    fn guard(self, guard: ICGuard) -> Self;
    fn exec(self, op: ICOp) -> Self::Output;
}
