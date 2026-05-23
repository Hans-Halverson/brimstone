use crate::runtime::{
    eval::common::{
        add_bigint_fast, add_number_fast, add_smi_fast, add_string_fast, bitwise_and_bigint_fast,
        bitwise_and_number_fast, bitwise_and_smi_fast, bitwise_or_bigint_fast,
        bitwise_or_number_fast, bitwise_or_smi_fast, bitwise_xor_bigint_fast,
        bitwise_xor_number_fast, bitwise_xor_smi_fast, div_bigint_fast, div_number_fast,
        div_smi_fast, exp_bigint_fast, exp_number_fast, exp_smi_fast, is_bigint, is_number, is_smi,
        is_string, mul_bigint_fast, mul_number_fast, mul_smi_fast, rem_bigint_fast,
        rem_number_fast, rem_smi_fast, shift_left_bigint_fast, shift_left_number_fast,
        shift_left_smi_fast, shift_right_arith_bigint_fast, shift_right_arith_number_fast,
        shift_right_arith_smi_fast, shift_right_logical_number_fast, shift_right_logical_smi_fast,
        sub_bigint_fast, sub_number_fast, sub_smi_fast,
    },
    ic::{
        fgc::FGC,
        stubs::{binary_arith::BailReason, ICBackend, ICGuard, ICOp, Operand},
    },
    Context, EvalResult, Handle, Value,
};

macro_rules! ic_op {
    ($state:expr, $cx:expr, $func:ident) => {
        $state.map(move |(l, r)| Ok($func($cx, l, r)))
    };
    ($state:expr, $cx:expr, $func:ident?) => {
        $state.map(move |(l, r)| $func($cx, l, r).map_err(Into::into))
    };
}

pub struct BinaryExecutor {
    cx: Context,
    state: FGC<(Handle<Value>, Handle<Value>), BailReason>,
}

impl BinaryExecutor {
    #[inline(always)]
    pub fn new(cx: Context, left: Handle<Value>, right: Handle<Value>) -> Self {
        BinaryExecutor { cx, state: FGC::new((left, right)) }
    }
}

impl ICBackend for BinaryExecutor {
    type Output = FGC<EvalResult<Handle<Value>>, BailReason>;

    #[inline(always)]
    fn guard(self, guard: ICGuard) -> Self {
        BinaryExecutor {
            cx: self.cx,
            state: match guard {
                ICGuard::Smi(op, bail) => self.state.check(
                    |(l, r)| match op {
                        Operand::Left => is_smi(*l),
                        Operand::Right => is_smi(*r),
                    },
                    bail,
                ),
                ICGuard::Number(op, bail) => self.state.check(
                    |(l, r)| match op {
                        Operand::Left => is_number(*l),
                        Operand::Right => is_number(*r),
                    },
                    bail,
                ),
                ICGuard::String(op, bail) => self.state.check(
                    |(l, r)| match op {
                        Operand::Left => is_string(*l),
                        Operand::Right => is_string(*r),
                    },
                    bail,
                ),
                ICGuard::BigInt(op, bail) => self.state.check(
                    |(l, r)| match op {
                        Operand::Left => is_bigint(*l),
                        Operand::Right => is_bigint(*r),
                    },
                    bail,
                ),
            },
        }
    }

    #[inline(always)]
    fn exec(self, op: ICOp) -> FGC<EvalResult<Handle<Value>>, BailReason> {
        let BinaryExecutor { cx, state } = self;

        match op {
            ICOp::AddSmi => ic_op!(state, cx, add_smi_fast),
            ICOp::AddNumber => ic_op!(state, cx, add_number_fast),
            ICOp::AddBigInt => ic_op!(state, cx, add_bigint_fast?),
            ICOp::ConcatString => {
                state.map(move |(l, r)| Ok(add_string_fast(cx, l.as_string(), r.as_string())?))
            }
            ICOp::SubSmi => ic_op!(state, cx, sub_smi_fast),
            ICOp::SubNumber => ic_op!(state, cx, sub_number_fast),
            ICOp::SubBigInt => ic_op!(state, cx, sub_bigint_fast?),
            ICOp::MulSmi => ic_op!(state, cx, mul_smi_fast),
            ICOp::MulNumber => ic_op!(state, cx, mul_number_fast),
            ICOp::MulBigInt => ic_op!(state, cx, mul_bigint_fast?),
            ICOp::DivSmi => ic_op!(state, cx, div_smi_fast),
            ICOp::DivNumber => ic_op!(state, cx, div_number_fast),
            ICOp::DivBigInt => ic_op!(state, cx, div_bigint_fast?),
            ICOp::RemSmi => ic_op!(state, cx, rem_smi_fast),
            ICOp::RemNumber => ic_op!(state, cx, rem_number_fast),
            ICOp::RemBigInt => ic_op!(state, cx, rem_bigint_fast?),
            ICOp::ExpSmi => ic_op!(state, cx, exp_smi_fast),
            ICOp::ExpNumber => ic_op!(state, cx, exp_number_fast),
            ICOp::ExpBigInt => ic_op!(state, cx, exp_bigint_fast?),
            ICOp::BitAndSmi => ic_op!(state, cx, bitwise_and_smi_fast),
            ICOp::BitAndNumber => ic_op!(state, cx, bitwise_and_number_fast?),
            ICOp::BitAndBigInt => ic_op!(state, cx, bitwise_and_bigint_fast?),
            ICOp::BitOrSmi => ic_op!(state, cx, bitwise_or_smi_fast),
            ICOp::BitOrNumber => ic_op!(state, cx, bitwise_or_number_fast?),
            ICOp::BitOrBigInt => ic_op!(state, cx, bitwise_or_bigint_fast?),
            ICOp::BitXorSmi => ic_op!(state, cx, bitwise_xor_smi_fast),
            ICOp::BitXorNumber => ic_op!(state, cx, bitwise_xor_number_fast?),
            ICOp::BitXorBigInt => ic_op!(state, cx, bitwise_xor_bigint_fast?),
            ICOp::ShiftLeftSmi => ic_op!(state, cx, shift_left_smi_fast?),
            ICOp::ShiftLeftNumber => ic_op!(state, cx, shift_left_number_fast?),
            ICOp::ShiftLeftBigInt => ic_op!(state, cx, shift_left_bigint_fast?),
            ICOp::ShiftRightArithSmi => ic_op!(state, cx, shift_right_arith_smi_fast?),
            ICOp::ShiftRightArithNumber => ic_op!(state, cx, shift_right_arith_number_fast?),
            ICOp::ShiftRightArithBigInt => ic_op!(state, cx, shift_right_arith_bigint_fast?),
            ICOp::ShiftRightLogicalSmi => ic_op!(state, cx, shift_right_logical_smi_fast?),
            ICOp::ShiftRightLogicalNumber => ic_op!(state, cx, shift_right_logical_number_fast?),
        }
    }
}
