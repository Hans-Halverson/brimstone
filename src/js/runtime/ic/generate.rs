use crate::runtime::ic::stubs::binary_arith::{
    AddICStub, BailReason, BinaryArithICStubEmitter, BinaryArithICStubExecutor, BitAndICStub,
    BitOrICStub, BitXorICStub, DivICStub, ExpICStub, MulICStub, RemICStub, ShiftLeftICStub,
    ShiftRightArithICStub, ShiftRightLogicalICStub, SubICStub,
};
use crate::runtime::ic::stubs::{ICBackend, ICGuard, ICOp, Operand};
use crate::runtime::{alloc_error::AllocResult, Context, Handle, Value};

macro_rules! define_stub {
    ($name:ident, $guard:ident, $bail:ident, $op:ident) => {
        fn $name<B: ICBackend>(b: B) -> B::Output {
            b.guard(ICGuard::$guard(Operand::Left, BailReason::$bail))
                .guard(ICGuard::$guard(Operand::Right, BailReason::$bail))
                .exec(ICOp::$op)
        }
    };
}

macro_rules! def_new_stub_for {
    ($($op:ident, $smi:ident, $number:ident $(, $bigint:ident)? $(+ $string:ident)?);* $(;)?) => {
        $(
            paste::paste! {
                pub fn [<new_stub_for_ $op:lower>](
                    &self,
                    left: &Value,
                    right: &Value,
                    result: &Value,
                ) -> Option<AllocResult<Handle<[<$op ICStub>]>>> {
                    let (execute, emit): (BinaryArithICStubExecutor, BinaryArithICStubEmitter) =
                        if left.is_smi() && right.is_smi() {
                            if result.is_smi() {
                                ($smi, $smi)
                            } else {
                                ($number, $number)
                            }
                        } else if left.is_number() && right.is_number() {
                            ($number, $number)
                        }
                        $(
                            else if left.is_string() && right.is_string() {
                                ($string, $string)
                            }
                        )?
                        $(
                            else if left.is_bigint() && right.is_bigint() {
                                ($bigint, $bigint)
                            }
                        )?
                        else {
                            return None;
                        };

                    Some([<$op ICStub>]::new(self.cx, execute, emit))
                }
            }
        )*
    };
}

// add
define_stub!(add_smi_stub, Smi, ExpectedSmi, AddSmi);
define_stub!(add_number_stub, Number, ExpectedNumber, AddNumber);
define_stub!(add_bigint_stub, BigInt, ExpectedBigInt, AddBigInt);
define_stub!(concat_string_stub, String, ExpectedString, ConcatString);
// sub
define_stub!(sub_smi_stub, Smi, ExpectedSmi, SubSmi);
define_stub!(sub_number_stub, Number, ExpectedNumber, SubNumber);
define_stub!(sub_bigint_stub, BigInt, ExpectedBigInt, SubBigInt);
// mul
define_stub!(mul_smi_stub, Smi, ExpectedSmi, MulSmi);
define_stub!(mul_number_stub, Number, ExpectedNumber, MulNumber);
define_stub!(mul_bigint_stub, BigInt, ExpectedBigInt, MulBigInt);
// div
define_stub!(div_smi_stub, Smi, ExpectedSmi, DivSmi);
define_stub!(div_number_stub, Number, ExpectedNumber, DivNumber);
define_stub!(div_bigint_stub, BigInt, ExpectedBigInt, DivBigInt);
// rem
define_stub!(rem_smi_stub, Smi, ExpectedSmi, RemSmi);
define_stub!(rem_number_stub, Number, ExpectedNumber, RemNumber);
define_stub!(rem_bigint_stub, BigInt, ExpectedBigInt, RemBigInt);
// exp
define_stub!(exp_smi_stub, Smi, ExpectedSmi, ExpSmi);
define_stub!(exp_number_stub, Number, ExpectedNumber, ExpNumber);
define_stub!(exp_bigint_stub, BigInt, ExpectedBigInt, ExpBigInt);
// bit and
define_stub!(bit_and_smi_stub, Smi, ExpectedSmi, BitAndSmi);
define_stub!(bit_and_number_stub, Number, ExpectedNumber, BitAndNumber);
define_stub!(bit_and_bigint_stub, BigInt, ExpectedBigInt, BitAndBigInt);
// bit or
define_stub!(bit_or_smi_stub, Smi, ExpectedSmi, BitOrSmi);
define_stub!(bit_or_number_stub, Number, ExpectedNumber, BitOrNumber);
define_stub!(bit_or_bigint_stub, BigInt, ExpectedBigInt, BitOrBigInt);
// bit xor
define_stub!(bit_xor_smi_stub, Smi, ExpectedSmi, BitXorSmi);
define_stub!(bit_xor_number_stub, Number, ExpectedNumber, BitXorNumber);
define_stub!(bit_xor_bigint_stub, BigInt, ExpectedBigInt, BitXorBigInt);
// shift left
define_stub!(shift_left_smi_stub, Smi, ExpectedSmi, ShiftLeftSmi);
define_stub!(shift_left_number_stub, Number, ExpectedNumber, ShiftLeftNumber);
define_stub!(shift_left_bigint_stub, BigInt, ExpectedBigInt, ShiftLeftBigInt);
// shift right arith
define_stub!(shift_right_arith_smi_stub, Smi, ExpectedSmi, ShiftRightArithSmi);
define_stub!(shift_right_arith_number_stub, Number, ExpectedNumber, ShiftRightArithNumber);
define_stub!(shift_right_arith_bigint_stub, BigInt, ExpectedBigInt, ShiftRightArithBigInt);
// shift right logical
define_stub!(shift_right_logical_smi_stub, Smi, ExpectedSmi, ShiftRightLogicalSmi);
define_stub!(shift_right_logical_number_stub, Number, ExpectedNumber, ShiftRightLogicalNumber);

pub struct ICStubGenerator<'cx> {
    cx: &'cx Context,
}

impl<'cx> ICStubGenerator<'cx> {
    pub fn new(cx: &'cx Context) -> Self {
        Self { cx }
    }

    def_new_stub_for! {
        Add, add_smi_stub, add_number_stub, add_bigint_stub + concat_string_stub;
        Sub, sub_smi_stub, sub_number_stub, sub_bigint_stub;
        Mul, mul_smi_stub, mul_number_stub, mul_bigint_stub;
        Div, div_smi_stub, div_number_stub, div_bigint_stub;
        Rem, rem_smi_stub, rem_number_stub, rem_bigint_stub;
        Exp, exp_smi_stub, exp_number_stub, exp_bigint_stub;
        BitAnd, bit_and_smi_stub, bit_and_number_stub, bit_and_bigint_stub;
        BitOr, bit_or_smi_stub, bit_or_number_stub, bit_or_bigint_stub;
        BitXor, bit_xor_smi_stub, bit_xor_number_stub, bit_xor_bigint_stub;
        ShiftLeft, shift_left_smi_stub, shift_left_number_stub, shift_left_bigint_stub;
        ShiftRightArith, shift_right_arith_smi_stub, shift_right_arith_number_stub, shift_right_arith_bigint_stub;
        ShiftRightLogical, shift_right_logical_smi_stub, shift_right_logical_number_stub;
    }
}
