use crate::js::runtime::Value;

use super::{
    instruction::{
        CallInstruction, CallWithReceiverInstruction, Instruction, JumpFalseConstantInstruction,
        JumpFalseInstruction, JumpNotNullishConstantInstruction, JumpNotNullishInstruction,
        JumpNotUndefinedConstantInstruction, JumpNotUndefinedInstruction,
        JumpNullishConstantInstruction, JumpNullishInstruction,
        JumpToBooleanFalseConstantInstruction, JumpToBooleanFalseInstruction,
        JumpToBooleanTrueConstantInstruction, JumpToBooleanTrueInstruction,
        JumpTrueConstantInstruction, JumpTrueInstruction,
    },
    operand::{ConstantIndex, Register, SInt, UInt},
    width::Width,
};

/// Generic trait for call instructions, such as CallInstruction and CallWithReceiverInstruction.
pub trait GenericCallInstruction<W: Width>: Instruction {
    fn dest(&self) -> Register<W>;
    fn function(&self) -> Register<W>;
    fn argc(&self) -> UInt<W>;
    fn argv(&self) -> Register<W>;
    fn receiver(&self) -> Option<Register<W>>;
}

impl<W: Width> GenericCallInstruction<W> for CallInstruction<W> {
    #[inline]
    fn dest(&self) -> Register<W> {
        self.dest()
    }

    #[inline]
    fn function(&self) -> Register<W> {
        self.function()
    }

    #[inline]
    fn argc(&self) -> UInt<W> {
        self.argc()
    }

    #[inline]
    fn argv(&self) -> Register<W> {
        self.argv()
    }

    #[inline]
    fn receiver(&self) -> Option<Register<W>> {
        None
    }
}

impl<W: Width> GenericCallInstruction<W> for CallWithReceiverInstruction<W> {
    #[inline]
    fn dest(&self) -> Register<W> {
        self.dest()
    }

    #[inline]
    fn function(&self) -> Register<W> {
        self.function()
    }

    #[inline]
    fn argc(&self) -> UInt<W> {
        self.argc()
    }

    #[inline]
    fn argv(&self) -> Register<W> {
        self.argv()
    }

    #[inline]
    fn receiver(&self) -> Option<Register<W>> {
        Some(self.receiver())
    }
}

/// Generic trait for jump if boolean instructions (non-constant, non-coercing).
pub trait GenericJumpBooleanInstruction<W: Width>: Instruction {
    fn condition(&self) -> Register<W>;
    fn offset(&self) -> SInt<W>;
    fn cond_function(value: Value) -> bool;
}

impl<W: Width> GenericJumpBooleanInstruction<W> for JumpTrueInstruction<W> {
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn offset(&self) -> SInt<W> {
        self.offset()
    }

    #[inline]
    fn cond_function(value: Value) -> bool {
        Value::is_true(&value)
    }
}

impl<W: Width> GenericJumpBooleanInstruction<W> for JumpFalseInstruction<W> {
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn offset(&self) -> SInt<W> {
        self.offset()
    }

    #[inline]
    fn cond_function(value: Value) -> bool {
        Value::is_false(&value)
    }
}

/// Generic trait for jump if boolean instructions (constant, non-coercing).
pub trait GenericJumpBooleanConstantInstruction<W: Width>: Instruction {
    fn condition(&self) -> Register<W>;
    fn constant_index(&self) -> ConstantIndex<W>;
    fn cond_function(value: Value) -> bool;
}

impl<W: Width> GenericJumpBooleanConstantInstruction<W> for JumpTrueConstantInstruction<W> {
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn constant_index(&self) -> ConstantIndex<W> {
        self.constant_index()
    }

    #[inline]
    fn cond_function(value: Value) -> bool {
        Value::is_true(&value)
    }
}

impl<W: Width> GenericJumpBooleanConstantInstruction<W> for JumpFalseConstantInstruction<W> {
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn constant_index(&self) -> ConstantIndex<W> {
        self.constant_index()
    }

    #[inline]
    fn cond_function(value: Value) -> bool {
        Value::is_false(&value)
    }
}

/// Generic trait for jump if boolean instructions (non-constant, coercing).
pub trait GenericJumpToBooleanInstruction<W: Width>: Instruction {
    fn condition(&self) -> Register<W>;
    fn offset(&self) -> SInt<W>;
    fn cond_function(value: bool) -> bool;
}

impl<W: Width> GenericJumpToBooleanInstruction<W> for JumpToBooleanTrueInstruction<W> {
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn offset(&self) -> SInt<W> {
        self.offset()
    }

    #[inline]
    fn cond_function(value: bool) -> bool {
        value
    }
}

impl<W: Width> GenericJumpToBooleanInstruction<W> for JumpToBooleanFalseInstruction<W> {
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn offset(&self) -> SInt<W> {
        self.offset()
    }

    #[inline]
    fn cond_function(value: bool) -> bool {
        !value
    }
}

/// Generic trait for jump if boolean instructions (constant, coercing).
pub trait GenericJumpToBooleanConstantInstruction<W: Width>: Instruction {
    fn condition(&self) -> Register<W>;
    fn constant_index(&self) -> ConstantIndex<W>;
    fn cond_function(value: bool) -> bool;
}

impl<W: Width> GenericJumpToBooleanConstantInstruction<W>
    for JumpToBooleanTrueConstantInstruction<W>
{
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn constant_index(&self) -> ConstantIndex<W> {
        self.constant_index()
    }

    #[inline]
    fn cond_function(value: bool) -> bool {
        value
    }
}

impl<W: Width> GenericJumpToBooleanConstantInstruction<W>
    for JumpToBooleanFalseConstantInstruction<W>
{
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn constant_index(&self) -> ConstantIndex<W> {
        self.constant_index()
    }

    #[inline]
    fn cond_function(value: bool) -> bool {
        !value
    }
}

/// Generic trait for jump if undefined instructions (non-constant).
pub trait GenericJumpUndefinedInstruction<W: Width>: Instruction {
    fn condition(&self) -> Register<W>;
    fn offset(&self) -> SInt<W>;
    fn cond_function(value: Value) -> bool;
}

impl<W: Width> GenericJumpUndefinedInstruction<W> for JumpNotUndefinedInstruction<W> {
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn offset(&self) -> SInt<W> {
        self.offset()
    }

    #[inline]
    fn cond_function(value: Value) -> bool {
        !value.is_undefined()
    }
}

/// Generic trait for jump if undefined instructions (constant).
pub trait GenericJumpUndefinedConstantInstruction<W: Width>: Instruction {
    fn condition(&self) -> Register<W>;
    fn constant_index(&self) -> ConstantIndex<W>;
    fn cond_function(value: Value) -> bool;
}

impl<W: Width> GenericJumpUndefinedConstantInstruction<W>
    for JumpNotUndefinedConstantInstruction<W>
{
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn constant_index(&self) -> ConstantIndex<W> {
        self.constant_index()
    }

    #[inline]
    fn cond_function(value: Value) -> bool {
        !value.is_undefined()
    }
}

/// Generic trait for jump if nullish/not nullish instructions (non-constant).
pub trait GenericJumpNullishInstruction<W: Width>: Instruction {
    fn condition(&self) -> Register<W>;
    fn offset(&self) -> SInt<W>;
    fn cond_function(value: Value) -> bool;
}

impl<W: Width> GenericJumpNullishInstruction<W> for JumpNullishInstruction<W> {
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn offset(&self) -> SInt<W> {
        self.offset()
    }

    #[inline]
    fn cond_function(value: Value) -> bool {
        value.is_nullish()
    }
}

impl<W: Width> GenericJumpNullishInstruction<W> for JumpNotNullishInstruction<W> {
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn offset(&self) -> SInt<W> {
        self.offset()
    }

    #[inline]
    fn cond_function(value: Value) -> bool {
        !value.is_nullish()
    }
}

/// Generic trait for jump if nullish/not nullish instructions (constant).
pub trait GenericJumpNullishConstantInstruction<W: Width>: Instruction {
    fn condition(&self) -> Register<W>;
    fn constant_index(&self) -> ConstantIndex<W>;
    fn cond_function(value: Value) -> bool;
}

impl<W: Width> GenericJumpNullishConstantInstruction<W> for JumpNullishConstantInstruction<W> {
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn constant_index(&self) -> ConstantIndex<W> {
        self.constant_index()
    }

    #[inline]
    fn cond_function(value: Value) -> bool {
        value.is_nullish()
    }
}

impl<W: Width> GenericJumpNullishConstantInstruction<W> for JumpNotNullishConstantInstruction<W> {
    #[inline]
    fn condition(&self) -> Register<W> {
        self.condition()
    }

    #[inline]
    fn constant_index(&self) -> ConstantIndex<W> {
        self.constant_index()
    }

    #[inline]
    fn cond_function(value: Value) -> bool {
        !value.is_nullish()
    }
}
