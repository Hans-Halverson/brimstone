use crate::js::runtime::Value;

use super::{
    instruction::{
        CallInstruction, CallMaybeEvalInstruction, CallMaybeEvalVarargsInstruction,
        CallVarargsInstruction, CallWithReceiverInstruction, ConstructInstruction,
        ConstructVarargsInstruction, Instruction, JumpFalseConstantInstruction,
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

pub enum GenericCallArgs<W: Width> {
    Stack { argv: Register<W>, argc: UInt<W> },
    Varargs { array: Register<W> },
}

/// Generic trait for call instructions, such as Call and CallWithReceiver.
pub trait GenericCallInstruction<W: Width>: Instruction {
    fn dest(&self) -> Register<W>;
    fn function(&self) -> Register<W>;
    fn args(&self) -> GenericCallArgs<W>;
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
    fn args(&self) -> GenericCallArgs<W> {
        GenericCallArgs::Stack { argv: self.argv(), argc: self.argc() }
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
    fn args(&self) -> GenericCallArgs<W> {
        GenericCallArgs::Stack { argv: self.argv(), argc: self.argc() }
    }

    #[inline]
    fn receiver(&self) -> Option<Register<W>> {
        Some(self.receiver())
    }
}

impl<W: Width> GenericCallInstruction<W> for CallVarargsInstruction<W> {
    #[inline]
    fn dest(&self) -> Register<W> {
        self.dest()
    }

    #[inline]
    fn function(&self) -> Register<W> {
        self.function()
    }

    #[inline]
    fn args(&self) -> GenericCallArgs<W> {
        GenericCallArgs::Varargs { array: self.args() }
    }

    #[inline]
    fn receiver(&self) -> Option<Register<W>> {
        Some(self.receiver())
    }
}

impl<W: Width> GenericCallInstruction<W> for CallMaybeEvalInstruction<W> {
    #[inline]
    fn dest(&self) -> Register<W> {
        self.dest()
    }

    #[inline]
    fn function(&self) -> Register<W> {
        self.function()
    }

    #[inline]
    fn args(&self) -> GenericCallArgs<W> {
        GenericCallArgs::Stack { argv: self.argv(), argc: self.argc() }
    }

    #[inline]
    fn receiver(&self) -> Option<Register<W>> {
        None
    }
}

impl<W: Width> GenericCallInstruction<W> for CallMaybeEvalVarargsInstruction<W> {
    #[inline]
    fn dest(&self) -> Register<W> {
        self.dest()
    }

    #[inline]
    fn function(&self) -> Register<W> {
        self.function()
    }

    #[inline]
    fn args(&self) -> GenericCallArgs<W> {
        GenericCallArgs::Varargs { array: self.args() }
    }

    #[inline]
    fn receiver(&self) -> Option<Register<W>> {
        None
    }
}

/// Generic trait for construct instructions, such as Construct and ConstructVarargs.
pub trait GenericConstructInstruction<W: Width>: Instruction {
    fn dest(&self) -> Register<W>;
    fn function(&self) -> Register<W>;
    fn args(&self) -> GenericCallArgs<W>;
    fn new_target(&self) -> Register<W>;
}

impl<W: Width> GenericConstructInstruction<W> for ConstructInstruction<W> {
    #[inline]
    fn dest(&self) -> Register<W> {
        self.dest()
    }

    #[inline]
    fn function(&self) -> Register<W> {
        self.function()
    }

    #[inline]
    fn args(&self) -> GenericCallArgs<W> {
        GenericCallArgs::Stack { argv: self.argv(), argc: self.argc() }
    }

    #[inline]
    fn new_target(&self) -> Register<W> {
        self.new_target()
    }
}

impl<W: Width> GenericConstructInstruction<W> for ConstructVarargsInstruction<W> {
    #[inline]
    fn dest(&self) -> Register<W> {
        self.dest()
    }

    #[inline]
    fn function(&self) -> Register<W> {
        self.function()
    }

    #[inline]
    fn args(&self) -> GenericCallArgs<W> {
        GenericCallArgs::Varargs { array: self.args() }
    }

    #[inline]
    fn new_target(&self) -> Register<W> {
        self.new_target()
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
