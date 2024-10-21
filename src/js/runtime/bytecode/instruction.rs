use bitflags::bitflags;

use std::{
    collections::{HashMap, HashSet},
    fmt,
};

use super::{
    operand::{ConstantIndex, Operand, OperandType, Register, SInt, UInt},
    width::{ExtraWide, Narrow, SignedWidthRepr, UnsignedWidthRepr, Wide, Width, WidthEnum},
    writer::BytecodeWriter,
};

use crate::{count, js::runtime::debug_print::DebugPrinter, replace_expr};

/// Generic properties of instructions.
#[allow(dead_code)]
pub trait Instruction: fmt::Display {
    fn opcode(&self) -> OpCode;
    fn num_operands(&self) -> usize;
    fn operand_types(&self) -> &[OperandType];
    fn width(&self) -> WidthEnum;

    /// Return a raw operand at the given index. The operand is extended to a usize, though may
    /// actually represent a smaller or unsigned integer.
    fn get_raw_operand_signed(&self, index: usize) -> isize;

    /// Total length in bytes of this instruction's operands. Does not include the opcode or width
    /// prefix.
    fn byte_length(&self) -> usize;
}

/// A utility macro for optionally including a term in a macro expansion.
///
/// All this does is return the second argument. But if the first argument is set to an optional
/// macro variable while inside a $( ... )? block, the second argument will be written instead.
macro_rules! if_set {
    ($x:tt, $y:tt) => {
        $y
    };
}

/// Generate bytecode instructions using a custom DSL.
///
/// Instructions are defined using the following syntax:
///
/// // The short instruction name
/// InstructionName {
///
///   // The full instruction name in camel case
///   camel_case: InstructionNameInstruction,
///
///   // The full instruction name in snake case
///   snake_case: instruction_name_instruction,
///
///   // (Optional) Marks that this instruction can throw an error. Source code positions are only
///   // recorded in the source map for instructions that can throw.
///   can_throw: true,
///
///   // The operands of the instruction in the form `[operand_index] operand_name: operand_type`
///   operands: {
///     [0] operand_name_1: OperandType1,
///     [1] operand_name_2: OperandType2,
///     ...
///   }
/// }
macro_rules! define_instructions {
    ($(
        $(#[$($attrs:tt)*])* $short_name:ident {
            camel_case: $instr_name_camel:ident,
            snake_case: $instr_name_snake:ident,
            $(can_throw: $can_throw:ident,)?
            operands: {
                $([$operand_idx:expr] $operand_name:ident: $operand_type:ident,)*
            }
        }
    )*) => {
        $(
            // Statically verify that only "true" can be passed to the "can_throw" property
            $(
                const _: () = assert!($can_throw, "Only 'true' is allowed for the 'can_throw' property");
            )?

            $(#[$($attrs)*])*
            #[repr(C)]
            pub struct $instr_name_camel<W: Width>([W::UInt; count!($($operand_name)*)]);

            impl <W: Width> $instr_name_camel<W> {
                $(
                    pub fn $operand_name(&self) -> $operand_type<W> {
                        $operand_type::<W>::from_unsigned(self.0[$operand_idx])
                    }
                )*

                const OPERAND_TYPES: &'static [OperandType] = &[
                    $(
                        OperandType::$operand_type,
                    )*
                ];

                const OPCODE: OpCode = OpCode::$short_name;

                const NUM_OPERANDS: usize = Self::OPERAND_TYPES.len();

                const BYTE_LENGTH: usize = Self::NUM_OPERANDS * W::NUM_BYTES;

                /// Write the instruction into the given bytecode stream.
                pub fn write(
                    writer: &mut BytecodeWriter,
                    $($operand_name: $operand_type<W>,)*
                    $(if_set!($can_throw, pos): usize, )?
                ) {
                    // Write instruction to bytecode buffer
                    writer.write_width_prefix(W::ENUM);
                    writer.write_opcode(Self::OPCODE);
                    $(writer.write_operand($operand_name);)*

                    // Write a source map entry if this instruction can throw
                    $(if_set!($can_throw, writer).write_source_map_entry(pos);)?
                }
            }

            impl <W: Width> Instruction for $instr_name_camel<W> {
                #[inline]
                fn opcode(&self) -> OpCode {
                    Self::OPCODE
                }

                #[inline]
                fn num_operands(&self) -> usize {
                    Self::NUM_OPERANDS
                }

                #[inline]
                fn operand_types(&self) -> &[OperandType] {
                    Self::OPERAND_TYPES
                }

                #[inline]
                fn byte_length(&self) -> usize {
                    Self::BYTE_LENGTH
                }

                #[inline]
                fn width(&self) -> WidthEnum {
                    W::ENUM
                }

                #[inline]
                fn get_raw_operand_signed(&self, index: usize) -> isize {
                    self.0[index].as_signed().to_isize()
                }
            }

            /// Instruction printing functions.
            impl <W: Width> fmt::Display for $instr_name_camel<W> {
                #[allow(unused_assignments, unused_mut, unused_variables)]
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    // Write opcode
                    f.write_fmt(format_args!("{:?}", Self::OPCODE))?;

                    // Write width if non-narrow
                    if W::ENUM == WidthEnum::Wide {
                        f.write_str(" (Wide)")?;
                    } else if W::ENUM == WidthEnum::ExtraWide {
                        f.write_str(" (ExtraWide)")?;
                    }

                    f.write_str(" ")?;

                    // Write operands separated by commas
                    let mut i = 0;

                    $(
                        f.write_fmt(format_args!("{}", self.$operand_name()))?;

                        // Do not add a comma separator after the last element
                        if Self::NUM_OPERANDS > 1 && i < Self::NUM_OPERANDS - 1 {
                            f.write_str(", ")?;
                        }

                        i += 1;
                    )*

                    Ok(())
                }
            }
        )*

        #[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
        #[repr(u8)]
        pub enum OpCode {
            $($short_name,)*
        }

        /// Instruction writing with minimum width in bytecode builder.
        impl BytecodeWriter {
            $(
                #[allow(unused_mut, clippy::wrong_self_convention)]
                pub fn $instr_name_snake(
                    &mut self,
                    $($operand_name: $operand_type<ExtraWide>,)*
                    $(if_set!($can_throw, pos): usize,)?
                ) {
                    // Calculate the width needed to fit all operands
                    let mut width = WidthEnum::Narrow;
                    $(width = $operand_name.min_width().max(width);)*

                    // Write an instruction with that minimum width
                    match width {
                        WidthEnum::Narrow => $instr_name_camel::<Narrow>::write(
                            self,
                            $($operand_name.to_narrow(),)*
                            $(if_set!($can_throw, pos),)?
                        ),
                        WidthEnum::Wide => $instr_name_camel::<Wide>::write(
                            self,
                            $($operand_name.to_wide(),)*
                            $(if_set!($can_throw, pos),)?
                        ),
                        WidthEnum::ExtraWide => $instr_name_camel::<ExtraWide>::write(
                            self,
                            $($operand_name,)*
                            $(if_set!($can_throw, pos),)?
                        ),
                    }
                }
            )*
        }

        /// Given an opcode and an instruction stream pointing to the byte following the opcode,
        /// return the narrow instruction.
        fn decode_narrow_instruction(opcode: OpCode, instruction_stream: &[u8]) -> &dyn Instruction {
            match opcode {
                $(OpCode::$short_name => {
                    let instr: &$instr_name_camel<Narrow> = unsafe {
                        &*instruction_stream.as_ptr().cast()
                    };
                    instr
                })*
            }
        }

        /// Given an opcode and an instruction stream pointing to the byte following the opcode,
        /// return the wide instruction.
        fn decode_wide_instruction(opcode: OpCode, instruction_stream: &[u8]) -> &dyn Instruction {
            match opcode {
                $(OpCode::$short_name => {
                    let instr: &$instr_name_camel<Wide> = unsafe {
                       &*instruction_stream.as_ptr().cast()
                    };
                    instr
                })*
            }
        }

        /// Given an opcode and an instruction stream pointing to the byte following the opcode,
        /// return the extra wide instruction.
        fn decode_extra_wide_instruction(opcode: OpCode, instruction_stream: &[u8]) -> &dyn Instruction {
            match opcode {
                $(OpCode::$short_name => {
                    let instr: &$instr_name_camel<ExtraWide> = unsafe {
                        &*instruction_stream.as_ptr().cast()
                    };
                    instr
                })*
            }
        }
    };
}

define_instructions!(
    /// Prefix denoting that the next instruction is wide.
    WidePrefix {
        camel_case: WidePrefixInstruction,
        snake_case: wide_prefix_instruction,
        operands: {}
    }

    /// Prefix denoting that the next instruction is extra wide.
    ExtraWidePrefix {
        camel_case: ExtraWidePrefixInstruction,
        snake_case: extra_wide_prefix_instruction,
        operands: {}
    }

    /// Copy a value between registers.
    Mov {
        camel_case: MovInstruction,
        snake_case: mov_instruction,
        operands: {
            [0] dest: Register,
            [1] src: Register,
        }
    }

    /// Load an immediate into a register.
    LoadImmediate {
        camel_case: LoadImmediateInstruction,
        snake_case: load_immediate_instruction,
        operands: {
            [0] dest: Register,
            [1] immediate: SInt,
        }
    }

    /// Load a constant from the constant table into a register.
    LoadConstant {
        camel_case: LoadConstantInstruction,
        snake_case: load_constant_instruction,
        operands: {
            [0] dest: Register,
            [1] constant_index: ConstantIndex,
        }
    }

    /// Load the value `undefined` into a register.
    LoadUndefined {
        camel_case: LoadUndefinedInstruction,
        snake_case: load_undefined_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Load the value `null` into a register.
    LoadNull {
        camel_case: LoadNullInstruction,
        snake_case: load_null_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Load the empty value into a register.
    LoadEmpty {
        camel_case: LoadEmptyInstruction,
        snake_case: load_empty_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Load the value `true` into a register.
    LoadTrue {
        camel_case: LoadTrueInstruction,
        snake_case: load_true_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Load the value `false` into a register.
    LoadFalse {
        camel_case: LoadFalseInstruction,
        snake_case: load_false_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Load a global variable into a register, erroring if the global does not exist. The global
    /// variable's name is stored in the constant table.
    LoadGlobal {
        camel_case: LoadGlobalInstruction,
        snake_case: load_global_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] constant_index: ConstantIndex,
        }
    }

    /// Load a global variable into a register, loading undefined if the global does not exist. The
    /// global variable's name is stored in the constant table.
    LoadGlobalOrUnresolved {
        camel_case: LoadGlobalOrUnresolvedInstruction,
        snake_case: load_global_or_unresolved_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] constant_index: ConstantIndex,
        }
    }

    /// Store a register into a global variable. The global variable's name is stored in the
    /// constant table.
    StoreGlobal {
        camel_case: StoreGlobalInstruction,
        snake_case: store_global_instruction,
        can_throw: true,
        operands: {
            [0] value: Register,
            [1] constant_index: ConstantIndex,
        }
    }

    /// Load a variable with the given name into dest, erroring if the name could not be resolved.
    /// Dynamically look up the variable by name in the scope chain. The name is stored in the
    /// constant table.
    LoadDynamic {
        camel_case: LoadDynamicInstruction,
        snake_case: load_dynamic_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] name_index: ConstantIndex,
        }
    }

    /// Load a variable with the given name into dest, loading undefiend if the name could not be
    /// resolved. Dynamically look up the variable by name in the scope chain. The name is stored in
    /// the constant table.
    LoadDynamicOrUnresolved {
        camel_case: LoadDynamicOrUnresolvedInstruction,
        snake_case: load_dynamic_or_unresolved_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] name_index: ConstantIndex,
        }
    }

    /// Store a value into a variable. Dynamically look up the variable by name in the scope chain.
    /// The name is stored in the constant table.
    StoreDynamic {
        camel_case: StoreDynamicInstruction,
        snake_case: store_dynamic_instruction,
        can_throw: true,
        operands: {
            [0] value: Register,
            [1] name_index: ConstantIndex,
        }
    }

    /// Call a function. Arguments are passed in contiguous sequence of registers starting at argv,
    /// of length argc. The receiver is undefined.
    Call {
        camel_case: CallInstruction,
        snake_case: call_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] function: Register,
            [2] argv: Register,
            [3] argc: UInt,
        }
    }

    /// Call a function, specifying the receiver to be used. All other arguments are passed in a
    /// contiguous sequence of registers starting at argv, of length argc.
    CallWithReceiver {
        camel_case: CallWithReceiverInstruction,
        snake_case: call_with_receiver_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] function: Register,
            [2] receiver: Register,
            [3] argv: Register,
            [4] argc: UInt,
        }
    }

    /// Call a function with a variable number of arguments, specifying the receiver to be used. All
    /// other arguments are passed in an array.
    CallVarargs {
        camel_case: CallVarargsInstruction,
        snake_case: call_varargs_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] function: Register,
            [2] receiver: Register,
            [3] args: Register,
        }
    }

    /// Call a function which may be a direct eval. If this is a direct eval then perform the direct
    /// eval, otherwise identical to the call instruction.
    ///
    /// Arguments are passed in contiguous sequence of registers starting at argv, of length argc.
    /// The receiver is undefined.
    CallMaybeEval {
        camel_case: CallMaybeEvalInstruction,
        snake_case: call_maybe_eval_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] function: Register,
            [2] argv: Register,
            [3] argc: UInt,
            [4] flags: UInt,
        }
    }

    /// Call a function with variable number of arguments, where the function may be a direct eval.
    /// If this is a direct eval then perform the direct eval, otherwise identical to the call
    /// instruction.
    ///
    /// The receiver is undefined. All other arguments are passed in an array.
    CallMaybeEvalVarargs {
        camel_case: CallMaybeEvalVarargsInstruction,
        snake_case: call_maybe_eval_varargs_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] function: Register,
            [2] args: Register,
            [3] flags: UInt,
        }
    }

    /// Call a constructor. Arguments are passed in contiguous sequence of registers starting at
    /// argv, of length argc. The new target is passed in its own register.
    Construct {
        camel_case: ConstructInstruction,
        snake_case: construct_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] function: Register,
            [2] new_target: Register,
            [3] argv: Register,
            [4] argc: UInt,
        }
    }

    /// Call a constructor with a variable number of arguments, where arguments are passed in an
    /// array. The new target is passed in its own register.
    ConstructVarargs {
        camel_case: ConstructVarargsInstruction,
        snake_case: construct_varargs_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] function: Register,
            [2] new_target: Register,
            [3] args: Register,
        }
    }

    /// Call the super constructor in a default derived constructor, storing the result in `this`.
    DefaultSuperCall {
        camel_case: DefaultSuperCallInstruction,
        snake_case: default_super_call_instruction,
        can_throw: true,
        operands: {}
    }

    /// Return from a function, producing a value.
    Ret {
        camel_case: RetInstruction,
        snake_case: ret_instruction,
        operands: {
            [0] return_value: Register,
        }
    }

    /// Add two values together, storing the result in dest.
    Add {
        camel_case: AddInstruction,
        snake_case: add_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Subtract one value from another, storing the result in dest.
    Sub {
        camel_case: SubInstruction,
        snake_case: sub_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Multiply one value with another, storing the result in dest.
    Mul {
        camel_case: MulInstruction,
        snake_case: mul_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Divide one value with another, storing the result in dest.
    Div {
        camel_case: DivInstruction,
        snake_case: div_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Find the remainder from dividing the left value with the right value, storing the result in
    /// dest.
    Rem {
        camel_case: RemInstruction,
        snake_case: rem_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Raise the left value to the power of the right value, storing the result in dest.
    Exp {
        camel_case: ExpInstruction,
        snake_case: exp_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Bitwise and the two operands, storing the result in dest.
    BitAnd {
        camel_case: BitAndInstruction,
        snake_case: bit_and_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Bitwise or the two operands, storing the result in dest.
    BitOr {
        camel_case: BitOrInstruction,
        snake_case: bit_or_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Bitwise xor the two operands, storing the result in dest.
    BitXor {
        camel_case: BitXorInstruction,
        snake_case: bit_xor_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Shift the left operand left by the right operand, storing the result in dest.
    ShiftLeft {
        camel_case: ShiftLeftInstruction,
        snake_case: shift_left_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Shift the left operand right by the right operand, storing the result in dest. Extends the
    /// highest bit of the left operand, preserving the sign of the left operand.
    ShiftRightArithmetic {
        camel_case: ShiftRightArithmeticInstruction,
        snake_case: shift_right_arithmetic_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Shift the left operand right by the right operand, storing the result in dest. New bits are
    /// filled with zeroes.
    ShiftRightLogical {
        camel_case: ShiftRightLogicalInstruction,
        snake_case: shift_right_logical_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Test if the two operands are loosely equal to each other (`==`), storing the result in dest.
    LooseEqual {
        camel_case: LooseEqualInstruction,
        snake_case: loose_equal_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Test if the two operands are loosely not equal to each other (`!=`), storing the result in
    /// dest.
    LooseNotEqual {
        camel_case: LooseNotEqualInstruction,
        snake_case: loose_not_equal_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Test if the two operands are strictly equal to each other (`===`), storing the result in
    /// dest.
    StrictEqual {
        camel_case: StrictEqualInstruction,
        snake_case: strict_equal_instruction,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Test if the two operands are strictly not equal to each other (`!==`), storing the result in
    /// dest.
    StrictNotEqual {
        camel_case: StrictNotEqualInstruction,
        snake_case: strict_not_equal_instruction,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Test if the left operand is less than the right operand, storing the result in dest.
    LessThan {
        camel_case: LessThanInstruction,
        snake_case: less_than_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Test if the left operand is less than or equal to the right operand, storing the result in
    /// dest.
    LessThanOrEqual {
        camel_case: LessThanOrEqualInstruction,
        snake_case: less_than_or_equal_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Test if the left operand is greater than the right operand, storing the result in dest.
    GreaterThan {
        camel_case: GreaterThanInstruction,
        snake_case: greater_than_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Test if the left operand is greater than or equal to the right operand, storing the result
    /// in dest.
    GreaterThanOrEqual {
        camel_case: GreaterThanOrEqualInstruction,
        snake_case: greater_than_or_equal_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] left: Register,
            [2] right: Register,
        }
    }

    /// Negate a value with number conversion `-ToNumber(value)`, storing the result in dest.
    Neg {
        camel_case: NegInstruction,
        snake_case: neg_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] value: Register,
        }
    }

    /// Increment a register, storing the result in that same register. Assumes the value is numeric
    /// and does not perform conversion, so caller must ensure that the value is a number or BigInt.
    Inc {
        camel_case: IncInstruction,
        snake_case: inc_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Decrement a register, storing the result in that same register. Assumes the value is numeric
    /// and does not perform conversion, so caller must ensure that the value is a number or BigInt.
    Dec {
        camel_case: DecInstruction,
        snake_case: dec_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Logical not of a value with boolean conversion `!ToBoolean(value)`, storing the result in
    /// dest.
    LogNot {
        camel_case: LogNotInstruction,
        snake_case: log_not_instruction,
        operands: {
            [0] dest: Register,
            [1] value: Register,
        }
    }

    /// Bitwise not of a value with integer conversion `~ToNumeric(value)`, storing the result in
    /// dest.
    BitNot {
        camel_case: BitNotInstruction,
        snake_case: bit_not_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] value: Register,
        }
    }

    /// Find the type of a value as a string, storing the result in dest.
    TypeOf {
        camel_case: TypeOfInstruction,
        snake_case: type_of_instruction,
        operands: {
            [0] dest: Register,
            [1] value: Register,
        }
    }

    /// Evaluate an `in` expression, storing the result in dest.
    In {
        camel_case: InInstruction,
        snake_case: in_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] object: Register,
            [2] key: Register,
        }
    }

    /// Evaluate an `instanceof` expression, storing the result in dest.
    InstanceOf {
        camel_case: InstanceOfInstruction,
        snake_case: instance_of_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] object: Register,
            [2] constructor: Register,
        }
    }

    /// Apply the ToNumber abstract operation to a value, storing the result in dest.
    ToNumber {
        camel_case: ToNumberInstruction,
        snake_case: to_number_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] value: Register,
        }
    }

    /// Apply the ToNumber abstract operation to a value, storing the result in dest.
    ToNumeric {
        camel_case: ToNumericInstruction,
        snake_case: to_numeric_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] value: Register,
        }
    }

    /// Apply the ToString abstract operation to a value, storing the result in dest.
    ToString {
        camel_case: ToStringInstruction,
        snake_case: to_string_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] value: Register,
        }
    }

    /// Apply the ToPropertyKey abstract operation to a value, storing the result in dest.
    ToPropertyKey {
        camel_case: ToPropertyKeyInstruction,
        snake_case: to_property_key_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] value: Register,
        }
    }

    /// Apply the ToObject abstract operation to a value, storing the result in dest.
    ToObject {
        camel_case: ToObjectInstruction,
        snake_case: to_object_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] value: Register,
        }
    }

    /// Unconditionally jump to the given instruction, specified inline as a byte offset from the
    /// start of the current instruction.
    Jump {
        camel_case: JumpInstruction,
        snake_case: jump_instruction,
        operands: {
            [0] offset: SInt,
        }
    }

    /// Unconditionally jump to the given instruction, specified as an index into the constant table
    /// which holds the byte offset from the start of the current instruction.
    JumpConstant {
        camel_case: JumpConstantInstruction,
        snake_case: jump_constant_instruction,
        operands: {
            [0] constant_index: ConstantIndex,
        }
    }

    /// Conditionally jump to the given instruction if the condition is true, using an inline
    /// offset. Does not convert its operand to a boolean first, only checks for `false ``exactly.
    JumpTrue {
        camel_case: JumpTrueInstruction,
        snake_case: jump_true_instruction,
        operands: {
            [0] condition: Register,
            [1] offset: SInt,
        }
    }

    /// Conditionally jump to the given instruction if the condition is true, with offset stored
    /// in the constant table. Does not convert its operand to a boolean first, only checks for
    /// `false` exactly.
    JumpTrueConstant {
        camel_case: JumpTrueConstantInstruction,
        snake_case: jump_true_constant_instruction,
        operands: {
            [0] condition: Register,
            [1] constant_index: ConstantIndex,
        }
    }

    /// Conditionally jump to the given instruction if ToBoolean(condition) is true, using an
    /// inline ofset.
    JumpToBooleanTrue {
        camel_case: JumpToBooleanTrueInstruction,
        snake_case: jump_to_boolean_true_instruction,
        operands: {
            [0] condition: Register,
            [1] offset: SInt,
        }
    }

    /// Conditionally jump to the given instruction if ToBoolean(condition) is true, with offset
    /// stored in the constant table.
    JumpToBooleanTrueConstant {
        camel_case: JumpToBooleanTrueConstantInstruction,
        snake_case: jump_to_boolean_true_constant_instruction,
        operands: {
            [0] condition: Register,
            [1] constant_index: ConstantIndex,
        }
    }

    /// Conditionally jump to the given instruction if the condition is false, using an inline
    /// offset. Does not convert its operand to a boolean first, only checks for `false ``exactly.
    JumpFalse {
        camel_case: JumpFalseInstruction,
        snake_case: jump_false_instruction,
        operands: {
            [0] condition: Register,
            [1] offset: SInt,
        }
    }

    /// Conditionally jump to the given instruction if the condition is false, with offset stored
    /// in the constant table. Does not convert its operand to a boolean first, only checks for
    /// `false` exactly.
    JumpFalseConstant {
        camel_case: JumpFalseConstantInstruction,
        snake_case: jump_false_constant_instruction,
        operands: {
            [0] condition: Register,
            [1] constant_index: ConstantIndex,
        }
    }

    /// Conditionally jump to the given instruction if ToBoolean(condition) is false, using an
    /// inline ofset.
    JumpToBooleanFalse {
        camel_case: JumpToBooleanFalseInstruction,
        snake_case: jump_to_boolean_false_instruction,
        operands: {
            [0] condition: Register,
            [1] offset: SInt,
        }
    }

    /// Conditionally jump to the given instruction if ToBoolean(condition) is false, with offset
    /// stored in the constant table.
    JumpToBooleanFalseConstant {
        camel_case: JumpToBooleanFalseConstantInstruction,
        snake_case: jump_to_boolean_false_constant_instruction,
        operands: {
            [0] condition: Register,
            [1] constant_index: ConstantIndex,
        }
    }

    /// Conditionally jump to the given instruction if the condition is not undefined, using an
    /// inline offset.
    JumpNotUndefined {
        camel_case: JumpNotUndefinedInstruction,
        snake_case: jump_not_undefined_instruction,
        operands: {
            [0] condition: Register,
            [1] offset: SInt,
        }
    }

    /// Conditionally jump to the given instruction if the condition is not undefined, with offset
    /// stored in the constant table.
    JumpNotUndefinedConstant {
        camel_case: JumpNotUndefinedConstantInstruction,
        snake_case: jump_not_undefined_constant_instruction,
        operands: {
            [0] condition: Register,
            [1] constant_index: ConstantIndex,
        }
    }

    /// Conditionally jump to the given instruction if the condition is nullish, using an inline
    /// offset.
    JumpNullish {
        camel_case: JumpNullishInstruction,
        snake_case: jump_nullish_instruction,
        operands: {
            [0] condition: Register,
            [1] offset: SInt,
        }
    }

    /// Conditionally jump to the given instruction if the condition is nullish, with offset stored
    /// in the constant table.
    JumpNullishConstant {
        camel_case: JumpNullishConstantInstruction,
        snake_case: jump_nullish_constant_instruction,
        operands: {
            [0] condition: Register,
            [1] constant_index: ConstantIndex,
        }
    }

    /// Conditionally jump to the given instruction if the condition is not nullish, using an inline
    /// offset.
    JumpNotNullish {
        camel_case: JumpNotNullishInstruction,
        snake_case: jump_not_nullish_instruction,
        operands: {
            [0] condition: Register,
            [1] offset: SInt,
        }
    }

    /// Conditionally jump to the given instruction if the condition is not nullish, with offset
    /// stored in the constant table.
    JumpNotNullishConstant {
        camel_case: JumpNotNullishConstantInstruction,
        snake_case: jump_not_nullish_constant_instruction,
        operands: {
            [0] condition: Register,
            [1] constant_index: ConstantIndex,
        }
    }

    /// Create a new closure from the function at the given index in the constant table.
    NewClosure {
        camel_case: NewClosureInstruction,
        snake_case: new_closure_instruction,
        operands: {
            [0] dest: Register,
            [1] function_index: ConstantIndex,
        }
    }

    /// Create a new closure from the async function at the given index in the constant table.
    NewAsyncClosure {
        camel_case: NewAsyncClosureInstruction,
        snake_case: new_async_closure_instruction,
        operands: {
            [0] dest: Register,
            [1] function_index: ConstantIndex,
        }
    }

    /// Create a new generator closure from the function at the given index in the constant table.
    NewGenerator {
        camel_case: NewGeneratorInstruction,
        snake_case: new_generator_instruction,
        operands: {
            [0] dest: Register,
            [1] function_index: ConstantIndex,
        }
    }

    /// Create a new async generator closure from the function at the given index in the constant
    /// table.
    NewAsyncGenerator {
        camel_case: NewAsyncGeneratorInstruction,
        snake_case: new_async_generator_instruction,
        operands: {
            [0] dest: Register,
            [1] function_index: ConstantIndex,
        }
    }

    /// Create a new empty object stored in dest.
    NewObject {
        camel_case: NewObjectInstruction,
        snake_case: new_object_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Create a new empty array stored in dest.
    NewArray {
        camel_case: NewArrayInstruction,
        snake_case: new_array_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Create a new RegExp object from the compiled RegExp at the given index in the constant
    /// table.
    NewRegExp {
        camel_case: NewRegExpInstruction,
        snake_case: new_regexp_instruction,
        operands: {
            [0] dest: Register,
            [1] regexp_index: ConstantIndex,
        }
    }

    /// Create a new mapped arguments object for the current function, and store in dest.
    NewMappedArguments {
        camel_case: NewMappedArgumentsInstruction,
        snake_case: new_mapped_arguments_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Create a new unmapped arguments object for the current function, and store in dest.
    ///
    /// Copies the current function's arguments (up to argc) to the new arguments object.
    NewUnmappedArguments {
        camel_case: NewUnmappedArgumentsInstruction,
        snake_case: new_unmapped_arguments_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Create a new class given the constructor, methods, and static class names object. The
    /// resulting constructor is stored in dest.
    ///
    /// - The static class names object is at the given index in the constant table.
    /// - The constructor's BytecodeFunction is at the given index in the constant table.
    /// - The super class is passed in a register. Empty is passed if there is no super class.
    /// - The class's methods (and computed property keys) are passed in a contiguous sequence of
    ///   registers starting at `methods`. The length of the sequence is in the class names object.
    NewClass {
        camel_case: NewClassInstruction,
        snake_case: new_class_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] class_names_index: ConstantIndex,
            [2] constructor_function_index: ConstantIndex,
            [3] super_class: Register,
            [4] methods: Register,
        }
    }

    /// Create a new accessor value from the getter and setter functions. The result is stored in
    /// dest. Getter and setter operands must both be closures.
    NewAccessor {
        camel_case: NewAccessorInstruction,
        snake_case: new_accessor_instruction,
        operands: {
            [0] dest: Register,
            [1] getter: Register,
            [2] setter: Register,
        }
    }

    /// Create a new private symbol and store in dest. The private symbol has the name stored at the
    /// given index in the constant table. Name is not prefixed with "#".
    NewPrivateSymbol {
        camel_case: NewPrivateSymbolInstruction,
        snake_case: new_private_symbol_instruction,
        operands: {
            [0] dest: Register,
            [1] name_index: ConstantIndex,
        }
    }

    /// Get a property from an object, storing the result in dest. The property key may be any
    /// value.
    GetProperty {
        camel_case: GetPropertyInstruction,
        snake_case: get_property_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] object: Register,
            [2] key: Register,
        }
    }

    /// Set a property on an object to the given value. The property key may be any value.
    SetProperty {
        camel_case: SetPropertyInstruction,
        snake_case: set_property_instruction,
        operands: {
            [0] object: Register,
            [1] key: Register,
            [2] value: Register,
        }
    }

    /// Define a property on an object with the given value. The property key may be any value.
    DefineProperty {
        camel_case: DefinePropertyInstruction,
        snake_case: define_property_instruction,
        operands: {
            [0] object: Register,
            [1] key: Register,
            [2] value: Register,
            [3] flags: UInt,
        }
    }

    /// Get a named property from an object, storing the result in dest. The name must be a string
    /// literal in the constant table.
    GetNamedProperty {
        camel_case: GetNamedPropertyInstruction,
        snake_case: get_named_property_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] object: Register,
            [2] name_constant_index: ConstantIndex,
        }
    }

    /// Set a named property on an object to the given value. The name must be a string literal in
    /// the constant table.
    SetNamedProperty {
        camel_case: SetNamedPropertyInstruction,
        snake_case: set_named_property_instruction,
        operands: {
            [0] object: Register,
            [1] name_constant_index: ConstantIndex,
            [2] value: Register,
        }
    }

    /// Define a named property on an object with the given value. The name must be a string literal
    /// in the constant table.
    DefineNamedProperty {
        camel_case: DefineNamedPropertyInstruction,
        snake_case: define_named_property_instruction,
        operands: {
            [0] object: Register,
            [1] name_constant_index: ConstantIndex,
            [2] value: Register,
        }
    }

    /// Get a super property from an object, storing the result in dest. The property key may be any
    /// value. Takes both the home object and receiver of the enclosing method.
    GetSuperProperty {
        camel_case: GetSuperPropertyInstruction,
        snake_case: get_super_property_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] home_object: Register,
            [2] receiver: Register,
            [3] key: Register,
        }
    }

    /// Get a named super property, storing the result in dest. The name must be a string literal
    /// in the constant table. Takes both the home object and receiver of the enclosing method.
    GetNamedSuperProperty {
        camel_case: GetNamedSuperPropertyInstruction,
        snake_case: get_named_super_property_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] home_object: Register,
            [2] receiver: Register,
            [3] name_constant_index: ConstantIndex,
        }
    }

    /// Set a super property on an object to the given value. The property key may be any value.
    /// Takes both the home object and receiver of the enclosing method.
    SetSuperProperty {
        camel_case: SetSuperPropertyInstruction,
        snake_case: set_super_property_instruction,
        operands: {
            [0] home_object: Register,
            [1] receiver: Register,
            [2] key: Register,
            [3] value: Register,
        }
    }

    /// Delete the property with the given key from an object. The property key may be any value
    /// and will be converted to a property key with ToPropertyKey.
    DeleteProperty {
        camel_case: DeletePropertyInstruction,
        snake_case: delete_property_instruction,
        operands: {
            [0] dest: Register,
            [1] object: Register,
            [2] key: Register,
        }
    }

    /// The `delete` operator when applied to an identifier. The name of the binding is stored in
    /// the constant table, and the result according to the `delete` operator is placed in dest.
    DeleteBinding {
        camel_case: DeleteBindingInstruction,
        snake_case: delete_binding_instruction,
        operands: {
            [0] dest: Register,
            [1] name_constant_index: ConstantIndex,
        }
    }

    /// Get a private property from an object, storing the result in dest. The key must be a private
    /// symbol.
    GetPrivateProperty {
        camel_case: GetPrivatePropertyInstruction,
        snake_case: get_private_property_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] object: Register,
            [2] key: Register,
        }
    }

    /// Set a private property on an object to the given value. The key must be a private symbol.
    SetPrivateProperty {
        camel_case: SetPrivatePropertyInstruction,
        snake_case: set_private_property_instruction,
        operands: {
            [0] object: Register,
            [1] key: Register,
            [2] value: Register,
        }
    }

    /// Define a private property on an object with the given value. The key must be a private
    /// symbol.
    DefinePrivateProperty {
        camel_case: DefinePrivatePropertyInstruction,
        snake_case: define_private_property_instruction,
        operands: {
            [0] object: Register,
            [1] key: Register,
            [2] value: Register,
            [3] flags: UInt,
        }
    }

    /// Set a property on an array literal to the given value.
    SetArrayProperty {
        camel_case: SetArrayPropertyInstruction,
        snake_case: set_array_property_instruction,
        operands: {
            [0] array: Register,
            [1] index: Register,
            [2] value: Register,
        }
    }

    /// Set the prototype of an object to the given value. Assumes the object value is an object.
    /// Does nothing if the new prototype is not an object or null.
    SetPrototypeOf {
        camel_case: SetPrototypeOfInstruction,
        snake_case: set_prototype_of_instruction,
        operands: {
            [0] object: Register,
            [1] prototype: Register,
        }
    }

    /// Copy the data properties from a source object to a destination object. Destination must be
    /// an object, but source may be any value.
    ///
    /// Property keys to exclude are passed in a contiguous sequence of registers starting at
    /// `argv`, of length `argc`. Caller must ensure that all values are property keys.
    CopyDataProperties {
        camel_case: CopyDataPropertiesInstruction,
        snake_case: copy_data_properties,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] source: Register,
            [2] argv: Register,
            [3] argc: UInt,
        }
    }

    /// Lookup a method with the given name on a value, storing the result in dest. If there is no
    /// property with the given name the result is undefined. If there is a property but it is not
    /// nullish and not callable then error.
    GetMethod {
        camel_case: GetMethodInstruction,
        snake_case: get_method_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] object: Register,
            [2] name: ConstantIndex,
        }
    }

    /// Create a new lexical scope and push it to the stack, becoming the current scope. The old
    /// current scope becomes the parent scope.
    ///
    /// The scope names to use are at the given index in the constant table.
    PushLexicalScope {
        camel_case: PushLexicalScopeInstruction,
        snake_case: push_lexical_scope_instruction,
        operands: {
            [0] scope_names_index: ConstantIndex,
        }
    }

    /// Create a new function scope and push it to the stack, becoming the current scope. The old
    /// current scope becomes the parent scope.
    ///
    /// A function scope is treated as a var scope when looking up the containing var scope.
    ///
    /// The scope names to use are at the given index in the constant table.
    PushFunctionScope {
        camel_case: PushFunctionScopeInstruction,
        snake_case: push_function_scope_instruction,
        operands: {
            [0] scope_names_index: ConstantIndex,
        }
    }

    /// Create a new with scope for the given object and push it to the stack, becoming the current
    /// scope. The old current scope becomes the parent scope.
    ///
    /// Any value may be passed as the object. It will be converted to an object with ToObject.
    ///
    /// The scope names to use are at the given index in the constant table.
    PushWithScope {
        camel_case: PushWithScopeInstruction,
        snake_case: push_with_scope_instruction,
        can_throw: true,
        operands: {
            [0] object: Register,
            [1] scope_names_index: ConstantIndex,
        }
    }

    /// Pop a scope off the stack, replacing it with its parent scope.
    PopScope {
        camel_case: PopScopeInstruction,
        snake_case: pop_scope_instruction,
        operands: {}
    }

    /// Duplicate the current scope and replace the current scope with the duplicate.
    DupScope {
        camel_case: DupScopeInstruction,
        snake_case: dup_scope_instruction,
        operands: {}
    }

    /// Load the value at `scope_index` from a scope and store in dest. The scope is found at
    /// `parent_depth` levels up the scope chain. A `parent_depth` of 0 refers to the current scope.
    LoadFromScope {
        camel_case: LoadFromScopeInstruction,
        snake_case: load_from_scope_instruction,
        operands: {
            [0] dest: Register,
            [1] scope_index: UInt,
            [2] parent_depth: UInt,
        }
    }

    /// Store the `value` into a scope at `scope_index`. The scope is found at `parent_depth`
    /// levels up the scope chain. A `parent_depth` of 0 refers to the current scope.
    StoreToScope {
        camel_case: StoreToScopeInstruction,
        snake_case: store_to_scope_instruction,
        operands: {
            [0] value: Register,
            [1] scope_index: UInt,
            [2] parent_depth: UInt,
        }
    }

    /// Load the value at `scope_index` from a module scope and store in dest. The scope is found at
    /// `parent_depth` levels up the scope chain. A `parent_depth` of 0 refers to the current scope.
    LoadFromModule {
        camel_case: LoadFromModuleInstruction,
        snake_case: load_from_module_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] scope_index: UInt,
            [2] parent_depth: UInt,
        }
    }

    /// Store the `value` into a module scope at `scope_index`. The scope is found at `parent_depth`
    /// levels up the scope chain. A `parent_depth` of 0 refers to the current scope.
    StoreToModule {
        camel_case: StoreToModuleInstruction,
        snake_case: store_to_module_instruction,
        operands: {
            [0] value: Register,
            [1] scope_index: UInt,
            [2] parent_depth: UInt,
        }
    }

    /// Throw an error.
    Throw {
        camel_case: ThrowInstruction,
        snake_case: throw_instruction,
        operands: {
            [0] error: Register,
        }
    }

    /// Create a rest parameter for the current function and store in dest.
    RestParameter {
        camel_case: RestParameterInstruction,
        snake_case: rest_parameter_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Get the super constructor of a derived constructor, storing the result in dest.
    ///
    /// Errors if the super constructor is not a constructor.
    GetSuperConstructor {
        camel_case: GetSuperConstructorInstruction,
        snake_case: get_super_constructor_instruction,
        operands: {
            [0] dest: Register,
            [1] derived_constructor: Register,
        }
    }

    /// Check if a binding is being accessed in the Temporal Dead Zone (TDZ), meaning the time
    /// where it is in scope but not yet initialized. If so, throw a ReferenceError.
    ///
    /// A value in the TDZ is represented by the empty value. A constant index for the name of the
    /// binding is provided as the second operand.
    CheckTdz {
        camel_case: CheckTdzInstruction,
        snake_case: check_tdz_instruction,
        can_throw: true,
        operands: {
            [0] value: Register,
            [1] name_constant_index: ConstantIndex,
        }
    }

    /// Check whether a `this` value is initialized, throwing a ReferenceError if it is not.
    CheckThisInitialized {
        camel_case: CheckThisInitializedInstruction,
        snake_case: check_this_initialized_instruction,
        operands: {
            [0] value: Register,
        }
    }

    /// Check whether a `this` value is initialized, meaning super() has already been called.
    CheckSuperAlreadyCalled {
        camel_case: CheckSuperAlreadyCalledInstruction,
        snake_case: check_super_already_called_instruction,
        can_throw: true,
        operands: {
            [0] value: Register,
        }
    }

    /// Check whether a value is valid as the return value of an iterator, throwing an error if not.
    CheckIteratorResultObject {
        camel_case: CheckIteratorResultObjectInstruction,
        snake_case: check_iterator_result_object_instruction,
        can_throw: true,
        operands: {
            [0] value: Register,
        }
    }

    /// Throw a TypeError for attempting to assign an already initialized const binding.
    ErrorConst {
        camel_case: ErrorConstInstruction,
        snake_case: error_const_instruction,
        can_throw: true,
        operands: {
            [0] name_constant_index: ConstantIndex,
        }
    }

    /// Throw a ReferenceError for attempting to delete a super property.
    ErrorDeleteSuperProperty {
        camel_case: ErrorDeleteSuperPropertyInstruction,
        snake_case: error_delete_super_property_instruction,
        can_throw: true,
        operands: {}
    }

    /// Throw a TypeError for a throw completion in a yield* where the iterator does not have a
    /// throw method.
    ErrorIteratorNoThrowMethod {
        camel_case: ErrorIteratorNoThrowMethodInstruction,
        snake_case: error_iterator_no_throw_method_instruction,
        can_throw: true,
        operands: {}
    }

    /// Create a new for-in iterator for the given object, storing in dest. Gathers all the iterable
    /// keys of the object and its prototype chain, storing them in the iterator. Expects that the
    /// object value is not nullish, all other values will be coerced with ToObject.
    NewForInIterator {
        camel_case: NewForInIteratorInstruction,
        snake_case: new_for_in_iterator_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] object: Register,
        }
    }

    /// Call the `next` method on a for-in iterator, storing the result in dest. Returns either the
    /// next string key or undefined if there are no more keys.
    ForInNext {
        camel_case: ForInNextInstruction,
        snake_case: for_in_next_instruction,
        can_throw: true,
        operands: {
            [0] dest: Register,
            [1] iterator: Register,
        }
    }

    /// Get the iterator for a given iterable, storing the iterator and its next method in
    /// registers.
    GetIterator {
        camel_case: GetIteratorInstruction,
        snake_case: get_iterator_instruction,
        can_throw: true,
        operands: {
            [0] iterator: Register,
            [1] next_method: Register,
            [2] iterable: Register,
        }
    }

    /// Get the async iterator for a given iterable, storing the iterator and its next method in
    /// registers.
    GetAsyncIterator {
        camel_case: GetAsyncIteratorInstruction,
        snake_case: get_async_iterator_instruction,
        can_throw: true,
        operands: {
            [0] iterator: Register,
            [1] next_method: Register,
            [2] iterable: Register,
        }
    }

    /// Call the `next` method on an iterator, storing the returned value and boolean `is_done` flag
    /// in registers.
    IteratorNext {
        camel_case: IteratorNextInstruction,
        snake_case: iterator_next_instruction,
        can_throw: true,
        operands: {
            [0] value: Register,
            [1] is_done: Register,
            [2] iterator: Register,
            [3] next_method: Register,
        }
    }

    /// Unpack an iterator result object, storing the returned value and boolean `is_done` flag in
    /// registers. Errors if the iterator result is not an object.
    IteratorUnpackResult {
        camel_case: IteratorUnpackResultInstruction,
        snake_case: iterator_unpack_result_instruction,
        can_throw: true,
        operands: {
            [0] value: Register,
            [1] is_done: Register,
            [2] iterator_result: Register,
        }
    }

    /// Close an iterator, calling its `return` method if it exists.
    IteratorClose {
        camel_case: IteratorCloseInstruction,
        snake_case: iterator_close_instruction,
        can_throw: true,
        operands: {
            [0] iterator: Register,
        }
    }

    /// The portion of AsyncIteratorClose before the await. Closes an async iterator, calling its
    /// `return` method if it exists. Stores whether the return method exists, and if so also stores
    /// the return result in a register.
    AsyncIteratorCloseStart {
        camel_case: AsyncIteratorCloseStartInstruction,
        snake_case: async_iterator_close_start_instruction,
        can_throw: true,
        operands: {
            [0] return_result: Register,
            [1] has_return_method: Register,
            [2] iterator: Register,
        }
    }

    /// The portion of AsyncIteratorClose after the await, taking the intermediate value from
    /// AsyncIteratorCloseStart.
    AsyncIteratorCloseFinish {
        camel_case: AsyncIteratorCloseFinishInstruction,
        snake_case: async_iterator_close_finish_instruction,
        can_throw: true,
        operands: {
            [0] return_result: Register,
        }
    }

    /// Create a generator in the current function. Suspends the current function and returns the
    /// generator to the caller. Also stores the generator to the provided register.
    GeneratorStart {
        camel_case: GeneratorStartInstruction,
        snake_case: generator_start_instruction,
        operands: {
            [0] generator: Register,
        }
    }

    /// Yield a value from the current function, returning the value to the caller. Takes the
    /// generator for the function and the value to yield. Can take a normal or async generator.
    ///
    /// The completion passed into GeneratorResume when the generator resumes will be stored in the
    /// a pair of registers holding the completion value and completion type. The completion type
    /// is true for normal completions, undefined for returns, and null for throws.
    Yield {
        camel_case: YieldInstruction,
        snake_case: yield_instruction,
        operands: {
            [0] completion_value_dest: Register,
            [1] completion_type_dest: Register,
            [2] generator: Register,
            [3] yield_value: Register,
        }
    }

    /// Create a new promise object at the start of an async function and store it in dest.
    NewPromise {
        camel_case: NewPromiseInstruction,
        snake_case: new_promise_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Await a promise passed as an argument, suspend the current async function until the promise
    /// is resolved or rejected. When resumed place the completion value that the await evaluates to
    /// in the completion value registers.
    ///
    /// The completion type must be either true for normal completions or null for throws, no other
    /// completion types are allowed.
    ///
    /// For async functions the promise for the current function is returned to the caller.
    ///
    /// For async generators the generator is passed in place of the return promise, and empty will
    /// be returned to signal that the async generator has suspended.
    Await {
        camel_case: AwaitInstruction,
        snake_case: await_instruction,
        operands: {
            [0] completion_value_dest: Register,
            [1] completion_type_dest: Register,
            [2] return_promise_or_generator: Register,
            [3] argument_promise: Register,
        }
    }

    /// Resolve a promise with a value.
    ResolvePromise {
        camel_case: ResolvePromiseInstruction,
        snake_case: resolve_promise_instruction,
        operands: {
            [0] promise: Register,
            [1] value: Register,
        }
    }

    /// Reject a promise with a value.
    RejectPromise {
        camel_case: RejectPromiseInstruction,
        snake_case: reject_promise_instruction,
        operands: {
            [0] promise: Register,
            [1] value: Register,
        }
    }

    /// Load the `import.meta` object into dest.
    ImportMeta {
        camel_case: ImportMetaInstruction,
        snake_case: import_meta_instruction,
        operands: {
            [0] dest: Register,
        }
    }

    /// Import the module with the given specifier, storing the result of the import into dest.
    DynamicImport {
        camel_case: DynamicImportInstruction,
        snake_case: dynamic_import_instruction,
        operands: {
            [0] dest: Register,
            [1] specifier: Register,
        }
    }
);

bitflags! {
    /// Flags for the DefineProperty instruction.
    #[derive(Clone, Copy)]
    pub struct DefinePropertyFlags: u8 {
        /// Whether the value is an unnamed closure that needs its name set to the key with
        /// SetFunctionName.
        const NEEDS_NAME = 1 << 0;
        /// Whether to define a getter property.
        const GETTER = 1 << 1;
        /// Whether to define a setter property.
        const SETTER = 1 << 2;
    }
}

bitflags! {
    /// Flags for the DefinePrivateProperty instruction.
    #[derive(Clone, Copy, PartialEq)]
    pub struct DefinePrivatePropertyFlags: u8 {
        /// Whether to define a method property.
        const METHOD = 1 << 0;
        /// Whether to define a setter property.
        const GETTER = 1 << 1;
        /// Whether to define a setter property.
        const SETTER = 1 << 2;
    }
}

bitflags! {
    /// Flags for the CallMaybeEval instructions.
    #[derive(Clone, Copy)]
    pub struct EvalFlags: u8 {
        /// Whether eval is in a non-arrow function, meaning new.target expressions are allowed.
        const IN_FUNCTION = 1 << 0;
        /// Whether eval is in a method, meaning super member expressions are allowed.
        const IN_METHOD = 1 << 1;
        /// Whether eval is in a static method or class field, meaning super member expressions
        /// reference the static home object.
        const IN_STATIC = 1 << 2;
        /// Whether eval is in a derived constructor meaning super constructor calls are allowed.
        const IN_DERIVED_CONSTRUCTOR = 1 << 3;
        /// Whether eval is in a class static initializer block, meaning `await` is not allowed.
        const IN_STATIC_INITIALIZER = 1 << 4;
        /// Whether eval is in a class field initializer meaning `arguments` is not allowed.
        const IN_CLASS_FIELD_INITIALIZER = 1 << 5;
    }
}

impl OpCode {
    #[inline]
    fn from_u8(value: u8) -> OpCode {
        unsafe { std::mem::transmute(value) }
    }
}

/// Round the byte index of a wide prefix to the byte index of the prefixed opcode. The opcode
/// appears one byte before the operands, which are aligned to a wide byte index.
pub fn wide_prefix_index_to_opcode_index(prefix_index: usize) -> usize {
    (prefix_index + 1) | 0x1
}

/// Round the byte index of a wide prefix to the byte index of the prefixed opcode. The opcode
/// appears one byte before the operands, which are aligned to an extra wide byte index.
pub fn extra_wide_prefix_index_to_opcode_index(prefix_index: usize) -> usize {
    (prefix_index + 1) | 0x3
}

pub struct DecodeInfo {
    pub width: WidthEnum,
    pub opcode: OpCode,
    pub opcode_index: usize,
}

/// Return the instruction, width, and opcode start for the instruction at the given index in the
/// provided bytecode.
pub fn decode_width_and_opcode_at_index(bytecode: &[u8], index: usize) -> DecodeInfo {
    let opcode = OpCode::from_u8(bytecode[index]);

    match opcode {
        OpCode::WidePrefix => {
            let opcode_index = wide_prefix_index_to_opcode_index(index);
            let opcode = OpCode::from_u8(bytecode[opcode_index]);

            DecodeInfo { width: WidthEnum::Wide, opcode, opcode_index }
        }
        OpCode::ExtraWidePrefix => {
            let opcode_index = extra_wide_prefix_index_to_opcode_index(index);
            let opcode = OpCode::from_u8(bytecode[opcode_index]);

            DecodeInfo { width: WidthEnum::ExtraWide, opcode, opcode_index }
        }
        opcode => DecodeInfo { width: WidthEnum::Narrow, opcode, opcode_index: index },
    }
}

/// Iterator over instructions in a buffer containing bytecode. Returns both the instruction trait
/// object and the offset of the start of that instruction.
pub struct InstructionIterator<'a> {
    pos: usize,
    bytecode: &'a [u8],
}

impl<'a> InstructionIterator<'a> {
    fn new(bytecode: &'a [u8]) -> Self {
        Self { pos: 0, bytecode }
    }
}

impl<'a> Iterator for InstructionIterator<'a> {
    type Item = (&'a dyn Instruction, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.bytecode.len() {
            return None;
        }

        let (instr, opcode_index) = match OpCode::from_u8(self.bytecode[self.pos]) {
            OpCode::WidePrefix => {
                let opcode_index = wide_prefix_index_to_opcode_index(self.pos);
                let opcode = OpCode::from_u8(self.bytecode[opcode_index]);
                let instr = decode_wide_instruction(opcode, &self.bytecode[(opcode_index + 1)..]);

                (instr, opcode_index)
            }
            OpCode::ExtraWidePrefix => {
                let opcode_index = extra_wide_prefix_index_to_opcode_index(self.pos);
                let opcode = OpCode::from_u8(self.bytecode[opcode_index]);
                let instr =
                    decode_extra_wide_instruction(opcode, &self.bytecode[(opcode_index + 1)..]);

                (instr, opcode_index)
            }
            // Must be a narrow instruction
            opcode => {
                let instr = decode_narrow_instruction(opcode, &self.bytecode[(self.pos + 1)..]);
                (instr, self.pos)
            }
        };

        let start_pos = self.pos;
        self.pos = opcode_index + 1 + instr.byte_length();

        Some((instr, start_pos))
    }
}

pub fn debug_format_instructions(bytecode: &[u8], printer: &mut DebugPrinter) {
    // Initial pass to find the max instruction length and max offset in the bytecode to calculate
    // padding. Also determine the jump targets so that they can be labeled.
    let mut prev_offset = 0;
    let mut max_instr_length = 0;
    let mut offsets = vec![];
    let mut jump_targets = HashSet::new();

    for (instr, offset) in InstructionIterator::new(bytecode) {
        max_instr_length = max_instr_length.max(offset - prev_offset);
        prev_offset = offset;
        offsets.push(offset);

        if let Some(jump_offset) = get_jump_offset(instr) {
            jump_targets.insert((offset as isize + jump_offset) as usize);
        }
    }

    max_instr_length = max_instr_length.max(bytecode.len() - prev_offset);
    offsets.push(bytecode.len());

    let offset_width = prev_offset.max(1).ilog10() as usize + 1;

    // Sort jump targets so that the label index is known
    let mut jump_targets: Vec<usize> = jump_targets.into_iter().collect();
    jump_targets.sort();
    let jump_targets: HashMap<usize, usize> = jump_targets
        .into_iter()
        .enumerate()
        .map(|(i, offset)| (offset, i))
        .collect();

    // Second pass through instructions, this time actually writing them
    for (i, (instr, _)) in InstructionIterator::new(bytecode).enumerate() {
        let offset = offsets[i];
        let next_offset = offsets[i + 1];

        // Print the label on its own line if this is a jump target
        if let Some(label_index) = jump_targets.get(&offset) {
            printer.dec_indent();
            printer.write_indent();
            printer.inc_indent();

            printer.write(&format!(".L{}:\n", label_index));
        }

        // First print the padded instruction offset
        printer.write_indent();
        printer.write(&format!("{offset:>offset_width$}: "));

        if !printer.ignore_raw_bytes() {
            // Then print the raw bytes of the instruction
            for byte in &bytecode[offset..next_offset] {
                printer.write(&format!("{:02x} ", byte));
            }

            // Pad the raw bytes to the max instruction length
            printer.write(&"   ".repeat(max_instr_length - (next_offset - offset)));

            // Print separator between the raw bytes and the instruction
            printer.write("  ");
        }

        // Then print the instruction in a readable form
        printer.write(&instr.to_string());

        // If this is a jump instruction, print the target label following the jump offset
        if let Some(jump_offset) = get_jump_offset(instr) {
            let target_offset = (offset as isize + jump_offset) as usize;
            let target_label = jump_targets[&target_offset];

            printer.write(&format!(" (.L{})", target_label));
        }

        printer.write("\n");
    }
}

fn get_jump_offset(instr: &dyn Instruction) -> Option<isize> {
    let opcode = instr.opcode();

    if opcode == OpCode::Jump || opcode == OpCode::JumpConstant {
        Some(instr.get_raw_operand_signed(0))
    } else if opcode >= OpCode::JumpTrue && opcode <= OpCode::JumpNotNullishConstant {
        Some(instr.get_raw_operand_signed(1))
    } else {
        None
    }
}
