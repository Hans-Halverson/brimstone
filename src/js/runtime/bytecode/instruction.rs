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

macro_rules! define_instructions {
    ($(
        $(#[$($attrs:tt)*])* $short_name:ident ($instr_name_camel:ident, $instr_name_snake:ident) {
            $([$operand_idx:expr] $operand_name:ident: $operand_type:ident,)*
        })
    *) => {
        $(
            $(#[$($attrs)*])*
            #[repr(C)]
            pub struct $instr_name_camel<W: Width>([W::UInt; count!($($operand_name)*)]);

            impl <W: Width> $instr_name_camel<W> {
                $(
                    pub fn $operand_name(&self) -> $operand_type<W> {
                        $operand_type::<W>::from_unsigned(self.0[$operand_idx])
                    }
                )*

                const OPERAND_TYPES: &[OperandType] = &[
                    $(
                        OperandType::$operand_type,
                    )*
                ];

                const OPCODE: OpCode = OpCode::$short_name;

                const NUM_OPERANDS: usize = Self::OPERAND_TYPES.len();

                const BYTE_LENGTH: usize = Self::NUM_OPERANDS * W::NUM_BYTES;

                /// Write the instruction into the given bytecode stream.
                pub fn write(writer: &mut BytecodeWriter, $($operand_name: $operand_type<W>,)*) {
                    writer.write_width_prefix(W::ENUM);
                    writer.write_opcode(Self::OPCODE);
                    $(writer.write_operand($operand_name);)*
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
                #[allow(unused_mut)]
                pub fn $instr_name_snake(
                    &mut self,
                    $($operand_name: $operand_type<ExtraWide>,)*
                ) {
                    // Calculate the width needed to fit all operands
                    let mut width = WidthEnum::Narrow;
                    $(width = $operand_name.min_width().max(width);)*

                    // Write an instruction with that minimum width
                    match width {
                        WidthEnum::Narrow => $instr_name_camel::<Narrow>::write(
                            self,
                            $($operand_name.to_narrow(),)*
                        ),
                        WidthEnum::Wide => $instr_name_camel::<Wide>::write(
                            self,
                            $($operand_name.to_wide(),)*
                        ),
                        WidthEnum::ExtraWide => $instr_name_camel::<ExtraWide>::write(
                            self,
                            $($operand_name,)*
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
                        std::mem::transmute(instruction_stream.as_ptr())
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
                        std::mem::transmute(instruction_stream.as_ptr())
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
                        std::mem::transmute(instruction_stream.as_ptr())
                    };
                    instr
                })*
            }
        }
    };
}

define_instructions!(
    /// Prefix denoting that the next instruction is wide.
    WidePrefix (WidePrefixInstruction, wide_prefix_instruction) {}

    /// Prefix denoting that the next instruction is extra wide.
    ExtraWidePrefix (ExtraWidePrefixInstruction, extra_wide_prefix_instruction) {}

    /// Copy a value between registers.
    Mov (MovInstruction, mov_instruction) {
        [0] dest: Register,
        [1] src: Register,
    }

    /// Load an immediate into a register.
    LoadImmediate (LoadImmediateInstruction, load_immediate_instruction) {
        [0] dest: Register,
        [1] immediate: SInt,
    }

    /// Load a constant from the constant table into a register.
    LoadConstant (LoadConstantInstruction, load_constant_instruction) {
        [0] dest: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Load the value `undefined` into a register.
    LoadUndefined (LoadUndefinedInstruction, load_undefined_instruction) {
        [0] dest: Register,
    }

    /// Load the value `null` into a register.
    LoadNull (LoadNullInstruction, load_null_instruction) {
        [0] dest: Register,
    }

    /// Load the empty value into a register.
    LoadEmpty (LoadEmptyInstruction, load_empty_instruction) {
        [0] dest: Register,
    }

    /// Load the value `true` into a register.
    LoadTrue (LoadTrueInstruction, load_true_instruction) {
        [0] dest: Register,
    }

    /// Load the value `false` into a register.
    LoadFalse (LoadFalseInstruction, load_false_instruction) {
        [0] dest: Register,
    }

    /// Load a global variable into a register, erroring if the global does not exist. The global
    /// variable's name is stored in the constant table.
    LoadGlobal (LoadGlobalInstruction, load_global_instruction) {
        [0] dest: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Load a global variable into a register, loading undefined if the global does not exist. The
    /// global variable's name is stored in the constant table.
    LoadGlobalOrUnresolved (LoadGlobalOrUnresolvedInstruction, load_global_or_unresolved_instruction) {
        [0] dest: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Store a register into a global variable. The global variable's name is stored in the
    /// constant table.
    StoreGlobal (StoreGlobalInstruction, store_global_instruction) {
        [0] value: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Load a variable with the given name into dest, erroring if the name could not be resolved.
    /// Dynamically look up the variable by name in the scope chain. The name is stored in the
    /// constant table.
    LoadDynamic (LoadDynamicInstruction, load_dynamic_instruction) {
        [0] dest: Register,
        [1] name_index: ConstantIndex,
    }

    /// Load a variable with the given name into dest, loading undefiend if the name could not be
    /// resolved. Dynamically look up the variable by name in the scope chain. The name is stored in
    /// the constant table.
    LoadDynamicOrUnresolved (LoadDynamicOrUnresolvedInstruction, load_dynamic_or_unresolved_instruction) {
        [0] dest: Register,
        [1] name_index: ConstantIndex,
    }

    /// Store a value into a variable. Dynamically look up the variable by name in the scope chain.
    /// The name is stored in the constant table.
    StoreDynamic (StoreDynamicInstruction, store_dynamic_instruction) {
        [0] value: Register,
        [1] name_index: ConstantIndex,
    }

    /// Call a function. Arguments are passed in contiguous sequence of registers starting at argv,
    /// of length argc. The receiver is undefined.
    Call (CallInstruction, call_instruction) {
        [0] dest: Register,
        [1] function: Register,
        [2] argv: Register,
        [3] argc: UInt,
    }

    /// Call a function, specifying the receiver to be used. All other arguments are passed in a
    /// contiguous sequence of registers starting at argv, of length argc.
    CallWithReceiver (CallWithReceiverInstruction, call_with_receiver_instruction) {
        [0] dest: Register,
        [1] function: Register,
        [2] receiver: Register,
        [3] argv: Register,
        [4] argc: UInt,
    }

    /// Call a function which may be a direct eval. If this is a direct eval then perform the direct
    /// eval, otherwise identical to the call instruction.
    ///
    /// Arguments are passed in contiguous sequence of registers starting at argv, of length argc.
    /// The receiver is undefined.
    CallMaybeEval (CallMaybeEvalInstruction, call_maybe_eval_instruction) {
        [0] dest: Register,
        [1] function: Register,
        [2] argv: Register,
        [3] argc: UInt,
    }

    /// Call a constructor. Arguments are passed in contiguous sequence of registers starting at
    /// argv, of length argc. The new target is passed in its own register.
    Construct (ConstructInstruction, construct_instruction) {
        [0] dest: Register,
        [1] function: Register,
        [2] new_target: Register,
        [3] argv: Register,
        [4] argc: UInt,
    }

    /// Return from a function, producing a value.
    Ret (RetInstruction, ret_instruction) {
        [0] return_value: Register,
    }

    /// Add two values together, storing the result in dest.
    Add (AddInstruction, add_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Subtract one value from another, storing the result in dest.
    Sub (SubInstruction, sub_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Multiply one value with another, storing the result in dest.
    Mul (MulInstruction, mul_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Divide one value with another, storing the result in dest.
    Div (DivInstruction, div_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Find the remainder from dividing the left value with the right value, storing the result in
    /// dest.
    Rem (RemInstruction, rem_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Raise the left value to the power of the right value, storing the result in dest.
    Exp (ExpInstruction, exp_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Bitwise and the two operands, storing the result in dest.
    BitAnd (BitAndInstruction, bit_and_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Bitwise or the two operands, storing the result in dest.
    BitOr (BitOrInstruction, bit_or_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Bitwise xor the two operands, storing the result in dest.
    BitXor (BitXorInstruction, bit_xor_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Shift the left operand left by the right operand, storing the result in dest.
    ShiftLeft (ShiftLeftInstruction, shift_left_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Shift the left operand right by the right operand, storing the result in dest. Extends the
    /// highest bit of the left operand, preserving the sign of the left operand.
    ShiftRightArithmetic (ShiftRightArithmeticInstruction, shift_right_arithmetic_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Shift the left operand right by the right operand, storing the result in dest. New bits are
    /// filled with zeroes.
    ShiftRightLogical (ShiftRightLogicalInstruction, shift_right_logical_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Test if the two operands are loosely equal to each other (`==`), storing the result in dest.
    LooseEqual (LooseEqualInstruction, loose_equal_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Test if the two operands are loosely not equal to each other (`!=`), storing the result in
    /// dest.
    LooseNotEqual (LooseNotEqualInstruction, loose_not_equal_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Test if the two operands are strictly equal to each other (`===`), storing the result in
    /// dest.
    StrictEqual (StrictEqualInstruction, strict_equal_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Test if the two operands are strictly not equal to each other (`!==`), storing the result in
    /// dest.
    StrictNotEqual (StrictNotEqualInstruction, strict_not_equal_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Test if the left operand is less than the right operand, storing the result in dest.
    LessThan (LessThanInstruction, less_than_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Test if the left operand is less than or equal to the right operand, storing the result in
    /// dest.
    LessThanOrEqual (LessThanOrEqualInstruction, less_than_or_equal_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Test if the left operand is greater than the right operand, storing the result in dest.
    GreaterThan (GreaterThanInstruction, greater_than_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Test if the left operand is greater than or equal to the right operand, storing the result
    /// in dest.
    GreaterThanOrEqual (GreaterThanOrEqualInstruction, greater_than_or_equal_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
    }

    /// Negate a value with number conversion `-ToNumber(value)`, storing the result in dest.
    Neg (NegInstruction, neg_instruction) {
        [0] dest: Register,
        [1] value: Register,
    }

    /// Increment a register, storing the result in that same register. Assumes the value is numeric
    /// and does not perform conversion, so caller must ensure that the value is a number or BigInt.
    Inc (IncInstruction, inc_instruction) {
        [0] dest: Register,
    }

    /// Decrement a register, storing the result in that same register. Assumes the value is numeric
    /// and does not perform conversion, so caller must ensure that the value is a number or BigInt.
    Dec (DecInstruction, dec_instruction) {
        [0] dest: Register,
    }

    /// Logical not of a value with boolean conversion `!ToBoolean(value)`, storing the result in
    /// dest.
    LogNot (LogNotInstruction, log_not_instruction) {
        [0] dest: Register,
        [1] value: Register,
    }

    /// Bitwise not of a value with integer conversion `~ToNumeric(value)`, storing the result in
    /// dest.
    BitNot (BitNotInstruction, bit_not_instruction) {
        [0] dest: Register,
        [1] value: Register,
    }

    /// Find the type of a value as a string, storing the result in dest.
    TypeOf (TypeOfInstruction, type_of_instruction) {
        [0] dest: Register,
        [1] value: Register,
    }

    /// Evaluate an `in` expression, storing the result in dest.
    In (InInstruction, in_instruction) {
        [0] dest: Register,
        [1] object: Register,
        [2] key: Register,
    }

    /// Evaluate an `instanceof` expression, storing the result in dest.
    InstanceOf (InstanceOfInstruction, instance_of_instruction) {
        [0] dest: Register,
        [1] object: Register,
        [2] constructor: Register,
    }

    /// Apply the ToNumber abstract operation to a value, storing the result in dest.
    ToNumber (ToNumberInstruction, to_number_instruction) {
        [0] dest: Register,
        [1] value: Register,
    }

    /// Apply the ToNumber abstract operation to a value, storing the result in dest.
    ToNumeric (ToNumericInstruction, to_numeric_instruction) {
        [0] dest: Register,
        [1] value: Register,
    }

    /// Apply the ToString abstract operation to a value, storing the result in dest.
    ToString (ToStringInstruction, to_string_instruction) {
        [0] dest: Register,
        [1] value: Register,
    }

    /// Apply the ToPropertyKey abstract operation to a value, storing the result in dest.
    ToPropertyKey (ToPropertyKeyInstruction, to_property_key_instruction) {
        [0] dest: Register,
        [1] value: Register,
    }

    /// Unconditionally jump to the given instruction, specified inline as a byte offset from the
    /// start of the current instruction.
    Jump (JumpInstruction, jump_instruction) {
        [0] offset: SInt,
    }

    /// Unconditionally jump to the given instruction, specified as an index into the constant table
    /// which holds the byte offset from the start of the current instruction.
    JumpConstant (JumpConstantInstruction, jump_constant_instruction) {
        [0] constant_index: ConstantIndex,
    }

    /// Conditionally jump to the given instruction if the condition is true, using an inline
    /// offset. Does not convert its operand to a boolean first, only checks for `false ``exactly.
    JumpTrue (JumpTrueInstruction, jump_true_instruction) {
        [0] condition: Register,
        [1] offset: SInt,
    }

    /// Conditionally jump to the given instruction if the condition is true, with offset stored
    /// in the constant table. Does not convert its operand to a boolean first, only checks for
    /// `false` exactly.
    JumpTrueConstant (JumpTrueConstantInstruction, jump_true_constant_instruction) {
        [0] condition: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Conditionally jump to the given instruction if ToBoolean(condition) is true, using an
    /// inline ofset.
    JumpToBooleanTrue (JumpToBooleanTrueInstruction, jump_to_boolean_true_instruction) {
        [0] condition: Register,
        [1] offset: SInt,
    }

    /// Conditionally jump to the given instruction if ToBoolean(condition) is true, with offset
    /// stored in the constant table.
    JumpToBooleanTrueConstant (JumpToBooleanTrueConstantInstruction, jump_to_boolean_true_constant_instruction) {
        [0] condition: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Conditionally jump to the given instruction if the condition is false, using an inline
    /// offset. Does not convert its operand to a boolean first, only checks for `false ``exactly.
    JumpFalse (JumpFalseInstruction, jump_false_instruction) {
        [0] condition: Register,
        [1] offset: SInt,
    }

    /// Conditionally jump to the given instruction if the condition is false, with offset stored
    /// in the constant table. Does not convert its operand to a boolean first, only checks for
    /// `false` exactly.
    JumpFalseConstant (JumpFalseConstantInstruction, jump_false_constant_instruction) {
        [0] condition: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Conditionally jump to the given instruction if ToBoolean(condition) is false, using an
    /// inline ofset.
    JumpToBooleanFalse (JumpToBooleanFalseInstruction, jump_to_boolean_false_instruction) {
        [0] condition: Register,
        [1] offset: SInt,
    }

    /// Conditionally jump to the given instruction if ToBoolean(condition) is false, with offset
    /// stored in the constant table.
    JumpToBooleanFalseConstant (JumpToBooleanFalseConstantInstruction, jump_to_boolean_false_constant_instruction) {
        [0] condition: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Conditionally jump to the given instruction if the condition is not undefined, using an
    /// inlin offset.
    JumpNotUndefined (JumpNotUndefinedInstruction, jump_not_undefined_instruction) {
        [0] condition: Register,
        [1] offset: SInt,
    }

    /// Conditionally jump to the given instruction if the condition is not undefined, with offset
    /// stored in the constant table.
    JumpNotUndefinedConstant (JumpNotUndefinedConstantInstruction, jump_not_undefined_constant_instruction) {
        [0] condition: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Conditionally jump to the given instruction if the condition is nullish, using an inline
    /// offset.
    JumpNullish (JumpNullishInstruction, jump_nullish_instruction) {
        [0] condition: Register,
        [1] offset: SInt,
    }

    /// Conditionally jump to the given instruction if the condition is nullish, with offset stored
    /// in the constant table.
    JumpNullishConstant (JumpNullishConstantInstruction, jump_nullish_constant_instruction) {
        [0] condition: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Conditionally jump to the given instruction if the condition is not nullish, using an inline
    /// offset.
    JumpNotNullish (JumpNotNullishInstruction, jump_not_nullish_instruction) {
        [0] condition: Register,
        [1] offset: SInt,
    }

    /// Conditionally jump to the given instruction if the condition is not nullish, with offset
    /// stored in the constant table.
    JumpNotNullishConstant (JumpNotNullishConstantInstruction, jump_not_nullish_constant_instruction) {
        [0] condition: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Create a new closure from the function at the given index in the constant table.
    NewClosure (NewClosureInstruction, new_closure_instruction) {
        [0] dest: Register,
        [1] function_index: ConstantIndex,
    }

    /// Create a new empty object stored in dest.
    NewObject (NewObjectInstruction, new_object_instruction) {
        [0] dest: Register,
    }

    /// Create a new empty array stored in dest.
    NewArray (NewArrayInstruction, new_array_instruction) {
        [0] dest: Register,
    }

    /// Create a new RegExp object from the compiled RegExp at the given index in the constant
    /// table.
    NewRegExp (NewRegExpInstruction, new_regexp_instruction) {
        [0] dest: Register,
        [1] regexp_index: ConstantIndex,
    }

    /// Create a new mapped arguments object for the current function, and store in dest.
    NewMappedArguments (NewMappedArgumentsInstruction, new_mapped_arguments_instruction) {
        [0] dest: Register,
    }

    /// Create a new unmapped arguments object for the current function, and store in dest.
    ///
    /// Copies the current function's arguments (up to argc) to the new arguments object.
    NewUnmappedArguments (NewUnmappedArgumentsInstruction, new_unmapped_arguments_instruction) {
        [0] dest: Register,
    }

    /// Get a property from an object, storing the result in dest. The property key may be any
    /// value.
    GetProperty (GetPropertyInstruction, get_property_instruction) {
        [0] dest: Register,
        [1] object: Register,
        [2] key: Register,
    }

    /// Set a property on an object to the given value. The property key may be any value.
    SetProperty (SetPropertyInstruction, set_property_instruction) {
        [0] object: Register,
        [1] key: Register,
        [2] value: Register,
    }

    /// Define a property on an object with the given value. The property key may be any value.
    ///
    /// The `is_named` operand is a boolean flag (0 or 1) specifying whether the value is an unnamed
    /// closure that needs its name set to the key with SetFunctionName.
    DefineProperty (DefinePropertyInstruction, define_property_instruction) {
        [0] object: Register,
        [1] key: Register,
        [2] value: Register,
        [3] flags: UInt,
    }

    /// Get a named property from an object, storing the result in dest. The name must be a string
    /// literal in the constant table.
    GetNamedProperty (GetNamedPropertyInstruction, get_named_property_instruction) {
        [0] dest: Register,
        [1] object: Register,
        [2] name_constant_index: ConstantIndex,
    }

    /// Set a named property on an object to the given value. The name must be a string literal in
    /// the constant table.
    SetNamedProperty (SetNamedPropertyInstruction, set_named_property_instruction) {
        [0] object: Register,
        [1] name_constant_index: ConstantIndex,
        [2] value: Register,
    }

    /// Define a named property on an object with the given value. The name must be a string literal
    /// in the constant table.
    DefineNamedProperty (DefineNamedPropertyInstruction, define_named_property_instruction) {
        [0] object: Register,
        [1] name_constant_index: ConstantIndex,
        [2] value: Register,
    }

    /// Delete the property with the given key from an object. The property key may be any value
    /// and will be converted to a property key with ToPropertyKey.
    DeleteProperty (DeletePropertyInstruction, delete_property_instruction) {
        [0] dest: Register,
        [1] object: Register,
        [2] key: Register,
    }

    /// Set a property on an array literal to the given value.
    SetArrayProperty (SetArrayPropertyInstruction, set_array_property_instruction) {
        [0] array: Register,
        [1] index: Register,
        [2] value: Register,
    }

    /// Set the prototype of an object to the given value. Assumes the object value is an object.
    /// Does nothing if the new prototype is not an object or null.
    SetPrototypeOf (SetPrototypeOfInstruction, set_prototype_of_instruction) {
        [0] object: Register,
        [1] prototype: Register,
    }

    /// Copy the data properties from a source object to a destination object. Destination must be
    /// an object, but source may be any value.
    ///
    /// Property keys to exclude are passed in a contiguous sequence of registers starting at
    /// `argv`, of length `argc`. Caller must ensure that all values are property keys.
    CopyDataProperties (CopyDataPropertiesInstruction, copy_data_properties) {
        [0] dest: Register,
        [1] source: Register,
        [2] argv: Register,
        [3] argc: UInt,
    }

    /// Create a new lexical scope and push it to the stack, becoming the current scope. The old
    /// current scope becomes the parent scope.
    ///
    /// The scope names to use are at the given index in the constant table.
    PushLexicalScope (PushLexicalScopeInstruction, push_lexical_scope_instruction) {
        [0] scope_names_index: ConstantIndex,
    }

    /// Create a new function scope and push it to the stack, becoming the current scope. The old
    /// current scope becomes the parent scope.
    ///
    /// A function scope is treated as a var scope when looking up the containing var scope.
    ///
    /// The scope names to use are at the given index in the constant table.
    PushFunctionScope (PushFunctionScopeInstruction, push_function_scope_instruction) {
        [0] scope_names_index: ConstantIndex,
    }

    /// Create a new with scope for the given object and push it to the stack, becoming the current
    /// scope. The old current scope becomes the parent scope.
    ///
    /// Any value may be passed as the object. It will be converted to an object with ToObject.
    ///
    /// The scope names to use are at the given index in the constant table.
    PushWithScope (PushWithScopeInstruction, push_with_scope_instruction) {
        [0] object: Register,
        [1] scope_names_index: ConstantIndex,
    }

    /// Pop a scope off the stack, replacing it with its parent scope.
    PopScope (PopScopeInstruction, pop_scope_instruction) {}

    /// Duplicate the current scope and replace the current scope with the duplicate.
    DupScope (DupScopeInstruction, dup_scope_instruction) {}

    /// Load the value at `scope_index` from a scope and store in dest. The scope is found at
    /// `parent_depth` levels up the scope chain. A `parent_depth` of 0 refers to the current scope.
    LoadFromScope (LoadFromScopeInstruction, load_from_scope_instruction) {
        [0] dest: Register,
        [1] scope_index: UInt,
        [2] parent_depth: UInt,
    }

    /// Store the `value` into a scope at `scope_index`. The scope is found at `parent_depth`
    /// levels up the scope chain. A `parent_depth` of 0 refers to the current scope.
    StoreToScope (StoreToScopeInstruction, store_to_scope_instruction) {
        [0] value: Register,
        [1] scope_index: UInt,
        [2] parent_depth: UInt,
    }

    /// Throw an error.
    Throw(ThrowInstruction, throw_instruction) {
        [0] error: Register,
    }

    /// Create a rest parameter for the current function and store in dest.
    RestParameter(RestParameterInstruction, rest_parameter_instruction) {
        [0] dest: Register,
    }

    /// Check if a binding is being accessed in the Temporal Dead Zone (TDZ), meaning the time
    /// where it is in scope but not yet initialized. If so, throw a ReferenceError.
    ///
    /// A value in the TDZ is represented by the empty value. A constant index for the name of the
    /// binding is provided as the second operand.
    CheckTdz(CheckTdzInstruction, check_tdz_instruction) {
        [0] value: Register,
        [1] name_constant_index: ConstantIndex,
    }

    /// Create a new for-in iterator for the given object, storing in dest. Gathers all the iterable
    /// keys of the object and its prototype chain, storing them in the iterator. Expects that the
    /// object value is not nullish, all other values will be coerced with ToObject.
    NewForInIterator (NewForInIteratorInstruction, new_for_in_iterator_instruction) {
        [0] dest: Register,
        [1] object: Register,
    }

    /// Call the `next` method on a for-in iterator, storing the result in dest. Returns either the
    /// next string key or undefined if there are no more keys.
    ForInNext(ForInNextInstruction, for_in_next_instruction) {
        [0] dest: Register,
        [1] iterator: Register,
    }

    /// Initialize a global scope, declaring all names in a GlobalNames object stored in the
    /// constant table. Must be called in the global scope.
    GlobalInit(GlobalInitInstruction, global_init_instruction) {
        [0] global_names_index: ConstantIndex,
    }

    /// Initialize a sloppy direct eval scope, declaring all names in a GlobalNames object stored in
    /// the constant table. Must be called in the parent scope for the direct eval.
    EvalInit(EvalInitInstruction, eval_init_instruction) {
        [0] global_names_index: ConstantIndex,
    }

    GetIterator(GetIteratorInstruction, get_iterator_instruction) {
        [0] iterator: Register,
        [1] next_method: Register,
        [2] iterable: Register,
    }

    IteratorNext(IteratorNextInstruction, iterator_next_instruction) {
        [0] value: Register,
        [1] is_done: Register,
        [2] iterator: Register,
        [3] next_method: Register,
    }

    IteratorClose(IteratorCloseInstruction, iterator_close_instruction) {
        [0] iterator: Register,
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
