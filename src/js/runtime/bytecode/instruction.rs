use std::{
    collections::{HashMap, HashSet},
    fmt,
};

use super::{
    operand::{ConstantIndex, Operand, OperandType, Register, SInt, UInt},
    width::{ExtraWide, Narrow, UnsignedWidthRepr, Wide, Width, WidthEnum},
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
    fn get_raw_operand(&self, index: usize) -> usize;

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
                fn get_raw_operand(&self, index: usize) -> usize {
                    self.0[index].to_usize()
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

    /// Load a global variable into a register. The global variable's name is stored in the constant
    /// table.
    LoadGlobal (LoadGlobalInstruction, load_global_instruction) {
        [0] dest: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Store a register into a global variable. The global variable's name is stored in the
    /// constant table.
    StoreGlobal (StoreGlobalInstruction, store_global_instruction) {
        [0] value: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Call a function. Arguments are passed in contiguous sequence of registers starting at argv,
    /// of length argc. The first argument is the receiver.
    Call (CallInstruction, call_instruction) {
        [0] dest: Register,
        [1] function: Register,
        [2] argv: Register,
        [3] argc: UInt,
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

    /// Test if the left operand is less than the right operand, storing the result in dest.
    LessThan (LessThanInstruction, less_than_instruction) {
        [0] dest: Register,
        [1] left: Register,
        [2] right: Register,
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

    /// Conditionally jump to the given instruction if the condition is false, using an inline offset.
    JumpFalse(JumpFalseInstruction, jump_false_instruction) {
        [0] condition: Register,
        [1] offset: SInt,
    }

    /// Conditionally jump to the given instruction if the condition is false, with offset stored
    /// in the constant table.
    JumpFalseConstant(JumpFalseConstantInstruction, jump_false_constant_instruction) {
        [0] condition: Register,
        [1] constant_index: ConstantIndex,
    }

    /// Create a new closure from the function at the given index in the constant table.
    NewClosure(NewClosureInstruction, new_closure_instruction) {
        [0] dest: Register,
        [1] function_index: ConstantIndex,
    }
);

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

    let offset_width = prev_offset.ilog10() as usize + 1;

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

        // Then print the raw bytes of the instruction
        for byte in &bytecode[offset..next_offset] {
            printer.write(&format!("{:02x} ", byte));
        }

        // Pad the raw bytes to the max instruction length
        printer.write(&"   ".repeat(max_instr_length - (next_offset - offset)));

        // Then print the instruction in a readable form
        printer.write(&format!("  {instr}"));

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
        Some(instr.get_raw_operand(0) as isize)
    } else if opcode >= OpCode::JumpFalse && opcode <= OpCode::JumpFalseConstant {
        Some(instr.get_raw_operand(1) as isize)
    } else {
        None
    }
}
