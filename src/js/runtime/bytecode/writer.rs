use super::{
    instruction::{
        decode_width_and_opcode_at_index, extra_wide_prefix_index_to_opcode_index,
        wide_prefix_index_to_opcode_index, DecodeInfo, OpCode,
    },
    operand::Operand,
    width::{Width, WidthEnum},
};

pub struct BytecodeWriter {
    bytes: Vec<u8>,
}

impl BytecodeWriter {
    pub fn new() -> Self {
        Self { bytes: Vec::new() }
    }

    pub fn finish(self) -> Vec<u8> {
        self.bytes
    }

    pub fn decode_width_and_opcode_at_index(&mut self, index: usize) -> DecodeInfo {
        decode_width_and_opcode_at_index(&self.bytes, index)
    }

    pub fn current_offset(&self) -> usize {
        self.bytes.len()
    }

    pub fn set_u8(&mut self, index: usize, value: u8) {
        self.bytes[index] = value;
    }

    pub fn set_u16(&mut self, index: usize, value: u16) {
        let value_bytes = value.to_ne_bytes();

        self.bytes[index] = value_bytes[0];
        self.bytes[index + 1] = value_bytes[1];
    }

    pub fn set_u32(&mut self, index: usize, value: u32) {
        let value_bytes = value.to_ne_bytes();

        self.bytes[index] = value_bytes[0];
        self.bytes[index + 1] = value_bytes[1];
        self.bytes[index + 2] = value_bytes[2];
        self.bytes[index + 3] = value_bytes[3];
    }

    fn write_u8(&mut self, value: u8) {
        self.bytes.push(value);
    }

    pub fn write_opcode(&mut self, opcode: OpCode) {
        self.write_u8(opcode as u8);
    }

    pub fn write_operand<W: Width, O: Operand<W>>(&mut self, operand: O) {
        W::write_into(&mut self.bytes, operand.unsigned());
    }

    /// Write the width prefix for a particular width, followed by padding up until one byte before
    /// the width's alignment.
    pub fn write_width_prefix(&mut self, width: WidthEnum) {
        match width {
            // No width prefix necessary
            WidthEnum::Narrow => {}
            WidthEnum::Wide => {
                let opcode_index = wide_prefix_index_to_opcode_index(self.bytes.len());
                self.wide_prefix_instruction();

                for _ in self.bytes.len()..opcode_index {
                    self.write_u8(0);
                }
            }
            WidthEnum::ExtraWide => {
                let opcode_index = extra_wide_prefix_index_to_opcode_index(self.bytes.len());
                self.extra_wide_prefix_instruction();

                for _ in self.bytes.len()..opcode_index {
                    self.write_u8(0);
                }
            }
        }
    }
}
