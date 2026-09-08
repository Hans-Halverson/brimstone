use std::{marker::PhantomData, mem::size_of};

use crate::{
    common::{
        string_iterators::CodePointIterator,
        unicode::{CodePoint, to_string_or_unicode_escape_sequence},
        unicode_property::UnicodeProperty,
    },
    runtime::regexp::code_point_set::{CodePointSetFlags, EncodedCodePointSet},
    static_assert,
};

/// Instructions are encoded as a variable width sequence of 4-byte integers.
#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum OpCode {
    /// Consume a single code point, failing if code point does not match literal or there is no
    /// code point to consume.
    ///
    /// Layout: [[opcode: u8] [code_point: u24]]
    CodePointLiteral,

    /// Consume an entire string of one-byte code points, failing if the full string does not match.
    /// String bytes are stored in the constant table at the given offset, with the given length in
    /// code units.
    ///
    /// Layout: [[opcode: u8] [length: u24]] [string_data_offset: u32]
    OneByteStringLiteral,

    /// Consume an entire string of two-byte code points, failing if the full string does not match.
    /// String bytes are stored in the constant table at the given offset, with the given length in
    /// code units.
    ///
    /// Layout: [[opcode: u8] [length: u24]] [string_data_offset: u32]
    TwoByteStringLiteral,

    /// Consume a single code point if it is a member of the encoded code point set stored in the
    /// constant table at the given offset (or if it is not a member when the inverted flag is set).
    /// Fails if there is no code point to consume. The set flags describe the layout of the set.
    ///
    /// Layout: [[opcode: u8] [flags: u8] [padding: u16] [set_offset: u32]]
    CodePointSet,

    /// Greedily consume as many code points as possible while they are members of the encoded code
    /// point set stored in the constant table. Uses the same encoding scheme and flags as the
    /// CodePointSet instruction. May match no code points.
    ///
    /// Layout: [[opcode: u8] [flags: u8] [padding: u16] [set_offset: u32]]
    GreedyLoop,

    /// Consume a single code point, failing if there is no code point to consume. This is the
    /// behavior of the wildcard with the `s` flag.
    ///
    /// Layout: [[opcode: u8] [padding: u24]]
    Wildcard,

    /// Consume a single code point, failing if the code point is a newline or there is no code
    /// point to consume. This is the default behavior of the wildcard `.`.
    ///
    /// Layout: [[opcode: u8] [padding: u24]]
    WildcardNoNewline,

    /// Continue execution at the target instruction
    ///
    /// Layout: [[opcode: u8] [padding: u24] [target: u32]]
    Jump,

    /// Branch between two instructions given their indices, following first then second branch
    ///
    /// Layout: [[opcode: u8] [padding: u24] [first_branch: u32] [second_branch: u32]]
    Branch,

    /// Found a match
    ///
    /// Layout: [[opcode: u8] [padding: u24]]
    Accept,

    /// Did not find a match, backtrack if necessary
    ///
    /// Layout: [[opcode: u8] [padding: u24]]
    Fail,

    /// Mark the current location in the string on the capture stack. Takes a capture point index,
    /// which may be the beginning or end of a capture group.
    ///
    /// Layout: [[opcode: u8] [padding: u24] [capture_point_index: u32]]
    MarkCapturePoint,

    /// Mark that a particular capture group has not been matched. This allows us to clear capture
    /// groups that were previously matched but are no longer matched e.g. due another pass over
    /// a quantifier that does not match a previously matched group. Operand is the index of the
    /// capture group (1-indexed).
    ///
    /// Layout: [[opcode: u8] [padding: u24] [capture_group_index: u32]]
    ClearCapture,

    /// Mark the current location in the string in the progress array at the given progress index,
    /// fail if this string location has already been visited for the progress index. This is used
    /// to avoid epsilon loops.
    ///
    /// Layout: [[opcode: u8] [padding: u24] [progress_index: u32]]
    Progress,

    /// Set the current location in the string in the progress array at the given progress index.
    /// Do not check if progress has been made.
    ///
    /// Layout: [[opcode: u8] [padding: u24] [progress_index: u32]]
    SetProgress,

    /// Check if the loop register is less than the provided max value. Proceed if so, otherwise
    /// jump to the end branch instruction. Always increment the loop register by 1.
    ///
    /// Layout: [[opcode: u8] [padding: u24] [loop_register_index: u32] [loop_max_value: u32] [end_branch: u32]]
    Loop,

    /// Assert the start of the input (^)
    ///
    /// Layout: [[opcode: u8] [padding: u24]]
    AssertStart,

    /// Assert the end of the input ($)
    ///
    /// Layout: [[opcode: u8] [padding: u24]]
    AssertEnd,

    /// Assert start of the input or a newline (^ with multiline flag)
    ///
    /// Layout: [[opcode: u8] [padding: u24]]
    AssertStartOrNewline,

    /// Assert end of the input or a newline ($ with multiline flag)
    ///
    /// Layout: [[opcode: u8] [padding: u24]]
    AssertEndOrNewline,

    /// Assert a word boundary (\b) or, if negated, a non-word boundary (\B). The word set is an
    /// encoded code point set stored in the constant table at the given offset, whose layout is
    /// described by the set flags.
    ///
    /// Layout: [[opcode: u8] [is_negated: u8] [flags: u8] [padding: u8] [set_offset: u32]]
    AssertWordBoundary,

    /// Consume the same code points as a previously captured group, failing if the previously
    /// captured group cannot be matched.
    ///
    /// Layout: [[opcode: u8] [is_case_insensitive: u8] [capture_group_index: u32]]
    Backreference,

    /// Start a lookahead with operands `is_ahead`, `is_positive`, and `body_branch`
    /// which is the instruction that starts the lookaround body.
    ///
    /// Layout: [[opcode: u8] [is_ahead: u8] [is_positive: u8] [padding: u8] [body_branch: u32]]
    Lookaround,
}

static_assert!(size_of::<UnicodeProperty>() == 4);

impl OpCode {
    /// Return the number of u32 words this instruction takes up.
    fn size(&self) -> usize {
        match *self {
            OpCode::CodePointLiteral => CodePointLiteralInstruction::SIZE,
            OpCode::OneByteStringLiteral => OneByteStringLiteralInstruction::SIZE,
            OpCode::TwoByteStringLiteral => TwoByteStringLiteralInstruction::SIZE,
            OpCode::CodePointSet => CodePointSetInstruction::SIZE,
            OpCode::GreedyLoop => GreedyLoopInstruction::SIZE,
            OpCode::Wildcard => WildcardInstruction::SIZE,
            OpCode::WildcardNoNewline => WildcardNoNewlineInstruction::SIZE,
            OpCode::Jump => JumpInstruction::SIZE,
            OpCode::Branch => BranchInstruction::SIZE,
            OpCode::Accept => AcceptInstruction::SIZE,
            OpCode::Fail => FailInstruction::SIZE,
            OpCode::MarkCapturePoint => MarkCapturePointInstruction::SIZE,
            OpCode::ClearCapture => ClearCaptureInstruction::SIZE,
            OpCode::Progress => ProgressInstruction::SIZE,
            OpCode::SetProgress => SetProgressInstruction::SIZE,
            OpCode::Loop => LoopInstruction::SIZE,
            OpCode::AssertStart => AssertStartInstruction::SIZE,
            OpCode::AssertEnd => AssertEndInstruction::SIZE,
            OpCode::AssertStartOrNewline => AssertStartOrNewlineInstruction::SIZE,
            OpCode::AssertEndOrNewline => AssertEndOrNewlineInstruction::SIZE,
            OpCode::AssertWordBoundary => AssertWordBoundaryInstruction::SIZE,
            OpCode::Backreference => BackreferenceInstruction::SIZE,
            OpCode::Lookaround => LookaroundInstruction::SIZE,
        }
    }
}

/// Generic representation of an encoded RegExp instruction. Used to check the opcode of the
/// instruction, then cast to a specific instruction.
#[repr(C)]
pub struct Instruction(OpCode);

impl Instruction {
    #[inline]
    pub fn opcode(&self) -> OpCode {
        self.0
    }

    #[inline]
    pub fn size(&self) -> usize {
        self.opcode().size()
    }

    #[inline]
    pub fn cast<T: TInstruction>(&self) -> &T {
        unsafe { std::mem::transmute(self) }
    }

    #[inline]
    pub fn cast_mut<T: TInstruction>(&mut self) -> &mut T {
        unsafe { std::mem::transmute(self) }
    }

    pub fn debug_print(&self, constants_data: &[u8]) -> String {
        match self.opcode() {
            OpCode::CodePointLiteral => self.cast::<CodePointLiteralInstruction>().debug_print(),
            OpCode::OneByteStringLiteral => self
                .cast::<OneByteStringLiteralInstruction>()
                .debug_print(constants_data),
            OpCode::TwoByteStringLiteral => self
                .cast::<TwoByteStringLiteralInstruction>()
                .debug_print(constants_data),
            OpCode::CodePointSet => self
                .cast::<CodePointSetInstruction>()
                .debug_print(constants_data),
            OpCode::GreedyLoop => self
                .cast::<GreedyLoopInstruction>()
                .debug_print(constants_data),
            OpCode::Wildcard => self.cast::<WildcardInstruction>().debug_print(),
            OpCode::WildcardNoNewline => self.cast::<WildcardNoNewlineInstruction>().debug_print(),
            OpCode::Jump => self.cast::<JumpInstruction>().debug_print(),
            OpCode::Branch => self.cast::<BranchInstruction>().debug_print(),
            OpCode::Accept => self.cast::<AcceptInstruction>().debug_print(),
            OpCode::Fail => self.cast::<FailInstruction>().debug_print(),
            OpCode::MarkCapturePoint => self.cast::<MarkCapturePointInstruction>().debug_print(),
            OpCode::ClearCapture => self.cast::<ClearCaptureInstruction>().debug_print(),
            OpCode::Progress => self.cast::<ProgressInstruction>().debug_print(),
            OpCode::SetProgress => self.cast::<SetProgressInstruction>().debug_print(),
            OpCode::Loop => self.cast::<LoopInstruction>().debug_print(),
            OpCode::AssertStart => self.cast::<AssertStartInstruction>().debug_print(),
            OpCode::AssertEnd => self.cast::<AssertEndInstruction>().debug_print(),
            OpCode::AssertStartOrNewline => {
                self.cast::<AssertStartOrNewlineInstruction>().debug_print()
            }
            OpCode::AssertEndOrNewline => {
                self.cast::<AssertEndOrNewlineInstruction>().debug_print()
            }
            OpCode::AssertWordBoundary => {
                self.cast::<AssertWordBoundaryInstruction>().debug_print()
            }
            OpCode::Backreference => self.cast::<BackreferenceInstruction>().debug_print(),
            OpCode::Lookaround => self.cast::<LookaroundInstruction>().debug_print(),
        }
    }
}

/// Generic trait implemented by all RegExp instructions.
pub trait TInstruction {
    /// Size of the instruction in u32 words.
    const SIZE: usize;

    /// Opcode of the instruction.
    const OPCODE: OpCode;
}

/// Return the u24 operand packed with the opcode for the given instruction.
#[inline]
fn get_packed_u24_operand(opcode_and_operand: u32) -> u32 {
    opcode_and_operand >> 8
}

/// Return the u8 operand at the given u8 index into the instruction, starting with 1.
#[inline]
fn get_packed_u8_operand(ptr: *const u32, index: usize) -> u8 {
    unsafe { *(ptr.cast::<u8>().add(index)) }
}

macro_rules! write_u32 {
    ($buf:expr, $opcode:expr) => {
        $buf.push($opcode as u32);
    };
}

macro_rules! write_opcode_with_u24_operand {
    ($buf:expr, $opcode:expr, $operand:expr) => {
        write_u32!($buf, ($opcode as u32) | ($operand << 8));
    };
}

macro_rules! regexp_bytecode_instruction {
    ($name:ident, $opcode:expr, $size:expr, impl { $($regexp_impl_item:item)* } ) => {
        #[allow(dead_code)]
        pub struct $name([u32; $size]);

        impl $name {
            $($regexp_impl_item)*
        }

        impl TInstruction for $name {
            const SIZE: usize = $size;

            const OPCODE: OpCode = $opcode;
        }
    };
}

macro_rules! nullary_regexp_bytcode_instruction {
    ($name:ident, $opcode:expr) => {
        regexp_bytecode_instruction!($name, $opcode, 1, impl {
            fn debug_print(&self) -> String {
                format!("{:?}", Self::OPCODE)
            }
        });

        impl $name {
            pub fn write(buf: &mut Vec<u32>) {
                write_u32!(buf, Self::OPCODE);
            }
        }
    };
}

nullary_regexp_bytcode_instruction!(WildcardInstruction, OpCode::Wildcard);
nullary_regexp_bytcode_instruction!(WildcardNoNewlineInstruction, OpCode::WildcardNoNewline);
nullary_regexp_bytcode_instruction!(AcceptInstruction, OpCode::Accept);
nullary_regexp_bytcode_instruction!(FailInstruction, OpCode::Fail);
nullary_regexp_bytcode_instruction!(AssertStartInstruction, OpCode::AssertStart);
nullary_regexp_bytcode_instruction!(AssertEndInstruction, OpCode::AssertEnd);
nullary_regexp_bytcode_instruction!(AssertStartOrNewlineInstruction, OpCode::AssertStartOrNewline);
nullary_regexp_bytcode_instruction!(AssertEndOrNewlineInstruction, OpCode::AssertEndOrNewline);

regexp_bytecode_instruction!(
    CodePointLiteralInstruction,
    OpCode::CodePointLiteral,
    1,
    impl {
        fn debug_print(&self) -> String {
            let code_point_string = to_string_or_unicode_escape_sequence(self.code_point());
            format!("{:?}({})", Self::OPCODE, code_point_string)
        }
    }
);

impl CodePointLiteralInstruction {
    #[inline]
    pub fn code_point(&self) -> u32 {
        get_packed_u24_operand(self.0[0])
    }

    pub fn write(buf: &mut Vec<u32>, code_point: u32) {
        write_opcode_with_u24_operand!(buf, Self::OPCODE, code_point);
    }
}

regexp_bytecode_instruction!(
    OneByteStringLiteralInstruction,
    OpCode::OneByteStringLiteral,
    2,
    impl {
        fn debug_print(&self, constants_data: &[u8]) -> String {
            let string_data = self.string_data(constants_data.as_ptr());
            let string = string_data.iter()
                .map(|&b| to_string_or_unicode_escape_sequence(b as CodePoint))
                .collect::<String>();

            format!("{:?}(\"{}\")", Self::OPCODE, string)
        }
    }
);

impl OneByteStringLiteralInstruction {
    /// Length of the string in code units.
    #[inline]
    pub fn length(&self) -> usize {
        get_packed_u24_operand(self.0[0]) as usize
    }

    #[inline]
    fn string_data_offset(&self) -> usize {
        self.0[1] as usize
    }

    #[inline]
    pub fn string_data<'a>(&self, constants_base: *const u8) -> &'a [u8] {
        unsafe {
            let start_ptr = constants_base.add(self.string_data_offset());
            std::slice::from_raw_parts(start_ptr, self.length())
        }
    }

    pub fn write(buf: &mut Vec<u32>, length: u32, string_data_offset: u32) {
        write_opcode_with_u24_operand!(buf, Self::OPCODE, length);
        write_u32!(buf, string_data_offset);
    }
}

regexp_bytecode_instruction!(
    TwoByteStringLiteralInstruction,
    OpCode::TwoByteStringLiteral,
    2,
    impl {
        fn debug_print(&self, constants_data: &[u8]) -> String {
            let string_data = self.string_data(constants_data.as_ptr());
            let string = CodePointIterator::from_raw_two_byte_slice(string_data)
                .map(to_string_or_unicode_escape_sequence)
                .collect::<String>();

            format!("{:?}(\"{}\")", Self::OPCODE, string)
        }
    }
);

impl TwoByteStringLiteralInstruction {
    /// Length of the string in code units.
    #[inline]
    pub fn length(&self) -> usize {
        get_packed_u24_operand(self.0[0]) as usize
    }

    #[inline]
    fn string_data_offset(&self) -> usize {
        self.0[1] as usize
    }

    #[inline]
    pub fn string_data<'a>(&self, constants_base: *const u8) -> &'a [u16] {
        unsafe {
            let start_ptr = constants_base.add(self.string_data_offset()).cast::<u16>();
            std::slice::from_raw_parts(start_ptr, self.length())
        }
    }

    pub fn write(buf: &mut Vec<u32>, length: u32, string_data_offset: u32) {
        write_opcode_with_u24_operand!(buf, Self::OPCODE, length);
        write_u32!(buf, string_data_offset);
    }
}

regexp_bytecode_instruction!(
    CodePointSetInstruction,
    OpCode::CodePointSet,
    2,
    impl {
        fn debug_print(&self, constants_data: &[u8]) -> String {
            let set = self.set_data(constants_data.as_ptr());
            let set_string = set.debug_format();

            if self.flags().is_inverted() {
                format!("{:?}(inverted, {})", Self::OPCODE, set_string)
            } else {
                format!("{:?}({})", Self::OPCODE, set_string)
            }
        }
    }
);

impl CodePointSetInstruction {
    #[inline]
    fn flags(&self) -> CodePointSetFlags {
        let encoded_flags = get_packed_u8_operand(self.0.as_ptr(), 1);
        CodePointSetFlags::from_bits_retain(encoded_flags)
    }

    #[inline]
    fn set_base(&self, constants_base: *const u8) -> *const u32 {
        unsafe { constants_base.add(self.0[1] as usize).cast::<u32>() }
    }

    #[inline]
    pub fn set_data(&self, constants_base: *const u8) -> EncodedCodePointSet {
        let flags = self.flags();
        let encoded_base = self.set_base(constants_base);

        EncodedCodePointSet { flags, encoded_base }
    }

    pub fn write(buf: &mut Vec<u32>, flags: CodePointSetFlags, set_offset: u32) {
        write_u32!(buf, (Self::OPCODE as u32) | ((flags.bits() as u32) << 8));
        write_u32!(buf, set_offset);
    }
}

regexp_bytecode_instruction!(
    GreedyLoopInstruction,
    OpCode::GreedyLoop,
    2,
    impl {
        fn debug_print(&self, constants_data: &[u8]) -> String {
            let set = self.set_data(constants_data.as_ptr());
            let set_string = set.debug_format();

            if self.flags().is_inverted() {
                format!("{:?}(inverted, {})", Self::OPCODE, set_string)
            } else {
                format!("{:?}({})", Self::OPCODE, set_string)
            }
        }
    }
);

impl GreedyLoopInstruction {
    #[inline]
    fn flags(&self) -> CodePointSetFlags {
        let encoded_flags = get_packed_u8_operand(self.0.as_ptr(), 1);
        CodePointSetFlags::from_bits_retain(encoded_flags)
    }

    #[inline]
    fn set_base(&self, constants_base: *const u8) -> *const u32 {
        unsafe { constants_base.add(self.0[1] as usize).cast::<u32>() }
    }

    #[inline]
    pub fn set_data(&self, constants_base: *const u8) -> EncodedCodePointSet {
        let flags = self.flags();
        let encoded_base = self.set_base(constants_base);

        EncodedCodePointSet { flags, encoded_base }
    }

    pub fn write(buf: &mut Vec<u32>, flags: CodePointSetFlags, set_offset: u32) {
        write_u32!(buf, (Self::OPCODE as u32) | ((flags.bits() as u32) << 8));
        write_u32!(buf, set_offset);
    }
}

regexp_bytecode_instruction!(
    JumpInstruction,
    OpCode::Jump,
    2,
    impl {
        fn debug_print(&self) -> String {
            format!("{:?}({})", Self::OPCODE, self.target())
        }
    }
);

impl JumpInstruction {
    #[inline]
    pub fn target(&self) -> u32 {
        self.0[1]
    }

    #[inline]
    pub fn set_target(&mut self, target: u32) {
        self.0[1] = target;
    }

    pub fn write(buf: &mut Vec<u32>, target: u32) {
        write_u32!(buf, Self::OPCODE);
        write_u32!(buf, target);
    }
}

regexp_bytecode_instruction!(
    BranchInstruction,
    OpCode::Branch,
    3,
    impl {
        fn debug_print(&self) -> String {
            format!("{:?}({}, {})", Self::OPCODE, self.first_branch(), self.second_branch())
        }
    }
);

impl BranchInstruction {
    #[inline]
    pub fn first_branch(&self) -> u32 {
        self.0[1]
    }

    #[inline]
    pub fn second_branch(&self) -> u32 {
        self.0[2]
    }

    #[inline]
    pub fn set_first_branch(&mut self, first_branch: u32) {
        self.0[1] = first_branch;
    }

    #[inline]
    pub fn set_second_branch(&mut self, second_branch: u32) {
        self.0[2] = second_branch;
    }

    pub fn write(buf: &mut Vec<u32>, first_branch: u32, second_branch: u32) {
        write_u32!(buf, Self::OPCODE);
        write_u32!(buf, first_branch);
        write_u32!(buf, second_branch);
    }
}

regexp_bytecode_instruction!(
    MarkCapturePointInstruction,
    OpCode::MarkCapturePoint,
    2,
    impl {
        fn debug_print(&self) -> String {
            format!("{:?}({})", Self::OPCODE, self.capture_point_index())
        }
    }
);

impl MarkCapturePointInstruction {
    #[inline]
    pub fn capture_point_index(&self) -> u32 {
        self.0[1]
    }

    pub fn write(buf: &mut Vec<u32>, capture_point_index: u32) {
        write_u32!(buf, Self::OPCODE);
        write_u32!(buf, capture_point_index);
    }
}

regexp_bytecode_instruction!(
    ClearCaptureInstruction,
    OpCode::ClearCapture,
    2,
    impl {
        fn debug_print(&self) -> String {
            format!("{:?}({})", Self::OPCODE, self.capture_group_index())
        }
    }
);

impl ClearCaptureInstruction {
    #[inline]
    pub fn capture_group_index(&self) -> u32 {
        self.0[1]
    }

    pub fn write(buf: &mut Vec<u32>, capture_group_index: u32) {
        write_u32!(buf, Self::OPCODE);
        write_u32!(buf, capture_group_index);
    }
}

regexp_bytecode_instruction!(
    ProgressInstruction,
    OpCode::Progress,
    2,
    impl {
        fn debug_print(&self) -> String {
            format!("{:?}({})", Self::OPCODE, self.progress_index())
        }
    }
);

impl ProgressInstruction {
    #[inline]
    pub fn progress_index(&self) -> u32 {
        self.0[1]
    }

    pub fn write(buf: &mut Vec<u32>, progress_index: u32) {
        write_u32!(buf, Self::OPCODE);
        write_u32!(buf, progress_index);
    }
}

regexp_bytecode_instruction!(
    SetProgressInstruction,
    OpCode::SetProgress,
    2,
    impl {
        fn debug_print(&self) -> String {
            format!("{:?}({})", Self::OPCODE, self.progress_index())
        }
    }
);

impl SetProgressInstruction {
    #[inline]
    pub fn progress_index(&self) -> u32 {
        self.0[1]
    }

    pub fn write(buf: &mut Vec<u32>, progress_index: u32) {
        write_u32!(buf, Self::OPCODE);
        write_u32!(buf, progress_index);
    }
}

regexp_bytecode_instruction!(
    LoopInstruction,
    OpCode::Loop,
    4,
    impl {
        fn debug_print(&self) -> String {
            format!(
                "{:?}({}, {}, {})",
                Self::OPCODE,
                self.loop_register_index(),
                self.loop_max_value(),
                self.end_branch()
            )
        }
    }
);

impl LoopInstruction {
    #[inline]
    pub fn loop_register_index(&self) -> u32 {
        self.0[1]
    }

    #[inline]
    pub fn loop_max_value(&self) -> u32 {
        self.0[2]
    }

    #[inline]
    pub fn end_branch(&self) -> u32 {
        self.0[3]
    }

    #[inline]
    pub fn set_end_branch(&mut self, end_branch: u32) {
        self.0[3] = end_branch;
    }

    pub fn write(
        buf: &mut Vec<u32>,
        loop_register_index: u32,
        loop_max_value: u32,
        end_branch: u32,
    ) {
        write_u32!(buf, Self::OPCODE);
        write_u32!(buf, loop_register_index);
        write_u32!(buf, loop_max_value);
        write_u32!(buf, end_branch);
    }
}

regexp_bytecode_instruction!(
    BackreferenceInstruction,
    OpCode::Backreference,
    2,
    impl {
        fn debug_print(&self) -> String {
            format!("{:?}({}, {})", Self::OPCODE, self.capture_group_index(), self.is_case_insensitive())
        }
    }
);

impl BackreferenceInstruction {
    #[inline]
    pub fn is_case_insensitive(&self) -> bool {
        get_packed_u8_operand(self.0.as_ptr(), 1) != 0
    }

    #[inline]
    pub fn capture_group_index(&self) -> u32 {
        self.0[1]
    }

    pub fn write(buf: &mut Vec<u32>, is_case_insensitive: bool, capture_group_index: u32) {
        let mut first_u32 = Self::OPCODE as u32;

        if is_case_insensitive {
            first_u32 |= 1 << 8;
        }

        write_u32!(buf, first_u32);
        write_u32!(buf, capture_group_index);
    }
}

regexp_bytecode_instruction!(
    AssertWordBoundaryInstruction,
    OpCode::AssertWordBoundary,
    2,
    impl {
        fn debug_print(&self) -> String {
            format!("{:?}({})", Self::OPCODE, self.is_negated())
        }
    }
);

impl AssertWordBoundaryInstruction {
    #[inline]
    pub fn is_negated(&self) -> bool {
        get_packed_u8_operand(self.0.as_ptr(), 1) != 0
    }

    #[inline]
    fn flags(&self) -> CodePointSetFlags {
        let encoded_flags = get_packed_u8_operand(self.0.as_ptr(), 2);
        CodePointSetFlags::from_bits_retain(encoded_flags)
    }

    #[inline]
    fn set_base(&self, constants_base: *const u8) -> *const u32 {
        unsafe { constants_base.add(self.0[1] as usize).cast::<u32>() }
    }

    #[inline]
    pub fn set_data(&self, constants_base: *const u8) -> EncodedCodePointSet {
        let flags = self.flags();
        let encoded_base = self.set_base(constants_base);

        EncodedCodePointSet { flags, encoded_base }
    }

    pub fn write(buf: &mut Vec<u32>, is_negated: bool, flags: CodePointSetFlags, set_offset: u32) {
        let mut first_u32 = Self::OPCODE as u32;

        if is_negated {
            first_u32 |= 1 << 8;
        }

        first_u32 |= (flags.bits() as u32) << 16;

        write_u32!(buf, first_u32);
        write_u32!(buf, set_offset);
    }
}

regexp_bytecode_instruction!(
    LookaroundInstruction,
    OpCode::Lookaround,
    2,
    impl {
        fn debug_print(&self) -> String {
            format!(
                "{:?}({}, {}, {})",
                Self::OPCODE,
                self.is_ahead(),
                self.is_positive(),
                self.body_branch()
            )
        }
    }
);

impl LookaroundInstruction {
    #[inline]
    pub fn is_ahead(&self) -> bool {
        get_packed_u8_operand(self.0.as_ptr(), 1) != 0
    }

    #[inline]
    pub fn is_positive(&self) -> bool {
        get_packed_u8_operand(self.0.as_ptr(), 2) != 0
    }

    #[inline]
    pub fn body_branch(&self) -> u32 {
        self.0[1]
    }

    #[inline]
    pub fn set_body_branch(&mut self, body_branch: u32) {
        self.0[1] = body_branch;
    }

    pub fn write(buf: &mut Vec<u32>, is_ahead: bool, is_positive: bool, body_branch: u32) {
        let mut first_byte = Self::OPCODE as u32;

        if is_ahead {
            first_byte |= 1 << 8;
        }

        if is_positive {
            first_byte |= 1 << 16;
        }

        write_u32!(buf, first_byte);
        write_u32!(buf, body_branch);
    }
}

/// Iterator over a buffer of encoded instructions.
pub struct InstructionIterator<'a> {
    ptr: *const u32,
    end: *const u32,
    _marker: PhantomData<&'a u32>,
}

impl<'a> InstructionIterator<'a> {
    pub fn new(buf: &'a [u32]) -> Self {
        let ptr = buf.as_ptr();
        let end = unsafe { ptr.add(buf.len()) };

        Self { ptr, end, _marker: PhantomData }
    }

    pub fn is_end(&self) -> bool {
        self.ptr >= self.end
    }
}

impl<'a> Iterator for InstructionIterator<'a> {
    type Item = &'a Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_end() {
            return None;
        }

        let ptr = self.ptr;

        unsafe {
            let opcode = *ptr.cast::<OpCode>();
            self.ptr = ptr.add(opcode.size());

            Some(&*ptr.cast::<Instruction>())
        }
    }
}

/// Mutable iterator over a buffer of encoded instructions.
pub struct InstructionIteratorMut<'a> {
    ptr: *mut u32,
    end: *mut u32,
    _marker: PhantomData<&'a mut u32>,
}

impl<'a> InstructionIteratorMut<'a> {
    pub fn new(buf: &'a mut [u32]) -> Self {
        let ptr = buf.as_mut_ptr();
        let end = unsafe { ptr.add(buf.len()) };

        Self { ptr, end, _marker: PhantomData }
    }
}

impl<'a> Iterator for InstructionIteratorMut<'a> {
    type Item = &'a mut Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        if self.ptr >= self.end {
            return None;
        }

        let ptr = self.ptr;

        unsafe {
            let opcode = *ptr.cast::<OpCode>();
            self.ptr = ptr.add(opcode.size());

            Some(&mut *ptr.cast::<Instruction>())
        }
    }
}
