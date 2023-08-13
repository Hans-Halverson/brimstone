use std::mem::size_of;

use crate::{
    field_offset,
    js::{
        common::unicode::{to_string_or_unicode_escape_sequence, CodePoint},
        parser::regexp::{RegExp, RegExpFlags},
        runtime::{
            collections::InlineArray,
            gc::{HeapObject, HeapVisitor},
            object_descriptor::{ObjectDescriptor, ObjectKind},
            string_value::StringValue,
            Context, Handle, HeapPtr,
        },
    },
    set_uninit,
};

#[repr(C)]
pub struct CompiledRegExpObject {
    descriptor: HeapPtr<ObjectDescriptor>,
    pub flags: RegExpFlags,
    // Whether this regexp has any named capture groups
    pub has_named_capture_groups: bool,
    // Number of capture groups, not counting the implicit 0'th capture group for the entire match.
    pub num_capture_groups: u32,
    pub num_progress_points: u32,
    // Array of bytecode instructions
    instructions: InlineArray<Instruction>,
    // Array of capture groups, optionally containing capture group name. Field should not be
    // accessed directly since instructions array is variable sized.
    _capture_groups: [Option<HeapPtr<StringValue>>; 1],
}

#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    /// Consume a single code points, failing if code point does not match literal or there is no
    /// code point to consume.
    Literal(CodePoint),
    /// Consume a single code point, failing if there is no code point to consume
    Wildcard,
    /// Continue execution at the instruction with the given index
    Jump(u32),
    /// Branch between two instructions given their indicies, following first then second branch
    Branch(u32, u32),
    /// Found a match
    Accept,
    /// Mark the current location in the string on the capture stack. Takes a capture point index,
    /// which may be the beginning or end of a capture group.
    MarkCapturePoint(u32),
    /// Mark the current location in the string in the progress array at the given progress index,
    /// fail if this string location has already been visited for the progress index. This is used
    /// to avoid epsilon loops.
    Progress(u32),
    /// Assert the start of the input (^)
    AssertStart,
    /// Assert the end of the input ($)
    AssertEnd,
    /// Assert start of the input or a newline (^ with multiline flag)
    AssertStartOrNewline,
    /// Assert end of the input or a newline ($ with multiline flag)
    AssertEndOrNewline,
    /// Assert a word boundary (\b)
    AssertWordBoundary,
    /// Assert not a word boundary (\B)
    AssertNotWordBoundary,
}

const INSTRUCTIONS_BYTE_OFFSET: usize = field_offset!(CompiledRegExpObject, instructions);

impl CompiledRegExpObject {
    pub fn new(
        cx: &mut Context,
        instructions: Vec<Instruction>,
        regexp: &RegExp,
        num_progress_points: u32,
    ) -> Handle<CompiledRegExpObject> {
        let num_capture_groups = regexp.capture_groups.len() as u32;
        let mut has_named_capture_groups = false;

        let capture_group_handles = regexp
            .capture_groups
            .iter()
            .map(|capture_group| {
                if let Some(name_string) = capture_group {
                    has_named_capture_groups = true;
                    Some(cx.alloc_string(name_string))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        let size = Self::calculate_size_in_bytes(instructions.len(), num_capture_groups);
        let mut object = cx.heap.alloc_uninit_with_size::<CompiledRegExpObject>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::DenseArrayProperties));
        set_uninit!(object.flags, regexp.flags);
        set_uninit!(object.has_named_capture_groups, has_named_capture_groups);
        set_uninit!(object.num_capture_groups, num_capture_groups);
        set_uninit!(object.num_progress_points, num_progress_points);

        object.instructions.init_from_vec(instructions);

        // Initialize capture group strings
        let capture_group_ptrs = capture_group_handles
            .into_iter()
            .map(|capture_group| capture_group.map(|name_string| name_string.get_()))
            .collect::<Vec<_>>();
        object
            .capture_groups_as_slice_mut()
            .copy_from_slice(capture_group_ptrs.as_slice());

        object.to_handle()
    }

    fn calculate_size_in_bytes(num_instructions: usize, num_capture_groups: u32) -> usize {
        INSTRUCTIONS_BYTE_OFFSET
            + InlineArray::<Instruction>::calculate_size_in_bytes(num_instructions)
            + size_of::<Option<HeapPtr<StringValue>>>() * num_capture_groups as usize
    }

    #[inline]
    pub fn instructions(&self) -> &[Instruction] {
        self.instructions.as_slice()
    }

    // Capture groups accessors

    #[inline]
    fn capture_groups_as_ptr(&self) -> *const Option<HeapPtr<StringValue>> {
        let byte_offset = self.capture_groups_byte_offset();
        unsafe { (self as *const _ as *const u8).add(byte_offset).cast() }
    }

    #[inline]
    fn capture_groups_byte_offset(&self) -> usize {
        INSTRUCTIONS_BYTE_OFFSET
            + InlineArray::<Instruction>::calculate_size_in_bytes(self.instructions.len())
    }

    #[inline]
    pub fn capture_groups_as_slice(&self) -> &[Option<HeapPtr<StringValue>>] {
        unsafe {
            std::slice::from_raw_parts(
                self.capture_groups_as_ptr(),
                self.num_capture_groups as usize,
            )
        }
    }

    #[inline]
    pub fn capture_groups_as_slice_mut(&self) -> &mut [Option<HeapPtr<StringValue>>] {
        unsafe {
            std::slice::from_raw_parts_mut(
                self.capture_groups_as_ptr().cast_mut(),
                self.num_capture_groups as usize,
            )
        }
    }

    pub fn debug_print_instructions(&self) -> String {
        let mut string = String::new();

        for (index, instruction) in self.instructions.as_slice().iter().enumerate() {
            string.push_str(&format!("{:4}: {}\n", index, instruction.debug_print()));
        }

        string
    }
}

impl Instruction {
    fn debug_print(&self) -> String {
        match self {
            Instruction::Literal(code_point) => {
                let code_point_string = to_string_or_unicode_escape_sequence(*code_point);
                format!("Literal({})", code_point_string)
            }
            Instruction::Wildcard => String::from("Wildcard"),
            Instruction::Jump(index) => format!("Jump({})", index),
            Instruction::Branch(first_index, second_index) => {
                format!("Branch({}, {})", first_index, second_index)
            }
            Instruction::Accept => String::from("Accept"),
            Instruction::MarkCapturePoint(index) => format!("MarkCapture({})", index),
            Instruction::Progress(index) => format!("Progress({})", index),
            Instruction::AssertStart => String::from("AssertStart"),
            Instruction::AssertEnd => String::from("AssertEnd"),
            Instruction::AssertStartOrNewline => String::from("AssertStartOrNewline"),
            Instruction::AssertEndOrNewline => String::from("AssertEndOrNewline"),
            Instruction::AssertWordBoundary => String::from("AssertWordBoundary"),
            Instruction::AssertNotWordBoundary => String::from("AssertNotWordBoundary"),
        }
    }
}

impl HeapObject for HeapPtr<CompiledRegExpObject> {
    fn byte_size(&self) -> usize {
        CompiledRegExpObject::calculate_size_in_bytes(
            self.instructions.len(),
            self.num_capture_groups,
        )
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        for capture_group in self.capture_groups_as_slice_mut() {
            visitor.visit_pointer_opt(capture_group);
        }
    }
}
