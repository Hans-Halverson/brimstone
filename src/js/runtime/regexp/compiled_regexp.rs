use std::mem::size_of;

use crate::{
    common::math::round_to_power_of_two,
    field_offset,
    parser::regexp::{RegExp, RegExpFlags},
    runtime::{
        collections::InlineArray,
        debug_print::{DebugPrint, DebugPrinter},
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
        string_value::{FlatString, StringValue},
        Context, Handle, HeapPtr,
    },
    set_uninit,
};

use super::instruction::InstructionIterator;

#[repr(C)]
pub struct CompiledRegExpObject {
    descriptor: HeapPtr<HeapItemDescriptor>,
    // The pattern component of the original regexp as a string. Escaped so that it can be
    // parsed into exactly the same pattern again.
    escaped_pattern_source: HeapPtr<StringValue>,
    pub flags: RegExpFlags,
    // Whether this regexp has any named capture groups
    pub has_named_capture_groups: bool,
    // Whether this regexp has any duplicate named capture groups
    pub has_duplicate_named_capture_groups: bool,
    // Number of capture groups, not counting the implicit 0'th capture group for the entire match.
    pub num_capture_groups: u32,
    pub num_progress_points: u32,
    pub num_loop_registers: u32,
    // Array of bytecode instructions
    instructions: InlineArray<u32>,
    // Array of capture groups, optionally containing capture group name. Field should not be
    // accessed directly since instructions array is variable sized.
    _capture_groups: [Option<HeapPtr<FlatString>>; 1],
}

const INSTRUCTIONS_BYTE_OFFSET: usize = field_offset!(CompiledRegExpObject, instructions);

impl CompiledRegExpObject {
    pub fn new(
        mut cx: Context,
        instructions: Vec<u32>,
        regexp: &RegExp,
        escaped_pattern_source: Handle<StringValue>,
        num_progress_points: u32,
        num_loop_registers: u32,
    ) -> Handle<CompiledRegExpObject> {
        let num_capture_groups = regexp.capture_groups.len() as u32;
        let mut has_named_capture_groups = false;

        let capture_group_handles = regexp
            .capture_groups
            .iter()
            .map(|capture_group| {
                if let Some(name_string) = capture_group {
                    has_named_capture_groups = true;
                    Some(cx.alloc_wtf8_str_ptr(name_string).to_handle())
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        let size = Self::calculate_size_in_bytes(instructions.len(), num_capture_groups);
        let mut object = cx.alloc_uninit_with_size::<CompiledRegExpObject>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(HeapItemKind::CompiledRegExpObject));
        set_uninit!(object.escaped_pattern_source, *escaped_pattern_source);
        set_uninit!(object.flags, regexp.flags);
        set_uninit!(object.has_named_capture_groups, has_named_capture_groups);
        set_uninit!(
            object.has_duplicate_named_capture_groups,
            regexp.has_duplicate_named_capture_groups
        );
        set_uninit!(object.num_capture_groups, num_capture_groups);
        set_uninit!(object.num_progress_points, num_progress_points);
        set_uninit!(object.num_loop_registers, num_loop_registers);

        object.instructions.init_from_slice(&instructions);

        // Initialize capture group strings
        let capture_group_ptrs = capture_group_handles
            .into_iter()
            .map(|capture_group| capture_group.map(|name_string| *name_string))
            .collect::<Vec<_>>();
        object
            .capture_groups_as_slice_mut()
            .copy_from_slice(capture_group_ptrs.as_slice());

        object.to_handle()
    }

    #[inline]
    fn capture_groups_byte_offset(num_instructions: usize) -> usize {
        // Pad to 8 bytes to ensure that next field is properly aligned
        let instructions_field_size = InlineArray::<u32>::calculate_size_in_bytes(num_instructions);
        let instructions_field_with_padding = round_to_power_of_two(instructions_field_size, 8);

        INSTRUCTIONS_BYTE_OFFSET + instructions_field_with_padding
    }

    #[inline]
    fn calculate_size_in_bytes(num_instructions: usize, num_capture_groups: u32) -> usize {
        Self::capture_groups_byte_offset(num_instructions)
            + size_of::<Option<HeapPtr<FlatString>>>() * num_capture_groups as usize
    }

    #[inline]
    pub fn escaped_pattern_source(&self) -> Handle<StringValue> {
        self.escaped_pattern_source.to_handle()
    }

    #[inline]
    pub fn instructions(&self) -> &[u32] {
        self.instructions.as_slice()
    }

    // Capture groups accessors

    #[inline]
    fn capture_groups_as_ptr(&self) -> *const Option<HeapPtr<FlatString>> {
        let byte_offset = Self::capture_groups_byte_offset(self.instructions.len());
        unsafe { (self as *const _ as *const u8).add(byte_offset).cast() }
    }

    #[inline]
    pub fn capture_groups_as_slice(&self) -> &[Option<HeapPtr<FlatString>>] {
        unsafe {
            std::slice::from_raw_parts(
                self.capture_groups_as_ptr(),
                self.num_capture_groups as usize,
            )
        }
    }

    #[inline]
    pub fn capture_groups_as_slice_mut(&mut self) -> &mut [Option<HeapPtr<FlatString>>] {
        unsafe {
            std::slice::from_raw_parts_mut(
                self.capture_groups_as_ptr().cast_mut(),
                self.num_capture_groups as usize,
            )
        }
    }
}

impl DebugPrint for HeapPtr<CompiledRegExpObject> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        let source = format!("/{}/", self.escaped_pattern_source());
        printer.write_heap_item_with_context(self.cast(), &source);

        if printer.is_short_mode() {
            return;
        }

        printer.write(" {\n");
        printer.inc_indent();

        let mut offset = 0;

        for instruction in InstructionIterator::new(self.instructions.as_slice()) {
            printer.write_indent();
            printer.write(&format!("{:4}: {}\n", offset, instruction.debug_print()));

            offset += instruction.size();
        }

        printer.dec_indent();
        printer.write_indent();
        printer.write("}\n");
    }
}

impl HeapItem for HeapPtr<CompiledRegExpObject> {
    fn byte_size(&self) -> usize {
        CompiledRegExpObject::calculate_size_in_bytes(
            self.instructions.len(),
            self.num_capture_groups,
        )
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.escaped_pattern_source);

        for capture_group in self.capture_groups_as_slice_mut() {
            visitor.visit_pointer_opt(capture_group);
        }
    }
}
