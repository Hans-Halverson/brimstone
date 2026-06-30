use std::mem::size_of;

use crate::{
    common::{graphviz::DotGraphBuilder, math::round_to_power_of_two},
    field_offset,
    parser::regexp::{RegExp, RegExpFlags},
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr,
        alloc_error::AllocResult,
        bytecode::generator::alloc_wtf8_str_from_source,
        collections::InlineArray,
        debug_print::{DebugPrint, DebugPrinter},
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemDescriptor,
        regexp::{graphviz::compiled_regexp_to_dot_graph, instruction::InstructionIterator},
        string_value::{FlatString, StringValue},
    },
    set_uninit,
};

#[repr(C)]
pub struct CompiledRegExp {
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

const INSTRUCTIONS_BYTE_OFFSET: usize = field_offset!(CompiledRegExp, instructions);

impl CompiledRegExp {
    pub fn new(
        cx: Context,
        instructions: Vec<u32>,
        regexp: &RegExp,
        escaped_pattern_source: Handle<StringValue>,
        num_progress_points: u32,
        num_loop_registers: u32,
    ) -> AllocResult<Handle<CompiledRegExp>> {
        let num_capture_groups = regexp.capture_groups.len() as u32;
        let mut has_named_capture_groups = false;

        let mut capture_group_handles = vec![];
        for capture_group in regexp.capture_groups.iter() {
            let handle = if let Some(name_string) = capture_group {
                has_named_capture_groups = true;
                Some(alloc_wtf8_str_from_source(cx, name_string)?)
            } else {
                None
            };

            capture_group_handles.push(handle);
        }

        let size = Self::calculate_size_in_bytes(instructions.len(), num_capture_groups);
        let mut object = cx.alloc_uninit_with_size::<CompiledRegExp>(size)?;

        set_uninit!(object.descriptor, cx.descriptors.get(HeapItemKind::CompiledRegExp));
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

        Ok(object.to_handle())
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

impl HeapPtr<CompiledRegExp> {
    pub fn to_dot_graph(&self) -> DotGraphBuilder {
        compiled_regexp_to_dot_graph(*self)
    }
}

impl DebugPrint for HeapPtr<CompiledRegExp> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        let source = format!("/{}/", self.escaped_pattern_source().format().unwrap_or_default());
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

impl HeapItem for CompiledRegExp {
    fn byte_size(compiled_regexp: HeapPtr<Self>) -> usize {
        CompiledRegExp::calculate_size_in_bytes(
            compiled_regexp.instructions.len(),
            compiled_regexp.num_capture_groups,
        )
    }

    fn visit_pointers(mut compiled_regexp: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut compiled_regexp.descriptor);
        visitor.visit_pointer(&mut compiled_regexp.escaped_pattern_source);

        for capture_group in compiled_regexp.capture_groups_as_slice_mut() {
            visitor.visit_pointer_opt(capture_group);
        }
    }
}
