use crate::js::parser::loc::find_line_col_for_pos;

use super::{
    bytecode::{function::BytecodeFunction, source_map::BytecodeSourceMap},
    collections::BsArray,
    gc::{HandleScope, HeapObject, HeapVisitor},
    intrinsics::{error_constructor::CachedStackTraceInfo, rust_runtime::return_undefined},
    object_descriptor::ObjectKind,
    source_file::SourceFile,
    Context, Handle, HeapPtr,
};

/// An array of stack frame entries which contain the information necessary to construct a full
/// stack trace later if desired.
pub type StackFrameInfoArray = BsArray<HeapStackFrameInfo>;

pub struct HeapStackFrameInfo {
    /// The function executing in the stack frame.
    function: HeapPtr<BytecodeFunction>,
    /// Offset of the pc or saved return address in the bytecode. This will be used to report the
    /// source location in this stack frame.
    ///
    /// Set to 0 if there is no BytecodeSourceMap for this frame.
    bytecode_offset: usize,
}

struct StackFrameInfo {
    function: Handle<BytecodeFunction>,
    bytecode_offset: usize,
}

impl StackFrameInfo {
    fn to_heap(&self) -> HeapStackFrameInfo {
        HeapStackFrameInfo {
            function: *self.function,
            bytecode_offset: self.bytecode_offset,
        }
    }
}

/// Gather the stack frame information for each stack frame in the current stack.
fn gather_current_stack_frames(mut cx: Context, skip_current_frame: bool) -> Vec<StackFrameInfo> {
    let mut frames = vec![];

    // We may want to skip the first stack frame e.g. when in an error constructor
    let (mut stack_frame_opt, mut pc) = if skip_current_frame {
        let stack_frame = cx.vm().stack_frame();
        (stack_frame.previous_frame(), stack_frame.return_address())
    } else {
        (Some(cx.vm().stack_frame()), cx.vm().pc())
    };

    let stack_trace_top = cx.vm().stack_trace_top();

    while let Some(stack_frame) = stack_frame_opt {
        // Stop if we've reached the first stack frame that should not be included in the trace
        if Some(stack_frame) == stack_trace_top {
            break;
        }

        // Skip the last frame if it's a dummy frame for the realm
        if stack_frame.previous_frame().is_none() {
            let function = stack_frame.closure().function_ptr();
            if let Some(id) = function.rust_runtime_function_id() {
                if id == *cx.rust_runtime_functions.get_id(return_undefined).unwrap() {
                    break;
                }
            }
        }

        // Gather the function and bytecode offset if necessary
        let function = stack_frame.closure().function();
        let bytecode_offset = if function.source_map_ptr().is_some() {
            pc as usize - function.bytecode().as_ptr() as usize
        } else {
            0
        };

        frames.push(StackFrameInfo { function, bytecode_offset });

        // Move to the parent stack frame
        pc = stack_frame.return_address();
        stack_frame_opt = stack_frame.previous_frame();
    }

    frames
}

/// Create a StackFrameInfoArray for the current stack.
///
/// This contains the information necessary to construct a full stack trace for the error if the
/// `stack` getter is ever called.
pub fn create_current_stack_frame_info(
    cx: Context,
    skip_current_frame: bool,
) -> HeapPtr<StackFrameInfoArray> {
    HandleScope::new(cx, |cx| {
        let frames = gather_current_stack_frames(cx, skip_current_frame);

        let mut array =
            StackFrameInfoArray::new_uninit(cx, ObjectKind::StackFrameInfoArray, frames.len());

        for (i, frame) in frames.iter().enumerate() {
            array.as_mut_slice()[i] = frame.to_heap();
        }

        array
    })
}

/// Create the string representation of a strack trace for an error, given the stack frame info
/// cached from the time that the error was created.
pub fn create_stack_trace(
    mut cx: Context,
    stack_frame_info: Handle<StackFrameInfoArray>,
) -> CachedStackTraceInfo {
    // Do any preparatory work that may allocate
    prepare_for_stack_trace(cx, stack_frame_info);

    let mut stack_trace = String::new();
    let mut first_source_file_line_col = None;

    for (i, stack_frame) in stack_frame_info.as_slice().iter().enumerate() {
        // Each line of the stack trace starts indented
        stack_trace.push_str("  at ");

        // Followed by the name of the function
        let func = stack_frame.function;
        if let Some(name) = func.name() {
            stack_trace.push_str(&name.to_string());
        } else {
            stack_trace.push_str("<anonymous>");
        }

        stack_trace.push_str(" (");

        // Followed by the file where the function was defined
        if let Some(file_name) = func.source_file_ptr().map(|file| file.display_name()) {
            stack_trace.push_str(&file_name.to_string());
        } else {
            stack_trace.push_str("<native>");
        }

        // If we have source code positions for this function, add the line and column
        if let Some(source_map) = func.source_map_ptr() {
            // Map that bytecode offset to a source position using the source map
            let source_position =
                BytecodeSourceMap::get_source_position(source_map, stack_frame.bytecode_offset);

            if let Some(source_position) = source_position {
                // Get the line and column number for the source position. Will not allocate since
                // line offsets were already generated in `prepare_for_stack_trace`.
                let source_file = func.source_file_ptr().unwrap();
                let line_offsets = source_file.line_offsets_ptr_raw().unwrap();
                let (line, column) =
                    find_line_col_for_pos(source_position, line_offsets.as_slice());

                // Append the line and column to the function name
                stack_trace.push_str(&format!(":{}:{}", line, column));

                // Save the source position if it is for the first frame
                if i == 0 {
                    let source_file = func.source_file_ptr().unwrap().to_handle();
                    first_source_file_line_col = Some((source_file, line, column));
                }
            }
        }

        stack_trace.push(')');

        if i != stack_frame_info.len() - 1 {
            stack_trace.push('\n');
        }
    }

    let frames = cx.alloc_string_ptr(&stack_trace);
    let source_file_line_col =
        first_source_file_line_col.map(|(file, line, col)| (*file, line, col));

    CachedStackTraceInfo { frames, source_file_line_col }
}

/// Prepare for a stack trace to be formatted. Perform any allocations that will be needed, such as
/// calculating line offsets for all source files.
fn prepare_for_stack_trace(cx: Context, stack_frame_info: Handle<StackFrameInfoArray>) {
    // Handle is shared between iterations
    let mut source_file_handle: Handle<SourceFile> = Handle::empty(cx);

    // Generate the line offsets for all source files referenced in the stack trace. This may
    // allocate if line offsets have not been generated yet.
    for i in 0..stack_frame_info.len() {
        if let Some(source_file) = stack_frame_info.as_slice()[i].function.source_file_ptr() {
            source_file_handle.replace(source_file);
            source_file_handle.line_offsets_ptr(cx);
        }
    }
}

pub fn stack_frame_info_array_byte_size(stack_frame_array: HeapPtr<StackFrameInfoArray>) -> usize {
    StackFrameInfoArray::calculate_size_in_bytes(stack_frame_array.len())
}

pub fn stack_frame_info_array_visit_pointers(
    stack_frame_array: &mut HeapPtr<StackFrameInfoArray>,
    visitor: &mut impl HeapVisitor,
) {
    stack_frame_array.visit_pointers(visitor);

    for stack_frame in stack_frame_array.as_mut_slice() {
        visitor.visit_pointer(&mut stack_frame.function);
    }
}
