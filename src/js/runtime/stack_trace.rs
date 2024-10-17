use crate::js::parser::loc::find_line_col_for_pos;

use super::{
    abstract_operations::create_non_enumerable_data_property_or_throw,
    bytecode::source_map::BytecodeSourceMap,
    console::format_error_one_line,
    intrinsics::{error_constructor::ErrorObject, rust_runtime::return_undefined},
    source_file::SourceFile,
    Context, Handle,
};

/// Create the string representaton of the current stack trace and attach to an error object.
///
/// Only expected to be called when an error is constructed. Must be called after message has been
/// attached to the error.
pub fn attach_stack_trace_to_error(
    mut cx: Context,
    error: Handle<ErrorObject>,
    skip_current_frame: bool,
) {
    // Do any preparatory work that may allocate
    prepare_for_stack_trace(cx);

    // Stack trace starts with the error itself
    let mut stack_trace = format_error_one_line(cx, error.into());
    stack_trace.push('\n');

    // We may want to skip the first stack frame e.g. when in an error constructor
    let (mut stack_frame_opt, mut pc) = if skip_current_frame {
        let stack_frame = cx.vm().stack_frame();
        (stack_frame.previous_frame(), stack_frame.return_address())
    } else {
        (Some(cx.vm().stack_frame()), cx.vm().pc())
    };

    while let Some(stack_frame) = stack_frame_opt {
        // Skip the last frame if it's a dummy frame for the realm
        if stack_frame.previous_frame().is_none() {
            let function = stack_frame.closure().function_ptr();
            if let Some(id) = function.rust_runtime_function_id() {
                if id == *cx.rust_runtime_functions.get_id(return_undefined).unwrap() {
                    break;
                }
            }
        }

        // Each line of the stack trace starts indented
        stack_trace.push_str("  at ");

        // Followed by the name of the function
        let func = stack_frame.closure().function();
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
            // First find the bytecode offset of the pc or saved return address
            let bytecode_offset = pc as usize - func.bytecode().as_ptr() as usize;

            // Map that bytecode offset to a source position using the source map
            let source_position =
                BytecodeSourceMap::get_source_position(source_map, bytecode_offset);

            if let Some(source_position) = source_position {
                // Get the line and column number for the source position. Will not allocate since
                // line offsets were already generated in `prepare_for_stack_trace`.
                let source_file = func.source_file_ptr().unwrap();
                let line_offsets = source_file.line_offsets_ptr_raw().unwrap();
                let (line, column) =
                    find_line_col_for_pos(source_position, line_offsets.as_slice());

                // Append the line and column to the function name
                stack_trace.push_str(&format!(":{}:{}", line, column));
            }
        }

        stack_trace.push_str(")\n");

        // Move to the parent stack frame
        pc = stack_frame.return_address();
        stack_frame_opt = stack_frame.previous_frame();
    }

    // Attach the stack trace string to the error object itself
    let stack_trace_string = cx.alloc_string(&stack_trace);
    create_non_enumerable_data_property_or_throw(
        cx,
        error.into(),
        cx.names.stack(),
        stack_trace_string.into(),
    )
}

/// Prepare for a stack trace to be formatted. Perform any allocations that will be needed, such as
/// calculating line offsets for all source files.
fn prepare_for_stack_trace(mut cx: Context) {
    // Handle is shared between iterations
    let mut source_file_handle: Handle<SourceFile> = Handle::empty(cx);

    // Generate the line offsets for all source files referenced in the stack trace. This may
    // allocate if line offsets have not been generated yet.
    for stack_frame in cx.vm().stack_frame().iter() {
        if let Some(source_file) = stack_frame.closure().function_ptr().source_file_ptr() {
            source_file_handle.replace(source_file);
            source_file_handle.line_offsets_ptr(cx);
        }
    }
}
