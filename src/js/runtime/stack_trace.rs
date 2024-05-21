use super::{
    abstract_operations::create_non_enumerable_data_property_or_throw,
    console::format_error_one_line, intrinsics::error_constructor::ErrorObject, realm, Context,
    Handle,
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
    // Stack trace starts with the error itself
    let mut stack_trace = format_error_one_line(cx, error.into());
    stack_trace.push('\n');

    // We may want to skip the first stack frame e.g. when in an error constructor
    let mut stack_frame_opt = if skip_current_frame {
        cx.vm().stack_frame().previous_frame()
    } else {
        Some(cx.vm().stack_frame())
    };

    while let Some(stack_frame) = stack_frame_opt {
        // Skip the last frame if it's a dummy frame for the realm
        if stack_frame.previous_frame().is_none() {
            let function = stack_frame.closure().function_ptr();
            if let Some(id) = function.rust_runtime_function_id() {
                if id == *cx.rust_runtime_functions.get_id(realm::empty).unwrap() {
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
        if let Some(file_name) = func.source_file_ptr().map(|file| file.name()) {
            stack_trace.push_str(&file_name.to_string());
        } else {
            stack_trace.push_str("<native>");
        }

        stack_trace.push_str(")\n");

        // Move to the parent stack frame
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
