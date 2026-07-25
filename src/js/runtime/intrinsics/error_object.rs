use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        Context, Handle, HeapPtr, Value,
        alloc_error::AllocResult,
        common_shapes::CommonShape,
        eval_result::EvalResult,
        gc::{HeapItem, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::ObjectValue,
        ordinary_object::ObjectBuilder,
        source_file::SourceFile,
        stack_trace::{StackFrameInfoArray, create_current_stack_frame_info, create_stack_trace},
        string_value::{FlatString, StringValue},
    },
    set_uninit,
};

extend_object! {
    pub struct ErrorObject {
        /// Cached stack trace, or the minimal information cached to lazily generate the stack trace
        /// when first accessed.
        stack_trace_state: StackTraceState,
        /// Whether this is a stack overflow error.
        is_stack_overflow: bool,
    }
}

/// The stack trace is lazily generated. Contains either the cached stack trace string itself or the
/// minimal stack frame information needed to generated the full stack trace.
enum StackTraceState {
    /// No stack frame info has been generated yet. This should only be true while the error has not
    /// been fully created.
    Uninitialized,
    /// Minimal stack frame info has been generated which can be lazily used to build the full stack
    /// trace.
    StackFrameInfo(HeapPtr<StackFrameInfoArray>),
    /// The full stack trace string has been generated and cached.
    Generated(CachedStackTraceInfo),
}

#[derive(Clone, Copy)]
pub struct CachedStackTraceInfo {
    /// The formatted list of stack frames.
    pub frames: HeapPtr<FlatString>,
    /// The source file, line, and column if available.
    pub source_file_line_col: Option<(HeapPtr<SourceFile>, usize, usize)>,
}

impl ErrorObject {
    pub fn new_with_message(
        cx: Context,
        shape: CommonShape,
        message: Handle<StringValue>,
        skip_current_frame: bool,
    ) -> AllocResult<Handle<ErrorObject>> {
        let mut error_object = ObjectBuilder::<ErrorObject>::new(cx)
            .common_shape(shape)?
            .build()?
            .to_handle();

        set_uninit!(error_object.is_stack_overflow, false);

        Self::initialize_stack_trace(cx, error_object, skip_current_frame)?;

        error_object
            .as_object()
            .init_properties(cx, &[message.as_value()])?;

        Ok(error_object)
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        prototype: Intrinsic,
        skip_current_frame: bool,
    ) -> EvalResult<Handle<ErrorObject>> {
        let mut error = ObjectBuilder::<ErrorObject>::new(cx)
            .constructor_proto(constructor, prototype)?
            .build()?
            .to_handle();

        set_uninit!(error.is_stack_overflow, false);

        Self::initialize_stack_trace(cx, error, skip_current_frame)?;

        Ok(error)
    }

    /// AggregateError Objects (https://tc39.es/ecma262/#sec-aggregate-error-objects)
    pub fn new_aggregate(cx: Context, errors: Handle<Value>) -> AllocResult<Handle<ErrorObject>> {
        let mut error_object = ObjectBuilder::<ErrorObject>::new(cx)
            .common_shape(CommonShape::AggregateError)?
            .build()?
            .to_handle();

        set_uninit!(error_object.is_stack_overflow, false);

        Self::initialize_stack_trace(cx, error_object, /* skip_current_frame */ true)?;

        error_object.as_object().init_properties(cx, &[errors])?;

        Ok(error_object)
    }

    fn initialize_stack_trace(
        cx: Context,
        mut error: Handle<ErrorObject>,
        skip_current_frame: bool,
    ) -> AllocResult<()> {
        // Initialize remaining state before collecting stack frame info, as we must ensure all
        // fields are initialized before a GC could potentially occur.
        set_uninit!(error.stack_trace_state, StackTraceState::Uninitialized);

        // Collect and cache the minimal stack frame info for the current stack trace
        let stack_frame_info = create_current_stack_frame_info(cx, skip_current_frame)?;
        error.stack_trace_state = StackTraceState::StackFrameInfo(stack_frame_info);

        Ok(())
    }

    pub fn is_stack_overflow(&self) -> bool {
        self.is_stack_overflow
    }

    pub fn set_is_stack_overflow(&mut self, is_stack_overflow: bool) {
        self.is_stack_overflow = is_stack_overflow;
    }
}

impl Handle<ErrorObject> {
    /// Return the stack trace for this error. Stack trace is lazily generated on first access.
    pub fn get_stack_trace(&mut self, cx: Context) -> AllocResult<CachedStackTraceInfo> {
        match self.stack_trace_state {
            StackTraceState::Generated(cached_stack_trace) => Ok(cached_stack_trace),
            StackTraceState::StackFrameInfo(stack_frame_info) => {
                let stack_trace = create_stack_trace(cx, stack_frame_info.to_handle())?;
                self.stack_trace_state = StackTraceState::Generated(stack_trace);
                Ok(stack_trace)
            }
            StackTraceState::Uninitialized => {
                panic!("Expected stack trace state to be initialized")
            }
        }
    }
}

impl HeapItem for ErrorObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<ErrorObject>()
    }

    fn visit_pointers(mut error_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        error_object.visit_object_pointers(visitor);

        match &mut error_object.stack_trace_state {
            StackTraceState::Uninitialized => {}
            StackTraceState::StackFrameInfo(stack_frame_info) => {
                visitor.visit_pointer(stack_frame_info);
            }
            StackTraceState::Generated(stack_trace) => {
                visitor.visit_pointer(&mut stack_trace.frames);

                if let Some((source_file, _, _)) = stack_trace.source_file_line_col.as_mut() {
                    visitor.visit_pointer(source_file);
                }
            }
        }
    }
}
