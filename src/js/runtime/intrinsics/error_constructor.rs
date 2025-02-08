use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        abstract_operations::{create_non_enumerable_data_property_or_throw, get, has_property},
        builtin_function::BuiltinFunction,
        eval_result::EvalResult,
        function::get_argument,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{object_create, object_create_from_constructor},
        realm::Realm,
        stack_trace::{create_current_stack_frame_info, create_stack_trace, StackFrameInfoArray},
        string_value::FlatString,
        type_utilities::to_string,
        Context, Handle, HeapPtr, Value,
    },
    set_uninit,
};

use super::intrinsics::Intrinsic;

extend_object! {
    pub struct ErrorObject {
        // Cached stack trace, or the minimal information cached to lazily generate the stack trace
        // when first accessed.
        stack_trace_state: StackTraceState,
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
    Generated(HeapPtr<FlatString>),
}

impl ErrorObject {
    pub fn new(cx: Context, prototype: Intrinsic, skip_current_frame: bool) -> Handle<ErrorObject> {
        let error =
            object_create::<ErrorObject>(cx, ObjectKind::ErrorObject, prototype).to_handle();

        Self::initialize_stack_trace(cx, error, skip_current_frame);

        error
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        prototype: Intrinsic,
        skip_current_frame: bool,
    ) -> EvalResult<Handle<ErrorObject>> {
        let error = object_create_from_constructor::<ErrorObject>(
            cx,
            constructor,
            ObjectKind::ErrorObject,
            prototype,
        )?
        .to_handle();

        Self::initialize_stack_trace(cx, error, skip_current_frame);

        Ok(error)
    }

    fn initialize_stack_trace(
        cx: Context,
        mut error: Handle<ErrorObject>,
        skip_current_frame: bool,
    ) {
        // Initialize remaining state before collecting stack frame info, as we must ensure all
        // fields are initialized before a GC could potentially occur.
        set_uninit!(error.stack_trace_state, StackTraceState::Uninitialized);

        // Collect and cache the minimal stack frame info for the current stack trace
        let stack_frame_info = create_current_stack_frame_info(cx, skip_current_frame);
        error.stack_trace_state = StackTraceState::StackFrameInfo(stack_frame_info);
    }
}

impl Handle<ErrorObject> {
    /// Return the stack trace for this error. Stack trace is lazily generated on first access.
    pub fn get_stack_trace(&mut self, cx: Context) -> Handle<FlatString> {
        match self.stack_trace_state {
            StackTraceState::Generated(stack_frame) => stack_frame.to_handle(),
            StackTraceState::StackFrameInfo(stack_frame_info) => {
                let stack_trace = create_stack_trace(cx, *self, stack_frame_info.to_handle());
                self.stack_trace_state = StackTraceState::Generated(stack_trace);
                stack_trace.to_handle()
            }
            StackTraceState::Uninitialized => {
                panic!("Expected stack trace state to be initialized")
            }
        }
    }
}

pub struct ErrorConstructor;

impl ErrorConstructor {
    /// Properties of the Error Constructor (https://tc39.es/ecma262/#sec-properties-of-the-error-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.error(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::ErrorPrototype).into(),
        );

        func
    }

    /// Error (https://tc39.es/ecma262/#sec-error-message)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            cx.current_function()
        };

        let object = ErrorObject::new_from_constructor(
            cx,
            new_target,
            Intrinsic::ErrorPrototype,
            /* skip_current_frame */ true,
        )?;

        let message = get_argument(cx, arguments, 0);
        if !message.is_undefined() {
            let message_string = to_string(cx, message)?;
            create_non_enumerable_data_property_or_throw(
                cx,
                object.into(),
                cx.names.message(),
                message_string.into(),
            );
        }

        let options_arg = get_argument(cx, arguments, 1);
        install_error_cause(cx, object, options_arg)?;

        Ok(object.as_value())
    }
}

/// InstallErrorCause (https://tc39.es/ecma262/#sec-installerrorcause)
pub fn install_error_cause(
    cx: Context,
    object: Handle<ErrorObject>,
    options: Handle<Value>,
) -> EvalResult<()> {
    if options.is_object() {
        let options = options.as_object();
        if has_property(cx, options, cx.names.cause())? {
            let cause = get(cx, options, cx.names.cause())?;
            create_non_enumerable_data_property_or_throw(
                cx,
                object.into(),
                cx.names.cause(),
                cause,
            );
        }
    }

    Ok(())
}

impl HeapObject for HeapPtr<ErrorObject> {
    fn byte_size(&self) -> usize {
        size_of::<ErrorObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);

        match &mut self.stack_trace_state {
            StackTraceState::Uninitialized => {}
            StackTraceState::StackFrameInfo(stack_frame_info) => {
                visitor.visit_pointer(stack_frame_info);
            }
            StackTraceState::Generated(stack_trace) => visitor.visit_pointer(stack_trace),
        }
    }
}
