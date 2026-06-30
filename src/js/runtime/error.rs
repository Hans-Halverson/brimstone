use crate::runtime::intrinsics::error_object::ErrorObject;
use crate::{
    common::error::{ErrorFormatter, FormatOptions},
    eval_err,
    parser::{LocalizedParseError, LocalizedParseErrors},
    runtime::{
        Context, Handle, HeapPtr, PropertyKey, Value,
        alloc_error::{AllocError, AllocResult, format_oom_error_message},
        bytecode::generator::EmitError,
        eval_result::{EvalError, EvalResult},
        get,
        intrinsics::{
            error_constructor::new_heap_source_info,
            native_error::{RangeError, ReferenceError, SyntaxError, TypeError, URIError},
        },
        object_value::ObjectValue,
        stack_trace::create_stack_trace,
        string_value::{FlatString, StringValue},
        to_console_string,
    },
};

/// Top level error type for the JS engine. Encapsulates all possible error types that can occur
/// during parsing, analysis, or evaluation.
pub enum BsError {
    /// A single parse error can be returned from parsing.
    Parse(LocalizedParseError),
    /// Multiple parse errors can be returned from analysis.
    Analyze(LocalizedParseErrors),
    /// An error during bytecode generation.
    Emit(EmitError),
    /// Any value can be thrown as an error during evaluation.
    Eval(EvalError),
    /// Allocation error (e.g. out of memory).
    Alloc(AllocError),
}

impl From<LocalizedParseError> for BsError {
    fn from(error: LocalizedParseError) -> Self {
        BsError::Parse(error)
    }
}

impl From<LocalizedParseErrors> for BsError {
    fn from(errors: LocalizedParseErrors) -> Self {
        BsError::Analyze(errors)
    }
}

impl From<EmitError> for BsError {
    fn from(error: EmitError) -> Self {
        BsError::Emit(error)
    }
}

impl From<EvalError> for BsError {
    fn from(error: EvalError) -> Self {
        match error {
            EvalError::Value(_) => BsError::Eval(error),
            #[cfg(feature = "alloc_error")]
            EvalError::Alloc(err) => BsError::Alloc(err),
        }
    }
}

impl From<AllocError> for BsError {
    fn from(error: AllocError) -> Self {
        BsError::Alloc(error)
    }
}

impl BsError {
    pub fn format(&self, cx: Context, opts: &FormatOptions) -> String {
        match self {
            BsError::Parse(error) => error.format(opts),
            BsError::Analyze(errors) => errors.format(opts),
            BsError::Emit(error) => error.format(opts),
            BsError::Eval(EvalError::Value(error)) => format_eval_error_value(cx, *error, opts)
                .unwrap_or_else(|_| format_oom_error_message(opts)),
            BsError::Alloc(error) => error.format(opts),
            #[cfg(feature = "alloc_error")]
            BsError::Eval(EvalError::Alloc(error)) => error.format(opts),
        }
    }
}

fn format_eval_error_value(
    cx: Context,
    thrown_value: Handle<Value>,
    opts: &FormatOptions,
) -> AllocResult<String> {
    let thrown_value_string = to_console_string(cx, thrown_value, opts)?;

    if thrown_value.is::<ErrorObject>() {
        Ok(thrown_value_string)
    } else {
        format_thrown_non_error_value(cx, thrown_value, thrown_value_string, opts)
    }
}

/// Best effort attempt at formatting thrown non-error values.
/// - Include a stack trace if one was preserved in the VM
/// - Try to treat user-defined error types the same as native errors. Format them with their name
///   and message property if the thrown value is an object.
fn format_thrown_non_error_value(
    mut cx: Context,
    thrown_value: Handle<Value>,
    formatted_value: String,
    opts: &FormatOptions,
) -> AllocResult<String> {
    // Save preserved stack trace before potentially re-entering JS when fetching error properties
    let stack_trace_info = cx.vm().thrown_non_error_info().map(|info| info.to_handle());

    let mut error_name = "Thrown".to_string();
    let mut error_message = None;

    if thrown_value.is_object() {
        let thrown_object = thrown_value.as_object();

        if let Some(name) = safe_get_string_property(cx, thrown_object, cx.names.name()) {
            error_name = name;
        }

        if let Some(message) = safe_get_string_property(cx, thrown_object, cx.names.message()) {
            error_message = Some(message);
        }
    }

    let mut formatter = ErrorFormatter::new(error_name, opts);
    formatter.set_message(error_message.unwrap_or(formatted_value));

    if let Some(info) = stack_trace_info {
        let stack_trace = create_stack_trace(cx, info)?;

        formatter.set_stack_trace(stack_trace.frames.to_string());

        if let Some(source_info) = new_heap_source_info(cx, &stack_trace)? {
            formatter.set_source_info(source_info);
        }
    }

    Ok(formatter.build())
}

/// Get a string property of an object ignoring all types of failure.
fn safe_get_string_property(
    cx: Context,
    object: Handle<ObjectValue>,
    property_key: Handle<PropertyKey>,
) -> Option<String> {
    let Ok(value) = get(cx, object, property_key) else {
        return None;
    };

    if !value.is_string() {
        return None;
    }

    value.as_string().format().ok()
}

/// Generic result type from the JS engine.
pub type BsResult<T> = Result<T, BsError>;

pub fn syntax_error_value(cx: Context, message: &str) -> EvalResult<Handle<Value>> {
    Ok(SyntaxError::new_with_message(cx, message.to_owned())?.into())
}

pub fn type_error_value(cx: Context, message: &str) -> EvalResult<Handle<Value>> {
    Ok(TypeError::new_with_message(cx, message.to_owned())?.into())
}

fn reference_error_value(cx: Context, message: &str) -> EvalResult<Handle<Value>> {
    Ok(ReferenceError::new_with_message(cx, message.to_owned())?.into())
}

pub fn range_error_value(cx: Context, message: &str) -> EvalResult<Handle<Value>> {
    Ok(RangeError::new_with_message(cx, message.to_owned())?.into())
}

fn uri_error_value(cx: Context, message: &str) -> EvalResult<Handle<Value>> {
    Ok(URIError::new_with_message(cx, message.to_owned())?.into())
}

pub fn syntax_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    eval_err!(syntax_error_value(cx, message)?)
}

pub fn type_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    eval_err!(type_error_value(cx, message)?)
}

pub fn reference_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    eval_err!(reference_error_value(cx, message)?)
}

pub fn range_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    eval_err!(range_error_value(cx, message)?)
}

pub fn uri_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    eval_err!(uri_error_value(cx, message)?)
}

pub fn err_not_defined<T>(cx: Context, name: Handle<StringValue>) -> EvalResult<T> {
    reference_error(cx, &format!("`{}` is not defined", name.format()?))
}

pub fn err_assign_constant<T>(cx: Context, name: HeapPtr<FlatString>) -> EvalResult<T> {
    type_error(cx, &format!("cannot assign constant `{name}`"))
}

pub fn err_cannot_set_property<T>(cx: Context, name: impl std::fmt::Display) -> EvalResult<T> {
    type_error(cx, &format!("cannot set property `{name}`"))
}

pub fn syntax_parse_error<T>(cx: Context, error: &LocalizedParseError) -> EvalResult<T> {
    // Need error string without "SyntaxError:" prefix, since prefix will be added when printing
    // the SyntaxError object.
    syntax_error(cx, &error.to_string_without_name())
}

pub fn stack_overflow_error<T>(cx: Context) -> EvalResult<T> {
    // Mark special RangeError as stack overflow
    let mut error = RangeError::new_with_message(cx, "Stack Overflow".to_owned())?;
    error.set_is_stack_overflow(true);
    eval_err!(error.into())
}
