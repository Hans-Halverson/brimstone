use crate::js::{
    common::error::FormatOptions,
    parser::{LocalizedParseError, LocalizedParseErrors},
};

use super::{
    bytecode::generator::EmitError,
    eval_result::EvalResult,
    intrinsics::native_error::{RangeError, ReferenceError, SyntaxError, TypeError, URIError},
    string_value::{FlatString, StringValue},
    to_console_string, Context, Handle, HeapPtr, Value,
};

/// Top level error type for the JS engine. Encapsulates all possible errors types that can occur
/// during parsing, analysis, or evaluation.
pub enum BsError {
    /// A single parse error can be returned from parsing.
    Parse(LocalizedParseError),
    /// Multiple parse errors can be returned from analysis.
    Analyze(LocalizedParseErrors),
    /// An error during bytecode generation.
    Emit(EmitError),
    /// Any value can be thrown as an error during evaluation.
    Eval(Handle<Value>),
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

impl From<Handle<Value>> for BsError {
    fn from(error: Handle<Value>) -> Self {
        BsError::Eval(error)
    }
}

impl BsError {
    pub fn format(&self, cx: Context, opts: &FormatOptions) -> String {
        match self {
            BsError::Parse(error) => error.format(opts),
            BsError::Analyze(errors) => errors.format(opts),
            BsError::Emit(error) => error.format(opts),
            BsError::Eval(value) => to_console_string(cx, *value, opts),
        }
    }
}

/// Generic result type from the JS engine.
pub type BsResult<T> = Result<T, BsError>;

pub fn syntax_error_value(cx: Context, message: &str) -> Handle<Value> {
    SyntaxError::new_with_message(cx, message.to_owned()).into()
}

pub fn type_error_value(cx: Context, message: &str) -> Handle<Value> {
    TypeError::new_with_message(cx, message.to_owned()).into()
}

fn reference_error_value(cx: Context, message: &str) -> Handle<Value> {
    ReferenceError::new_with_message(cx, message.to_owned()).into()
}

pub fn range_error_value(cx: Context, message: &str) -> Handle<Value> {
    RangeError::new_with_message(cx, message.to_owned()).into()
}

fn uri_error_value(cx: Context, message: &str) -> Handle<Value> {
    URIError::new_with_message(cx, message.to_owned()).into()
}

pub fn syntax_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    Err(syntax_error_value(cx, message))
}

pub fn type_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    Err(type_error_value(cx, message))
}

pub fn reference_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    Err(reference_error_value(cx, message))
}

pub fn range_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    Err(range_error_value(cx, message))
}

pub fn uri_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    Err(uri_error_value(cx, message))
}

pub fn err_not_defined<T>(cx: Context, name: Handle<StringValue>) -> EvalResult<T> {
    reference_error(cx, &format!("{} is not defined", name))
}

pub fn err_assign_constant<T>(cx: Context, name: HeapPtr<FlatString>) -> EvalResult<T> {
    type_error(cx, &format!("can't assign constant `{}`", name))
}

pub fn err_cannot_set_property<T>(cx: Context, name: impl std::fmt::Display) -> EvalResult<T> {
    type_error(cx, &format!("can't set property {}", name))
}

pub fn syntax_parse_error<T>(cx: Context, error: &LocalizedParseError) -> EvalResult<T> {
    // Need error string without "SyntaxError:" prefix, since prefix will be added when printing
    // the SyntaxError object.
    syntax_error(cx, &error.to_string_without_name())
}
