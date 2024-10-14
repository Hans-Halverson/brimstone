use crate::js::{common::error::print_error_message_and_exit, parser::LocalizedParseError};

use super::{
    eval_result::EvalResult,
    intrinsics::native_error::{RangeError, ReferenceError, SyntaxError, TypeError, URIError},
    string_value::{FlatString, StringValue},
    to_console_string, Context, Handle, HeapPtr, Value,
};

pub fn syntax_error_value(cx: Context, message: &str) -> Handle<Value> {
    SyntaxError::new_with_message(cx, message.to_owned()).into()
}

pub fn type_error_value(cx: Context, message: &str) -> Handle<Value> {
    TypeError::new_with_message(cx, message.to_owned()).into()
}

fn reference_error_value(cx: Context, message: &str) -> Handle<Value> {
    ReferenceError::new_with_message(cx, message.to_owned()).into()
}

fn range_error_value(cx: Context, message: &str) -> Handle<Value> {
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

pub fn print_eval_error_and_exit(cx: Context, error: Handle<Value>) {
    let error_string = to_console_string(cx, error);
    print_error_message_and_exit(&error_string);
}

pub fn syntax_parse_error<T>(cx: Context, error: &LocalizedParseError) -> EvalResult<T> {
    // Need error string without "SyntaxError:" prefix, since prefix will be added when printing
    // the SyntaxError object.
    syntax_error(cx, &error.to_string_without_name())
}
