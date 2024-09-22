use crate::js::common::error::print_error_message_and_exit;

use super::{
    builtin_function::BuiltinFunction,
    completion::EvalResult,
    function::get_argument,
    intrinsics::{
        native_error::{RangeError, ReferenceError, SyntaxError, TypeError, URIError},
        promise_prototype::perform_promise_then,
    },
    object_value::ObjectValue,
    promise_object::PromiseObject,
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
    EvalResult::Throw(syntax_error_value(cx, message))
}

pub fn type_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(type_error_value(cx, message))
}

pub fn reference_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(reference_error_value(cx, message))
}

pub fn range_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(range_error_value(cx, message))
}

pub fn uri_error<T>(cx: Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(uri_error_value(cx, message))
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

/// Prints the error passed as the first argument then exits the process.
pub fn print_eval_error_and_exit_runtime(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let error = get_argument(cx, arguments, 0);
    print_eval_error_and_exit(cx, error);

    cx.undefined().into()
}

/// Panics with a message referencing the error passed as the first argument.
pub fn panic_runtime(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let error = get_argument(cx, arguments, 0);
    let error_string = to_console_string(cx, error);

    panic!("{}", error_string);
}

/// Print an error to the console and exit if the provided promise rejects.
pub fn print_error_and_exit_if_rejects(cx: Context, promise: Handle<PromiseObject>) {
    let on_reject = BuiltinFunction::create(
        cx,
        print_eval_error_and_exit_runtime,
        1,
        cx.names.empty_string(),
        cx.current_realm(),
        None,
        None,
    );

    perform_promise_then(cx, promise, cx.undefined(), on_reject.into(), None);
}

/// Panic if the provided promise rejects.
pub fn panic_if_rejects(cx: Context, promise: Handle<PromiseObject>) {
    let on_reject = BuiltinFunction::create(
        cx,
        panic_runtime,
        1,
        cx.names.empty_string(),
        cx.current_realm(),
        None,
        None,
    );

    perform_promise_then(cx, promise, cx.undefined(), on_reject.into(), None);
}
