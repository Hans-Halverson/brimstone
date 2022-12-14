use super::{
    completion::{Completion, EvalResult},
    intrinsics::native_error::{ReferenceError, SyntaxError, TypeError},
    value::Value,
    Context,
};

fn syntax_error_value(cx: &mut Context, message: &str) -> Value {
    SyntaxError::new_with_message(cx, message.to_owned()).into()
}

fn type_error_value(cx: &mut Context, message: &str) -> Value {
    TypeError::new_with_message(cx, message.to_owned()).into()
}

fn reference_error_value(cx: &mut Context, message: &str) -> Value {
    ReferenceError::new_with_message(cx, message.to_owned()).into()
}

pub fn syntax_error(cx: &mut Context, message: &str) -> Completion {
    Completion::throw(syntax_error_value(cx, message))
}

pub fn type_error(cx: &mut Context, message: &str) -> Completion {
    Completion::throw(type_error_value(cx, message))
}

pub fn reference_error(cx: &mut Context, message: &str) -> Completion {
    Completion::throw(reference_error_value(cx, message))
}

pub fn syntax_error_<T>(cx: &mut Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(syntax_error_value(cx, message))
}

pub fn type_error_<T>(cx: &mut Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(type_error_value(cx, message))
}

pub fn reference_error_<T>(cx: &mut Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(reference_error_value(cx, message))
}

pub fn err_not_defined_<T>(cx: &mut Context, name: &str) -> EvalResult<T> {
    reference_error_(cx, &format!("{} is not defined", name))
}

pub fn err_uninitialized_<T>(cx: &mut Context, name: &str) -> EvalResult<T> {
    reference_error_(cx, &format!("{} is not initialized", name))
}
