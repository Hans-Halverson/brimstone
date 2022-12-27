use super::{
    completion::{AbstractResult, Completion},
    value::Value,
    Context,
};

fn syntax_error_value(cx: &mut Context, message: &str) -> Value {
    cx.heap
        .alloc_string(format!("SyntaxError: {}", message))
        .into()
}

fn type_error_value(cx: &mut Context, message: &str) -> Value {
    cx.heap
        .alloc_string(format!("TypeError: {}", message))
        .into()
}

fn reference_error_value(cx: &mut Context, message: &str) -> Value {
    cx.heap
        .alloc_string(format!("ReferenceError: {}", message))
        .into()
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

pub fn syntax_error_<T>(cx: &mut Context, message: &str) -> AbstractResult<T> {
    AbstractResult::Throw(syntax_error_value(cx, message))
}

pub fn type_error_<T>(cx: &mut Context, message: &str) -> AbstractResult<T> {
    AbstractResult::Throw(type_error_value(cx, message))
}

pub fn reference_error_<T>(cx: &mut Context, message: &str) -> AbstractResult<T> {
    AbstractResult::Throw(reference_error_value(cx, message))
}

pub fn err_not_defined_<T>(cx: &mut Context, name: &str) -> AbstractResult<T> {
    reference_error_(cx, &format!("{} is not defined", name))
}

pub fn err_uninitialized_<T>(cx: &mut Context, name: &str) -> AbstractResult<T> {
    reference_error_(cx, &format!("{} is not initialized", name))
}
