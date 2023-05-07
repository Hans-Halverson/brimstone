use super::{
    completion::{Completion, EvalResult},
    gc::HandleValue,
    intrinsics::native_error::{RangeError, ReferenceError, SyntaxError, TypeError},
    string_value::StringValue,
    Context, Handle,
};

fn syntax_error_value(cx: &mut Context, message: &str) -> HandleValue {
    SyntaxError::new_with_message(cx, message.to_owned()).into()
}

fn type_error_value(cx: &mut Context, message: &str) -> HandleValue {
    TypeError::new_with_message(cx, message.to_owned()).into()
}

fn reference_error_value(cx: &mut Context, message: &str) -> HandleValue {
    ReferenceError::new_with_message(cx, message.to_owned()).into()
}

fn range_error_value(cx: &mut Context, message: &str) -> HandleValue {
    RangeError::new_with_message(cx, message.to_owned()).into()
}

pub fn type_error(cx: &mut Context, message: &str) -> Completion {
    Completion::throw(type_error_value(cx, message))
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

pub fn range_error_<T>(cx: &mut Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(range_error_value(cx, message))
}

pub fn err_not_defined_<T>(cx: &mut Context, name: Handle<StringValue>) -> EvalResult<T> {
    reference_error_(cx, &format!("{} is not defined", name))
}

pub fn err_uninitialized_<T>(cx: &mut Context, name: Handle<StringValue>) -> EvalResult<T> {
    reference_error_(cx, &format!("{} is not initialized", name))
}
