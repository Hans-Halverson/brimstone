use super::{
    completion::EvalResult,
    intrinsics::native_error::{RangeError, ReferenceError, SyntaxError, TypeError, URIError},
    string_value::{FlatString, StringValue},
    Context, Handle, HeapPtr, Value,
};

fn syntax_error_value(cx: Context, message: &str) -> Handle<Value> {
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

pub fn syntax_error_<T>(cx: Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(syntax_error_value(cx, message))
}

pub fn type_error_<T>(cx: Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(type_error_value(cx, message))
}

pub fn reference_error_<T>(cx: Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(reference_error_value(cx, message))
}

pub fn range_error_<T>(cx: Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(range_error_value(cx, message))
}

pub fn uri_error_<T>(cx: Context, message: &str) -> EvalResult<T> {
    EvalResult::Throw(uri_error_value(cx, message))
}

pub fn err_not_defined_<T>(cx: Context, name: Handle<StringValue>) -> EvalResult<T> {
    reference_error_(cx, &format!("{} is not defined", name))
}

pub fn err_assign_constant<T>(cx: Context, name: HeapPtr<FlatString>) -> EvalResult<T> {
    type_error_(cx, &format!("can't assign constant `{}`", name))
}

pub fn err_cannot_set_property<T>(cx: Context, name: impl std::fmt::Display) -> EvalResult<T> {
    type_error_(cx, &format!("can't set property {}", name))
}
