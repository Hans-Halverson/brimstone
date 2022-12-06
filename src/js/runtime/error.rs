use super::{
    completion::{AbstractResult, Completion},
    value::Value,
};

fn type_error_value(message: &str) -> Value {
    Value::String(Box::new(format!("TypeError: {}", message)))
}

fn reference_error_value(message: &str) -> Value {
    Value::String(Box::new(format!("ReferenceError: {}", message)))
}

pub fn type_error(message: &str) -> Completion {
    Completion::Throw(reference_error_value(message))
}

pub fn reference_error(message: &str) -> Completion {
    Completion::Throw(type_error_value(message))
}

pub fn type_error_<T>(message: &str) -> AbstractResult<T> {
    AbstractResult::Throw(type_error_value(message))
}

pub fn reference_error_<T>(message: &str) -> AbstractResult<T> {
    AbstractResult::Throw(reference_error_value(message))
}

pub fn err_not_defined_<T>(name: &str) -> AbstractResult<T> {
    reference_error_(&format!("{} is not defined", name))
}

pub fn err_uninitialized_<T>(name: &str) -> AbstractResult<T> {
    reference_error_(&format!("{} is not defined", name))
}
