use super::{value::Value, Handle};

/// EvalResult is for functions which are either sucessful or throw a value.
pub type EvalResult<T> = Result<T, Handle<Value>>;

/// Unwrap an EvalResult that must never throw
#[macro_export]
macro_rules! must {
    ($a:expr) => {{
        let result = $a;
        match result {
            Ok(value) => value,
            _ => panic!("Unexpected abnormal completion"),
        }
    }};
}
