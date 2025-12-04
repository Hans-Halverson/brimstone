use crate::runtime::{gc::Escapable, Context};

use super::{value::Value, Handle};

/// Any JS value can be thrown as an evaluation error.
///
/// Wrap in a zero-cost newtype for type shenanigans.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct EvalError(Handle<Value>);

impl EvalError {
    #[inline]
    pub fn new(value: Handle<Value>) -> Self {
        EvalError(value)
    }
}

impl EvalError {
    #[inline]
    pub fn value(self) -> Handle<Value> {
        self.0
    }
}

impl AsRef<Handle<Value>> for EvalError {
    #[inline]
    fn as_ref(&self) -> &Handle<Value> {
        &self.0
    }
}

impl AsMut<Handle<Value>> for EvalError {
    #[inline]
    fn as_mut(&mut self) -> &mut Handle<Value> {
        &mut self.0
    }
}

impl Escapable for EvalError {
    #[inline]
    fn escape(&self, cx: Context) -> Self {
        EvalError::new(self.0.escape(cx))
    }
}

/// EvalResult is for functions which are either sucessful or throw a value.
pub type EvalResult<T> = Result<T, EvalError>;

/// Unwrap an EvalResult that must never throw
#[macro_export]
macro_rules! must {
    ($a:expr) => {{
        let result = $a;
        match result {
            $crate::runtime::eval_result::EvalResult::Ok(value) => value,
            _ => panic!("Unexpected abnormal completion"),
        }
    }};
}

/// Create a new EvalError from a JS value
#[macro_export]
macro_rules! eval_err {
    ($value:expr) => {
        Err($crate::runtime::eval_result::EvalError::new($value))
    };
}
