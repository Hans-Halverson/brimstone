use crate::runtime::{gc::Escapable, Context};

use super::{value::Value, Handle};

#[cfg(feature = "alloc_error")]
use super::alloc_error::AllocError;

/// Any JS value can be thrown as an evaluation error.
///
/// Wrap in a zero-cost newtype for type shenanigans.
#[derive(Clone, Copy)]
pub enum EvalError {
    /// A thrown error JS value.
    Value(Handle<Value>),
    /// An allocation error.
    #[cfg(feature = "alloc_error")]
    Alloc(AllocError),
}

impl EvalError {
    #[inline]
    pub fn new_value(value: Handle<Value>) -> Self {
        EvalError::Value(value)
    }

    #[cfg(feature = "alloc_error")]
    #[inline]
    pub fn new_alloc(err: AllocError) -> Self {
        EvalError::Alloc(err)
    }
}

impl Escapable for EvalError {
    #[inline]
    fn escape(&self, cx: Context) -> Self {
        match self {
            Self::Value(value) => Self::Value(value.escape(cx)),
            #[cfg(feature = "alloc_error")]
            Self::Alloc(err) => Self::Alloc(err.escape(cx)),
        }
    }
}

/// EvalResult is for functions which are either sucessful or throw a value.
pub type EvalResult<T> = Result<T, EvalError>;

/// Unwrap an EvalResult that must never throw
#[macro_export]
macro_rules! must {
    ($a:expr) => {{
        use $crate::runtime::eval_result::{EvalError, EvalResult};

        let result = $a;
        match result {
            EvalResult::Ok(value) => value,
            // Propagate OOMs upwards instead of failing the assertion
            #[cfg(feature = "alloc_error")]
            Err(EvalError::Alloc(alloc_err)) => {
                return Err(alloc_err.into());
            }
            // A thrown value. Propagate upwards only if it is a stack overflow, otherwise fail
            // the assertion.
            Err(EvalError::Value(value)) => {
                if value.is_object() {
                    if let Some(error) = value.as_object().as_error() {
                        if error.is_stack_overflow() {
                            return Err(EvalError::Value(value.into()));
                        }
                    }
                }

                panic!("Unexpected abnormal completion")
            }
        }
    }};
}

/// Unwrap an EvalResult that must never throw inside an AllocResult
#[macro_export]
macro_rules! must_a {
    ($a:expr) => {{
        use $crate::runtime::eval_result::{EvalError, EvalResult};

        let result = $a;
        match result {
            EvalResult::Ok(value) => value,
            // Propagate OOMs upwards instead of failing the assertion
            #[cfg(feature = "alloc_error")]
            Err(EvalError::Alloc(alloc_err)) => {
                return Err(alloc_err.into());
            }
            // Fail assertion on any thrown value including stack overflows
            Err(EvalError::Value(_)) => {
                panic!("Unexpected abnormal completion")
            }
        }
    }};
}

/// Create a new EvalError from a JS value
#[macro_export]
macro_rules! eval_err {
    ($value:expr) => {
        Err($crate::runtime::eval_result::EvalError::new_value($value))
    };
}

#[macro_export]
macro_rules! completion_value {
    ($result:expr) => {
        match $result {
            Ok(value) => Ok(value),
            Err($crate::runtime::eval_result::EvalError::Value(value)) => Err(value),
            #[cfg(feature = "alloc_error")]
            Err($crate::runtime::eval_result::EvalError::Alloc(err)) => return Err(err.into()),
        }
    };
}
