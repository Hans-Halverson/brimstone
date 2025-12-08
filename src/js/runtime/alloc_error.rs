use std::fmt;

use crate::{
    common::error::{ErrorFormatter, FormatOptions},
    runtime::{bytecode::generator::EmitError, eval_result::EvalError, gc::Escapable, Context},
};

pub type AllocResult<T> = Result<T, AllocError>;

#[derive(Clone, Copy)]
pub enum AllocError {
    #[cfg(feature = "alloc_error")]
    Oom(()),
    #[cfg(all(not(feature = "alloc_error"), feature = "nightly"))]
    Oom(!),
    #[cfg(all(not(feature = "alloc_error"), not(feature = "nightly")))]
    Oom(()),
}

impl AllocError {
    #[cfg(feature = "alloc_error")]
    pub fn oom() -> Self {
        AllocError::Oom(())
    }
}

// Automatically convert AllocResult to an EvalResult
impl From<AllocError> for EvalError {
    fn from(_err: AllocError) -> Self {
        #[cfg(feature = "alloc_error")]
        {
            EvalError::Alloc(_err)
        }

        #[cfg(not(feature = "alloc_error"))]
        {
            unreachable!()
        }
    }
}

// Automatically convert AllocResult to an EmitResult
impl From<AllocError> for EmitError {
    fn from(err: AllocError) -> Self {
        EmitError::Alloc(err)
    }
}

impl Escapable for AllocError {
    #[inline]
    fn escape(&self, _: Context) -> Self {
        *self
    }
}

pub fn format_oom_error_message(opts: &FormatOptions) -> String {
    let mut formatter = ErrorFormatter::new("Error".to_string(), opts);
    formatter.set_message("Ran out of heap memory".to_owned());
    formatter.build()
}

impl AllocError {
    pub fn format(&self, opts: &FormatOptions) -> String {
        match self {
            Self::Oom(_) => format_oom_error_message(opts),
        }
    }
}

impl fmt::Display for AllocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Oom(_) => write!(f, "Ran out of heap memory"),
        }
    }
}

impl fmt::Debug for AllocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <AllocError as fmt::Display>::fmt(self, f)
    }
}
