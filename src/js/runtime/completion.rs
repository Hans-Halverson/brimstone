use super::{
    gc::Escapable,
    object_value::ObjectValue,
    string_value::StringValue,
    value::{BigIntValue, SymbolValue, Value},
    Context, Handle, HeapPtr,
};

/// EvalResult is for functions which are either sucessful or throw a value.
#[must_use]
pub enum EvalResult<T> {
    Ok(T),
    Throw(Handle<Value>),
}

impl<T> EvalResult<T> {
    pub fn into_rust_result(self) -> Result<T, Handle<Value>> {
        match self {
            EvalResult::Ok(value) => Ok(value),
            EvalResult::Throw(value) => Err(value),
        }
    }
}

impl<T: Clone> Clone for EvalResult<T> {
    fn clone(&self) -> Self {
        match self {
            EvalResult::Ok(ok) => EvalResult::Ok(ok.clone()),
            EvalResult::Throw(throw) => EvalResult::Throw(*throw),
        }
    }
}

impl<T> From<T> for EvalResult<T> {
    #[inline]
    fn from(value: T) -> Self {
        EvalResult::Ok(value)
    }
}

impl From<bool> for EvalResult<Value> {
    #[inline]
    fn from(value: bool) -> Self {
        EvalResult::Ok(value.into())
    }
}

impl From<f64> for EvalResult<Value> {
    #[inline]
    fn from(value: f64) -> Self {
        EvalResult::Ok(value.into())
    }
}

impl From<HeapPtr<StringValue>> for EvalResult<Value> {
    #[inline]
    fn from(value: HeapPtr<StringValue>) -> Self {
        EvalResult::Ok(value.into())
    }
}

impl From<HeapPtr<SymbolValue>> for EvalResult<Value> {
    #[inline]
    fn from(value: HeapPtr<SymbolValue>) -> Self {
        EvalResult::Ok(value.into())
    }
}

impl From<HeapPtr<BigIntValue>> for EvalResult<Value> {
    #[inline]
    fn from(value: HeapPtr<BigIntValue>) -> Self {
        EvalResult::Ok(value.into())
    }
}

impl<T: Into<Handle<ObjectValue>>> From<T> for EvalResult<Handle<Value>> {
    #[inline]
    fn from(value: T) -> Self {
        EvalResult::Ok(value.into().into())
    }
}

impl<T: Escapable> Escapable for EvalResult<T> {
    #[inline]
    fn escape(&self, cx: Context) -> Self {
        match self {
            EvalResult::Ok(ok) => EvalResult::Ok(ok.escape(cx)),
            EvalResult::Throw(thrown) => EvalResult::Throw(thrown.escape(cx)),
        }
    }
}

/// Unwrap an EvalResult, returning if throw
#[macro_export]
macro_rules! maybe {
    ($a:expr) => {{
        let result = $a;
        match result {
            EvalResult::Ok(value) => value,
            EvalResult::Throw(value) => return EvalResult::Throw(value),
        }
    }};
}

/// Unwrap an EvalResult that must never throw
#[macro_export]
macro_rules! must {
    ($a:expr) => {{
        let result = $a;
        match result {
            EvalResult::Ok(value) => value,
            _ => panic!("Unexpected abnormal completion"),
        }
    }};
}
