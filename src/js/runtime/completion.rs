use super::{gc::Gc, object_value::ObjectValue, value::Value};

/// 6.2.3 Completion Record
pub enum Completion {
    Normal(Option<Value>),
    Return(Value),
    Throw(Value),
    Break,
    Continue,
}

impl Completion {
    pub fn empty() -> Completion {
        Completion::Normal(None)
    }
}

impl From<Value> for Completion {
    #[inline]
    fn from(value: Value) -> Self {
        Completion::Normal(Some(value))
    }
}

impl From<bool> for Completion {
    #[inline]
    fn from(value: bool) -> Self {
        Completion::Normal(Some(Value::bool(value)))
    }
}

/// AbstractResult is for functions which are either sucessful or throw a value.
pub enum AbstractResult<T> {
    Ok(T),
    Throw(Value),
}

impl<T> From<T> for AbstractResult<T> {
    #[inline]
    fn from(value: T) -> Self {
        AbstractResult::Ok(value)
    }
}

impl From<Gc<ObjectValue>> for AbstractResult<Value> {
    #[inline]
    fn from(value: Gc<ObjectValue>) -> Self {
        AbstractResult::Ok(value.into())
    }
}

impl<T: Into<Completion>> From<AbstractResult<T>> for Completion {
    #[inline]
    fn from(value: AbstractResult<T>) -> Self {
        match value {
            AbstractResult::Ok(value) => value.into(),
            AbstractResult::Throw(value) => Completion::Throw(value),
        }
    }
}

/// Unwrap a Completion record, returning if abornmal
#[macro_export]
macro_rules! maybe {
    ($a:expr) => {
        match $a {
            Completion::Normal(value) => value,
            other => return other,
        }
    };
}

/// Unwrap a Completion record that must be normal
#[macro_export]
macro_rules! must {
    ($a:expr) => {
        match $a {
            Completion::Normal(value) => value,
            _ => panic!("Unexepcted abnormal completion"),
        }
    };
}

/// Unwrap an AbstractResult, returning if throw
#[macro_export]
macro_rules! maybe_ {
    ($a:expr) => {
        match $a {
            AbstractResult::Ok(value) => value,
            AbstractResult::Throw(value) => return AbstractResult::Throw(value),
        }
    };
}

/// Unwrap an AbstractResult that must never throw
#[macro_export]
macro_rules! must_ {
    ($a:expr) => {
        match $a {
            AbstractResult::Ok(value) => value,
            _ => panic!("Unexepcted abnormal completion"),
        }
    };
}

/// Unwrap an AbstractResult, returning a completion if throw
#[macro_export]
macro_rules! maybe__ {
    ($a:expr) => {
        match $a {
            AbstractResult::Ok(value) => value,
            AbstractResult::Throw(value) => return Completion::Throw(value),
        }
    };
}
