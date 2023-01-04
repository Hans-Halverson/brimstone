use super::{
    gc::Gc,
    object_value::ObjectValue,
    value::{StringValue, Value},
};

/// 6.2.3 Completion Record
#[derive(Clone, Copy, PartialEq)]
pub enum CompletionKind {
    Normal,
    Return,
    Throw,
    Break,
    Continue,
}

pub struct Completion {
    kind: CompletionKind,
    label: u16,
    value: Value,
}

const EMPTY_LABEL: u16 = 0;

impl Completion {
    #[inline]
    pub const fn normal(value: Value) -> Completion {
        Completion {
            kind: CompletionKind::Normal,
            label: EMPTY_LABEL,
            value,
        }
    }

    #[inline]
    pub const fn throw(value: Value) -> Completion {
        Completion {
            kind: CompletionKind::Throw,
            label: EMPTY_LABEL,
            value,
        }
    }

    #[inline]
    pub const fn return_(value: Value) -> Completion {
        Completion {
            kind: CompletionKind::Return,
            label: EMPTY_LABEL,
            value,
        }
    }

    #[inline]
    pub const fn empty() -> Completion {
        Completion::normal(Value::empty())
    }

    #[inline]
    pub fn kind(&self) -> CompletionKind {
        self.kind
    }

    #[inline]
    pub fn value(&self) -> Value {
        self.value
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.value.is_empty()
    }

    #[inline]
    pub fn is_normal(&self) -> bool {
        self.kind == CompletionKind::Normal
    }

    // 6.2.3.4 UpdateEmpty
    #[inline]
    pub fn update_if_empty(mut self, value: Value) -> Completion {
        if self.is_empty() {
            self.value = value;
        }

        self
    }
}

impl<T: Into<Value>> From<T> for Completion {
    #[inline]
    fn from(value: T) -> Self {
        Completion::normal(value.into())
    }
}

/// EvalResult is for functions which are either sucessful or throw a value.
pub enum EvalResult<T> {
    Ok(T),
    Throw(Value),
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

impl From<Gc<StringValue>> for EvalResult<Value> {
    #[inline]
    fn from(value: Gc<StringValue>) -> Self {
        EvalResult::Ok(value.into())
    }
}

impl<T: Into<Gc<ObjectValue>>> From<T> for EvalResult<Value> {
    #[inline]
    fn from(value: T) -> Self {
        EvalResult::Ok(value.into().into())
    }
}

impl<T: Into<Completion>> From<EvalResult<T>> for Completion {
    #[inline]
    fn from(value: EvalResult<T>) -> Self {
        match value {
            EvalResult::Ok(value) => value.into(),
            EvalResult::Throw(value) => Completion::throw(value),
        }
    }
}

/// Unwrap an EvalResult, returning if throw
#[macro_export]
macro_rules! maybe {
    ($a:expr) => {
        match $a {
            EvalResult::Ok(value) => value,
            EvalResult::Throw(value) => return EvalResult::Throw(value),
        }
    };
}

/// Unwrap an EvalResult that must never throw
#[macro_export]
macro_rules! must {
    ($a:expr) => {
        match $a {
            EvalResult::Ok(value) => value,
            _ => panic!("Unexepcted abnormal completion"),
        }
    };
}

/// Unwrap a Completion record, returning if abornmal
#[macro_export]
macro_rules! maybe_ {
    ($completion:expr) => {
        if $completion.is_normal() {
            $completion.value()
        } else {
            return $completion;
        }
    };
}

/// Unwrap a Completion record that must be normal
#[macro_export]
macro_rules! must_ {
    ($completion:expr) => {
        if $completion.is_normal() {
            $completion.value()
        } else {
            panic!("Unexepcted abnormal completion")
        }
    };
}

/// Unwrap an EvalResult, returning a completion if throw
#[macro_export]
macro_rules! maybe__ {
    ($a:expr) => {
        match $a {
            EvalResult::Ok(value) => value,
            EvalResult::Throw(value) => return Completion::throw(value),
        }
    };
}
