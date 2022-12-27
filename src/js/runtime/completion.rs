use super::{gc::Gc, object_value::ObjectValue, value::Value};

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
            AbstractResult::Throw(value) => Completion::throw(value),
        }
    }
}

/// Unwrap a Completion record, returning if abornmal
#[macro_export]
macro_rules! maybe {
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
macro_rules! must {
    ($completion:expr) => {
        if $completion.is_normal() {
            $completion.value()
        } else {
            panic!("Unexepcted abnormal completion")
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
            AbstractResult::Throw(value) => return Completion::throw(value),
        }
    };
}
