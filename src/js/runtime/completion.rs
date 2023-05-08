use crate::js::parser::ast::LabelId;

use super::{
    gc::HandleValue,
    object_value::ObjectValue,
    string_value::StringValue,
    value::{BigIntValue, SymbolValue, Value},
    Context, HeapPtr,
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

#[derive(Clone)]
pub struct Completion {
    kind: CompletionKind,
    label: LabelId,
    value: HandleValue,
}

pub const EMPTY_LABEL: u16 = 0;

impl Completion {
    #[inline]
    pub const fn normal(value: HandleValue) -> Completion {
        Completion { kind: CompletionKind::Normal, label: EMPTY_LABEL, value }
    }

    #[inline]
    pub const fn throw(value: HandleValue) -> Completion {
        Completion { kind: CompletionKind::Throw, label: EMPTY_LABEL, value }
    }

    #[inline]
    pub const fn return_(value: HandleValue) -> Completion {
        Completion { kind: CompletionKind::Return, label: EMPTY_LABEL, value }
    }

    #[inline]
    pub fn break_(cx: &mut Context, label: LabelId) -> Completion {
        Completion { kind: CompletionKind::Break, label, value: cx.empty() }
    }

    #[inline]
    pub fn continue_(cx: &mut Context, label: LabelId) -> Completion {
        Completion { kind: CompletionKind::Continue, label, value: cx.empty() }
    }

    #[inline]
    pub fn empty(cx: &mut Context) -> Completion {
        Completion::normal(cx.empty())
    }

    #[inline]
    pub fn kind(&self) -> CompletionKind {
        self.kind
    }

    #[inline]
    pub fn value(&self) -> HandleValue {
        self.value
    }

    #[inline]
    pub fn label(&self) -> LabelId {
        self.label
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
    pub fn update_if_empty(mut self, value: HandleValue) -> Completion {
        if self.is_empty() {
            self.value = value;
        }

        self
    }

    /// Convert a completion into an EvalResult, panicking if the completion is a non-throw
    /// abnormal completion. This is only safe to call when the completion must be normal or throw,
    /// such as for expression evaluation.
    #[inline]
    pub fn into_eval_result(&self) -> EvalResult<HandleValue> {
        match self.kind() {
            CompletionKind::Normal => EvalResult::Ok(self.value()),
            CompletionKind::Throw => EvalResult::Throw(self.value()),
            CompletionKind::Return | CompletionKind::Break | CompletionKind::Continue => {
                unreachable!("")
            }
        }
    }
}

impl<T: Into<HandleValue>> From<T> for Completion {
    #[inline]
    fn from(value: T) -> Self {
        Completion::normal(value.into())
    }
}

/// EvalResult is for functions which are either sucessful or throw a value.
pub enum EvalResult<T> {
    Ok(T),
    Throw(HandleValue),
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

impl<T: Into<HeapPtr<ObjectValue>>> From<T> for EvalResult<Value> {
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

/// Unwrap a Completion record, returning if abornmal
#[macro_export]
macro_rules! maybe_ {
    ($expr:expr) => {{
        let completion = $expr;
        if completion.is_normal() {
            completion.value()
        } else {
            return completion;
        }
    }};
}

/// Unwrap a Completion record that must be normal
#[macro_export]
macro_rules! must_ {
    ($expr:expr) => {{
        let completion = $expr;
        if completion.is_normal() {
            completion.value()
        } else {
            panic!("Unexepcted abnormal completion")
        }
    }};
}

/// Unwrap an EvalResult, returning a completion if throw
#[macro_export]
macro_rules! maybe__ {
    ($a:expr) => {{
        let result = $a;
        match result {
            EvalResult::Ok(value) => value,
            EvalResult::Throw(value) => return Completion::throw(value),
        }
    }};
}
