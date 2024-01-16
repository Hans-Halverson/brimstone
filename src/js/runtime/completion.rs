use crate::js::parser::ast::LabelId;

use super::{
    gc::Escapable,
    object_value::ObjectValue,
    string_value::StringValue,
    value::{BigIntValue, SymbolValue, Value},
    Context, Handle, HeapPtr,
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
    value: Handle<Value>,
}

pub const EMPTY_LABEL: u16 = 0;

impl Completion {
    #[inline]
    pub const fn normal(value: Handle<Value>) -> Completion {
        Completion { kind: CompletionKind::Normal, label: EMPTY_LABEL, value }
    }

    #[inline]
    pub const fn throw(value: Handle<Value>) -> Completion {
        Completion { kind: CompletionKind::Throw, label: EMPTY_LABEL, value }
    }

    #[inline]
    pub const fn return_(value: Handle<Value>) -> Completion {
        Completion { kind: CompletionKind::Return, label: EMPTY_LABEL, value }
    }

    #[inline]
    pub fn break_(cx: Context, label: LabelId) -> Completion {
        Completion { kind: CompletionKind::Break, label, value: cx.empty() }
    }

    #[inline]
    pub fn continue_(cx: Context, label: LabelId) -> Completion {
        Completion { kind: CompletionKind::Continue, label, value: cx.empty() }
    }

    #[inline]
    pub fn empty(cx: Context) -> Completion {
        Completion::normal(cx.empty())
    }

    #[inline]
    pub fn kind(&self) -> CompletionKind {
        self.kind
    }

    #[inline]
    pub fn value(&self) -> Handle<Value> {
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
    pub fn update_if_empty(mut self, value: Handle<Value>) -> Completion {
        if self.is_empty() {
            self.value = value;
        }

        self
    }

    /// Convert a completion into an EvalResult, panicking if the completion is a non-throw
    /// abnormal completion. This is only safe to call when the completion must be normal or throw,
    /// such as for expression evaluation.
    #[inline]
    pub fn into_eval_result(&self) -> EvalResult<Handle<Value>> {
        match self.kind() {
            CompletionKind::Normal => EvalResult::Ok(self.value()),
            CompletionKind::Throw => EvalResult::Throw(self.value()),
            CompletionKind::Return | CompletionKind::Break | CompletionKind::Continue => {
                unreachable!("")
            }
        }
    }
}

impl<T: Into<Handle<Value>>> From<T> for Completion {
    #[inline]
    fn from(value: T) -> Self {
        Completion::normal(value.into())
    }
}

/// EvalResult is for functions which are either sucessful or throw a value.
#[must_use]
pub enum EvalResult<T> {
    Ok(T),
    Throw(Handle<Value>),
}

impl<T> EvalResult<T> {
    pub fn to_rust_result(self) -> Result<T, Handle<Value>> {
        match self {
            EvalResult::Ok(value) => Ok(value),
            EvalResult::Throw(value) => Err(value),
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

impl<T: Into<Completion>> From<EvalResult<T>> for Completion {
    #[inline]
    fn from(value: EvalResult<T>) -> Self {
        match value {
            EvalResult::Ok(value) => value.into(),
            EvalResult::Throw(value) => Completion::throw(value),
        }
    }
}

impl Escapable for Completion {
    #[inline]
    fn escape(&self, cx: Context) -> Self {
        let value = self.value().escape(cx);
        Completion { kind: self.kind(), label: self.label(), value }
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

/// Unwrap an EvalResult, returning an escaped completion if throw
#[macro_export]
macro_rules! maybe_escape__ {
    ($cx:ident, $handle_scope:ident, $a:expr) => {{
        let result = $a;
        match result {
            EvalResult::Ok(value) => value,
            EvalResult::Throw(value) => {
                let escaped_value = $handle_scope.escape($cx, value);
                return Completion::throw(escaped_value);
            }
        }
    }};
}
