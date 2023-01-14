use std::{fmt, hash};

use super::{
    value::{StringValue, SymbolValue},
    Gc,
};

#[derive(Clone, Copy)]
pub enum PropertyKey {
    // TODO: Add numeric keys
    String(Gc<StringValue>),
    Symbol(Gc<SymbolValue>),
}

impl PartialEq for PropertyKey {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(str1), Self::String(str2)) => str1.str() == str2.str(),
            (Self::Symbol(sym1), Self::Symbol(sym2)) => sym1.ptr_eq(sym2),
            _ => false,
        }
    }
}

impl Eq for PropertyKey {}

impl hash::Hash for PropertyKey {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::String(str) => str.str().hash(state),
            Self::Symbol(sym) => sym.description().as_deref().unwrap_or("").hash(state),
        }
    }
}

impl fmt::Display for PropertyKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(str) => f.write_str(str.str()),
            Self::Symbol(sym) => {
                write!(f, "Symbol({})", sym.description().as_deref().unwrap_or(""))
            }
        }
    }
}
