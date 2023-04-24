use std::{cell::RefCell, fmt, hash};

use crate::maybe;

use super::{
    numeric_constants::MAX_U32_AS_F64, string_parsing::parse_string_to_u32,
    string_value::StringValue, to_string, type_utilities::is_integral_number, value::SymbolValue,
    Context, EvalResult, Gc, Value,
};

#[derive(Clone)]
pub struct PropertyKey {
    data: RefCell<KeyData>,
}

#[derive(Clone)]
enum KeyData {
    String(StringData),
    Symbol { value: Gc<SymbolValue> },
    // Array index property key
    ArrayIndex { value: u32 },
}

#[derive(Clone)]
struct StringData {
    value: Gc<StringValue>,
    // Wether this string has been checked to see if it is a number yet. If this string could
    // be a number this is initially true, then will either be switched to false if the string
    // is not a number, or the data will be modified to be a number property key.
    can_be_number: bool,
}

impl PropertyKey {
    pub const fn uninit() -> PropertyKey {
        PropertyKey::string(Gc::uninit())
    }

    #[inline]
    pub const fn string(value: Gc<StringValue>) -> PropertyKey {
        PropertyKey {
            data: RefCell::new(KeyData::String(StringData { value, can_be_number: true })),
        }
    }

    /// Create a string property key that is known to not be a number property. Be sure to not pass
    /// string property keys that may be a number to this function.
    #[inline]
    pub const fn string_not_number(value: Gc<StringValue>) -> PropertyKey {
        PropertyKey {
            data: RefCell::new(KeyData::String(StringData { value, can_be_number: false })),
        }
    }

    #[inline]
    pub const fn symbol(value: Gc<SymbolValue>) -> PropertyKey {
        PropertyKey { data: RefCell::new(KeyData::Symbol { value }) }
    }

    #[inline]
    pub fn array_index(cx: &mut Context, value: u32) -> PropertyKey {
        if value == u32::MAX {
            return PropertyKey::string_not_number(cx.alloc_string(value.to_string()));
        }

        PropertyKey { data: RefCell::new(KeyData::ArrayIndex { value }) }
    }

    pub const fn from_u8(value: u8) -> PropertyKey {
        PropertyKey {
            data: RefCell::new(KeyData::ArrayIndex { value: value as u32 }),
        }
    }

    pub fn from_u64(cx: &mut Context, value: u64) -> PropertyKey {
        if value > u32::MAX as u64 {
            return PropertyKey::string_not_number(cx.alloc_string(value.to_string()));
        }

        PropertyKey {
            data: RefCell::new(KeyData::ArrayIndex { value: value as u32 }),
        }
    }

    pub fn from_value(cx: &mut Context, value: Value) -> EvalResult<PropertyKey> {
        if is_integral_number(value) {
            let number = value.as_double();
            if number >= 0.0 && number < MAX_U32_AS_F64 {
                return PropertyKey::array_index(cx, number as u32).into();
            }
        }

        if value.is_symbol() {
            PropertyKey::symbol(value.as_symbol()).into()
        } else {
            PropertyKey::string(maybe!(to_string(cx, value))).into()
        }
    }

    #[inline]
    pub fn is_array_index(&self) -> bool {
        match &*self.data.borrow() {
            KeyData::String(StringData { can_be_number: true, .. }) => {}
            KeyData::String(StringData { can_be_number: false, .. }) | KeyData::Symbol { .. } => {
                return false
            }
            KeyData::ArrayIndex { .. } => return true,
        }

        return self.check_is_number();
    }

    #[inline]
    pub fn as_array_index(&self) -> u32 {
        match &*self.data.borrow() {
            KeyData::ArrayIndex { value } => *value,
            _ => unreachable!("expected array index"),
        }
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        match &*self.data.borrow() {
            KeyData::String(StringData { can_be_number: true, .. }) => {}
            KeyData::String(StringData { can_be_number: false, .. }) => return true,
            KeyData::Symbol { .. } | KeyData::ArrayIndex { .. } => return false,
        }

        return !self.check_is_number();
    }

    #[inline]
    pub fn as_string(&self) -> Gc<StringValue> {
        match &*self.data.borrow() {
            KeyData::String(StringData { value, .. }) => *value,
            _ => unreachable!("expected string key"),
        }
    }

    #[inline]
    pub fn as_string_opt(&self) -> Option<Gc<StringValue>> {
        match &*self.data.borrow() {
            KeyData::String(StringData { value, .. }) => Some(*value),
            _ => None,
        }
    }

    #[inline]
    pub fn as_symbol(&self) -> Option<Gc<SymbolValue>> {
        return match &*self.data.borrow() {
            KeyData::Symbol { value } => Some(*value),
            _ => None,
        };
    }

    #[inline]
    pub fn non_symbol_to_string(&self, cx: &mut Context) -> Gc<StringValue> {
        return match &*self.data.borrow() {
            KeyData::String(StringData { value, .. }) => *value,
            KeyData::ArrayIndex { value } => cx.alloc_string(value.to_string()),
            KeyData::Symbol { .. } => unreachable!("expected non-symbol property key"),
        };
    }

    #[inline]
    pub fn to_value(&self, cx: &mut Context) -> Value {
        if let Some(symbol_value) = self.as_symbol() {
            symbol_value.into()
        } else {
            self.non_symbol_to_string(cx).into()
        }
    }

    #[inline]
    fn check_is_number(&self) -> bool {
        let number_key = match &mut *self.data.borrow_mut() {
            KeyData::String(string_key @ StringData { can_be_number: true, .. }) => {
                let string = string_key.value;

                // Try parsing as integer index, caching failure
                match parse_string_to_u32(string) {
                    None => {
                        string_key.can_be_number = false;
                        return false;
                    }
                    // 2 ^ 32 - 1 is outside the array index range
                    Some(array_index) if array_index == u32::MAX => {
                        string_key.can_be_number = false;
                        return false;
                    }
                    Some(array_index) => array_index,
                }
            }
            _ => return false,
        };

        self.data.replace(KeyData::ArrayIndex { value: number_key });
        true
    }
}

impl PartialEq for PropertyKey {
    fn eq(&self, other: &Self) -> bool {
        if std::ptr::eq(self, other) {
            return true;
        }

        self.check_is_number();
        other.check_is_number();

        match (&*self.data.borrow(), &*other.data.borrow()) {
            (
                KeyData::String(StringData { value: str1, .. }),
                KeyData::String(StringData { value: str2, .. }),
            ) => str1 == str2,
            (KeyData::ArrayIndex { value: num1 }, KeyData::ArrayIndex { value: num2 }) => {
                *num1 == *num2
            }
            (KeyData::Symbol { value: sym1 }, KeyData::Symbol { value: sym2 }) => sym1.ptr_eq(sym2),
            _ => false,
        }
    }
}

impl Eq for PropertyKey {}

impl hash::Hash for PropertyKey {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.check_is_number();

        match &*self.data.borrow() {
            KeyData::String(StringData { value, .. }) => {
                value.hash(state);
            }
            KeyData::ArrayIndex { value } => {
                value.hash(state);
            }
            KeyData::Symbol { value } => {
                value.hash(state);
            }
        }
    }
}

impl fmt::Display for PropertyKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.data.borrow() {
            KeyData::String(StringData { value, .. }) => value.fmt(f),
            KeyData::Symbol { value } => match value.description() {
                None => write!(f, "Symbol()"),
                Some(description) => write!(f, "Symbol({})", description),
            },
            KeyData::ArrayIndex { value, .. } => write!(f, "{}", value),
        }
    }
}
