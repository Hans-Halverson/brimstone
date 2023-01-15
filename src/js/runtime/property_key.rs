use std::{cell::RefCell, fmt, hash};

use super::{
    value::{StringValue, SymbolValue},
    Context, Gc,
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
    #[inline]
    pub const fn string(value: Gc<StringValue>) -> PropertyKey {
        PropertyKey {
            data: RefCell::new(KeyData::String(StringData {
                value,
                can_be_number: true,
            })),
        }
    }

    /// Create a string property key that is known to not be a number property. Be sure to not pass
    /// string property keys that may be a number to this function.
    #[inline]
    pub const fn string_not_number(value: Gc<StringValue>) -> PropertyKey {
        PropertyKey {
            data: RefCell::new(KeyData::String(StringData {
                value,
                can_be_number: false,
            })),
        }
    }

    #[inline]
    pub const fn symbol(value: Gc<SymbolValue>) -> PropertyKey {
        PropertyKey {
            data: RefCell::new(KeyData::Symbol { value }),
        }
    }

    #[inline]
    pub const fn array_index(value: u32) -> PropertyKey {
        PropertyKey {
            data: RefCell::new(KeyData::ArrayIndex { value }),
        }
    }

    #[inline]
    pub fn is_array_index(&self) -> bool {
        match &*self.data.borrow() {
            KeyData::String(StringData {
                can_be_number: true,
                ..
            }) => {}
            KeyData::String(StringData {
                can_be_number: false,
                ..
            })
            | KeyData::Symbol { .. } => return false,
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
            KeyData::String(StringData {
                can_be_number: true,
                ..
            }) => {}
            KeyData::String(StringData {
                can_be_number: false,
                ..
            }) => return true,
            KeyData::Symbol { .. } | KeyData::ArrayIndex { .. } => return false,
        }

        return !self.check_is_number();
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
            KeyData::ArrayIndex { value } => cx.heap.alloc_string(value.to_string()),
            KeyData::Symbol { .. } => unreachable!("expected non-symbol property key"),
        };
    }

    #[inline]
    fn check_is_number(&self) -> bool {
        let number_key = match &mut *self.data.borrow_mut() {
            KeyData::String(
                string_key @ StringData {
                    can_be_number: true,
                    ..
                },
            ) => {
                let str = string_key.value.str();

                // Empty string can never be number
                if str.is_empty() {
                    string_key.can_be_number = false;
                    return false;
                }

                // First character must be numeric to be a canonical array index string
                let first_char = str.chars().nth(0).unwrap();
                if first_char < '0' || first_char > '9' {
                    string_key.can_be_number = false;
                    return false;
                } else if first_char == '0' && str.len() > 1 {
                    // First character can be a 0 only if it is not a leading zero
                    string_key.can_be_number = false;
                    return false;
                }

                // Try parsing as integer index, cacheing failure
                match str::parse::<u32>(str) {
                    Err(_) => {
                        string_key.can_be_number = false;
                        return false;
                    }
                    Ok(num) => num,
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
            ) => str1.str() == str2.str(),
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
                value.str().hash(state);
            }
            KeyData::ArrayIndex { value } => {
                value.hash(state);
            }
            KeyData::Symbol { value } => {
                value.description().as_deref().unwrap_or("").hash(state);
            }
        }
    }
}

impl fmt::Display for PropertyKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.data.borrow() {
            KeyData::String(StringData { value, .. }) => f.write_str(value.str()),
            KeyData::Symbol { value } => {
                write!(
                    f,
                    "Symbol({})",
                    value.description().as_deref().unwrap_or("")
                )
            }
            KeyData::ArrayIndex { value, .. } => write!(f, "{}", value),
        }
    }
}
