use std::{fmt, hash};

use crate::maybe;

use super::{
    gc::{Handle, HandleValue, HeapPtr},
    interned_strings::InternedStrings,
    numeric_constants::MAX_U32_AS_F64,
    object_descriptor::ObjectKind,
    string_parsing::parse_string_to_u32,
    string_value::StringValue,
    to_string,
    type_utilities::is_integral_number,
    value::SymbolValue,
    Context, EvalResult, Value,
};

/// A property key must be either an interned string or a symbol for named properties,
/// or a smi if key is a valid array index. Note that smis technically have an i32 range but
/// array indices have a u32 range, but we can cast between signed an unsigned values appropriately.
///
/// Always stored on the stack.
#[derive(Clone, Copy)]
pub struct PropertyKey {
    value: HandleValue,
}

/// A property key that is stored on the heap.
#[derive(Clone)]
pub struct HeapPropertyKey {
    value: Value,
}

impl PropertyKey {
    pub fn uninit() -> PropertyKey {
        PropertyKey { value: HandleValue::uninit() }
    }

    #[inline]
    pub fn string(cx: &mut Context, value: Handle<StringValue>) -> PropertyKey {
        // String value may represent an array index
        match parse_string_to_u32(value.get_()) {
            None => PropertyKey::string_not_array_index(cx, value),
            Some(u32::MAX) => PropertyKey::string_not_array_index(cx, value),
            Some(array_index) => PropertyKey::array_index(cx, array_index),
        }
    }

    /// Create a string property key that is known to not be an array index. Be sure to not pass
    /// string property keys that may be an array index to this function.
    #[inline]
    pub fn string_not_array_index(cx: &mut Context, value: Handle<StringValue>) -> PropertyKey {
        // Enforce that all string property keys are interned
        PropertyKey { value: InternedStrings::get(cx, value).into() }
    }

    #[inline]
    pub fn symbol(value: Handle<SymbolValue>) -> PropertyKey {
        PropertyKey { value: Value::symbol(value) }
    }

    #[inline]
    pub fn array_index(cx: &mut Context, value: u32) -> PropertyKey {
        if value == u32::MAX {
            let string_value = cx.alloc_string(value.to_string());
            return PropertyKey::string_not_array_index(cx, string_value);
        }

        // Intentionally store u32 value in i32 smi payload
        PropertyKey { value: Value::smi(value as i32) }
    }

    pub const fn from_u8(value: u8) -> PropertyKey {
        PropertyKey { value: Value::smi(value as i32) }
    }

    pub fn from_u64(cx: &mut Context, value: u64) -> PropertyKey {
        if value >= u32::MAX as u64 {
            let string_value = cx.alloc_string(value.to_string());
            return PropertyKey::string_not_array_index(cx, string_value);
        }

        PropertyKey { value: Value::smi(value as u32 as i32) }
    }

    pub fn from_value(cx: &mut Context, value_handle: HandleValue) -> EvalResult<PropertyKey> {
        let value = value_handle.get();
        if is_integral_number(value) {
            let number = value.as_double();
            if number >= 0.0 && number < MAX_U32_AS_F64 {
                return PropertyKey::array_index(cx, number as u32).into();
            }
        }

        if value.is_symbol() {
            PropertyKey::symbol(value_handle.as_symbol()).into()
        } else {
            let string_value = maybe!(to_string(cx, value_handle));
            PropertyKey::string(cx, string_value).into()
        }
    }

    #[inline]
    pub fn is_array_index(&self) -> bool {
        self.value.is_smi()
    }

    #[inline]
    pub fn as_array_index(&self) -> u32 {
        // A u32 value was stored in the i32 smi payload
        self.value.as_smi() as u32
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        self.value.is_string()
    }

    #[inline]
    pub fn as_string(&self) -> Handle<StringValue> {
        self.value.as_string()
    }

    #[inline]
    pub fn as_string_opt(&self) -> Option<Handle<StringValue>> {
        if self.value.is_string() {
            Some(self.value.as_string())
        } else {
            None
        }
    }

    #[inline]
    pub fn is_symbol(&self) -> bool {
        self.value.is_symbol()
    }

    #[inline]
    pub fn as_symbol(&self) -> Handle<SymbolValue> {
        self.value.as_symbol()
    }

    #[inline]
    pub fn as_symbol_opt(&self) -> Option<Handle<SymbolValue>> {
        if self.value.is_symbol() {
            Some(self.value.as_symbol())
        } else {
            None
        }
    }

    /// Convert this property key to a string or symbol value as defined in the spec. All array
    /// index keys will be converted to strings.
    #[inline]
    pub fn to_value(&self, cx: &mut Context) -> HandleValue {
        if self.value.is_smi() {
            let array_index_string = self.as_array_index().to_string();
            cx.alloc_string(array_index_string).into()
        } else {
            self.value
        }
    }

    pub fn to_heap(&self) -> HeapPropertyKey {
        HeapPropertyKey { value: self.value.get() }
    }

    pub fn from_heap(heap_property_key: &HeapPropertyKey) -> PropertyKey {
        PropertyKey { value: heap_property_key.value }
    }
}

impl PartialEq for HeapPropertyKey {
    fn eq(&self, other: &Self) -> bool {
        self.value.as_raw_bits() == other.value.as_raw_bits()
    }
}

impl Eq for HeapPropertyKey {}

impl hash::Hash for HeapPropertyKey {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        if self.is_array_index() {
            self.as_array_index().hash(state);
        } else if self.value.as_pointer().descriptor().kind() == ObjectKind::String {
            self.as_string().hash(state)
        } else {
            self.as_symbol().hash(state)
        }
    }
}

impl fmt::Display for HeapPropertyKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_array_index() {
            write!(f, "{}", self.as_array_index())
        } else if self.value.as_pointer().descriptor().kind() == ObjectKind::String {
            self.as_string().fmt(f)
        } else {
            match self.as_symbol().description() {
                None => write!(f, "Symbol()"),
                Some(description) => write!(f, "Symbol({})", description),
            }
        }
    }
}

impl PartialEq for PropertyKey {
    fn eq(&self, other: &Self) -> bool {
        self.to_heap().eq(&other.to_heap())
    }
}

impl Eq for PropertyKey {}

impl hash::Hash for PropertyKey {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.to_heap().hash(state)
    }
}

impl fmt::Display for PropertyKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.to_heap().fmt(f)
    }
}

impl HeapPropertyKey {
    #[inline]
    pub fn is_array_index(&self) -> bool {
        self.value.is_smi()
    }

    #[inline]
    pub fn as_array_index(&self) -> u32 {
        // A u32 value was stored in the i32 smi payload
        self.value.as_smi() as u32
    }

    #[inline]
    pub fn as_string(&self) -> HeapPtr<StringValue> {
        self.value.as_string()
    }

    #[inline]
    pub fn as_string_opt(&self) -> Option<HeapPtr<StringValue>> {
        if self.value.is_string() {
            Some(self.value.as_string())
        } else {
            None
        }
    }

    #[inline]
    pub fn as_symbol(&self) -> HeapPtr<SymbolValue> {
        self.value.as_symbol()
    }

    #[inline]
    pub fn as_symbol_opt(&self) -> Option<HeapPtr<SymbolValue>> {
        if self.value.is_symbol() {
            Some(self.value.as_symbol())
        } else {
            None
        }
    }
}
