use std::{fmt, hash};

use super::{
    gc::{Handle, HandleContents, ToHandleContents},
    heap_item_descriptor::HeapItemKind,
    interned_strings::InternedStrings,
    numeric_constants::MAX_U32_AS_F64,
    string_parsing::parse_string_to_u32,
    string_value::StringValue,
    to_string,
    type_utilities::is_integral_number,
    value::SymbolValue,
    Context, EvalResult, HeapPtr, Value,
};

/// A property key must be either an interned string or a symbol for named properties,
/// or a smi if key is a valid array index. Note that smis technically have an i32 range but
/// array indices have a u32 range, but we can cast between signed an unsigned values appropriately.
///
/// Since strings are interned they are also guaranteed to be flat.
///
/// Always stored on the stack.
#[derive(Clone, Copy)]
pub struct PropertyKey {
    value: Value,
}

impl PropertyKey {
    pub const fn uninit() -> PropertyKey {
        PropertyKey { value: Value::empty() }
    }

    #[inline]
    pub fn string(cx: Context, value: Handle<StringValue>) -> PropertyKey {
        // String value may represent an array index
        match parse_string_to_u32(value) {
            None => PropertyKey::string_not_array_index(cx, value),
            Some(u32::MAX) => PropertyKey::string_not_array_index(cx, value),
            Some(array_index) => PropertyKey::array_index(cx, array_index),
        }
    }

    /// Create a string property key that is known to not be an array index. Be sure to not pass
    /// string property keys that may be an array index to this function.
    #[inline]
    pub fn string_not_array_index(cx: Context, value: Handle<StringValue>) -> PropertyKey {
        // Enforce that all string property keys are interned
        let flat_string = value.flatten();
        let interned_string = InternedStrings::get(cx, *flat_string).as_string();
        PropertyKey { value: interned_string.into() }
    }

    #[inline]
    pub fn symbol(value: Handle<SymbolValue>) -> Handle<PropertyKey> {
        value.cast()
    }

    #[inline]
    pub fn array_index(mut cx: Context, value: u32) -> PropertyKey {
        if value == u32::MAX {
            let string_value = cx.alloc_string(&value.to_string()).as_string();
            return PropertyKey::string_not_array_index(cx, string_value);
        }

        // Intentionally store u32 value in i32 smi payload
        PropertyKey { value: Value::smi(value as i32) }
    }

    pub const fn from_u8(value: u8) -> PropertyKey {
        PropertyKey { value: Value::smi(value as i32) }
    }

    pub fn from_u64(mut cx: Context, value: u64) -> PropertyKey {
        if value >= u32::MAX as u64 {
            let string_value = cx.alloc_string(&value.to_string()).as_string();
            return PropertyKey::string_not_array_index(cx, string_value);
        }

        PropertyKey { value: Value::smi(value as u32 as i32) }
    }

    pub fn from_value(cx: Context, value_handle: Handle<Value>) -> EvalResult<PropertyKey> {
        let value = *value_handle;
        if is_integral_number(value) {
            let number = value.as_double();
            if (0.0..MAX_U32_AS_F64).contains(&number) {
                return Ok(PropertyKey::array_index(cx, number as u32));
            }
        }

        if value.is_symbol() {
            Ok(*PropertyKey::symbol(value_handle.as_symbol()))
        } else {
            let string_value = to_string(cx, value_handle)?;
            Ok(PropertyKey::string(cx, string_value))
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
    pub fn as_string(&self) -> HeapPtr<StringValue> {
        self.value.as_string()
    }

    #[inline]
    pub fn is_symbol(&self) -> bool {
        self.value.is_symbol()
    }

    #[inline]
    pub fn as_symbol(&self) -> HeapPtr<SymbolValue> {
        self.value.as_symbol()
    }
}

impl Handle<PropertyKey> {
    #[inline]
    pub fn as_string(&self) -> Handle<StringValue> {
        self.cast()
    }

    #[inline]
    pub fn as_symbol(&self) -> Handle<SymbolValue> {
        self.cast()
    }

    /// Convert this property key to a string or symbol value as defined in the spec. All array
    /// index keys will be converted to strings.
    #[inline]
    pub fn to_value(self, mut cx: Context) -> Handle<Value> {
        if self.value.is_smi() {
            let array_index_string = self.as_array_index().to_string();
            cx.alloc_string(&array_index_string).into()
        } else {
            self.cast()
        }
    }
}

impl PartialEq for PropertyKey {
    fn eq(&self, other: &Self) -> bool {
        self.value.as_raw_bits() == other.value.as_raw_bits()
    }
}

impl Eq for PropertyKey {}

impl hash::Hash for PropertyKey {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        if self.is_array_index() {
            self.as_array_index().hash(state);
        } else if self.value.as_pointer().descriptor().kind() == HeapItemKind::String {
            // Strings must always be flat before they can be placed into hash tables to
            // avoid allocating in the hash function.
            let string = self.as_string();
            debug_assert!(string.is_flat());

            string.as_flat().hash(state)
        } else {
            self.as_symbol().hash(state)
        }
    }
}

impl fmt::Display for PropertyKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_array_index() {
            write!(f, "{}", self.as_array_index())
        } else if self.value.as_pointer().descriptor().kind() == HeapItemKind::String {
            self.as_string().fmt(f)
        } else {
            match self.as_symbol().description_ptr() {
                None => write!(f, "Symbol()"),
                Some(description) => write!(f, "Symbol({description})"),
            }
        }
    }
}

impl PartialEq for Handle<PropertyKey> {
    fn eq(&self, other: &Self) -> bool {
        (**self).eq(other)
    }
}

impl Eq for Handle<PropertyKey> {}

impl hash::Hash for Handle<PropertyKey> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}

impl fmt::Display for Handle<PropertyKey> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl Handle<PropertyKey> {
    #[inline]
    pub fn from_fixed_non_heap_ptr(value_ref: &PropertyKey) -> Handle<PropertyKey> {
        Handle::<Value>::from_fixed_non_heap_ptr(&value_ref.value).cast()
    }
}

impl ToHandleContents for PropertyKey {
    type Impl = PropertyKey;

    #[inline]
    fn to_handle_contents(key: PropertyKey) -> HandleContents {
        Value::to_handle_contents(key.value)
    }
}

impl PropertyKey {
    #[inline]
    pub fn to_handle(self, cx: Context) -> Handle<PropertyKey> {
        self.value.to_handle(cx).cast()
    }
}
