use std::fmt;

use super::{
    gc::{Gc, GcDeref},
    object_value::ObjectValue,
};

/// Values implemented with NaN boxing on 64-bit IEEE-754 floating point numbers. Inspired by NaN
/// packing implementation in SerenityOS's LibWeb.
///
/// Floating point numbers have the following format:
///
/// Components: s exponent mantissa
/// Bits:       1 11       52
///
/// NaNs have an exponent of all 1s. In addition the highest bit of the mantissa determines
/// whether the NaN is quiet (1) or signaling (0). To avoid ambiguity with signaling NaNs, the
/// only canonical NaN allowed for JS numbers is the quiet NaN.
///
/// This leaves the low 51 bits of the mantissa and the sign bit for tagging.
///
/// The sign bit is used to determine whether this is a primitive value (0) or a pointer value (1).
///
/// Primitives use the top three bits with the following tags:
///
///   Undefined:  0b001
///   Null:       0b011
///   Empty:      0b010
///   Bool:       0b100, and the lowest bit of the mantissa stores the bool value
///
/// Pointers use the top three bits with the following tags, leaving 48 bits to store the pointer:
///
///   Object:   0b000
///   String:   0b001
///   Symbol:   0b010
///   BigInt:   0b011
///   Accessor: 0b100

const TAG_SHIFT: u64 = 48;
const NAN_MASK: u64 = (NAN_TAG as u64) << TAG_SHIFT;

// Only the top 16 bits need to be checked as a tag, allowing for a right shift and check
const NAN_TAG: u16 = 0x7FF8;
const POINTER_TAG: u16 = 0x8000;

pub const UNDEFINED_TAG: u16 = 0b001 | NAN_TAG;
pub const NULL_TAG: u16 = 0b011 | NAN_TAG;
// Empty value in a completion record. Can use instead of Option<Value> to fit into single word.
const EMPTY_TAG: u16 = 0b010 | NAN_TAG;
pub const BOOL_TAG: u16 = 0b100 | NAN_TAG;

pub const OBJECT_TAG: u16 = 0b001 | POINTER_TAG | NAN_TAG;
pub const STRING_TAG: u16 = 0b010 | POINTER_TAG | NAN_TAG;
pub const SYMBOL_TAG: u16 = 0b011 | POINTER_TAG | NAN_TAG;
pub const BIGINT_TAG: u16 = 0b100 | POINTER_TAG | NAN_TAG;
const ACCESSOR_TAG: u16 = 0b101 | POINTER_TAG | NAN_TAG;

// Mask that converts a null tag to an undefined tag, so that a nullish check can be performed with:
// TAG & NULLISH_MASK == UNDEFINED_TAG
const NULLISH_TAG_MASK: u16 = 0xFFFD;

// Bit patterns for known constants
const NEGATIVE_INFINITY: u64 = 0xFFF0 << TAG_SHIFT;
const TRUE: u64 = Value::bool(true).as_raw_bits();
const FALSE: u64 = Value::bool(false).as_raw_bits();

#[derive(Clone, Copy)]
pub struct Value {
    // Used as raw bitfield
    raw_bits: u64,
}

impl Value {
    // Type checks

    #[inline]
    pub const fn get_tag(&self) -> u16 {
        (self.raw_bits >> TAG_SHIFT) as u16
    }

    #[inline]
    const fn has_tag(&self, tag: u16) -> bool {
        self.get_tag() == tag
    }

    #[inline]
    pub const fn is_number(&self) -> bool {
        // Make sure to check if this is the canonical NaN value
        (self.raw_bits & NAN_MASK != NAN_MASK) || self.is_nan()
    }

    #[inline]
    pub const fn is_nan(&self) -> bool {
        self.raw_bits == NAN_MASK
    }

    /// Whether number is positive or negative infinity
    #[inline]
    pub const fn is_infinity(&self) -> bool {
        // Set sign bit to check positive and negative infinity at the same time
        self.raw_bits | (1 << 63) == NEGATIVE_INFINITY
    }

    #[inline]
    pub fn is_positive_zero(&self) -> bool {
        self.raw_bits == f64::to_bits(0.0)
    }

    #[inline]
    pub fn is_negative_zero(&self) -> bool {
        self.raw_bits == f64::to_bits(-0.0)
    }

    #[inline]
    pub fn is_true(&self) -> bool {
        self.raw_bits == TRUE
    }

    #[inline]
    pub fn is_false(&self) -> bool {
        self.raw_bits == FALSE
    }

    #[inline]
    pub const fn is_undefined(&self) -> bool {
        self.has_tag(UNDEFINED_TAG)
    }

    #[inline]
    pub const fn is_null(&self) -> bool {
        self.has_tag(NULL_TAG)
    }

    #[inline]
    pub const fn is_nullish(&self) -> bool {
        (self.get_tag() & NULLISH_TAG_MASK) == UNDEFINED_TAG
    }

    #[inline]
    pub const fn is_bool(&self) -> bool {
        self.has_tag(BOOL_TAG)
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.has_tag(EMPTY_TAG)
    }

    #[inline]
    pub const fn is_object(&self) -> bool {
        self.has_tag(OBJECT_TAG)
    }

    #[inline]
    pub const fn is_string(&self) -> bool {
        self.has_tag(STRING_TAG)
    }

    #[inline]
    pub const fn is_symbol(&self) -> bool {
        self.has_tag(SYMBOL_TAG)
    }

    #[inline]
    pub const fn is_bigint(&self) -> bool {
        self.has_tag(BIGINT_TAG)
    }

    #[inline]
    pub const fn is_accessor(&self) -> bool {
        self.has_tag(ACCESSOR_TAG)
    }

    // Type casts

    #[inline]
    pub const fn as_raw_bits(&self) -> u64 {
        self.raw_bits
    }

    #[inline]
    pub fn as_number(&self) -> f64 {
        f64::from_bits(self.raw_bits)
    }

    #[inline]
    pub const fn as_bool(&self) -> bool {
        (self.raw_bits & 1) != 0
    }

    // In x86_64 pointers must be in "canonical form", meaning the top 16 bits must be the same a
    // the highest pointer bit (bit 47).
    #[inline]
    const fn restore_pointer_bits<T>(&self) -> *mut T {
        ((self.raw_bits << 16) >> 16) as *mut T
    }

    #[inline]
    pub const fn as_object(&self) -> Gc<ObjectValue> {
        Gc::from_ptr(self.restore_pointer_bits())
    }

    #[inline]
    pub const fn as_string(&self) -> Gc<StringValue> {
        Gc::from_ptr(self.restore_pointer_bits())
    }

    #[inline]
    pub const fn as_accessor(&self) -> Gc<AccessorValue> {
        Gc::from_ptr(self.restore_pointer_bits())
    }

    // Constructors

    #[inline]
    pub fn number(value: f64) -> Value {
        Value {
            raw_bits: f64::to_bits(value),
        }
    }

    #[inline]
    pub const fn undefined() -> Value {
        Value {
            raw_bits: (UNDEFINED_TAG as u64) << TAG_SHIFT,
        }
    }

    #[inline]
    pub const fn null() -> Value {
        Value {
            raw_bits: (NULL_TAG as u64) << TAG_SHIFT,
        }
    }

    #[inline]
    pub const fn empty() -> Value {
        Value {
            raw_bits: (EMPTY_TAG as u64) << TAG_SHIFT,
        }
    }

    #[inline]
    pub const fn bool(value: bool) -> Value {
        Value {
            raw_bits: (((BOOL_TAG as u64) << TAG_SHIFT) | (value as u64)),
        }
    }

    #[inline]
    pub fn object(value: Gc<ObjectValue>) -> Value {
        Value {
            raw_bits: ((OBJECT_TAG as u64) << TAG_SHIFT) | (value.as_ptr() as u64),
        }
    }

    #[inline]
    pub fn string(value: Gc<StringValue>) -> Value {
        Value {
            raw_bits: ((STRING_TAG as u64) << TAG_SHIFT) | (value.as_ptr() as u64),
        }
    }

    #[inline]
    pub fn accessor(value: Gc<AccessorValue>) -> Value {
        Value {
            raw_bits: ((ACCESSOR_TAG as u64) << TAG_SHIFT) | (value.as_ptr() as u64),
        }
    }

    #[inline]
    pub const fn nan() -> Value {
        Value { raw_bits: NAN_MASK }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::bool(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::number(value)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::number(value.into())
    }
}

impl<T: Into<Gc<ObjectValue>>> From<T> for Value {
    fn from(value: T) -> Self {
        Value::object(value.into())
    }
}

impl From<Gc<StringValue>> for Value {
    fn from(value: Gc<StringValue>) -> Self {
        Value::string(value)
    }
}

impl From<Gc<AccessorValue>> for Value {
    fn from(value: Gc<AccessorValue>) -> Self {
        Value::accessor(value)
    }
}

pub struct StringValue(String);

impl StringValue {
    pub fn new(str: String) -> StringValue {
        StringValue(str)
    }
}

impl Gc<StringValue> {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn str<'a, 'b>(&'a self) -> &'b str {
        // Intentionally break lifetime, as StringValues are managed by the Gc heap
        unsafe { std::mem::transmute(self.0.as_str()) }
    }
}

impl GcDeref for StringValue {}

impl fmt::Display for StringValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq for StringValue {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

pub struct AccessorValue {
    pub get: Option<Gc<ObjectValue>>,
    pub set: Option<Gc<ObjectValue>>,
}
