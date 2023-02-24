use std::{collections::HashMap, hash};

use num_bigint::BigInt;

use super::{
    gc::{Gc, GcDeref},
    object_value::ObjectValue,
    string_value::StringValue,
    type_utilities::same_value_zero,
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
///   Smi:        0b110, and the low 32 bits of the mantissa stores the i32 value
///
/// Pointers use the top three bits with the following tags, leaving 48 bits to store the pointer:
///
///   Object:   0b001
///   String:   0b010
///   Symbol:   0b011
///   BigInt:   0b100
///   Accessor: 0b101

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
pub const SMI_TAG: u16 = 0b110 | NAN_TAG;

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

const SMI_MAX: f64 = i32::MAX as f64;
const SMI_MIN: f64 = i32::MIN as f64;
const SMI_ZERO_BITS: u64 = Value::smi(0).as_raw_bits();

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
    pub const fn is_double(&self) -> bool {
        // Make sure to check if this is the canonical NaN value
        (self.raw_bits & NAN_MASK != NAN_MASK) || self.is_nan()
    }

    #[inline]
    pub const fn is_number(&self) -> bool {
        self.is_smi() || self.is_double()
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
        self.raw_bits == f64::to_bits(0.0) || (self.is_smi() && self.raw_bits == SMI_ZERO_BITS)
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
    pub const fn is_smi(&self) -> bool {
        self.has_tag(SMI_TAG)
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
        if self.is_smi() {
            return f64::from(self.as_smi());
        }

        self.as_double()
    }

    #[inline]
    pub fn as_double(&self) -> f64 {
        f64::from_bits(self.raw_bits)
    }

    #[inline]
    pub const fn as_bool(&self) -> bool {
        (self.raw_bits & 1) != 0
    }

    #[inline]
    pub const fn as_smi(&self) -> i32 {
        unsafe { std::mem::transmute::<u32, i32>(self.raw_bits as u32) }
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
    pub const fn as_symbol(&self) -> Gc<SymbolValue> {
        Gc::from_ptr(self.restore_pointer_bits())
    }

    #[inline]
    pub const fn as_bigint(&self) -> Gc<BigIntValue> {
        Gc::from_ptr(self.restore_pointer_bits())
    }

    #[inline]
    pub const fn as_accessor(&self) -> Gc<AccessorValue> {
        Gc::from_ptr(self.restore_pointer_bits())
    }

    // Constructors

    #[inline]
    pub fn number(value: f64) -> Value {
        // Check if this number should be converted into a smi
        if value.trunc() == value
            && value >= SMI_MIN
            && value <= SMI_MAX
            && f64::to_bits(value) != f64::to_bits(-0.0)
        {
            return Value::smi(value as i32);
        } else if value.is_nan() {
            return Value::nan();
        }

        Value { raw_bits: f64::to_bits(value) }
    }

    #[inline]
    pub const fn undefined() -> Value {
        Value { raw_bits: (UNDEFINED_TAG as u64) << TAG_SHIFT }
    }

    #[inline]
    pub const fn null() -> Value {
        Value { raw_bits: (NULL_TAG as u64) << TAG_SHIFT }
    }

    #[inline]
    pub const fn empty() -> Value {
        Value { raw_bits: (EMPTY_TAG as u64) << TAG_SHIFT }
    }

    #[inline]
    pub const fn bool(value: bool) -> Value {
        Value {
            raw_bits: (((BOOL_TAG as u64) << TAG_SHIFT) | (value as u64)),
        }
    }

    #[inline]
    pub const fn smi(value: i32) -> Value {
        let smi_value_bits = unsafe { std::mem::transmute::<i32, u32>(value) as u64 };
        Value { raw_bits: (((SMI_TAG as u64) << TAG_SHIFT) | smi_value_bits) }
    }

    pub fn from_u64(value: u64) -> Value {
        if value > i32::MAX as u64 {
            // TODO: This conversion is lossy
            return Value { raw_bits: f64::to_bits(value as f64) };
        }

        Value::smi(value as i32)
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
    pub fn symbol(value: Gc<SymbolValue>) -> Value {
        Value {
            raw_bits: ((SYMBOL_TAG as u64) << TAG_SHIFT) | (value.as_ptr() as u64),
        }
    }

    #[inline]
    pub fn bigint(value: Gc<BigIntValue>) -> Value {
        Value {
            raw_bits: ((BIGINT_TAG as u64) << TAG_SHIFT) | (value.as_ptr() as u64),
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
        Value::smi(value)
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

impl From<Gc<SymbolValue>> for Value {
    fn from(value: Gc<SymbolValue>) -> Self {
        Value::symbol(value)
    }
}

impl From<Gc<BigIntValue>> for Value {
    fn from(value: Gc<BigIntValue>) -> Self {
        Value::bigint(value)
    }
}

impl From<Gc<AccessorValue>> for Value {
    fn from(value: Gc<AccessorValue>) -> Self {
        Value::accessor(value)
    }
}

pub struct SymbolValue(Option<Gc<StringValue>>);

impl SymbolValue {
    pub fn new(description: Option<Gc<StringValue>>) -> SymbolValue {
        SymbolValue(description)
    }
}

impl Gc<SymbolValue> {
    pub fn description(&self) -> Option<Gc<StringValue>> {
        // Intentionally break lifetime, as SymbolValues are managed by the Gc heap
        self.0
    }
}

impl GcDeref for SymbolValue {}

pub struct BigIntValue(BigInt);

impl BigIntValue {
    pub fn new(value: BigInt) -> BigIntValue {
        BigIntValue(value)
    }
}

impl Gc<BigIntValue> {
    pub fn bigint<'a, 'b>(&'a self) -> &'b BigInt {
        // Intentionally break lifetime, as BigIntValues are managed by the Gc heap
        unsafe { std::mem::transmute(&self.0) }
    }
}

impl GcDeref for BigIntValue {}

pub struct AccessorValue {
    pub get: Option<Gc<ObjectValue>>,
    pub set: Option<Gc<ObjectValue>>,
}

impl GcDeref for AccessorValue {}

/// A wrapper around values that are used as keys in ValueMap and ValueSet.
struct ValueCollectionKey(Value);

impl From<Value> for ValueCollectionKey {
    fn from(value: Value) -> Self {
        ValueCollectionKey(value)
    }
}

impl PartialEq for ValueCollectionKey {
    fn eq(&self, other: &Self) -> bool {
        same_value_zero(self.0, other.0)
    }
}

impl Eq for ValueCollectionKey {}

impl hash::Hash for ValueCollectionKey {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        if self.0.is_number() {
            // Make sure that -0 has the same hash as +0
            return if self.0.is_negative_zero() {
                (0 as u64).hash(state);
            } else {
                self.0.as_raw_bits().hash(state);
            };
        }

        let tag = self.0.get_tag();
        if tag == STRING_TAG {
            self.0.as_string().hash(state);
        } else if tag == BIGINT_TAG {
            self.0.as_bigint().bigint().hash(state);
        } else {
            self.0.as_raw_bits().hash(state);
        }
    }
}

pub struct ValueMap<T> {
    map: HashMap<ValueCollectionKey, T>,
}

impl<T> ValueMap<T> {
    pub fn new() -> ValueMap<T> {
        ValueMap { map: HashMap::new() }
    }

    pub fn clear(&mut self) {
        self.map.clear()
    }

    pub fn contains_key(&self, key: Value) -> bool {
        self.map.contains_key(&key.into())
    }

    pub fn get(&self, key: Value) -> Option<&T> {
        self.map.get(&key.into())
    }

    pub fn insert(&mut self, key: Value, value: T) -> Option<T> {
        self.map.insert(key.into(), value)
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn remove(&mut self, key: Value) -> Option<T> {
        self.map.remove(&key.into())
    }
}
