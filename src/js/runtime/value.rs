use std::hash;

use indexmap::{IndexMap, IndexSet};
use num_bigint::BigInt;
use rand::Rng;

use super::{
    context::Context,
    gc::{Gc, GcDeref},
    object_descriptor::{HeapItem, ObjectDescriptor, ObjectKind},
    object_value::ObjectValue,
    string_value::StringValue,
    type_utilities::same_value_zero,
};

/// Values implemented with NaN boxing on 64-bit IEEE-754 floating point numbers. Inspired by NaN
/// packing implementation in JavaScriptCorre and SerenityOS's LibWeb.
///
/// Floating point numbers have the following format:
///
/// Components: sign exponent mantissa
/// Bits:       1    11       52
///
/// NaNs have an exponent of all 1s. In addition the highest bit of the mantissa determines
/// whether the NaN is quiet (1) or signaling (0). To avoid ambiguity with signaling NaNs, the
/// only canonical NaN allowed for JS numbers is the quiet NaN.
///
/// This leaves the low 51 bits of the mantissa and the sign bit for packing in additional data.
///
/// We choose to favor pointers over doubles, meaning pointer values are encoded directly and can
/// be used without needing a decoding step. A consequence of this is that type information for the
/// pointer value must be stored within the heap allocated object.
///
/// For pointers to be directly encoded their top 16 bits must be zero, meaning we must encode all
/// valid non-pointer values into a form where they never have their top 16 bits all be zero.
///
/// Imagine a scheme, referred to here as the "non-negated scheme", which our true scheme will be
/// built off of. In the non-negated scheme all non-pointer, non-double values are encoded by
/// placing a non-zero 3-bit tag in bits 48-50 of the mantissa, and placing an up to 48-bit payload
/// in bits 0-47 of the mantissa. Note that the sign bit is ignored, but could be used as an
/// additional tag bit if needed.
///
/// In this scheme all non-pointer values have the following representation:
///
////                    sign     exponent         mantissa
////                    (1 bit)  (11 bits)        (52 bits total. Top bit reserved for qNaN,
////                                               next 3 bits are tag, last 48 bits are payload)
///
/// +Inf:               0        11111111111      0 000 00...0
/// -Inf:               1        11111111111      0 000 00...0
/// Canonical NaN       0        11111111111      1 000 00...0
/// Other double        any      (any but 1..1)   any any any
/// Other values        any      11111111111      1 (tag - any but 000) (payload - any)
///
/// Observe that all non-pointer values have at least one 0 in their top 13 bits (and their top 16
/// bits for that matter). This means that if we bitwise negate any valid non-pointer value, the
/// result will never have its top 16 bits all be set to 0, distinguishing it from a pointer.
///
/// This brings us to our true encoding scheme:
/// - Pointers are represented directly, with all top 16 bits set to 0
/// - Doubles are represented by their bitwise negation
/// - All non-pointer, non-value doubles are represented by the bitwise negation of the
///   "non-negated scheme" described above.
///     - Undefined, Null, and Empty values each have a unique tag but no payload
///     - Bool values store their encoded bool value in the lowest bit of the payload
///     - Smi values store their encoded 32-bit int value in the low 32 bits of the payload
///
/// Technically bool and smi values store the bitwise negation of their payload, in order for the
/// payload to be extracted directly without needing to apply a bitwise negation.
///
/// Notes:
/// - All tags are statically known, so we can shift and compare tags to known constants for each type
/// - Decoding a pointer is a no-op, it is already encoded directly.
/// - Decoding a bool or smi involves directly copying the low bits (no bitwise negation needed)
/// - Decoding a double is simply bitwise negating the entire value. Same with encoding.
/// - Undefined and Null tags differ by a single bit, so can mask and compare to check nullish
///   values instead of needing two comparisons.

const TAG_SHIFT: u64 = 48;

// Only the top 16 bits need to be checked as a tag, allowing for a right shift and check
const NAN_TAG: u16 = 0x7FF8;

pub const POINTER_TAG: u16 = 0;
pub const UNDEFINED_TAG: u16 = !(0b001 | NAN_TAG);
pub const NULL_TAG: u16 = !(0b011 | NAN_TAG);
// Empty value in a completion record. Can use instead of Option<Value> to fit into single word.
const EMPTY_TAG: u16 = !(0b010 | NAN_TAG);
pub const BOOL_TAG: u16 = !(0b100 | NAN_TAG);
pub const SMI_TAG: u16 = !(0b110 | NAN_TAG);

// Mask that converts an undefined tag to a null tag, so that a nullish check can be performed with:
// TAG & NULLISH_MASK == NULL_TAG
const NULLISH_TAG_MASK: u16 = 0xFFFD;

// Bit patterns for known constants
const CANONICAL_NAN: u64 = !((NAN_TAG as u64) << TAG_SHIFT);
const DOUBLE_POSITIVE_ZERO: u64 = Value::double(0.0).as_raw_bits();
const DOUBLE_NEGATIVE_ZERO: u64 = Value::double(-0.0).as_raw_bits();
const POSITIVE_INFINITY: u64 = Value::double(f64::INFINITY).as_raw_bits();

const TRUE: u64 = Value::bool(true).as_raw_bits();
const FALSE: u64 = Value::bool(false).as_raw_bits();

const SMI_MAX: f64 = i32::MAX as f64;
const SMI_MIN: f64 = i32::MIN as f64;
const SMI_ZERO: u64 = Value::smi(0).as_raw_bits();

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
    pub const fn is_pointer(&self) -> bool {
        self.has_tag(POINTER_TAG)
    }

    #[inline]
    pub const fn is_double(&self) -> bool {
        // In the non-negated representation the only double pattern with the 12 canonical NaN bits
        // set is the canonical NaN itself. All other non-pointer values also have the 12 canonical
        // NaN bits set, so value is a double if either any of these 12 bits is unset in the
        // non-negated representation, or the value is the canonical NaN.
        (!self.raw_bits & !CANONICAL_NAN != !CANONICAL_NAN) || self.is_nan()
    }

    #[inline]
    pub const fn is_number(&self) -> bool {
        self.is_smi() || self.is_double()
    }

    #[inline]
    pub const fn is_nan(&self) -> bool {
        self.raw_bits == CANONICAL_NAN
    }

    /// Whether number is positive or negative infinity
    #[inline]
    pub const fn is_infinity(&self) -> bool {
        // Set sign bit to check positive and negative infinity at the same time. Note that setting
        // sign bit will convert negative to positive infinity because doubles are bitwise negated.
        self.raw_bits | (1 << 63) == POSITIVE_INFINITY
    }

    #[inline]
    pub fn is_positive_zero(&self) -> bool {
        self.raw_bits == DOUBLE_POSITIVE_ZERO || (self.is_smi() && self.raw_bits == SMI_ZERO)
    }

    #[inline]
    pub fn is_negative_zero(&self) -> bool {
        self.raw_bits == DOUBLE_NEGATIVE_ZERO
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
        // Apply mask that converts undefined tag into null tag, since they are one bit apart,
        // so only a single comparison is needed.
        (self.get_tag() & NULLISH_TAG_MASK) == NULL_TAG
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
    pub fn is_object(&self) -> bool {
        if !self.is_pointer() {
            return false;
        }

        self.as_pointer().descriptor().is_object()
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        if !self.is_pointer() {
            return false;
        }

        self.as_pointer().descriptor().kind() == ObjectKind::String
    }

    #[inline]
    pub fn is_symbol(&self) -> bool {
        if !self.is_pointer() {
            return false;
        }

        self.as_pointer().descriptor().kind() == ObjectKind::Symbol
    }

    #[inline]
    pub fn is_bigint(&self) -> bool {
        if !self.is_pointer() {
            return false;
        }

        self.as_pointer().descriptor().kind() == ObjectKind::BigInt
    }

    #[inline]
    pub fn is_accessor(&self) -> bool {
        if !self.is_pointer() {
            return false;
        }

        self.as_pointer().descriptor().kind() == ObjectKind::Accessor
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
        f64::from_bits(!self.raw_bits)
    }

    #[inline]
    pub const fn as_bool(&self) -> bool {
        (self.raw_bits & 1) != 0
    }

    #[inline]
    pub const fn as_smi(&self) -> i32 {
        unsafe { std::mem::transmute::<u32, i32>(self.raw_bits as u32) }
    }

    // Pointers are represented directly.
    #[inline]
    const fn restore_pointer_bits<T>(&self) -> *mut T {
        self.raw_bits as *mut T
    }

    #[inline]
    pub const fn as_pointer(&self) -> Gc<HeapItem> {
        Gc::from_ptr(self.restore_pointer_bits())
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

        Value::double(value)
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

    #[inline]
    pub const fn double(value: f64) -> Value {
        // f64::to_bits is not yet stable as a const fn. We only pass simple values in a const
        // context so perform a raw transmute until f64::to_bits is const stabilized.
        let double_bits = unsafe { std::mem::transmute::<f64, u64>(value) };
        Value { raw_bits: !double_bits }
    }

    #[inline]
    pub fn object(value: Gc<ObjectValue>) -> Value {
        Value { raw_bits: (value.as_ptr() as u64) }
    }

    #[inline]
    pub fn string(value: Gc<StringValue>) -> Value {
        Value { raw_bits: (value.as_ptr() as u64) }
    }

    #[inline]
    pub fn symbol(value: Gc<SymbolValue>) -> Value {
        Value { raw_bits: (value.as_ptr() as u64) }
    }

    #[inline]
    pub fn bigint(value: Gc<BigIntValue>) -> Value {
        Value { raw_bits: (value.as_ptr() as u64) }
    }

    #[inline]
    pub fn accessor(value: Gc<AccessorValue>) -> Value {
        Value { raw_bits: (value.as_ptr() as u64) }
    }

    #[inline]
    pub const fn nan() -> Value {
        Value { raw_bits: CANONICAL_NAN }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::bool(value)
    }
}

impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Value::smi(value as i32)
    }
}

impl From<i8> for Value {
    fn from(value: i8) -> Self {
        Value::smi(value as i32)
    }
}

impl From<u16> for Value {
    fn from(value: u16) -> Self {
        Value::smi(value as i32)
    }
}

impl From<i16> for Value {
    fn from(value: i16) -> Self {
        Value::smi(value as i32)
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        if value > i32::MAX as u32 {
            return Value::double(value as f64);
        }

        Value::smi(value as i32)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::smi(value)
    }
}

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        if value > i32::MAX as u64 {
            // TODO: This conversion is lossy
            return Value::double(value as f64);
        }

        Value::smi(value as i32)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        if value > i32::MAX as i64 || value < i32::MIN as i64 {
            // TODO: This conversion is lossy
            return Value::double(value as f64);
        }

        Value::smi(value as i32)
    }
}

impl From<usize> for Value {
    #[inline]
    fn from(value: usize) -> Self {
        Value::from(value as u64)
    }
}

impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Value::number(value as f64)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::number(value)
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

#[repr(C)]
pub struct SymbolValue {
    descriptor: Gc<ObjectDescriptor>,
    description: Option<Gc<StringValue>>,
    // Stable hash code for this symbol, since symbol can be moved by GC
    hash_code: u32,
}

impl SymbolValue {
    pub fn new(cx: &mut Context, description: Option<Gc<StringValue>>) -> Gc<SymbolValue> {
        let descriptor = cx.base_descriptors.get(ObjectKind::Symbol);
        let hash_code = rand::thread_rng().gen::<u32>();
        cx.heap
            .alloc(SymbolValue { descriptor, description, hash_code })
    }

    pub fn description(&self) -> Option<Gc<StringValue>> {
        // Intentionally break lifetime, as SymbolValues are managed by the Gc heap
        self.description
    }
}

impl hash::Hash for SymbolValue {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.hash_code.hash(state)
    }
}

impl GcDeref for SymbolValue {}

#[repr(C)]
pub struct BigIntValue {
    descriptor: Gc<ObjectDescriptor>,
    value: BigInt,
}

impl BigIntValue {
    pub fn new(cx: &mut Context, value: BigInt) -> Gc<BigIntValue> {
        let descriptor = cx.base_descriptors.get(ObjectKind::BigInt);
        cx.heap.alloc(BigIntValue { descriptor, value })
    }
}

impl Gc<BigIntValue> {
    pub fn bigint<'a, 'b>(&'a self) -> &'b BigInt {
        // Intentionally break lifetime, as BigIntValues are managed by the Gc heap
        // unsafe { std::mem::transmute(&self.0) }
        unsafe { std::mem::transmute(&self.value) }
    }
}

impl GcDeref for BigIntValue {}

#[repr(C)]
pub struct AccessorValue {
    descriptor: Gc<ObjectDescriptor>,
    pub get: Option<Gc<ObjectValue>>,
    pub set: Option<Gc<ObjectValue>>,
}

impl GcDeref for AccessorValue {}

impl AccessorValue {
    pub fn new(
        cx: &mut Context,
        get: Option<Gc<ObjectValue>>,
        set: Option<Gc<ObjectValue>>,
    ) -> Gc<AccessorValue> {
        let descriptor = cx.base_descriptors.get(ObjectKind::Accessor);
        cx.heap.alloc(AccessorValue { descriptor, get, set })
    }
}

/// A wrapper around values that are used as keys in ValueMap and ValueSet.
/// Uses the SameValueZero algorithm to check equality, and hash function conforms to SameValueZero.
#[derive(Clone, Copy)]
pub struct ValueCollectionKey(Value);

impl From<Value> for ValueCollectionKey {
    fn from(value: Value) -> Self {
        ValueCollectionKey(value)
    }
}

impl From<ValueCollectionKey> for Value {
    fn from(value: ValueCollectionKey) -> Self {
        value.0
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

        if self.0.is_pointer() {
            return match self.0.as_pointer().descriptor().kind() {
                ObjectKind::String => self.0.as_string().hash(state),
                ObjectKind::BigInt => self.0.as_bigint().bigint().hash(state),
                // Otherwise is an object or symbol. Hash code must represent object/symbol
                // identity, but objects/symbols can be moved by the GC. So use the stable hash code
                // stored in the object/symbol.
                ObjectKind::Symbol => self.0.as_symbol().hash(state),
                _ => self.0.as_object().hash_code().hash(state),
            };
        }

        // Non-pointer values can be hashed direclty off their bit representation
        self.0.as_raw_bits().hash(state);
    }
}

pub struct ValueMap<T> {
    map: IndexMap<ValueCollectionKey, T>,
}

pub type ValueMapIter<'a, T> = indexmap::map::Iter<'a, ValueCollectionKey, T>;

impl<T> ValueMap<T> {
    pub fn new() -> ValueMap<T> {
        ValueMap { map: IndexMap::new() }
    }

    pub fn clear(&mut self) {
        self.map.clear()
    }

    pub fn contains_key(&self, key: Value) -> bool {
        self.map.contains_key(&ValueCollectionKey::from(key))
    }

    pub fn get(&self, key: Value) -> Option<&T> {
        self.map.get(&ValueCollectionKey::from(key))
    }

    pub fn insert(&mut self, key: Value, value: T) -> Option<T> {
        self.map.insert(key.into(), value)
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn remove(&mut self, key: Value) -> Option<T> {
        self.map.remove(&ValueCollectionKey::from(key))
    }

    pub fn iter<'a, 'b>(&'a self) -> ValueMapIter<'b, T> {
        unsafe {
            // Intentionally break lifetime
            std::mem::transmute::<ValueMapIter<'a, T>, ValueMapIter<'b, T>>(self.map.iter())
        }
    }
}

pub struct ValueSet {
    set: IndexSet<ValueCollectionKey>,
}

pub type ValueSetIter<'a> = indexmap::set::Iter<'a, ValueCollectionKey>;

impl ValueSet {
    pub fn new() -> ValueSet {
        ValueSet { set: IndexSet::new() }
    }

    pub fn clear(&mut self) {
        self.set.clear()
    }

    pub fn contains(&self, value: Value) -> bool {
        self.set.contains(&ValueCollectionKey::from(value))
    }

    pub fn insert(&mut self, value: Value) -> bool {
        self.set.insert(value.into())
    }

    pub fn len(&self) -> usize {
        self.set.len()
    }

    pub fn remove(&mut self, value: Value) -> bool {
        self.set.remove(&ValueCollectionKey::from(value))
    }

    pub fn iter<'a, 'b>(&'a self) -> ValueSetIter<'b> {
        unsafe {
            // Intentionally break lifetime
            std::mem::transmute::<ValueSetIter<'a>, ValueSetIter<'b>>(self.set.iter())
        }
    }
}
