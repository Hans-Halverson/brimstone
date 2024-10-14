use std::{hash, mem::size_of, num::NonZeroU64, ptr::copy_nonoverlapping};

use num_bigint::{BigInt, Sign};
use rand::Rng;

use crate::{field_offset, set_uninit};

use super::{
    context::Context,
    debug_print::{DebugPrint, DebugPrinter},
    gc::{Handle, HandleContents, HeapItem, HeapObject, HeapPtr, HeapVisitor, ToHandleContents},
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::ObjectValue,
    string_value::{FlatString, StringValue},
    type_utilities::same_value_zero_non_allocating,
};

/// Values implemented with NaN boxing on 64-bit IEEE-754 floating point numbers. Inspired by NaN
/// packing implementation in JavaScriptCore and SerenityOS's LibJS.
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
///                     sign     exponent         mantissa
///                     (1 bit)  (11 bits)        (52 bits total. Top bit reserved for qNaN,
///                                                next 3 bits are tag, last 48 bits are payload)
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
    // Used as raw bitfield. NonZero for option inline niche optimization.
    raw_bits: NonZeroU64,
}

impl Value {
    #[inline]
    pub const fn from_raw_bits(raw_bits: u64) -> Value {
        Value { raw_bits: unsafe { NonZeroU64::new_unchecked(raw_bits) } }
    }

    #[inline]
    pub const fn as_raw_bits(&self) -> u64 {
        self.raw_bits.get()
    }

    // Type checks

    #[inline]
    pub const fn get_tag(&self) -> u16 {
        (self.as_raw_bits() >> TAG_SHIFT) as u16
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
        (!self.as_raw_bits() & !CANONICAL_NAN != !CANONICAL_NAN) || self.is_nan()
    }

    #[inline]
    pub const fn is_number(&self) -> bool {
        self.is_smi() || self.is_double()
    }

    #[inline]
    pub const fn is_nan(&self) -> bool {
        self.as_raw_bits() == CANONICAL_NAN
    }

    /// Whether number is positive or negative infinity
    #[inline]
    pub const fn is_infinity(&self) -> bool {
        // Set sign bit to check positive and negative infinity at the same time. Note that setting
        // sign bit will convert negative to positive infinity because doubles are bitwise negated.
        self.as_raw_bits() | (1 << 63) == POSITIVE_INFINITY
    }

    #[inline]
    pub fn is_positive_zero(&self) -> bool {
        self.as_raw_bits() == DOUBLE_POSITIVE_ZERO
            || (self.is_smi() && self.as_raw_bits() == SMI_ZERO)
    }

    #[inline]
    pub fn is_negative_zero(&self) -> bool {
        self.as_raw_bits() == DOUBLE_NEGATIVE_ZERO
    }

    #[inline]
    pub fn is_zero(&self) -> bool {
        // Set sign bit to check positive and negative zero at the same time. Note that setting
        // sign bit will convert zero to positive zero because doubles are bitwise negated.
        self.as_raw_bits() | (1 << 63) == DOUBLE_POSITIVE_ZERO
    }

    #[inline]
    pub fn is_true(&self) -> bool {
        self.as_raw_bits() == TRUE
    }

    #[inline]
    pub fn is_false(&self) -> bool {
        self.as_raw_bits() == FALSE
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

    // Type casts

    #[inline]
    pub fn as_number(&self) -> f64 {
        if self.is_smi() {
            return f64::from(self.as_smi());
        }

        self.as_double()
    }

    #[inline]
    pub fn as_double(&self) -> f64 {
        f64::from_bits(!self.as_raw_bits())
    }

    #[inline]
    pub const fn as_bool(&self) -> bool {
        (self.as_raw_bits() & 1) != 0
    }

    #[inline]
    pub const fn as_smi(&self) -> i32 {
        unsafe { std::mem::transmute::<u32, i32>(self.as_raw_bits() as u32) }
    }

    // Pointers are represented directly.
    #[inline]
    const fn restore_pointer_bits<T>(&self) -> *mut T {
        self.as_raw_bits() as *mut T
    }

    #[inline]
    pub const fn as_pointer(&self) -> HeapPtr<HeapItem> {
        HeapPtr::from_ptr(self.restore_pointer_bits())
    }

    #[inline]
    pub const fn as_object(&self) -> HeapPtr<ObjectValue> {
        HeapPtr::from_ptr(self.restore_pointer_bits())
    }

    #[inline]
    pub const fn as_string(&self) -> HeapPtr<StringValue> {
        HeapPtr::from_ptr(self.restore_pointer_bits())
    }

    #[inline]
    pub const fn as_symbol(&self) -> HeapPtr<SymbolValue> {
        HeapPtr::from_ptr(self.restore_pointer_bits())
    }

    #[inline]
    pub const fn as_bigint(&self) -> HeapPtr<BigIntValue> {
        HeapPtr::from_ptr(self.restore_pointer_bits())
    }

    // Constructors

    #[inline]
    pub fn number(value: f64) -> Value {
        // Check if this number should be converted into a smi
        if value.trunc() == value
            && (SMI_MIN..=SMI_MAX).contains(&value)
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
        Value::from_raw_bits((UNDEFINED_TAG as u64) << TAG_SHIFT)
    }

    #[inline]
    pub const fn null() -> Value {
        Value::from_raw_bits((NULL_TAG as u64) << TAG_SHIFT)
    }

    #[inline]
    pub const fn empty() -> Value {
        Value::from_raw_bits((EMPTY_TAG as u64) << TAG_SHIFT)
    }

    #[inline]
    pub const fn bool(value: bool) -> Value {
        Value::from_raw_bits(((BOOL_TAG as u64) << TAG_SHIFT) | (value as u64))
    }

    #[inline]
    pub const fn smi(value: i32) -> Value {
        let smi_value_bits = unsafe { std::mem::transmute::<i32, u32>(value) as u64 };
        Value::from_raw_bits(((SMI_TAG as u64) << TAG_SHIFT) | smi_value_bits)
    }

    #[inline]
    pub const fn double(value: f64) -> Value {
        // f64::to_bits is not yet stable as a const fn. We only pass simple values in a const
        // context so perform a raw transmute until f64::to_bits is const stabilized.
        let double_bits = unsafe { std::mem::transmute::<f64, u64>(value) };
        Value::from_raw_bits(!double_bits)
    }

    /// Convert any heap item into a Value. Note that only object subtypes, strings, symbols, and
    /// bigints are allowed to be treated as values in normal circumstances. Other heap items are
    /// converted to values for compatiblity in some scenarios (e.g. storing in a scope).
    #[inline]
    pub fn heap_item(value: HeapPtr<HeapItem>) -> Value {
        Value::from_raw_bits(value.as_ptr() as u64)
    }

    #[inline]
    pub fn object(value: HeapPtr<ObjectValue>) -> Value {
        Value::heap_item(value.as_heap_item())
    }

    #[inline]
    pub fn string(value: HeapPtr<StringValue>) -> Value {
        Value::heap_item(value.as_heap_item())
    }

    #[inline]
    pub fn symbol(value: HeapPtr<SymbolValue>) -> Value {
        Value::heap_item(value.as_heap_item())
    }

    #[inline]
    pub fn bigint(value: HeapPtr<BigIntValue>) -> Value {
        Value::heap_item(value.as_heap_item())
    }

    #[inline]
    pub const fn nan() -> Value {
        Value::from_raw_bits(CANONICAL_NAN)
    }

    #[inline]
    pub const fn uninit() -> Value {
        Value::empty()
    }
}

impl ToHandleContents for Value {
    type Impl = Value;

    #[inline]
    fn to_handle_contents(value: Value) -> HandleContents {
        value.as_raw_bits() as usize
    }
}

impl DebugPrint for Value {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        // Format primitive values
        if self.is_bool() {
            printer.write_default_with_context("bool", &self.as_bool().to_string())
        } else if self.is_smi() {
            printer.write_default_with_context("smi", &self.as_smi().to_string())
        } else if self.is_double() {
            printer.write_default_with_context("double", &self.as_double().to_string())
        } else if self.is_undefined() {
            printer.write_default("undefined")
        } else if self.is_null() {
            printer.write_default("null")
        } else if self.is_empty() {
            printer.write_default("empty")
        } else {
            debug_assert!(self.is_pointer());

            // Format heap allocated pointer values
            self.as_pointer().debug_format(printer)
        }
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

impl From<HeapPtr<ObjectValue>> for Value {
    fn from(value: HeapPtr<ObjectValue>) -> Self {
        Value::object(value)
    }
}

impl From<HeapPtr<StringValue>> for Value {
    fn from(value: HeapPtr<StringValue>) -> Self {
        Value::string(value)
    }
}

impl From<HeapPtr<SymbolValue>> for Value {
    fn from(value: HeapPtr<SymbolValue>) -> Self {
        Value::symbol(value)
    }
}

impl From<HeapPtr<BigIntValue>> for Value {
    fn from(value: HeapPtr<BigIntValue>) -> Self {
        Value::bigint(value)
    }
}

#[repr(C)]
pub struct SymbolValue {
    descriptor: HeapPtr<ObjectDescriptor>,
    description: Option<HeapPtr<FlatString>>,
    /// Stable hash code for this symbol, since symbol can be moved by GC
    hash_code: u32,
    /// Whether this symbol is for a private name
    is_private: bool,
}

impl SymbolValue {
    pub fn new(
        cx: Context,
        description: Option<Handle<StringValue>>,
        is_private: bool,
    ) -> Handle<SymbolValue> {
        let description = description.map(|d| d.flatten());
        let mut symbol = cx.alloc_uninit::<SymbolValue>();

        set_uninit!(symbol.descriptor, cx.base_descriptors.get(ObjectKind::Symbol));
        set_uninit!(symbol.description, description.map(|desc| desc.get_()));
        set_uninit!(symbol.hash_code, rand::thread_rng().gen::<u32>());
        set_uninit!(symbol.is_private, is_private);

        symbol.to_handle()
    }

    pub fn description_ptr(&self) -> Option<HeapPtr<FlatString>> {
        self.description
    }

    pub fn description(&self) -> Option<Handle<FlatString>> {
        self.description.map(|d| d.to_handle())
    }

    pub fn is_private(&self) -> bool {
        self.is_private
    }
}

impl DebugPrint for HeapPtr<SymbolValue> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        if let Some(description) = self.description_ptr() {
            printer.write_heap_item_with_context(self.cast(), &description.to_string())
        } else {
            printer.write_heap_item_default(self.cast())
        }
    }
}

impl hash::Hash for SymbolValue {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.hash_code.hash(state)
    }
}

impl hash::Hash for HeapPtr<SymbolValue> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.hash_code.hash(state)
    }
}

impl PartialEq for HeapPtr<SymbolValue> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.ptr_eq(other)
    }
}

impl Eq for HeapPtr<SymbolValue> {}

impl hash::Hash for Handle<SymbolValue> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.get_().hash(state)
    }
}

impl PartialEq for Handle<SymbolValue> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.get_().eq(&other.get_())
    }
}

impl Eq for Handle<SymbolValue> {}

impl From<Handle<SymbolValue>> for Handle<ObjectValue> {
    fn from(value: Handle<SymbolValue>) -> Self {
        value.cast()
    }
}

impl HeapObject for HeapPtr<SymbolValue> {
    fn byte_size(&self) -> usize {
        size_of::<SymbolValue>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer_opt(&mut self.description);
    }
}

#[repr(C)]
pub struct BigIntValue {
    descriptor: HeapPtr<ObjectDescriptor>,
    // Number of u32 digits in the BigInt
    len: usize,
    // Sign of the BigInt
    sign: Sign,
    // Start of the BigInt's array of digits. Variable sized but array has a single item for
    // alignment.
    digits: [u32; 1],
}

impl BigIntValue {
    const DIGITS_OFFSET: usize = field_offset!(BigIntValue, digits);

    pub fn new(cx: Context, value: BigInt) -> Handle<BigIntValue> {
        Self::new_ptr(cx, value).to_handle()
    }

    pub fn new_ptr(cx: Context, value: BigInt) -> HeapPtr<BigIntValue> {
        // Extract sign and digits from BigInt
        let (sign, digits) = value.to_u32_digits();
        let len = digits.len();

        let size = Self::calculate_size_in_bytes(len);
        let mut bigint = cx.alloc_uninit_with_size::<BigIntValue>(size);

        // Copy raw parts of BigInt into BigIntValue
        set_uninit!(bigint.descriptor, cx.base_descriptors.get(ObjectKind::BigInt));
        set_uninit!(bigint.len, digits.len());
        set_uninit!(bigint.sign, sign);

        unsafe { copy_nonoverlapping(digits.as_ptr(), bigint.digits.as_mut_ptr(), len) };

        bigint
    }

    pub fn calculate_size_in_bytes(num_u32_digits: usize) -> usize {
        // Calculate size of BigIntValue with inlined digits
        Self::DIGITS_OFFSET + num_u32_digits * size_of::<u32>()
    }

    pub fn bigint(&self) -> BigInt {
        // Recreate BigInt from stored raw parts
        let slice = unsafe { std::slice::from_raw_parts(self.digits.as_ptr(), self.len) };
        BigInt::from_slice(self.sign, slice)
    }
}

impl DebugPrint for HeapPtr<BigIntValue> {
    fn debug_format(&self, printer: &mut DebugPrinter) {
        printer.write_heap_item_with_context(self.cast(), &self.bigint().to_string())
    }
}

impl From<Handle<BigIntValue>> for Handle<ObjectValue> {
    fn from(value: Handle<BigIntValue>) -> Self {
        value.cast()
    }
}

impl HeapObject for HeapPtr<BigIntValue> {
    fn byte_size(&self) -> usize {
        BigIntValue::calculate_size_in_bytes(self.len)
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
    }
}

/// A wrapper around values that are used as keys in ValueMap and ValueSet.
/// Uses the SameValueZero algorithm to check equality, and hash function conforms to SameValueZero.
#[derive(Clone, Copy)]
pub struct ValueCollectionKey(Value);

impl ValueCollectionKey {
    // May allocate due to string flattening so do not implement From<Value> directly.
    pub fn from(value: Handle<Value>) -> Self {
        if value.is_string() {
            let flat_string = value.as_string().flatten();
            return ValueCollectionKey(flat_string.as_string().get_().into());
        }

        ValueCollectionKey(value.get())
    }

    pub fn get(&self) -> Value {
        self.0
    }

    pub fn value_mut(&mut self) -> &mut Value {
        &mut self.0
    }

    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_value(&mut self.0)
    }
}

impl From<ValueCollectionKey> for Value {
    fn from(value: ValueCollectionKey) -> Self {
        value.0
    }
}

impl PartialEq for ValueCollectionKey {
    fn eq(&self, other: &Self) -> bool {
        same_value_zero_non_allocating(self.0, other.0)
    }
}

impl Eq for ValueCollectionKey {}

impl hash::Hash for ValueCollectionKey {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        if self.0.is_number() {
            // Make sure that -0 has the same hash as +0
            return if self.0.is_negative_zero() {
                Value::smi(0).as_raw_bits().hash(state);
            } else {
                self.0.as_raw_bits().hash(state);
            };
        }

        if self.0.is_pointer() {
            return match self.0.as_pointer().descriptor().kind() {
                ObjectKind::String => {
                    // Strings must always be flat before they can be placed into hash tables to
                    // avoid allocating in the hash function.
                    let string = self.0.as_string();
                    debug_assert!(string.is_flat());

                    string.as_flat().hash(state)
                }
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

#[derive(Clone, Copy)]
pub struct ValueCollectionKeyHandle(Handle<Value>);

impl ValueCollectionKeyHandle {
    /// Identical to ValueCollectionKey::from but stores a handle instead.
    pub fn new(value: Handle<Value>) -> Self {
        if value.is_string() {
            let flat_string = value.as_string().flatten();
            return ValueCollectionKeyHandle(flat_string.as_string().into());
        }

        ValueCollectionKeyHandle(value)
    }

    pub fn get(&self) -> ValueCollectionKey {
        ValueCollectionKey(self.0.get())
    }
}
