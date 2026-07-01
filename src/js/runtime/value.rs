use std::{
    hash,
    mem::{MaybeUninit, size_of},
    num::NonZeroU64,
};

use crate::{
    common::numeric::Numeric,
    const_assert,
    runtime::{
        BigIntValue, HeapItemKind, SymbolValue,
        alloc_error::AllocResult,
        debug_print::{DebugPrint, DebugPrinter},
        gc::{
            AnyHeapItem, Handle, HandleContents, HeapPtr, HeapVisitor, ToHandleContents,
            WithHeapItemKind,
        },
        object_value::ObjectValue,
        string_value::StringValue,
        type_utilities::same_value_zero_non_allocating,
    },
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
#[derive(Clone, Copy)]
pub struct Value {
    // Used as raw bitfield. NonZero for option inline niche optimization.
    raw_bits: NonZeroU64,
}

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
const DOUBLE_POSITIVE_ZERO: u64 = Value::raw_double(0.0).as_raw_bits();
const DOUBLE_NEGATIVE_ZERO: u64 = Value::raw_double(-0.0).as_raw_bits();
const POSITIVE_INFINITY: u64 = Value::raw_double(f64::INFINITY).as_raw_bits();

const TRUE: u64 = Value::bool(true).as_raw_bits();
const FALSE: u64 = Value::bool(false).as_raw_bits();

const SMI_MAX: f64 = i32::MAX as f64;
const SMI_MIN: f64 = i32::MIN as f64;
const SMI_ZERO: u64 = Value::raw_smi(0).as_raw_bits();

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
        self.is::<StringValue>()
    }

    #[inline]
    pub fn is_symbol(&self) -> bool {
        self.is::<SymbolValue>()
    }

    #[inline]
    pub fn is_bigint(&self) -> bool {
        self.is::<BigIntValue>()
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
        (self.as_raw_bits() as u32).cast_signed()
    }

    // Pointers are represented directly.
    #[inline]
    const fn restore_pointer_bits<T>(&self) -> *mut T {
        self.as_raw_bits() as *mut T
    }

    #[inline]
    pub const fn as_pointer(&self) -> HeapPtr<AnyHeapItem> {
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

    /// Whether this is a heap item of a particular type.
    #[inline]
    pub fn is<T: WithHeapItemKind>(&self) -> bool {
        self.is_pointer() && self.as_pointer().descriptor().kind() == T::KIND
    }

    /// Return this value as a heap item of a particular type, or None if it is not of that type.
    #[inline]
    pub fn as_opt<T: WithHeapItemKind>(&self) -> Option<HeapPtr<T>> {
        if self.is::<T>() {
            Some(self.as_pointer().cast())
        } else {
            None
        }
    }

    // Constructors

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
    pub const fn raw_smi(value: i32) -> Value {
        Value::from_raw_bits(((SMI_TAG as u64) << TAG_SHIFT) | (value as u32 as u64))
    }

    #[inline]
    const fn raw_double(value: f64) -> Value {
        let double_bits = value.to_bits();
        Value::from_raw_bits(!double_bits)
    }

    #[inline]
    pub fn smi<T: Numeric>(value: T) -> Value {
        // Statically ensure that only safe smi types can be used
        const_assert!(T::IS_SAFE_SMI);

        Self::raw_smi(value.as_())
    }

    #[inline]
    pub fn number<T: Numeric>(value: T) -> Value {
        // Statically ensure that i128 and u128 are not used, and require external conversion
        const_assert!(!T::IS_I128 && !T::IS_U128);

        // Fast path for all integer types
        if T::IS_SAFE_SMI {
            Value::raw_smi(value.as_())
        } else if T::IS_U32 {
            let value_u32: u32 = value.as_();
            if let Ok(smi) = i32::try_from(value_u32) {
                Value::raw_smi(smi)
            } else {
                Value::raw_double(value.as_())
            }
        } else if T::IS_U64 {
            let value_u64: u64 = value.as_();
            if let Ok(smi) = i32::try_from(value_u64) {
                Value::raw_smi(smi)
            } else {
                Value::raw_double(value.as_())
            }
        } else if T::IS_I64 {
            let value_i64: i64 = value.as_();
            if let Ok(smi) = i32::try_from(value_i64) {
                Value::raw_smi(smi)
            } else {
                Value::raw_double(value.as_())
            }
        } else if T::IS_USIZE {
            let value_usize: usize = value.as_();
            if let Ok(smi) = i32::try_from(value_usize) {
                Value::raw_smi(smi)
            } else {
                Value::raw_double(value.as_())
            }
        } else if T::IS_ISIZE {
            let value_isize: isize = value.as_();
            if let Ok(smi) = i32::try_from(value_isize) {
                Value::raw_smi(smi)
            } else {
                Value::raw_double(value.as_())
            }
        } else if T::IS_F32 || T::IS_F64 {
            // Otherwise treat as a double
            let value_f64: f64 = value.as_();

            // Check if this number should be converted into a smi
            if value_f64.trunc() == value_f64
                && (SMI_MIN..=SMI_MAX).contains(&value_f64)
                && f64::to_bits(value_f64) != f64::to_bits(-0.0)
            {
                return Value::raw_smi(value_f64 as i32);
            } else if value_f64.is_nan() {
                return Value::nan();
            }

            Value::raw_double(value_f64)
        } else {
            unreachable!("Unsupported numeric type")
        }
    }

    /// Convert any heap item into a Value. Note that only object subtypes, strings, symbols, and
    /// bigints are allowed to be treated as values in normal circumstances. Other heap items are
    /// converted to values for compatibility in some scenarios (e.g. storing in a scope).
    #[inline]
    pub fn heap_item(value: HeapPtr<AnyHeapItem>) -> Value {
        Value::from_raw_bits(value.as_ptr() as u64)
    }

    #[inline]
    pub fn object(value: HeapPtr<ObjectValue>) -> Value {
        Value::heap_item(value.as_any())
    }

    #[inline]
    pub fn string(value: HeapPtr<StringValue>) -> Value {
        Value::heap_item(value.as_any())
    }

    #[inline]
    pub fn symbol(value: HeapPtr<SymbolValue>) -> Value {
        Value::heap_item(value.as_any())
    }

    #[inline]
    pub fn bigint(value: HeapPtr<BigIntValue>) -> Value {
        Value::heap_item(value.as_any())
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

impl Handle<Value> {
    /// Return this value as a heap item of a particular type, or None if it is not of that type.
    #[inline]
    pub fn as_opt<T: WithHeapItemKind>(&self) -> Option<Handle<T>> {
        if self.is::<T>() {
            Some(self.cast())
        } else {
            None
        }
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

/// Encoding scheme for packing a sequence of raw bytes into a sequence of Values. Each Value is
/// encoded as the EMPTY_TAG in the top 16-bits and the payload in the low 48-bits.
pub struct RawBytesEncoding;

impl RawBytesEncoding {
    /// Number of payload bytes packed into each Value, namely the 48 bits below the 16-bit tag.
    const PAYLOAD_BYTES_PER_VALUE: usize = (TAG_SHIFT / u8::BITS as u64) as usize;

    /// Mask selecting the low 48 payload bits of a Value's raw bits.
    const PAYLOAD_MASK: u64 = (1 << TAG_SHIFT) - 1;

    /// The number of Values needed to store the raw bytes of a value of type `T`.
    pub const fn num_values<T>() -> usize {
        size_of::<T>().div_ceil(Self::PAYLOAD_BYTES_PER_VALUE)
    }

    /// Return the offset and length of the payload within the bytes of a value of type `T` for the
    /// corresponding encoded Value at `value_index`.
    #[inline]
    fn payload_window_for_value<T>(value_index: usize) -> (usize, usize) {
        let offset = value_index * Self::PAYLOAD_BYTES_PER_VALUE;
        let len = (size_of::<T>() - offset).min(Self::PAYLOAD_BYTES_PER_VALUE);

        (offset, len)
    }

    #[inline]
    fn encode_payload_into_value(payload: [u8; 8]) -> Value {
        let payload = u64::from_le_bytes(payload);
        let raw_bits = ((EMPTY_TAG as u64) << TAG_SHIFT) | payload;
        Value::from_raw_bits(raw_bits)
    }

    #[inline]
    fn extract_payload_from_value(value: Value) -> [u8; 8] {
        (value.as_raw_bits() & Self::PAYLOAD_MASK).to_le_bytes()
    }

    /// Pack the raw bytes of a value of type `T` into a sequence of Values.
    pub fn encode<T, const N: usize>(src_value: &T) -> [Value; N] {
        debug_assert_eq!(N, RawBytesEncoding::num_values::<T>());

        let src = (src_value as *const T).cast::<u8>();

        let mut values = [Value::empty(); N];
        for (i, value) in values.iter_mut().enumerate() {
            let mut payload_bytes = [0u8; size_of::<u64>()];

            let (offset, len) = RawBytesEncoding::payload_window_for_value::<T>(i);
            unsafe {
                std::ptr::copy_nonoverlapping(src.add(offset), payload_bytes.as_mut_ptr(), len)
            };

            *value = RawBytesEncoding::encode_payload_into_value(payload_bytes);
        }

        values
    }

    /// Reconstruct a `T` from a sequence of Values previously produced by `RawBytesEncoding::encode`.
    pub fn decode<T, const N: usize>(values: &[Value; N]) -> T {
        debug_assert_eq!(N, RawBytesEncoding::num_values::<T>());

        let mut result = MaybeUninit::<T>::uninit();
        let dst = result.as_mut_ptr().cast::<u8>();

        for (i, value) in values.iter().enumerate() {
            let payload_bytes = RawBytesEncoding::extract_payload_from_value(*value);
            let (offset, len) = RawBytesEncoding::payload_window_for_value::<T>(i);
            unsafe { std::ptr::copy_nonoverlapping(payload_bytes.as_ptr(), dst.add(offset), len) };
        }

        unsafe { result.assume_init() }
    }
}

/// A wrapper around values that are used as keys in ValueIndexMap and ValueIndexSet.
/// Uses the SameValueZero algorithm to check equality, and hash function conforms to SameValueZero.
#[derive(Clone, Copy)]
pub struct ValueCollectionKey(Value);

impl ValueCollectionKey {
    // May allocate due to string flattening so do not implement From<Value> directly.
    pub fn from(value: Handle<Value>) -> AllocResult<Self> {
        if value.is_string() {
            let flat_string = value.as_string().flatten()?;
            return Ok(ValueCollectionKey(*flat_string.as_value()));
        }

        Ok(ValueCollectionKey(*value))
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
                HeapItemKind::StringValue => {
                    // Strings must always be flat before they can be placed into hash tables to
                    // avoid allocating in the hash function.
                    let string = self.0.as_string();
                    debug_assert!(string.is_flat());

                    string.as_flat().hash(state)
                }
                HeapItemKind::BigIntValue => self.0.as_bigint().bigint().hash(state),
                // Otherwise is an object or symbol. Hash code must represent object/symbol
                // identity, but objects/symbols can be moved by the GC. So use the stable hash code
                // stored in the object/symbol.
                HeapItemKind::SymbolValue => self.0.as_symbol().hash(state),
                _ => self.0.as_object().hash_code().hash(state),
            };
        }

        // Non-pointer values can be hashed directly off their bit representation
        self.0.as_raw_bits().hash(state);
    }
}

#[derive(Clone, Copy)]
pub struct ValueCollectionKeyHandle(Handle<Value>);

impl ValueCollectionKeyHandle {
    /// Identical to ValueCollectionKey::from but stores a handle instead.
    pub fn new(value: Handle<Value>) -> AllocResult<Self> {
        if value.is_string() {
            let flat_string = value.as_string().flatten()?;
            return Ok(ValueCollectionKeyHandle(flat_string.as_string().into()));
        }

        Ok(ValueCollectionKeyHandle(value))
    }

    pub fn get(&self) -> ValueCollectionKey {
        ValueCollectionKey(*self.0)
    }
}
