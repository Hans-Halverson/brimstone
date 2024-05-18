use std::cmp::Ordering;

use num_bigint::{BigInt, ToBigInt};

use crate::{js::common::math::modulo, maybe, must};

use super::{
    abstract_operations::{call_object, get, get_method},
    bytecode::function::Closure,
    completion::EvalResult,
    error::{range_error, syntax_error, type_error},
    gc::{Handle, HeapPtr},
    intrinsics::{
        bigint_constructor::BigIntObject, boolean_constructor::BooleanObject,
        number_constructor::NumberObject, symbol_constructor::SymbolObject,
    },
    numeric_constants::{MAX_SAFE_INTEGER_F64, MAX_U8_AS_F64},
    object_descriptor::ObjectKind,
    object_value::ObjectValue,
    property_key::PropertyKey,
    proxy_object::ProxyObject,
    string_object::StringObject,
    string_parsing::{parse_string_to_bigint, parse_string_to_number},
    string_value::StringValue,
    value::{BigIntValue, Value, BOOL_TAG, NULL_TAG, POINTER_TAG, SMI_TAG, UNDEFINED_TAG},
    Context,
};

#[derive(PartialEq)]
pub enum ToPrimitivePreferredType {
    None,
    String,
    Number,
}

// 7.1.1 ToPrimitive
#[inline]
pub fn to_primitive(
    mut cx: Context,
    value: Handle<Value>,
    mut preferred_type: ToPrimitivePreferredType,
) -> EvalResult<Handle<Value>> {
    if !value.is_object() {
        return value.into();
    }

    let to_primitive_key = cx.well_known_symbols.to_primitive();
    let exotic_prim = maybe!(get_method(cx, value, to_primitive_key));
    match exotic_prim {
        Some(exotic_prim) => {
            let hint_str = match preferred_type {
                ToPrimitivePreferredType::None => "default",
                ToPrimitivePreferredType::Number => "number",
                ToPrimitivePreferredType::String => "string",
            };
            let hint_value: Handle<Value> = cx.alloc_string(hint_str).into();

            let result = maybe!(call_object(cx, exotic_prim, value, &[hint_value]));
            if result.is_object() {
                return type_error(cx, "object cannot be converted to primitive");
            }

            result.into()
        }
        None => {
            if preferred_type == ToPrimitivePreferredType::None {
                preferred_type = ToPrimitivePreferredType::Number;
            }

            ordinary_to_primitive(cx, value.as_object(), preferred_type)
        }
    }
}

// 7.1.1.1 OrdinaryToPrimitive
pub fn ordinary_to_primitive(
    cx: Context,
    object: Handle<ObjectValue>,
    preferred_type: ToPrimitivePreferredType,
) -> EvalResult<Handle<Value>> {
    let object_value: Handle<Value> = object.into();

    macro_rules! call_method {
        ($method_name:expr) => {
            let method = maybe!(get(cx, object, $method_name));
            if is_callable(method) {
                let result = maybe!(call_object(cx, method.as_object(), object_value, &[]));
                if !result.is_object() {
                    return result.into();
                }
            }
        };
    }

    if preferred_type == ToPrimitivePreferredType::String {
        call_method!(cx.names.to_string());
        call_method!(cx.names.value_of());
    } else {
        call_method!(cx.names.value_of());
        call_method!(cx.names.to_string());
    }

    type_error(cx, "object cannot be converted to primitive")
}

// 7.1.2 ToBoolean
pub fn to_boolean(value: Value) -> bool {
    // Fast path
    let tag = value.get_tag();
    if tag == BOOL_TAG {
        return value.as_bool();
    }

    if value.is_pointer() {
        match value.as_pointer().descriptor().kind() {
            ObjectKind::String => !value.as_string().is_empty(),
            ObjectKind::BigInt => value.as_bigint().bigint().ne(&BigInt::default()),
            // Objects and symbols
            _ => true,
        }
    } else {
        match tag {
            NULL_TAG => false,
            UNDEFINED_TAG => false,
            SMI_TAG => value.as_smi() != 0,
            // Otherwise must be a double
            _ => value.as_double() != 0.0 && !value.is_nan(),
        }
    }
}

// 7.1.3 ToNumeric
pub fn to_numeric(cx: Context, value: Handle<Value>) -> EvalResult<Handle<Value>> {
    let prim_value = maybe!(to_primitive(cx, value, ToPrimitivePreferredType::Number));
    if prim_value.is_bigint() {
        return prim_value.into();
    }

    to_number(cx, prim_value)
}

// 7.1.4 ToNumber
pub fn to_number(cx: Context, value_handle: Handle<Value>) -> EvalResult<Handle<Value>> {
    // Safe since value is never referenced after allocation
    let value = value_handle.get();

    // Fast path
    if value.is_number() {
        return value_handle.into();
    }

    if value.is_pointer() {
        if value.as_pointer().descriptor().is_object() {
            let primitive_value =
                maybe!(to_primitive(cx, value_handle, ToPrimitivePreferredType::Number));
            to_number(cx, primitive_value)
        } else {
            match value.as_pointer().descriptor().kind() {
                // May allocate
                ObjectKind::String => string_to_number(value_handle.as_string())
                    .to_handle(cx)
                    .into(),
                ObjectKind::Symbol => type_error(cx, "symbol cannot be converted to number"),
                ObjectKind::BigInt => type_error(cx, "BigInt cannot be converted to number"),
                _ => unreachable!(),
            }
        }
    } else {
        match value.get_tag() {
            NULL_TAG => Value::smi(0).to_handle(cx).into(),
            UNDEFINED_TAG => Value::nan().to_handle(cx).into(),
            BOOL_TAG => {
                if value.as_bool() {
                    Value::smi(1).to_handle(cx).into()
                } else {
                    Value::smi(0).to_handle(cx).into()
                }
            }
            _ => unreachable!(),
        }
    }
}

// 7.1.4.1.1 StringToNumber
fn string_to_number(value: Handle<StringValue>) -> Value {
    match parse_string_to_number(value) {
        None => Value::nan(),
        Some(num) => Value::number(num),
    }
}

// 7.1.5 ToIntegerOrInfinity
pub fn to_integer_or_infinity(cx: Context, value: Handle<Value>) -> EvalResult<f64> {
    let number_handle = maybe!(to_number(cx, value));
    let number = number_handle.get();

    to_integer_or_infinity_f64(number.as_number()).into()
}

pub fn to_integer_or_infinity_f64(number_f64: f64) -> f64 {
    if number_f64.is_nan() || number_f64 == 0.0 {
        return 0.0.into();
    }

    if number_f64.is_infinite() {
        return number_f64.into();
    }

    let mut integer_f64 = number_f64.abs().floor();

    if number_f64 < 0.0 && integer_f64 != 0.0 {
        integer_f64 = -integer_f64;
    }

    integer_f64.into()
}

// 7.1.6 ToInt32
pub fn to_int32(cx: Context, value_handle: Handle<Value>) -> EvalResult<i32> {
    // Fast pass if the value is a smi
    let value = value_handle.get();
    if value.is_smi() {
        return value.as_smi().into();
    }

    let number_value = maybe!(to_number(cx, value_handle));
    let f64_number = number_value.as_number();

    // All zeros, infinities, and NaNs map to 0
    if f64_number == 0.0 || !f64_number.is_finite() {
        return 0.into();
    }

    // Round float to an integer
    let mut i32_number = f64_number.abs().floor() as i64;
    if f64_number < 0.0 {
        i32_number = -i32_number;
    }

    // Compute modulus according to spec
    let u32_max = u32::MAX as i64 + 1;
    i32_number = modulo(i32_number, u32_max);

    // Then center in i32 range around 0
    if i32_number >= (u32_max >> 1) {
        i32_number -= u32_max;
    }

    (i32_number as i32).into()
}

// 7.1.7 ToUint32
pub fn to_uint32(cx: Context, value_handle: Handle<Value>) -> EvalResult<u32> {
    // Fast pass if the value is a non-negative smi
    let value = value_handle.get();
    if value.is_smi() {
        let i32_value = value.as_smi();
        if i32_value >= 0 {
            return (i32_value as u32).into();
        }
    }

    let number_value = maybe!(to_number(cx, value_handle));
    let f64_number = number_value.as_number();

    // All zeros, infinities, and NaNs map to 0
    if f64_number == 0.0 || !f64_number.is_finite() {
        return 0.into();
    }

    // Round float to an integer
    let mut u32_number = f64_number.abs().floor() as i64;
    if f64_number < 0.0 {
        u32_number = -u32_number;
    }

    // Compute modulus according to spec
    let u32_max = u32::MAX as i64 + 1;
    u32_number = modulo(u32_number, u32_max);

    (u32_number as u32).into()
}

// 7.1.8 ToInt16
pub fn to_int16(cx: Context, value_handle: Handle<Value>) -> EvalResult<i16> {
    // Fast path if the value is a smi
    let value = value_handle.get();
    if value.is_smi() {
        return (value.as_smi() as i16).into();
    }

    let number_value = maybe!(to_number(cx, value_handle));
    let f64_number = number_value.as_number();

    // All zeros, infinities, and NaNs map to 0
    if f64_number == 0.0 || !f64_number.is_finite() {
        return 0.into();
    }

    // Round float to an integer
    let mut i16_number = f64_number.abs().floor() as i64;
    if f64_number < 0.0 {
        i16_number = -i16_number;
    }

    // Compute modulus according to spec
    let u16_max = u16::MAX as i64 + 1;
    i16_number = modulo(i16_number, u16_max);

    // Then center in i16 range around 0
    if i16_number > (i16::MAX as i64) {
        i16_number -= u16_max;
    }

    (i16_number as i16).into()
}

// 7.1.9 ToUint16
pub fn to_uint16(cx: Context, value_handle: Handle<Value>) -> EvalResult<u16> {
    // Fast path if the value is a non-negative smi
    let value = value_handle.get();
    if value.is_smi() {
        let i32_value = value.as_smi();
        if i32_value >= 0 {
            return (i32_value as u16).into();
        }
    }

    let number_value = maybe!(to_number(cx, value_handle));
    let f64_number = number_value.as_number();

    // All zeros, infinities, and NaNs map to 0
    if f64_number == 0.0 || !f64_number.is_finite() {
        return 0.into();
    }

    // Round float to an integer
    let mut u16_number = f64_number.abs().floor() as i64;
    if f64_number < 0.0 {
        u16_number = -u16_number;
    }

    // Compute modulus according to spec
    let u16_max = u16::MAX as i64 + 1;
    u16_number = modulo(u16_number, u16_max);

    (u16_number as u16).into()
}

// 7.1.10 ToInt8
pub fn to_int8(cx: Context, value_handle: Handle<Value>) -> EvalResult<i8> {
    // Fast path if the value is a smi
    let value = value_handle.get();
    if value.is_smi() {
        return (value.as_smi() as i8).into();
    }

    let number_value = maybe!(to_number(cx, value_handle));
    let f64_number = number_value.as_number();

    // All zeros, infinities, and NaNs map to 0
    if f64_number == 0.0 || !f64_number.is_finite() {
        return 0.into();
    }

    // Round float to an integer
    let mut i8_number = f64_number.abs().floor() as i64;
    if f64_number < 0.0 {
        i8_number = -i8_number;
    }

    // Compute modulus according to spec
    let u8_max = u8::MAX as i64 + 1;
    i8_number = modulo(i8_number, u8_max);

    // Then center in i8 range around 0
    if i8_number > (i8::MAX as i64) {
        i8_number -= u8_max;
    }

    (i8_number as i8).into()
}

// 7.1.11 ToUint8
pub fn to_uint8(cx: Context, value_handle: Handle<Value>) -> EvalResult<u8> {
    // Fast path if the value is a non-negative smi
    let value = value_handle.get();
    if value.is_smi() {
        let i32_value = value.as_smi();
        if i32_value >= 0 {
            return (i32_value as u8).into();
        }
    }

    let number_value = maybe!(to_number(cx, value_handle));
    let f64_number = number_value.as_number();

    // All zeros, infinities, and NaNs map to 0
    if f64_number == 0.0 || !f64_number.is_finite() {
        return 0.into();
    }

    // Round float to an integer
    let mut u8_number = f64_number.abs().floor() as i64;
    if f64_number < 0.0 {
        u8_number = -u8_number;
    }

    // Compute modulus according to spec
    let u8_max = u8::MAX as i64 + 1;
    u8_number = ((u8_number % u8_max) + u8_max) % u8_max;

    (u8_number as u8).into()
}

// 7.1.12 ToUint8Clamp
pub fn to_uint8_clamp(cx: Context, value_handle: Handle<Value>) -> EvalResult<u8> {
    // Fast path if the value is a smi
    let value = value_handle.get();
    if value.is_smi() {
        let i32_value = value.as_smi();

        // Clamp within range
        if i32_value <= 0 {
            return 0.into();
        } else if i32_value >= (u8::MAX as i32) {
            return u8::MAX.into();
        } else {
            return (i32_value as u8).into();
        }
    }

    let number_value = maybe!(to_number(cx, value_handle));
    let f64_number = number_value.as_number();

    // Clamp within range
    if f64_number <= 0.0 {
        return 0.into();
    } else if f64_number >= MAX_U8_AS_F64 {
        return u8::MAX.into();
    } else if f64_number.is_nan() {
        return 0.into();
    }

    // Round to closest integer
    let floor = f64_number.floor();
    if floor + 0.5 < f64_number {
        return ((floor + 1.0) as u8).into();
    } else if f64_number < floor + 0.5 {
        return (floor as u8).into();
    }

    // Round ties to even
    if floor % 2.0 == 1.0 {
        return ((floor + 1.0) as u8).into();
    } else {
        return (floor as u8).into();
    }
}

// 7.1.13 ToBigInt
pub fn to_bigint(cx: Context, value: Handle<Value>) -> EvalResult<Handle<BigIntValue>> {
    let primitive_handle = maybe!(to_primitive(cx, value, ToPrimitivePreferredType::Number));
    let primitive = primitive_handle.get();

    if primitive.is_pointer() {
        match primitive.as_pointer().descriptor().kind() {
            ObjectKind::BigInt => return primitive_handle.as_bigint().into(),
            ObjectKind::String => {
                // May allocate
                return if let Some(bigint) = string_to_bigint(primitive_handle.as_string()) {
                    BigIntValue::new(cx, bigint).into()
                } else {
                    syntax_error(cx, "string does not represent a BigInt")
                };
            }
            _ => {}
        }
    } else if primitive.is_bool() {
        return if primitive.as_bool() {
            BigIntValue::new(cx, 1.into()).into()
        } else {
            BigIntValue::new(cx, 0.into()).into()
        };
    }

    type_error(cx, "value cannot be converted to BigInt")
}

// 7.1.15 ToBigInt64
pub fn to_big_int64(cx: Context, value: Handle<Value>) -> EvalResult<BigInt> {
    let bigint = maybe!(to_bigint(cx, value)).bigint();

    // Compute modulus according to spec
    let u64_max = (u64::MAX) as u128 + 1;
    let mut i64_number = ((bigint % u64_max) + u64_max) % u64_max;

    // TODO: Do not create BigInt here
    if i64_number > BigInt::from(i64::MAX) {
        i64_number -= u64_max;
    }

    i64_number.into()
}

// 7.1.16 ToBigUint64
pub fn to_big_uint64(cx: Context, value: Handle<Value>) -> EvalResult<BigInt> {
    let bigint = maybe!(to_bigint(cx, value)).bigint();

    // Compute modulus according to spec
    let u64_max = (u64::MAX) as u128 + 1;
    let u64_number = ((bigint % u64_max) + u64_max) % u64_max;

    u64_number.into()
}

// 7.1.17 ToString
pub fn to_string(mut cx: Context, value_handle: Handle<Value>) -> EvalResult<Handle<StringValue>> {
    // Safe since value is never referenced after allocation
    let value = value_handle.get();

    // Fast path
    if value.is_string() {
        return value_handle.as_string().into();
    }

    if value.is_pointer() {
        if value.as_pointer().descriptor().is_object() {
            let primitive_value =
                maybe!(to_primitive(cx, value_handle, ToPrimitivePreferredType::String));
            to_string(cx, primitive_value)
        } else {
            match value.as_pointer().descriptor().kind() {
                ObjectKind::BigInt => {
                    let bigint_string = value.as_bigint().bigint().to_string();
                    cx.alloc_string(&bigint_string).into()
                }
                ObjectKind::Symbol => type_error(cx, "symbol cannot be converted to string"),
                _ => unreachable!(),
            }
        }
    } else {
        match value.get_tag() {
            NULL_TAG => cx.alloc_string("null").into(),
            UNDEFINED_TAG => cx.alloc_string("undefined").into(),
            BOOL_TAG => {
                let str = if value.as_bool() { "true" } else { "false" };
                cx.alloc_string(str).into()
            }
            SMI_TAG => {
                let smi_string = value.as_smi().to_string();
                cx.alloc_string(&smi_string).into()
            }
            // Otherwise must be double
            _ => {
                let double_string = number_to_string(value.as_double());
                cx.alloc_string(&double_string).into()
            }
        }
    }
}

// 7.1.18 ToObject
pub fn to_object(cx: Context, value_handle: Handle<Value>) -> EvalResult<Handle<ObjectValue>> {
    // Safe since pointer value is never referenced after allocation
    let value = value_handle.get();

    if value.is_pointer() {
        // Fast path
        if value.as_pointer().descriptor().is_object() {
            value_handle.as_object().into()
        } else {
            match value.as_pointer().descriptor().kind() {
                ObjectKind::String => {
                    let object: Handle<ObjectValue> =
                        StringObject::new_from_value(cx, value_handle.as_string()).into();
                    object.into()
                }
                ObjectKind::Symbol => {
                    let object: Handle<ObjectValue> =
                        SymbolObject::new_from_value(cx, value_handle.as_symbol()).into();
                    object.into()
                }
                ObjectKind::BigInt => {
                    let object: Handle<ObjectValue> =
                        BigIntObject::new_from_value(cx, value_handle.as_bigint()).into();
                    object.into()
                }
                _ => unreachable!(),
            }
        }
    } else {
        match value.get_tag() {
            NULL_TAG => type_error(cx, "null has no properties"),
            UNDEFINED_TAG => type_error(cx, "undefined has no properties"),
            BOOL_TAG => {
                let object: Handle<ObjectValue> = BooleanObject::new(cx, value.as_bool()).into();
                object.into()
            }
            // Otherwise is a number, either double or smi
            _ => {
                let object: Handle<ObjectValue> = NumberObject::new(cx, value.as_number()).into();
                object.into()
            }
        }
    }
}

// 7.1.20 ToLength
pub fn to_length(cx: Context, value: Handle<Value>) -> EvalResult<u64> {
    let len = maybe!(to_integer_or_infinity(cx, value));
    if len <= 0.0 {
        return 0.into();
    }

    let len_in_int_range = f64::min(len, MAX_SAFE_INTEGER_F64);

    // Safe since we have guaranteed that length is positive and within safe range
    let len_u64: u64 = unsafe { len_in_int_range.to_int_unchecked() };

    len_u64.into()
}

// 7.1.21 CanonicalNumericIndexString
// Determines if the given key is a canonical numeric index string, and validates that it can be
// used as the index into an array with a particular length.
//
// - Returns Some(Some(index)) if the key is a canonical numeric index that is an integer and in
//   range (greater than or equal to 0 and below the given array length).
// - Returns Some(None) if the key is a canonical numeric index but not an integer or not in range
// - Returns None if the key is not a canonical numeric index
pub fn canonical_numeric_index_string(
    cx: Context,
    key: Handle<PropertyKey>,
    array_length: usize,
) -> Option<Option<usize>> {
    if key.is_array_index() {
        // Fast path for array indices
        let array_index = key.as_array_index() as usize;
        if array_index < array_length {
            Some(Some(array_index))
        } else {
            Some(None)
        }
    } else if key.is_string() {
        // Otherwise must convert to number then back to string
        let key_string = key.as_string();
        let number_value = must!(to_number(cx, key_string.into()));

        // If string representations are equal, must be canonical numeric index
        let number_string = must!(to_string(cx, number_value));
        if key_string.eq(&number_string) {
            if !is_integral_number(number_value.get()) {
                return Some(None);
            }

            let number = number_value.as_number();
            if number.is_sign_negative() {
                return Some(None);
            }

            let number = number as usize;
            if number >= array_length {
                return Some(None);
            }

            Some(Some(number))
        } else if key_string
            .get_()
            .as_flat()
            .eq(&cx.names.negative_zero.as_string().as_flat())
        {
            // The string "-0" is a canonical numeric index but is never valid as an index
            Some(None)
        } else {
            None
        }
    } else {
        None
    }
}

/// Identical to CanonicalNumericIndexString, but for u32 string indices
pub fn canonical_numeric_string_index_string(
    cx: Context,
    key: Handle<PropertyKey>,
    string_length: u32,
) -> Option<Option<u32>> {
    if key.is_array_index() {
        // Fast path for array indices
        let array_index = key.as_array_index();
        if array_index < string_length {
            Some(Some(array_index))
        } else {
            Some(None)
        }
    } else if key.is_string() {
        // Otherwise must convert to number then back to string
        let key_string = key.as_string();
        let number_value = must!(to_number(cx, key_string.into()));

        // If string representations are equal, must be canonical numeric index
        let number_string = must!(to_string(cx, number_value));
        if key_string.eq(&number_string) {
            if !is_integral_number(number_value.get()) {
                return Some(None);
            }

            let number = number_value.as_number();
            if number.is_sign_negative() {
                return Some(None);
            }

            let number = number as usize;
            if number >= string_length as usize {
                return Some(None);
            }

            Some(Some(number as u32))
        } else if key_string
            .get_()
            .as_flat()
            .eq(&cx.names.negative_zero.as_string().as_flat())
        {
            // The string "-0" is a canonical numeric index but is never valid as an index
            Some(None)
        } else {
            None
        }
    } else {
        None
    }
}

// 7.1.22 ToIndex
pub fn to_index(cx: Context, value_handle: Handle<Value>) -> EvalResult<usize> {
    let value = value_handle.get();
    if value.is_smi() {
        let smi = value.as_smi();
        if smi < 0 {
            range_error(cx, &format!("{} is out of range for an array index", smi))
        } else {
            (smi as usize).into()
        }
    } else if value.is_undefined() {
        0.into()
    } else {
        let integer = maybe!(to_integer_or_infinity(cx, value_handle));
        if integer < 0.0 || integer > MAX_SAFE_INTEGER_F64 {
            range_error(cx, &format!("{} is out of range for an array index", integer))
        } else {
            (integer as usize).into()
        }
    }
}

// 7.2.1 RequireObjectCoercible
pub fn require_object_coercible(cx: Context, value: Handle<Value>) -> EvalResult<Handle<Value>> {
    if value.is_nullish() {
        if value.is_null() {
            return type_error(cx, "can't convert null to object");
        }

        return type_error(cx, "can't convert undefined to object");
    }

    value.into()
}

// 7.2.2 IsArray
pub fn is_array(cx: Context, value: Handle<Value>) -> EvalResult<bool> {
    if !value.is_object() {
        return false.into();
    }

    let object_value = value.as_object();
    if object_value.is_array() {
        return true.into();
    }

    if object_value.is_proxy() {
        let proxy = object_value.cast::<ProxyObject>();
        if proxy.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        return is_array(cx, proxy.target().unwrap().into());
    }

    return false.into();
}

// 7.2.3 IsCallable
pub fn is_callable(value: Handle<Value>) -> bool {
    if !value.is_object() {
        return false;
    }

    is_callable_object(value.as_object())
}

pub fn is_callable_object(value: Handle<ObjectValue>) -> bool {
    let kind = value.descriptor().kind();
    if kind == ObjectKind::Closure {
        true
    } else if kind == ObjectKind::Proxy {
        value.cast::<ProxyObject>().is_callable()
    } else {
        false
    }
}

// 7.2.4 IsConstructor
pub fn is_constructor_value(value: Handle<Value>) -> bool {
    if !value.is_object() {
        return false;
    }

    is_constructor_object_value(value.as_object())
}

pub fn is_constructor_object_value(value: Handle<ObjectValue>) -> bool {
    let kind = value.descriptor().kind();
    if kind == ObjectKind::Closure {
        value.cast::<Closure>().function_ptr().is_constructor()
    } else if kind == ObjectKind::Proxy {
        value.cast::<ProxyObject>().is_constructor()
    } else {
        false
    }
}

// 7.2.6 IsIntegralNumber
pub fn is_integral_number(value: Value) -> bool {
    if value.is_smi() {
        return true;
    } else if !value.is_double() {
        return false;
    } else if value.is_nan() || value.is_infinity() {
        return false;
    }

    let number = value.as_double();

    number.trunc() == number
}

// 7.2.8 IsRegExp
// If this returns true the value must be an object.
pub fn is_regexp(cx: Context, value: Handle<Value>) -> EvalResult<bool> {
    if !value.is_object() {
        return false.into();
    }

    let object = value.as_object();
    let match_key = cx.well_known_symbols.match_();
    let matcher = maybe!(get(cx, object, match_key));

    if !matcher.is_undefined() {
        return to_boolean(matcher.get()).into();
    }

    object.is_regexp_object().into()
}

// 7.2.11 SameValue
pub fn same_value(v1_handle: Handle<Value>, v2_handle: Handle<Value>) -> bool {
    let v1 = v1_handle;
    let v2 = v2_handle;

    // Same as is_strictly_equal, but treats NaN as equal to itself, and does not treat differently
    // signed zeros as equal.
    if v1.is_number() {
        if v2.is_number() {
            if v1.is_nan() && v2.is_nan() {
                return true;
            }

            if v1.is_positive_zero() && v2.is_negative_zero()
                || v1.is_negative_zero() && v2.is_positive_zero()
            {
                return false;
            }

            return v1.as_number() == v2.as_number();
        } else {
            return false;
        }
    }

    same_value_non_numeric(v1_handle, v2_handle)
}

// 7.2.12 SameValueZero
pub fn same_value_zero(v1_handle: Handle<Value>, v2_handle: Handle<Value>) -> bool {
    let v1 = v1_handle.get();
    let v2 = v2_handle.get();

    // Same as same)value, but treats differently signed zeros as equal
    if v1.is_number() {
        if v2.is_number() {
            if v1.is_nan() && v2.is_nan() {
                return true;
            }

            return v1.as_number() == v2.as_number();
        } else {
            return false;
        }
    }

    same_value_non_numeric(v1_handle, v2_handle)
}

// Same as same_value_zero but cannot allocate. Callers must ensure that all string values passed to
// this function are flat.
pub fn same_value_zero_non_allocating(v1: Value, v2: Value) -> bool {
    // Same as same_value, but treats differently signed zeros as equal
    if v1.is_number() {
        if v2.is_number() {
            if v1.is_nan() && v2.is_nan() {
                return true;
            }

            return v1.as_number() == v2.as_number();
        } else {
            return false;
        }
    }

    same_value_non_numeric_non_allocating(v1, v2)
}

// 7.2.13 SameValueNonNumeric, also includes BigInt handling
#[inline]
fn same_value_non_numeric(v1_handle: Handle<Value>, v2_handle: Handle<Value>) -> bool {
    let v1 = v1_handle.get();
    let v2 = v2_handle.get();

    // Fast path, if values have same bits they are always equal
    if v1.as_raw_bits() == v2.as_raw_bits() {
        return true;
    }

    // Only strings and BigInts may have the same value but different bit patterns, since
    // non-pointer values are all unique, and objects and symbols are identified by their address.
    if v1.is_pointer() && v2.is_pointer() {
        let kind1 = v1.as_pointer().descriptor().kind();
        if kind1 == v2.as_pointer().descriptor().kind() {
            match kind1 {
                ObjectKind::String => {
                    // May allocate
                    return v1_handle.as_string().eq(&v2_handle.as_string());
                }
                ObjectKind::BigInt => return v1.as_bigint().bigint().eq(&v2.as_bigint().bigint()),
                _ => {}
            }
        }
    }

    false.into()
}

// Same as same_value_non_numeric but cannot allocate. Callers must ensure that all string values
// passed to this function are flat (but only if both values are strings).
#[inline]
pub fn same_value_non_numeric_non_allocating(v1: Value, v2: Value) -> bool {
    // Fast path, if values have same bits they are always equal
    if v1.as_raw_bits() == v2.as_raw_bits() {
        return true;
    }

    // Only strings and BigInts may have the same value but different bit patterns, since
    // non-pointer values are all unique, and objects and symbols are identified by their address.
    if v1.is_pointer() && v2.is_pointer() {
        let kind1 = v1.as_pointer().descriptor().kind();
        if kind1 == v2.as_pointer().descriptor().kind() {
            match kind1 {
                ObjectKind::String => {
                    // Must be flat strings to be non allocating
                    let v1_string = v1.as_string();
                    let v2_string = v2.as_string();

                    debug_assert!(v1_string.is_flat());
                    debug_assert!(v2_string.is_flat());

                    // Cannot allocate
                    return v1_string.as_flat() == v2_string.as_flat();
                }
                ObjectKind::BigInt => return v1.as_bigint().bigint().eq(&v2.as_bigint().bigint()),
                _ => {}
            }
        }
    }

    false.into()
}

// 7.1.14 StringToBigInt
fn string_to_bigint(value: Handle<StringValue>) -> Option<BigInt> {
    parse_string_to_bigint(value)
}

// 7.1.19 ToPropertyKey
pub fn to_property_key(
    cx: Context,
    value_handle: Handle<Value>,
) -> EvalResult<Handle<PropertyKey>> {
    let value = value_handle.get();
    if value.is_smi() {
        let smi_value = value.as_smi();
        if smi_value >= 0 {
            return PropertyKey::array_index(cx, smi_value as u32)
                .to_handle(cx)
                .into();
        }
    }

    let key = maybe!(to_primitive(cx, value_handle, ToPrimitivePreferredType::String));
    if key.is_string() {
        return PropertyKey::string(cx, key.as_string())
            .to_handle(cx)
            .into();
    } else if key.is_symbol() {
        return PropertyKey::symbol(key.as_symbol()).into();
    }

    let string_key = maybe!(to_string(cx, key));
    PropertyKey::string(cx, string_key).to_handle(cx).into()
}

// 7.2.14 IsLessThan
// ToPrimitive calls are inlined at call sites instead of passing LeftFirst argument.
// Returns either a bool or undefined if values cannot be compared.
pub fn is_less_than(
    cx: Context,
    x_handle: Handle<Value>,
    y_handle: Handle<Value>,
) -> EvalResult<Value> {
    // Safe since allocation can only occur during to_numeric, and direct values are not held
    // across the to_numeric calls.
    let x = x_handle.get();
    let y = y_handle.get();

    if x.is_pointer() && y.is_pointer() {
        let x_kind = x.as_pointer().descriptor().kind();
        let y_kind = y.as_pointer().descriptor().kind();

        if x_kind == ObjectKind::String {
            if y_kind == ObjectKind::String {
                // May allocate
                return (x_handle.as_string().cmp(&y_handle.as_string()).is_lt()).into();
            } else if y_kind == ObjectKind::BigInt {
                // May allocate
                let x_bigint = string_to_bigint(x_handle.as_string());

                return if let Some(x_bigint) = x_bigint {
                    x_bigint.lt(&y_handle.as_bigint().bigint()).into()
                } else {
                    Value::undefined().into()
                };
            }
        }

        if x_kind == ObjectKind::BigInt && y_kind == ObjectKind::String {
            // May allocate
            let y_bigint = string_to_bigint(y_handle.as_string());

            return if let Some(y_bigint) = y_bigint {
                x_handle.as_bigint().bigint().lt(&y_bigint).into()
            } else {
                Value::undefined().into()
            };
        }
    }

    let num_x_handle = maybe!(to_numeric(cx, x_handle));
    let num_y_handle = maybe!(to_numeric(cx, y_handle));

    let num_x = num_x_handle.get();
    let num_y = num_y_handle.get();

    let x_is_bigint = num_x.is_bigint();
    let y_is_bigint = num_y.is_bigint();
    if x_is_bigint == y_is_bigint {
        if x_is_bigint {
            return num_x
                .as_bigint()
                .bigint()
                .lt(&num_y.as_bigint().bigint())
                .into();
        } else {
            // Both are numbers
            if num_x.is_nan() || num_y.is_nan() {
                return Value::undefined().into();
            }

            (num_x.as_number() < num_y.as_number()).into()
        }
    } else if x_is_bigint {
        // x is a BigInt and y is a number
        if num_y.is_nan() {
            return Value::undefined().into();
        }

        let y_f64 = num_y.as_number();
        if y_f64 == f64::INFINITY {
            return true.into();
        } else if y_f64 == f64::NEG_INFINITY {
            return false.into();
        }

        // BigInt conversion truncates towards 0, so we must account for the possible fraction part
        let y_has_fract = y_f64.trunc() != y_f64;
        let y_bigint = y_f64.to_bigint().unwrap();

        match num_x.as_bigint().bigint().cmp(&y_bigint) {
            Ordering::Less => true.into(),
            Ordering::Equal => {
                // If y had a fractional part it was truncated towards 0. This means that if y had a
                // fractional part and is positive it is greater than x, and if y had a fractional
                // part and is negative it is less than x.
                if y_has_fract {
                    (y_f64 > 0.0).into()
                } else {
                    false.into()
                }
            }
            Ordering::Greater => false.into(),
        }
    } else {
        // x is a number and y is a BigInt
        if num_x.is_nan() {
            return Value::undefined().into();
        }

        let x_f64 = num_x.as_number();
        if x_f64 == f64::INFINITY {
            return false.into();
        } else if x_f64 == f64::NEG_INFINITY {
            return true.into();
        }

        // BigInt conversion truncates towards 0, so we must account for the possible fraction part
        let x_has_fract = x_f64.trunc() != x_f64;
        let x_bigint = x_f64.to_bigint().unwrap();

        match x_bigint.cmp(&num_y.as_bigint().bigint()) {
            Ordering::Less => true.into(),
            Ordering::Equal => {
                // If x had a fractional part it was truncated towards 0. This means that if x had a
                // fractional part and is positive it is greater than y, and if x had a fractional
                // part and is negative it is less than y.
                if x_has_fract {
                    (x_f64 < 0.0).into()
                } else {
                    false.into()
                }
            }
            Ordering::Greater => false.into(),
        }
    }
}

// 7.2.15 IsLooselyEqual
pub fn is_loosely_equal(
    cx: Context,
    v1_handle: Handle<Value>,
    v2_handle: Handle<Value>,
) -> EvalResult<bool> {
    // Safe since allocation can only occur during to_number, to_primitive, and recursive calls to
    // is_loosely_equal, and direct values are not held across these calls.
    let v1 = v1_handle.get();
    let v2 = v2_handle.get();

    // If values have the same type, then use (inlined) is_strictly_equal.
    //
    // All type combinations from spec are broken up here and in the match expression at the end of
    // this function.
    if v1.is_number() {
        if v2.is_number() {
            return (v1.as_number() == v2.as_number()).into();
        }

        return if v2.is_pointer() {
            match v2.as_pointer().descriptor().kind() {
                ObjectKind::String => {
                    // May allocate
                    let number_v2 = string_to_number(v2_handle.as_string());
                    (v1_handle.as_number() == number_v2.as_number()).into()
                }
                ObjectKind::BigInt => {
                    if v1.is_nan() || v1.is_infinity() {
                        return false.into();
                    }

                    let v1_f64 = v1.as_number();

                    // Number must be an integer to be equal to a BigInt
                    if v1_f64.trunc() != v1_f64 {
                        return false.into();
                    }

                    // Now that we know number is an integer, it can losslessly be converted to a BigInt
                    let v1_bigint = v1_f64.to_bigint().unwrap();
                    let v2_bigint = v2.as_bigint().bigint();

                    (v1_bigint == v2_bigint).into()
                }
                ObjectKind::Symbol => false.into(),
                // Otherwise must be an object
                _ => {
                    let primitive_v2 =
                        maybe!(to_primitive(cx, v2_handle, ToPrimitivePreferredType::None));
                    is_loosely_equal(cx, v1_handle, primitive_v2)
                }
            }
        } else {
            if v2.is_bool() {
                let v2_number = maybe!(to_number(cx, v2_handle));
                is_loosely_equal(cx, v1_handle, v2_number)
            } else {
                false.into()
            }
        };
    }

    // Fast path - values with the same bit patterns are equal
    if v1.as_raw_bits() == v2.as_raw_bits() {
        return true.into();
    }

    let tag1 = v1.get_tag();
    let tag2 = v2.get_tag();

    // Handle comparisons of the same type
    if tag1 == tag2 {
        if tag1 == POINTER_TAG {
            let kind1 = v1.as_pointer().descriptor().kind();
            let kind2 = v2.as_pointer().descriptor().kind();
            if kind1 == kind2 {
                return match kind1 {
                    // Only strings and BigInts may have the same value but different bit patterns
                    ObjectKind::String => {
                        // May allocate
                        v1_handle.as_string().eq(&v2_handle.as_string()).into()
                    }
                    ObjectKind::BigInt => {
                        v1.as_bigint().bigint().eq(&v2.as_bigint().bigint()).into()
                    }
                    _ => false.into(),
                };
            // Two objects with different bit patterns are always unequal
            } else if v1.as_pointer().descriptor().is_object()
                && v2.as_pointer().descriptor().is_object()
            {
                return false.into();
            }
        } else {
            return false.into();
        }
    }

    // Handle comparisons that implicitly convert between types

    // Nullish values are loosely equal
    if (tag1 == NULL_TAG && tag2 == UNDEFINED_TAG) || (tag1 == UNDEFINED_TAG && tag2 == NULL_TAG) {
        return true.into();
    }

    // Convert bools to numbers and try again
    if tag1 == BOOL_TAG {
        let v1_number = maybe!(to_number(cx, v1_handle));
        return is_loosely_equal(cx, v1_number, v2_handle);
    } else if tag2 == BOOL_TAG {
        let v2_number = maybe!(to_number(cx, v2_handle));
        return is_loosely_equal(cx, v1_handle, v2_number);
    }

    // Implicit conversions involving numbers
    if tag2 == SMI_TAG || v2.is_double() {
        return if tag1 == POINTER_TAG {
            let kind = v1.as_pointer().descriptor().kind();
            match kind {
                ObjectKind::String => {
                    // May allocate
                    let v1_number = string_to_number(v1_handle.as_string());
                    (v1_number.as_number() == v2_handle.as_number()).into()
                }
                ObjectKind::BigInt => {
                    if v2.is_nan() || v2.is_infinity() {
                        return false.into();
                    }

                    let v2_f64 = v2.as_number();

                    // Number must be an integer to be equal to a BigInt
                    if v2_f64.trunc() != v2_f64 {
                        return false.into();
                    }

                    // Now that we know number is an integer, it can losslessly be converted to a BigInt
                    let v2_bigint = v2_f64.to_bigint().unwrap();
                    let v1_bigint = v1.as_bigint().bigint();

                    (v1_bigint == v2_bigint).into()
                }
                ObjectKind::Symbol => false.into(),
                // Otherwise must be an object
                _ => {
                    let v1_primitive =
                        maybe!(to_primitive(cx, v1_handle, ToPrimitivePreferredType::None));
                    is_loosely_equal(cx, v1_primitive, v2_handle)
                }
            }
        } else {
            false.into()
        };
    }

    if tag1 == POINTER_TAG && tag2 == POINTER_TAG {
        let kind1 = v1.as_pointer().descriptor().kind();
        let kind2 = v2.as_pointer().descriptor().kind();

        // Strings are implicitly converted to BigInts
        match (kind1, kind2) {
            (ObjectKind::String, ObjectKind::BigInt) => {
                // May allocate
                let v1_bigint = string_to_bigint(v1_handle.as_string());
                return if let Some(v1_bigint) = v1_bigint {
                    v1_bigint.eq(&v2_handle.as_bigint().bigint()).into()
                } else {
                    false.into()
                };
            }
            (ObjectKind::BigInt, ObjectKind::String) => {
                // May allocate
                let v2_bigint = string_to_bigint(v2_handle.as_string());
                return if let Some(v2_bigint) = v2_bigint {
                    v1_handle.as_bigint().bigint().eq(&v2_bigint).into()
                } else {
                    false.into()
                };
            }
            _ => {}
        }

        // At this point, if one value is an object the other value is guaranteed to be a non-object
        // pointer value (aka string, symbol, or BigInt).
        if v1.as_pointer().descriptor().is_object() {
            let v1_primitive = maybe!(to_primitive(cx, v1_handle, ToPrimitivePreferredType::None));
            return is_loosely_equal(cx, v1_primitive, v2_handle);
        } else if v2.as_pointer().descriptor().is_object() {
            let primitive_v2 = maybe!(to_primitive(cx, v2_handle, ToPrimitivePreferredType::None));
            return is_loosely_equal(cx, v1_handle, primitive_v2);
        }
    }

    false.into()
}

// 7.2.16 IsStrictlyEqual
pub fn is_strictly_equal(v1_handle: Handle<Value>, v2_handle: Handle<Value>) -> bool {
    let v1 = v1_handle.get();
    let v2 = v2_handle.get();

    if v1.is_number() {
        if v2.is_number() {
            return v1.as_number() == v2.as_number();
        } else {
            return false;
        }
    }

    same_value_non_numeric(v1_handle, v2_handle)
}

// Specialization of SameValue for objects, checks object identity
#[inline]
pub fn same_object_value(value1: HeapPtr<ObjectValue>, value2: HeapPtr<ObjectValue>) -> bool {
    value1.ptr_eq(&value2)
}

#[inline]
pub fn same_object_value_handles(value1: Handle<ObjectValue>, value2: Handle<ObjectValue>) -> bool {
    value1.get_().ptr_eq(&value2.get_())
}

// Specialization of SameValue for optional objects, checks object identity
#[inline]
pub fn same_opt_object_value(
    value1: Option<HeapPtr<ObjectValue>>,
    value2: Option<HeapPtr<ObjectValue>>,
) -> bool {
    match (value1, value2) {
        (None, None) => true,
        (Some(value1), Some(value2)) => value1.ptr_eq(&value2),
        _ => false,
    }
}

#[inline]
pub fn same_opt_object_value_handles(
    value1: Option<Handle<ObjectValue>>,
    value2: Option<Handle<ObjectValue>>,
) -> bool {
    match (value1, value2) {
        (None, None) => true,
        (Some(value1), Some(value2)) => value1.get_().ptr_eq(&value2.get_()),
        _ => false,
    }
}

// 6.1.6.1.20 Number::toString
pub fn number_to_string(x: f64) -> String {
    let mut buf = ryu_js::Buffer::new();
    buf.format(x).to_string()
}
