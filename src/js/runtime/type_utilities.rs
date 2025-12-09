use std::cmp::Ordering;

use num_bigint::{BigInt, ToBigInt};

use crate::{
    common::math::modulo,
    must_a,
    runtime::{alloc_error::AllocResult, string_parsing::StringLexer},
};

use super::{
    abstract_operations::{call_object, get, get_method},
    bytecode::function::Closure,
    error::{range_error, syntax_error, type_error},
    eval_result::EvalResult,
    gc::{Handle, HeapPtr},
    heap_item_descriptor::HeapItemKind,
    intrinsics::{
        bigint_constructor::BigIntObject, boolean_constructor::BooleanObject,
        number_constructor::NumberObject, symbol_constructor::SymbolObject,
    },
    numeric_constants::{MAX_SAFE_INTEGER_F64, MAX_U8_AS_F64},
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

/// ToPrimitive (https://tc39.es/ecma262/#sec-toprimitive)
#[inline]
pub fn to_primitive(
    mut cx: Context,
    value: Handle<Value>,
    mut preferred_type: ToPrimitivePreferredType,
) -> EvalResult<Handle<Value>> {
    if !value.is_object() {
        return Ok(value);
    }

    let to_primitive_key = cx.well_known_symbols.to_primitive();
    let exotic_prim = get_method(cx, value, to_primitive_key)?;
    match exotic_prim {
        Some(exotic_prim) => {
            let hint_str = match preferred_type {
                ToPrimitivePreferredType::None => "default",
                ToPrimitivePreferredType::Number => "number",
                ToPrimitivePreferredType::String => "string",
            };
            let hint_value: Handle<Value> = cx.alloc_string(hint_str)?.into();

            let result = call_object(cx, exotic_prim, value, &[hint_value])?;
            if result.is_object() {
                return type_error(cx, "object cannot be converted to primitive");
            }

            Ok(result)
        }
        None => {
            if preferred_type == ToPrimitivePreferredType::None {
                preferred_type = ToPrimitivePreferredType::Number;
            }

            ordinary_to_primitive(cx, value.as_object(), preferred_type)
        }
    }
}

/// OrdinaryToPrimitive (https://tc39.es/ecma262/#sec-ordinarytoprimitive)
pub fn ordinary_to_primitive(
    cx: Context,
    object: Handle<ObjectValue>,
    preferred_type: ToPrimitivePreferredType,
) -> EvalResult<Handle<Value>> {
    let object_value: Handle<Value> = object.into();

    macro_rules! call_method {
        ($method_name:expr) => {
            let method = get(cx, object, $method_name)?;
            if is_callable(method) {
                let result = call_object(cx, method.as_object(), object_value, &[])?;
                if !result.is_object() {
                    return Ok(result);
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

/// ToBoolean (https://tc39.es/ecma262/#sec-toboolean)
pub fn to_boolean(value: Value) -> bool {
    // Fast path
    let tag = value.get_tag();
    if tag == BOOL_TAG {
        return value.as_bool();
    }

    if value.is_pointer() {
        match value.as_pointer().descriptor().kind() {
            HeapItemKind::String => !value.as_string().is_empty(),
            HeapItemKind::BigInt => value.as_bigint().bigint().ne(&BigInt::default()),
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

/// ToNumeric (https://tc39.es/ecma262/#sec-tonumeric)
pub fn to_numeric(cx: Context, value: Handle<Value>) -> EvalResult<Handle<Value>> {
    let prim_value = to_primitive(cx, value, ToPrimitivePreferredType::Number)?;
    if prim_value.is_bigint() {
        return Ok(prim_value);
    }

    to_number(cx, prim_value)
}

/// ToNumber (https://tc39.es/ecma262/#sec-tonumber)
pub fn to_number(cx: Context, value_handle: Handle<Value>) -> EvalResult<Handle<Value>> {
    // Safe since value is never referenced after allocation
    let value = *value_handle;

    // Fast path
    if value.is_number() {
        return Ok(value_handle);
    }

    if value.is_pointer() {
        if value.as_pointer().descriptor().is_object() {
            let primitive_value = to_primitive(cx, value_handle, ToPrimitivePreferredType::Number)?;
            to_number(cx, primitive_value)
        } else {
            match value.as_pointer().descriptor().kind() {
                // May allocate
                HeapItemKind::String => {
                    Ok(string_to_number(value_handle.as_string())?.to_handle(cx))
                }
                HeapItemKind::Symbol => type_error(cx, "symbol cannot be converted to number"),
                HeapItemKind::BigInt => type_error(cx, "BigInt cannot be converted to number"),
                _ => unreachable!(),
            }
        }
    } else {
        match value.get_tag() {
            NULL_TAG => Ok(cx.zero()),
            UNDEFINED_TAG => Ok(cx.nan()),
            BOOL_TAG => {
                if value.as_bool() {
                    Ok(cx.one())
                } else {
                    Ok(cx.zero())
                }
            }
            _ => unreachable!(),
        }
    }
}

/// StringToNumber (https://tc39.es/ecma262/#sec-stringtonumber)
fn string_to_number(value: Handle<StringValue>) -> AllocResult<Value> {
    let lexer = StringLexer::new(value)?;
    Ok(match parse_string_to_number(lexer) {
        None => Value::nan(),
        Some(num) => Value::number(num),
    })
}

/// ToIntegerOrInfinity (https://tc39.es/ecma262/#sec-tointegerorinfinity)
pub fn to_integer_or_infinity(cx: Context, value: Handle<Value>) -> EvalResult<f64> {
    let number_handle = to_number(cx, value)?;
    let number = *number_handle;

    Ok(to_integer_or_infinity_f64(number.as_number()))
}

pub fn to_integer_or_infinity_f64(number_f64: f64) -> f64 {
    if number_f64.is_nan() || number_f64 == 0.0 {
        return 0.0;
    }

    if number_f64.is_infinite() {
        return number_f64;
    }

    let mut integer_f64 = number_f64.abs().floor();

    if number_f64 < 0.0 && integer_f64 != 0.0 {
        integer_f64 = -integer_f64;
    }

    integer_f64
}

/// ToInt32 (https://tc39.es/ecma262/#sec-toint32)
pub fn to_int32(cx: Context, value_handle: Handle<Value>) -> EvalResult<i32> {
    // Fast pass if the value is a smi
    let value = *value_handle;
    if value.is_smi() {
        return Ok(value.as_smi());
    }

    let number_value = to_number(cx, value_handle)?;
    let f64_number = number_value.as_number();

    // All zeros, infinities, and NaNs map to 0
    if f64_number == 0.0 || !f64_number.is_finite() {
        return Ok(0);
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

    Ok(i32_number as i32)
}

/// ToUint32 (https://tc39.es/ecma262/#sec-touint32)
pub fn to_uint32(cx: Context, value_handle: Handle<Value>) -> EvalResult<u32> {
    // Fast pass if the value is a non-negative smi
    let value = *value_handle;
    if value.is_smi() {
        let i32_value = value.as_smi();
        if i32_value >= 0 {
            return Ok(i32_value as u32);
        }
    }

    let number_value = to_number(cx, value_handle)?;
    let f64_number = number_value.as_number();

    // All zeros, infinities, and NaNs map to 0
    if f64_number == 0.0 || !f64_number.is_finite() {
        return Ok(0);
    }

    // Round float to an integer
    let mut u32_number = f64_number.abs().floor() as i64;
    if f64_number < 0.0 {
        u32_number = -u32_number;
    }

    // Compute modulus according to spec
    let u32_max = u32::MAX as i64 + 1;
    u32_number = modulo(u32_number, u32_max);

    Ok(u32_number as u32)
}

/// ToInt16 (https://tc39.es/ecma262/#sec-toint16)
pub fn to_int16(cx: Context, value_handle: Handle<Value>) -> EvalResult<i16> {
    // Fast path if the value is a smi
    let value = *value_handle;
    if value.is_smi() {
        return Ok(value.as_smi() as i16);
    }

    let number_value = to_number(cx, value_handle)?;
    let f64_number = number_value.as_number();

    // All zeros, infinities, and NaNs map to 0
    if f64_number == 0.0 || !f64_number.is_finite() {
        return Ok(0);
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

    Ok(i16_number as i16)
}

/// ToUint16 (https://tc39.es/ecma262/#sec-touint16)
pub fn to_uint16(cx: Context, value_handle: Handle<Value>) -> EvalResult<u16> {
    // Fast path if the value is a non-negative smi
    let value = *value_handle;
    if value.is_smi() {
        let i32_value = value.as_smi();
        if i32_value >= 0 {
            return Ok(i32_value as u16);
        }
    }

    let number_value = to_number(cx, value_handle)?;
    let f64_number = number_value.as_number();

    // All zeros, infinities, and NaNs map to 0
    if f64_number == 0.0 || !f64_number.is_finite() {
        return Ok(0);
    }

    // Round float to an integer
    let mut u16_number = f64_number.abs().floor() as i64;
    if f64_number < 0.0 {
        u16_number = -u16_number;
    }

    // Compute modulus according to spec
    let u16_max = u16::MAX as i64 + 1;
    u16_number = modulo(u16_number, u16_max);

    Ok(u16_number as u16)
}

/// ToInt8 (https://tc39.es/ecma262/#sec-toint8)
pub fn to_int8(cx: Context, value_handle: Handle<Value>) -> EvalResult<i8> {
    // Fast path if the value is a smi
    let value = *value_handle;
    if value.is_smi() {
        return Ok(value.as_smi() as i8);
    }

    let number_value = to_number(cx, value_handle)?;
    let f64_number = number_value.as_number();

    // All zeros, infinities, and NaNs map to 0
    if f64_number == 0.0 || !f64_number.is_finite() {
        return Ok(0);
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

    Ok(i8_number as i8)
}

/// ToUint8 (https://tc39.es/ecma262/#sec-touint8)
pub fn to_uint8(cx: Context, value_handle: Handle<Value>) -> EvalResult<u8> {
    // Fast path if the value is a non-negative smi
    let value = *value_handle;
    if value.is_smi() {
        let i32_value = value.as_smi();
        if i32_value >= 0 {
            return Ok(i32_value as u8);
        }
    }

    let number_value = to_number(cx, value_handle)?;
    let f64_number = number_value.as_number();

    // All zeros, infinities, and NaNs map to 0
    if f64_number == 0.0 || !f64_number.is_finite() {
        return Ok(0);
    }

    // Round float to an integer
    let mut u8_number = f64_number.abs().floor() as i64;
    if f64_number < 0.0 {
        u8_number = -u8_number;
    }

    // Compute modulus according to spec
    let u8_max = u8::MAX as i64 + 1;
    u8_number = ((u8_number % u8_max) + u8_max) % u8_max;

    Ok(u8_number as u8)
}

/// ToUint8Clamp (https://tc39.es/ecma262/#sec-touint8clamp)
pub fn to_uint8_clamp(cx: Context, value_handle: Handle<Value>) -> EvalResult<u8> {
    // Fast path if the value is a smi
    let value = *value_handle;
    if value.is_smi() {
        let i32_value = value.as_smi();

        // Clamp within range
        if i32_value <= 0 {
            return Ok(0);
        } else if i32_value >= (u8::MAX as i32) {
            return Ok(u8::MAX);
        } else {
            return Ok(i32_value as u8);
        }
    }

    let number_value = to_number(cx, value_handle)?;
    let f64_number = number_value.as_number();

    // Clamp within range
    if f64_number <= 0.0 {
        return Ok(0);
    } else if f64_number >= MAX_U8_AS_F64 {
        return Ok(u8::MAX);
    } else if f64_number.is_nan() {
        return Ok(0);
    }

    // Round to closest integer
    let floor = f64_number.floor();
    if floor + 0.5 < f64_number {
        return Ok((floor + 1.0) as u8);
    } else if f64_number < floor + 0.5 {
        return Ok(floor as u8);
    }

    // Round ties to even
    if floor % 2.0 == 1.0 {
        Ok((floor + 1.0) as u8)
    } else {
        Ok(floor as u8)
    }
}

/// ToBigInt (https://tc39.es/ecma262/#sec-tobigint)
pub fn to_bigint(cx: Context, value: Handle<Value>) -> EvalResult<Handle<BigIntValue>> {
    let primitive_handle = to_primitive(cx, value, ToPrimitivePreferredType::Number)?;
    let primitive = *primitive_handle;

    if primitive.is_pointer() {
        match primitive.as_pointer().descriptor().kind() {
            HeapItemKind::BigInt => return Ok(primitive_handle.as_bigint()),
            HeapItemKind::String => {
                // May allocate
                return if let Some(bigint) = string_to_bigint(primitive_handle.as_string())? {
                    Ok(BigIntValue::new(cx, bigint)?)
                } else {
                    syntax_error(cx, "string does not represent a BigInt")
                };
            }
            _ => {}
        }
    } else if primitive.is_bool() {
        return if primitive.as_bool() {
            Ok(BigIntValue::new(cx, 1.into())?)
        } else {
            Ok(BigIntValue::new(cx, 0.into())?)
        };
    }

    type_error(cx, "value cannot be converted to BigInt")
}

/// ToBigInt64 (https://tc39.es/ecma262/#sec-tobigint64)
pub fn to_big_int64(cx: Context, value: Handle<Value>) -> EvalResult<BigInt> {
    let bigint = to_bigint(cx, value)?.bigint();

    // Compute modulus according to spec
    let u64_max = (u64::MAX) as u128 + 1;
    let mut i64_number = ((bigint % u64_max) + u64_max) % u64_max;

    // TODO: Do not create BigInt here
    if i64_number > BigInt::from(i64::MAX) {
        i64_number -= u64_max;
    }

    Ok(i64_number)
}

/// ToBigUint64 (https://tc39.es/ecma262/#sec-tobiguint64)
pub fn to_big_uint64(cx: Context, value: Handle<Value>) -> EvalResult<BigInt> {
    let bigint = to_bigint(cx, value)?.bigint();

    // Compute modulus according to spec
    let u64_max = (u64::MAX) as u128 + 1;
    let u64_number = ((bigint % u64_max) + u64_max) % u64_max;

    Ok(u64_number)
}

/// ToString (https://tc39.es/ecma262/#sec-tostring)
pub fn to_string(mut cx: Context, value_handle: Handle<Value>) -> EvalResult<Handle<StringValue>> {
    // Safe since value is never referenced after allocation
    let value = *value_handle;

    // Fast path
    if value.is_string() {
        return Ok(value_handle.as_string());
    }

    if value.is_pointer() {
        if value.as_pointer().descriptor().is_object() {
            let primitive_value = to_primitive(cx, value_handle, ToPrimitivePreferredType::String)?;
            to_string(cx, primitive_value)
        } else {
            match value.as_pointer().descriptor().kind() {
                HeapItemKind::BigInt => {
                    let bigint_string = value.as_bigint().bigint().to_string();
                    Ok(cx.alloc_string(&bigint_string)?.as_string())
                }
                HeapItemKind::Symbol => type_error(cx, "symbol cannot be converted to string"),
                _ => unreachable!(),
            }
        }
    } else {
        match value.get_tag() {
            NULL_TAG => Ok(cx.alloc_string("null")?.as_string()),
            UNDEFINED_TAG => Ok(cx.alloc_string("undefined")?.as_string()),
            BOOL_TAG => {
                let str = if value.as_bool() { "true" } else { "false" };
                Ok(cx.alloc_string(str)?.as_string())
            }
            SMI_TAG => {
                let smi_string = value.as_smi().to_string();
                Ok(cx.alloc_string(&smi_string)?.as_string())
            }
            // Otherwise must be double
            _ => {
                let double_string = number_to_string(value.as_double());
                Ok(cx.alloc_string(&double_string)?.as_string())
            }
        }
    }
}

/// ToObject (https://tc39.es/ecma262/#sec-toobject)
pub fn to_object(cx: Context, value_handle: Handle<Value>) -> EvalResult<Handle<ObjectValue>> {
    // Safe since pointer value is never referenced after allocation
    let value = *value_handle;

    if value.is_pointer() {
        // Fast path
        if value.as_pointer().descriptor().is_object() {
            Ok(value_handle.as_object())
        } else {
            match value.as_pointer().descriptor().kind() {
                HeapItemKind::String => {
                    Ok(StringObject::new_from_value(cx, value_handle.as_string())?.as_object())
                }
                HeapItemKind::Symbol => {
                    Ok(SymbolObject::new_from_value(cx, value_handle.as_symbol())?.as_object())
                }
                HeapItemKind::BigInt => {
                    Ok(BigIntObject::new_from_value(cx, value_handle.as_bigint())?.as_object())
                }
                _ => unreachable!(),
            }
        }
    } else {
        match value.get_tag() {
            NULL_TAG => type_error(cx, "null has no properties"),
            UNDEFINED_TAG => type_error(cx, "undefined has no properties"),
            BOOL_TAG => Ok(BooleanObject::new(cx, value.as_bool())?.as_object()),
            // Otherwise is a number, either double or smi
            _ => Ok(NumberObject::new(cx, value.as_number())?.as_object()),
        }
    }
}

/// ToLength (https://tc39.es/ecma262/#sec-tolength)
pub fn to_length(cx: Context, value: Handle<Value>) -> EvalResult<u64> {
    let len = to_integer_or_infinity(cx, value)?;
    if len <= 0.0 {
        return Ok(0);
    }

    let len_in_int_range = f64::min(len, MAX_SAFE_INTEGER_F64);

    // Safe since we have guaranteed that length is positive and within safe range
    let len_u64: u64 = unsafe { len_in_int_range.to_int_unchecked() };

    Ok(len_u64)
}

/// Identical to CanonicalNumericIndexString, but for u32 string indices
pub fn canonical_numeric_string_index_string(
    cx: Context,
    key: Handle<PropertyKey>,
    string_length: u32,
) -> AllocResult<Option<u32>> {
    if key.is_array_index() {
        // Fast path for array indices
        let array_index = key.as_array_index();
        if array_index < string_length {
            Ok(Some(array_index))
        } else {
            Ok(None)
        }
    } else if key.is_string() {
        // Otherwise must convert to number then back to string
        let key_string = key.as_string();
        let number_value = must_a!(to_number(cx, key_string.into()));

        // If string representations are equal, must be canonical numeric index
        let number_string = must_a!(to_string(cx, number_value));
        if key_string.equals(&number_string)? {
            if !is_integral_number(*number_value) {
                return Ok(None);
            }

            let number = number_value.as_number();
            if number.is_sign_negative() {
                return Ok(None);
            }

            let number = number as usize;
            if number >= string_length as usize {
                return Ok(None);
            }

            Ok(Some(number as u32))
        } else if (*key_string)
            .as_flat()
            .eq(&cx.names.negative_zero.as_string().as_flat())
        {
            // The string "-0" is a canonical numeric index but is never valid as an index
            Ok(None)
        } else {
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

/// ToIndex (https://tc39.es/ecma262/#sec-toindex)
pub fn to_index(cx: Context, value_handle: Handle<Value>) -> EvalResult<usize> {
    let value = *value_handle;
    if value.is_smi() {
        let smi = value.as_smi();
        if smi < 0 {
            range_error(cx, &format!("{smi} is out of range for an array index"))
        } else {
            Ok(smi as usize)
        }
    } else if value.is_undefined() {
        Ok(0)
    } else {
        let integer = to_integer_or_infinity(cx, value_handle)?;
        if !(0.0..=MAX_SAFE_INTEGER_F64).contains(&integer) {
            range_error(cx, &format!("{integer} is out of range for an array index"))
        } else {
            Ok(integer as usize)
        }
    }
}

/// RequireObjectCoercible (https://tc39.es/ecma262/#sec-requireobjectcoercible)
pub fn require_object_coercible(cx: Context, value: Handle<Value>) -> EvalResult<Handle<Value>> {
    if value.is_nullish() {
        if value.is_null() {
            return type_error(cx, "can't convert null to object");
        }

        return type_error(cx, "can't convert undefined to object");
    }

    Ok(value)
}

/// IsArray (https://tc39.es/ecma262/#sec-isarray)
pub fn is_array(cx: Context, value: Handle<Value>) -> EvalResult<bool> {
    if !value.is_object() {
        return Ok(false);
    }

    let object_value = value.as_object();
    if object_value.is_array() {
        return Ok(true);
    }

    if let Some(proxy) = object_value.as_proxy() {
        if proxy.is_revoked() {
            return type_error(cx, "operation attempted on revoked proxy");
        }

        return is_array(cx, proxy.target().unwrap().into());
    }

    Ok(false)
}

/// IsCallable (https://tc39.es/ecma262/#sec-iscallable)
pub fn is_callable(value: Handle<Value>) -> bool {
    if !value.is_object() {
        return false;
    }

    is_callable_object(value.as_object())
}

pub fn is_callable_object(value: Handle<ObjectValue>) -> bool {
    let kind = value.descriptor().kind();
    if kind == HeapItemKind::Closure {
        true
    } else if kind == HeapItemKind::Proxy {
        value.cast::<ProxyObject>().is_callable()
    } else {
        false
    }
}

/// IsConstructor (https://tc39.es/ecma262/#sec-isconstructor)
pub fn is_constructor_value(value: Handle<Value>) -> bool {
    if !value.is_object() {
        return false;
    }

    is_constructor_object_value(value.as_object())
}

pub fn is_constructor_object_value(value: Handle<ObjectValue>) -> bool {
    let kind = value.descriptor().kind();
    if kind == HeapItemKind::Closure {
        value.cast::<Closure>().function_ptr().is_constructor()
    } else if kind == HeapItemKind::Proxy {
        value.cast::<ProxyObject>().is_constructor()
    } else {
        false
    }
}

pub fn is_integral_number(value: Value) -> bool {
    if value.is_smi() {
        return true;
    } else if !value.is_double() || value.is_nan() || value.is_infinity() {
        return false;
    }

    let number = value.as_double();

    number.trunc() == number
}

/// IsRegExp (https://tc39.es/ecma262/#sec-isregexp)
///
/// If this returns true the value must be an object.
pub fn is_regexp(cx: Context, value: Handle<Value>) -> EvalResult<bool> {
    if !value.is_object() {
        return Ok(false);
    }

    let object = value.as_object();
    let match_key = cx.well_known_symbols.match_();
    let matcher = get(cx, object, match_key)?;

    if !matcher.is_undefined() {
        return Ok(to_boolean(*matcher));
    }

    Ok(object.is_regexp_object())
}

/// SameValue (https://tc39.es/ecma262/#sec-samevalue)
pub fn same_value(v1_handle: Handle<Value>, v2_handle: Handle<Value>) -> AllocResult<bool> {
    let v1 = v1_handle;
    let v2 = v2_handle;

    // Same as is_strictly_equal, but treats NaN as equal to itself, and does not treat differently
    // signed zeros as equal.
    if v1.is_number() {
        if v2.is_number() {
            if v1.is_nan() && v2.is_nan() {
                return Ok(true);
            }

            if v1.is_positive_zero() && v2.is_negative_zero()
                || v1.is_negative_zero() && v2.is_positive_zero()
            {
                return Ok(false);
            }

            return Ok(v1.as_number() == v2.as_number());
        } else {
            return Ok(false);
        }
    }

    same_value_non_numeric(v1_handle, v2_handle)
}

/// SameValueZero (https://tc39.es/ecma262/#sec-samevaluezero)
pub fn same_value_zero(v1_handle: Handle<Value>, v2_handle: Handle<Value>) -> AllocResult<bool> {
    let v1 = *v1_handle;
    let v2 = *v2_handle;

    // Same as same_value, but treats differently signed zeros as equal
    if v1.is_number() {
        if v2.is_number() {
            if v1.is_nan() && v2.is_nan() {
                return Ok(true);
            }

            return Ok(v1.as_number() == v2.as_number());
        } else {
            return Ok(false);
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

/// SameValueNonNumeric (https://tc39.es/ecma262/#sec-samevaluenonnumber)
///
/// Also includes BigInt handling
#[inline]
fn same_value_non_numeric(v1_handle: Handle<Value>, v2_handle: Handle<Value>) -> AllocResult<bool> {
    let v1 = *v1_handle;
    let v2 = *v2_handle;

    // Fast path, if values have same bits they are always equal
    if v1.as_raw_bits() == v2.as_raw_bits() {
        return Ok(true);
    }

    // Only strings and BigInts may have the same value but different bit patterns, since
    // non-pointer values are all unique, and objects and symbols are identified by their address.
    if v1.is_pointer() && v2.is_pointer() {
        let kind1 = v1.as_pointer().descriptor().kind();
        if kind1 == v2.as_pointer().descriptor().kind() {
            match kind1 {
                HeapItemKind::String => {
                    // May allocate
                    return v1_handle.as_string().equals(&v2_handle.as_string());
                }
                HeapItemKind::BigInt => {
                    return Ok(v1.as_bigint().bigint().eq(&v2.as_bigint().bigint()))
                }
                _ => {}
            }
        }
    }

    Ok(false)
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
                HeapItemKind::String => {
                    // Must be flat strings to be non allocating
                    let v1_string = v1.as_string();
                    let v2_string = v2.as_string();

                    debug_assert!(v1_string.is_flat());
                    debug_assert!(v2_string.is_flat());

                    // Cannot allocate
                    return v1_string.as_flat() == v2_string.as_flat();
                }
                HeapItemKind::BigInt => {
                    return v1.as_bigint().bigint().eq(&v2.as_bigint().bigint())
                }
                _ => {}
            }
        }
    }

    false
}

/// StringToBigInt (https://tc39.es/ecma262/#sec-stringtobigint)
fn string_to_bigint(value: Handle<StringValue>) -> AllocResult<Option<BigInt>> {
    let lexer = StringLexer::new(value)?;
    Ok(parse_string_to_bigint(lexer))
}

/// ToPropertyKey (https://tc39.es/ecma262/#sec-topropertykey)
pub fn to_property_key(
    cx: Context,
    value_handle: Handle<Value>,
) -> EvalResult<Handle<PropertyKey>> {
    let value = *value_handle;
    if value.is_smi() {
        let smi_value = value.as_smi();
        if smi_value >= 0 {
            return Ok(PropertyKey::array_index_handle(cx, smi_value as u32)?);
        }
    }

    let key = to_primitive(cx, value_handle, ToPrimitivePreferredType::String)?;
    if key.is_string() {
        return Ok(PropertyKey::string_handle(cx, key.as_string())?);
    } else if key.is_symbol() {
        return Ok(PropertyKey::symbol(key.as_symbol()));
    }

    let string_key = to_string(cx, key)?;
    Ok(PropertyKey::string_handle(cx, string_key)?)
}

/// IsLessThan (https://tc39.es/ecma262/#sec-islessthan)
///
/// ToPrimitive calls are inlined at call sites instead of passing LeftFirst argument.
/// Returns either a bool or undefined if values cannot be compared.
pub fn is_less_than(
    cx: Context,
    x_handle: Handle<Value>,
    y_handle: Handle<Value>,
) -> EvalResult<Value> {
    // Safe since allocation can only occur during to_numeric, and direct values are not held
    // across the to_numeric calls.
    let x = *x_handle;
    let y = *y_handle;

    if x.is_pointer() && y.is_pointer() {
        let x_kind = x.as_pointer().descriptor().kind();
        let y_kind = y.as_pointer().descriptor().kind();

        if x_kind == HeapItemKind::String {
            if y_kind == HeapItemKind::String {
                // May allocate
                return Ok(x_handle
                    .as_string()
                    .compare(&y_handle.as_string())?
                    .is_lt()
                    .into());
            } else if y_kind == HeapItemKind::BigInt {
                // May allocate
                let x_bigint = string_to_bigint(x_handle.as_string())?;

                return if let Some(x_bigint) = x_bigint {
                    Ok(x_bigint.lt(&y_handle.as_bigint().bigint()).into())
                } else {
                    Ok(Value::undefined())
                };
            }
        }

        if x_kind == HeapItemKind::BigInt && y_kind == HeapItemKind::String {
            // May allocate
            let y_bigint = string_to_bigint(y_handle.as_string())?;

            return if let Some(y_bigint) = y_bigint {
                Ok(x_handle.as_bigint().bigint().lt(&y_bigint).into())
            } else {
                Ok(Value::undefined())
            };
        }
    }

    let num_x_handle = to_numeric(cx, x_handle)?;
    let num_y_handle = to_numeric(cx, y_handle)?;

    let num_x = *num_x_handle;
    let num_y = *num_y_handle;

    let x_is_bigint = num_x.is_bigint();
    let y_is_bigint = num_y.is_bigint();
    if x_is_bigint == y_is_bigint {
        if x_is_bigint {
            Ok(num_x
                .as_bigint()
                .bigint()
                .lt(&num_y.as_bigint().bigint())
                .into())
        } else {
            // Both are numbers
            if num_x.is_nan() || num_y.is_nan() {
                return Ok(Value::undefined());
            }

            Ok((num_x.as_number() < num_y.as_number()).into())
        }
    } else if x_is_bigint {
        // x is a BigInt and y is a number
        if num_y.is_nan() {
            return Ok(Value::undefined());
        }

        let y_f64 = num_y.as_number();
        if y_f64 == f64::INFINITY {
            return Ok(true.into());
        } else if y_f64 == f64::NEG_INFINITY {
            return Ok(false.into());
        }

        // BigInt conversion truncates towards 0, so we must account for the possible fraction part
        let y_has_fract = y_f64.trunc() != y_f64;
        let y_bigint = y_f64.to_bigint().unwrap();

        match num_x.as_bigint().bigint().cmp(&y_bigint) {
            Ordering::Less => Ok(true.into()),
            Ordering::Equal => {
                // If y had a fractional part it was truncated towards 0. This means that if y had a
                // fractional part and is positive it is greater than x, and if y had a fractional
                // part and is negative it is less than x.
                if y_has_fract {
                    Ok((y_f64 > 0.0).into())
                } else {
                    Ok(false.into())
                }
            }
            Ordering::Greater => Ok(false.into()),
        }
    } else {
        // x is a number and y is a BigInt
        if num_x.is_nan() {
            return Ok(Value::undefined());
        }

        let x_f64 = num_x.as_number();
        if x_f64 == f64::INFINITY {
            return Ok(false.into());
        } else if x_f64 == f64::NEG_INFINITY {
            return Ok(true.into());
        }

        // BigInt conversion truncates towards 0, so we must account for the possible fraction part
        let x_has_fract = x_f64.trunc() != x_f64;
        let x_bigint = x_f64.to_bigint().unwrap();

        match x_bigint.cmp(&num_y.as_bigint().bigint()) {
            Ordering::Less => Ok(true.into()),
            Ordering::Equal => {
                // If x had a fractional part it was truncated towards 0. This means that if x had a
                // fractional part and is positive it is greater than y, and if x had a fractional
                // part and is negative it is less than y.
                if x_has_fract {
                    Ok((x_f64 < 0.0).into())
                } else {
                    Ok(false.into())
                }
            }
            Ordering::Greater => Ok(false.into()),
        }
    }
}

/// IsLooselyEqual (https://tc39.es/ecma262/#sec-islooselyequal)
pub fn is_loosely_equal(
    cx: Context,
    v1_handle: Handle<Value>,
    v2_handle: Handle<Value>,
) -> EvalResult<bool> {
    // Safe since allocation can only occur during to_number, to_primitive, and recursive calls to
    // is_loosely_equal, and direct values are not held across these calls.
    let v1 = *v1_handle;
    let v2 = *v2_handle;

    // If values have the same type, then use (inlined) is_strictly_equal.
    //
    // All type combinations from spec are broken up here and in the match expression at the end of
    // this function.
    if v1.is_number() {
        if v2.is_number() {
            return Ok(v1.as_number() == v2.as_number());
        }

        return if v2.is_pointer() {
            match v2.as_pointer().descriptor().kind() {
                HeapItemKind::String => {
                    // May allocate
                    let number_v2 = string_to_number(v2_handle.as_string())?;
                    Ok(v1_handle.as_number() == number_v2.as_number())
                }
                HeapItemKind::BigInt => {
                    if v1.is_nan() || v1.is_infinity() {
                        return Ok(false);
                    }

                    let v1_f64 = v1.as_number();

                    // Number must be an integer to be equal to a BigInt
                    if v1_f64.trunc() != v1_f64 {
                        return Ok(false);
                    }

                    // Now that we know number is an integer, it can losslessly be converted to a BigInt
                    let v1_bigint = v1_f64.to_bigint().unwrap();
                    let v2_bigint = v2.as_bigint().bigint();

                    Ok(v1_bigint == v2_bigint)
                }
                HeapItemKind::Symbol => Ok(false),
                // Otherwise must be an object
                _ => {
                    let primitive_v2 = to_primitive(cx, v2_handle, ToPrimitivePreferredType::None)?;
                    is_loosely_equal(cx, v1_handle, primitive_v2)
                }
            }
        } else if v2.is_bool() {
            let v2_number = to_number(cx, v2_handle)?;
            is_loosely_equal(cx, v1_handle, v2_number)
        } else {
            Ok(false)
        };
    }

    // Fast path - values with the same bit patterns are equal
    if v1.as_raw_bits() == v2.as_raw_bits() {
        return Ok(true);
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
                    HeapItemKind::String => {
                        // May allocate
                        Ok(v1_handle.as_string().equals(&v2_handle.as_string())?)
                    }
                    HeapItemKind::BigInt => {
                        Ok(v1.as_bigint().bigint().eq(&v2.as_bigint().bigint()))
                    }
                    _ => Ok(false),
                };
            // Two objects with different bit patterns are always unequal
            } else if v1.as_pointer().descriptor().is_object()
                && v2.as_pointer().descriptor().is_object()
            {
                return Ok(false);
            }
        } else {
            return Ok(false);
        }
    }

    // Handle comparisons that implicitly convert between types

    // Nullish values are loosely equal
    if (tag1 == NULL_TAG && tag2 == UNDEFINED_TAG) || (tag1 == UNDEFINED_TAG && tag2 == NULL_TAG) {
        return Ok(true);
    }

    // Convert bools to numbers and try again
    if tag1 == BOOL_TAG {
        let v1_number = to_number(cx, v1_handle)?;
        return is_loosely_equal(cx, v1_number, v2_handle);
    } else if tag2 == BOOL_TAG {
        let v2_number = to_number(cx, v2_handle)?;
        return is_loosely_equal(cx, v1_handle, v2_number);
    }

    // Implicit conversions involving numbers
    if tag2 == SMI_TAG || v2.is_double() {
        return if tag1 == POINTER_TAG {
            let kind = v1.as_pointer().descriptor().kind();
            match kind {
                HeapItemKind::String => {
                    // May allocate
                    let v1_number = string_to_number(v1_handle.as_string())?;
                    Ok(v1_number.as_number() == v2_handle.as_number())
                }
                HeapItemKind::BigInt => {
                    if v2.is_nan() || v2.is_infinity() {
                        return Ok(false);
                    }

                    let v2_f64 = v2.as_number();

                    // Number must be an integer to be equal to a BigInt
                    if v2_f64.trunc() != v2_f64 {
                        return Ok(false);
                    }

                    // Now that we know number is an integer, it can losslessly be converted to a BigInt
                    let v2_bigint = v2_f64.to_bigint().unwrap();
                    let v1_bigint = v1.as_bigint().bigint();

                    Ok(v1_bigint == v2_bigint)
                }
                HeapItemKind::Symbol => Ok(false),
                // Otherwise must be an object
                _ => {
                    let v1_primitive = to_primitive(cx, v1_handle, ToPrimitivePreferredType::None)?;
                    is_loosely_equal(cx, v1_primitive, v2_handle)
                }
            }
        } else {
            Ok(false)
        };
    }

    if tag1 == POINTER_TAG && tag2 == POINTER_TAG {
        let kind1 = v1.as_pointer().descriptor().kind();
        let kind2 = v2.as_pointer().descriptor().kind();

        // Strings are implicitly converted to BigInts
        match (kind1, kind2) {
            (HeapItemKind::String, HeapItemKind::BigInt) => {
                // May allocate
                let v1_bigint = string_to_bigint(v1_handle.as_string())?;
                return if let Some(v1_bigint) = v1_bigint {
                    Ok(v1_bigint.eq(&v2_handle.as_bigint().bigint()))
                } else {
                    Ok(false)
                };
            }
            (HeapItemKind::BigInt, HeapItemKind::String) => {
                // May allocate
                let v2_bigint = string_to_bigint(v2_handle.as_string())?;
                return if let Some(v2_bigint) = v2_bigint {
                    Ok(v1_handle.as_bigint().bigint().eq(&v2_bigint))
                } else {
                    Ok(false)
                };
            }
            _ => {}
        }

        // At this point, if one value is an object the other value is guaranteed to be a non-object
        // pointer value (aka string, symbol, or BigInt).
        if v1.as_pointer().descriptor().is_object() {
            let v1_primitive = to_primitive(cx, v1_handle, ToPrimitivePreferredType::None)?;
            return is_loosely_equal(cx, v1_primitive, v2_handle);
        } else if v2.as_pointer().descriptor().is_object() {
            let primitive_v2 = to_primitive(cx, v2_handle, ToPrimitivePreferredType::None)?;
            return is_loosely_equal(cx, v1_handle, primitive_v2);
        }
    }

    Ok(false)
}

/// IsStrictlyEqual (https://tc39.es/ecma262/#sec-isstrictlyequal)
pub fn is_strictly_equal(v1_handle: Handle<Value>, v2_handle: Handle<Value>) -> AllocResult<bool> {
    let v1 = *v1_handle;
    let v2 = *v2_handle;

    if v1.is_number() {
        if v2.is_number() {
            return Ok(v1.as_number() == v2.as_number());
        } else {
            return Ok(false);
        }
    }

    same_value_non_numeric(v1_handle, v2_handle)
}

/// Specialization of SameValue for objects, checks object identity
#[inline]
pub fn same_object_value(value1: HeapPtr<ObjectValue>, value2: HeapPtr<ObjectValue>) -> bool {
    value1.ptr_eq(&value2)
}

#[inline]
pub fn same_object_value_handles(value1: Handle<ObjectValue>, value2: Handle<ObjectValue>) -> bool {
    same_object_value(*value1, *value2)
}

/// Specialization of SameValue for optional objects, checks object identity
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
        (Some(value1), Some(value2)) => same_object_value_handles(value1, value2),
        _ => false,
    }
}

/// Number::toString (https://tc39.es/ecma262/#sec-numeric-types-number-tostring)
pub fn number_to_string(x: f64) -> String {
    let mut buf = ryu_js::Buffer::new();
    buf.format(x).to_string()
}
