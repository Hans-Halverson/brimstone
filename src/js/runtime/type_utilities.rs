use num_bigint::BigInt;

use crate::maybe;

use super::{
    abstract_operations::{call_object, get, get_method},
    completion::EvalResult,
    error::{syntax_error_, type_error_},
    gc::Gc,
    intrinsics::{
        bigint_constructor::BigIntObject, boolean_constructor::BooleanObject,
        number_constructor::NumberObject, symbol_constructor::SymbolObject,
    },
    numeric_constants::MAX_SAFE_INTEGER_F64,
    object_value::ObjectValue,
    property_key::PropertyKey,
    proxy_object::ProxyObject,
    string_object::StringObject,
    string_parsing::parse_string_to_number,
    value::{
        BigIntValue, StringValue, Value, BIGINT_TAG, BOOL_TAG, NULL_TAG, OBJECT_TAG, SMI_TAG,
        STRING_TAG, SYMBOL_TAG, UNDEFINED_TAG,
    },
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
    cx: &mut Context,
    value: Value,
    mut preferred_type: ToPrimitivePreferredType,
) -> EvalResult<Value> {
    if !value.is_object() {
        return value.into();
    }

    let to_primitive_key = PropertyKey::symbol(cx.well_known_symbols.to_primitive);
    let exotic_prim = maybe!(get_method(cx, value, &to_primitive_key));
    match exotic_prim {
        Some(exotic_prim) => {
            let hint_str = match preferred_type {
                ToPrimitivePreferredType::None => "default",
                ToPrimitivePreferredType::Number => "number",
                ToPrimitivePreferredType::String => "string",
            };
            let hint_value: Value = cx.heap.alloc_string(hint_str.to_owned()).into();

            let result = maybe!(call_object(cx, exotic_prim, value, &[hint_value]));
            if result.is_object() {
                return type_error_(cx, "object cannot be converted to primitive");
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
fn ordinary_to_primitive(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    preferred_type: ToPrimitivePreferredType,
) -> EvalResult<Value> {
    let object_value: Value = object.into();

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
        call_method!(&cx.names.to_string());
        call_method!(&cx.names.value_of());
    } else {
        call_method!(&cx.names.value_of());
        call_method!(&cx.names.to_string());
    }

    type_error_(cx, "object cannot be converted to primitive")
}

// 7.1.2 ToBoolean
pub fn to_boolean(value: Value) -> bool {
    if value.is_double() {
        return value.as_double() != 0.0 && !value.is_nan();
    }

    match value.get_tag() {
        BOOL_TAG => value.as_bool(),
        NULL_TAG => false,
        UNDEFINED_TAG => false,
        SMI_TAG => value.as_smi() != 0,
        OBJECT_TAG => true,
        STRING_TAG => !value.as_string().is_empty(),
        SYMBOL_TAG => true,
        BIGINT_TAG => value.as_bigint().bigint().ne(&BigInt::default()),
        _ => unreachable!(),
    }
}

// 7.1.3 ToNumeric
pub fn to_numeric(cx: &mut Context, value: Value) -> EvalResult<Value> {
    let prim_value = maybe!(to_primitive(cx, value, ToPrimitivePreferredType::Number));
    if prim_value.is_bigint() {
        return prim_value.into();
    }

    to_number(cx, prim_value)
}

// 7.1.4 ToNumber
pub fn to_number(cx: &mut Context, value: Value) -> EvalResult<Value> {
    if value.is_number() {
        return value.into();
    }

    match value.get_tag() {
        NULL_TAG => Value::smi(0).into(),
        UNDEFINED_TAG => Value::nan().into(),
        BOOL_TAG => {
            if value.as_bool() {
                Value::smi(1).into()
            } else {
                Value::smi(0).into()
            }
        }
        STRING_TAG => string_to_number(value.as_string()).into(),
        OBJECT_TAG => {
            let primitive_value = maybe!(to_primitive(cx, value, ToPrimitivePreferredType::Number));
            to_number(cx, primitive_value)
        }
        SYMBOL_TAG => type_error_(cx, "symbol cannot be converted to number"),
        BIGINT_TAG => type_error_(cx, "BigInt cannot be converted to number"),
        _ => unreachable!(),
    }
}

// 7.1.5 ToIntegerOrInfinity
pub fn to_integer_or_infinity(cx: &mut Context, value: Value) -> EvalResult<f64> {
    let number = maybe!(to_number(cx, value));

    let number_f64 = number.as_number();
    if number.is_nan() || number_f64 == 0.0 {
        return 0.0.into();
    }

    if number.is_infinity() {
        return number_f64.into();
    }

    let mut integer_f64 = number_f64.floor().abs();

    if number_f64 < 0.0 {
        integer_f64 = -integer_f64;
    }

    integer_f64.into()
}

// 7.1.6 ToInt32
pub fn to_int32(cx: &mut Context, value: Value) -> EvalResult<i32> {
    // Fast pass if the value is a smi
    if value.is_smi() {
        return value.as_smi().into();
    }

    let number_value = maybe!(to_number(cx, value));
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
    i32_number = ((i32_number % u32_max) + u32_max) % u32_max;

    // Then center in i32 range around 0
    if i32_number >= (u32_max >> 1) {
        i32_number -= u32_max;
    }

    (i32_number as i32).into()
}

// 7.1.7 ToUint32
pub fn to_uint32(cx: &mut Context, value: Value) -> EvalResult<u32> {
    // Fast pass if the value is a non-negative smi
    if value.is_smi() {
        let i32_value = value.as_smi();
        if i32_value >= 0 {
            return (i32_value as u32).into();
        }
    }

    let number_value = maybe!(to_number(cx, value));
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
    u32_number = ((u32_number % u32_max) + u32_max) % u32_max;

    (u32_number as u32).into()
}

// 7.1.4.1.1 StringToNumber
fn string_to_number(value: Gc<StringValue>) -> Value {
    match parse_string_to_number(value.str()) {
        None => Value::nan(),
        Some(num) => Value::number(num),
    }
}

// 7.1.13 ToBigInt
pub fn to_bigint(cx: &mut Context, value: Value) -> EvalResult<Gc<BigIntValue>> {
    let primitive = maybe!(to_primitive(cx, value, ToPrimitivePreferredType::Number));

    if primitive.is_bigint() {
        return primitive.as_bigint().into();
    } else if primitive.is_number() {
        return type_error_(cx, "value cannot be converted to BigInt");
    }

    match value.get_tag() {
        BOOL_TAG => {
            if value.as_bool() {
                cx.heap.alloc_bigint(1.into()).into()
            } else {
                cx.heap.alloc_bigint(0.into()).into()
            }
        }
        STRING_TAG => {
            if let Some(bigint) = string_to_bigint(value.as_string()) {
                cx.heap.alloc_bigint(bigint).into()
            } else {
                syntax_error_(cx, "string does not represent a BigInt")
            }
        }
        _ => type_error_(cx, "value cannot be converted to BigInt"),
    }
}

// 7.1.17 ToString
pub fn to_string(cx: &mut Context, value: Value) -> EvalResult<Gc<StringValue>> {
    if value.is_string() {
        return value.as_string().into();
    } else if value.is_double() {
        return cx
            .heap
            .alloc_string(number_to_string(value.as_double()))
            .into();
    }

    match value.get_tag() {
        NULL_TAG => cx.heap.alloc_string("null".to_owned()).into(),
        UNDEFINED_TAG => cx.heap.alloc_string("undefined".to_owned()).into(),
        BOOL_TAG => {
            let str = if value.as_bool() { "true" } else { "false" };
            cx.heap.alloc_string(str.to_owned()).into()
        }
        SMI_TAG => cx.heap.alloc_string(value.as_smi().to_string()).into(),
        OBJECT_TAG => {
            let primitive_value = maybe!(to_primitive(cx, value, ToPrimitivePreferredType::String));
            to_string(cx, primitive_value)
        }
        BIGINT_TAG => cx
            .heap
            .alloc_string(value.as_bigint().bigint().to_string())
            .into(),
        SYMBOL_TAG => type_error_(cx, "symbol cannot be converted to string"),
        _ => unreachable!(),
    }
}

// 7.1.18 ToObject
pub fn to_object(cx: &mut Context, value: Value) -> EvalResult<Gc<ObjectValue>> {
    if value.is_object() {
        return value.as_object().into();
    } else if value.is_number() {
        let object: Gc<ObjectValue> = NumberObject::new_from_value(cx, value.as_number()).into();
        return object.into();
    }

    match value.get_tag() {
        NULL_TAG => type_error_(cx, "null has no properties"),
        UNDEFINED_TAG => type_error_(cx, "undefined has no properties"),
        BOOL_TAG => {
            let object: Gc<ObjectValue> = BooleanObject::new_from_value(cx, value.as_bool()).into();
            object.into()
        }
        STRING_TAG => {
            let object: Gc<ObjectValue> =
                StringObject::new_from_value(cx, value.as_string()).into();
            object.into()
        }
        SYMBOL_TAG => {
            let object: Gc<ObjectValue> =
                SymbolObject::new_from_value(cx, value.as_symbol()).into();
            object.into()
        }
        BIGINT_TAG => {
            let object: Gc<ObjectValue> =
                BigIntObject::new_from_value(cx, value.as_bigint()).into();
            object.into()
        }
        _ => unreachable!(),
    }
}

// 7.1.20 ToLength
pub fn to_length(cx: &mut Context, value: Value) -> EvalResult<u64> {
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
pub fn canonical_numeric_index_string(key: &PropertyKey) -> Option<u32> {
    // TODO: Support full safe integer range instead of just array index range
    if key.is_array_index() {
        Some(key.as_array_index())
    } else {
        None
    }
}

// 7.2.1 RequireObjectCoercible
pub fn require_object_coercible(cx: &mut Context, value: Value) -> EvalResult<Value> {
    if value.is_nullish() {
        if value.is_null() {
            return type_error_(cx, "can't convert null to object");
        }

        return type_error_(cx, "can't convert undefined to object");
    }

    value.into()
}

// 7.2.2 IsArray
pub fn is_array(cx: &mut Context, value: Value) -> EvalResult<bool> {
    if !value.is_object() {
        return false.into();
    }

    let object_value = value.as_object();
    if object_value.is_array() {
        return true.into();
    }

    if object_value.is_proxy() {
        let proxy = object_value.cast::<ProxyObject>();
        if proxy.handler().is_none() {
            return type_error_(cx, "operation attempted on revoked proxy");
        }

        return is_array(cx, proxy.target().unwrap().into());
    }

    return false.into();
}

// 7.2.3 IsCallable
pub fn is_callable(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }

    is_callable_object(value.as_object())
}

pub fn is_callable_object(value: Gc<ObjectValue>) -> bool {
    value.is_callable()
}

// 7.2.4 IsConstructor
pub fn is_constructor(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }

    is_constructor_object(value.as_object())
}

pub fn is_constructor_object(value: Gc<ObjectValue>) -> bool {
    value.is_constructor()
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

// 7.2.11 SameValue
pub fn same_value(v1: Value, v2: Value) -> bool {
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

    same_value_non_numeric(v1, v2)
}

// 7.2.12 SameValueZero
pub fn same_value_zero(v1: Value, v2: Value) -> bool {
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

    same_value_non_numeric(v1, v2)
}

// 7.2.13 SameValueNonNumeric, also includes BigInt handling
#[inline]
fn same_value_non_numeric(v1: Value, v2: Value) -> bool {
    let tag1 = v1.get_tag();
    if tag1 != v2.get_tag() {
        return false;
    }

    match tag1 {
        STRING_TAG => v1.as_string().as_ref() == v2.as_string().as_ref(),
        BIGINT_TAG => v1.as_bigint().bigint().eq(v2.as_bigint().bigint()),
        // Null, Undefined, and Bool all have a single canonical bit representation for each value,
        // so the bits can be compared directly. For Objects and Symbols there is a single
        // representation for a unique pointer, so can directly compare bits as well.
        _ => v1.as_raw_bits() == v2.as_raw_bits(),
    }
}

// 7.1.14 StringToBigInt
fn string_to_bigint(value: Gc<StringValue>) -> Option<BigInt> {
    unimplemented!("StringToBigInt")
}

// 7.1.19 ToPropertyKey
pub fn to_property_key(cx: &mut Context, value: Value) -> EvalResult<PropertyKey> {
    if value.is_smi() {
        let smi_value = value.as_smi();
        if smi_value >= 0 {
            return PropertyKey::array_index(cx, smi_value as u32).into();
        }
    }

    let key = maybe!(to_primitive(cx, value, ToPrimitivePreferredType::String));
    if key.is_string() {
        return PropertyKey::string(key.as_string()).into();
    } else if value.is_symbol() {
        return PropertyKey::symbol(key.as_symbol()).into();
    }

    let string_key = maybe!(to_string(cx, key));
    PropertyKey::string(string_key).into()
}

// 7.2.14 IsLessThan
// ToPrimitive calls are inlined at call sites instead of passing LeftFirst argument
pub fn is_less_than(cx: &mut Context, x: Value, y: Value) -> EvalResult<Value> {
    let x_tag = x.get_tag();
    let y_tag = y.get_tag();
    if x_tag == STRING_TAG {
        if y_tag == STRING_TAG {
            return (x.as_string().str() < y.as_string().str()).into();
        } else if y_tag == BIGINT_TAG {
            let x_bigint = string_to_bigint(x.as_string());

            return if let Some(x_bigint) = x_bigint {
                x_bigint.lt(y.as_bigint().bigint()).into()
            } else {
                Value::undefined().into()
            };
        }
    }

    if x_tag == BIGINT_TAG && y_tag == STRING_TAG {
        let y_bigint = string_to_bigint(x.as_string());

        return if let Some(y_bigint) = y_bigint {
            x.as_bigint().bigint().lt(&y_bigint).into()
        } else {
            Value::undefined().into()
        };
    }

    let num_x = maybe!(to_numeric(cx, x));
    let num_y = maybe!(to_numeric(cx, y));

    let x_is_bigint = num_x.is_bigint();
    let y_is_bigint = num_y.is_bigint();
    if x_is_bigint == y_is_bigint {
        if x_is_bigint {
            return num_x
                .as_bigint()
                .bigint()
                .lt(num_y.as_bigint().bigint())
                .into();
        } else {
            // Both are numbers
            if num_x.is_nan() || num_y.is_nan() {
                return Value::undefined().into();
            }

            (num_x.as_number() < num_y.as_number()).into()
        }
    } else {
        // One number and one BigInt
        unimplemented!("BigInt comparison to number")
    }
}

// 7.2.15 IsLooselyEqual
pub fn is_loosely_equal(cx: &mut Context, v1: Value, v2: Value) -> EvalResult<bool> {
    // If values have the same type, then use (inlined) is_strictly_equal.
    //
    // All type combinations from spec are broken up here and in the match expression at the end of
    // this function.
    if v1.is_number() {
        if v2.is_number() {
            return (v1.as_number() == v2.as_number()).into();
        }

        return match v2.get_tag() {
            STRING_TAG => {
                let number_v2 = string_to_number(v2.as_string());
                (v1.as_number() == number_v2.as_number()).into()
            }
            BOOL_TAG => {
                let v2_number = maybe!(to_number(cx, v2));
                is_loosely_equal(cx, v1, v2_number)
            }
            OBJECT_TAG => {
                let primitive_v2 = maybe!(to_primitive(cx, v2, ToPrimitivePreferredType::None));
                is_loosely_equal(cx, v1, primitive_v2)
            }
            BIGINT_TAG => unimplemented!("BigInt comparison to number"),
            _ => false.into(),
        };
    }

    let tag1 = v1.get_tag();
    let tag2 = v2.get_tag();
    if tag1 == tag2 {
        return match tag1 {
            STRING_TAG => (v1.as_string().as_ref() == v2.as_string().as_ref()).into(),
            BIGINT_TAG => v1.as_bigint().bigint().eq(v2.as_bigint().bigint()).into(),
            // Null, Undefined, and Bool all have a single canonical bit representation for each value,
            // so the bits can be compared directly. For Objects and Symbols there is a single
            // representation for a unique pointer, so can directly compare bits as well.
            _ => (v1.as_raw_bits() == v2.as_raw_bits()).into(),
        };
    }

    match (tag1, tag2) {
        (NULL_TAG, UNDEFINED_TAG) | (UNDEFINED_TAG, NULL_TAG) => true.into(),
        (STRING_TAG, _) if v2.is_number() => {
            let v1_number = string_to_number(v1.as_string());
            (v1_number.as_number() == v2.as_number()).into()
        }
        (STRING_TAG, BIGINT_TAG) => {
            let v1_bigint = string_to_bigint(v1.as_string());
            if let Some(v1_bigint) = v1_bigint {
                v1_bigint.eq(v2.as_bigint().bigint()).into()
            } else {
                false.into()
            }
        }
        (BOOL_TAG, _) => {
            let v1_number = maybe!(to_number(cx, v1));
            is_loosely_equal(cx, v1_number, v2)
        }
        (_, BOOL_TAG) => {
            let v2_number = maybe!(to_number(cx, v2));
            is_loosely_equal(cx, v1, v2_number)
        }
        (BIGINT_TAG, _) if v2.is_number() => unimplemented!("BigInt comparison to number"),
        (BIGINT_TAG, STRING_TAG) => {
            let v2_bigint = string_to_bigint(v2.as_string());
            if let Some(v2_bigint) = v2_bigint {
                v1.as_bigint().bigint().eq(&v2_bigint).into()
            } else {
                false.into()
            }
        }
        (OBJECT_TAG, _) if v2.is_number() => {
            let v1_primitive = maybe!(to_primitive(cx, v1, ToPrimitivePreferredType::None));
            is_loosely_equal(cx, v1_primitive, v2)
        }
        (OBJECT_TAG, STRING_TAG | BIGINT_TAG | SYMBOL_TAG) => {
            let v1_primitive = maybe!(to_primitive(cx, v1, ToPrimitivePreferredType::None));
            is_loosely_equal(cx, v1_primitive, v2)
        }
        (STRING_TAG | BIGINT_TAG | SYMBOL_TAG, OBJECT_TAG) => {
            let primitive_v2 = maybe!(to_primitive(cx, v2, ToPrimitivePreferredType::None));
            is_loosely_equal(cx, v1, primitive_v2)
        }
        _ => false.into(),
    }
}

// 7.2.16 IsStrictlyEqual
pub fn is_strictly_equal(v1: Value, v2: Value) -> bool {
    if v1.is_number() {
        if v2.is_number() {
            return v1.as_number() == v2.as_number();
        } else {
            return false;
        }
    }

    let tag1 = v1.get_tag();
    if tag1 != v2.get_tag() {
        return false;
    }

    match tag1 {
        STRING_TAG => v1.as_string().as_ref() == v2.as_string().as_ref(),
        BIGINT_TAG => v1.as_bigint().bigint().eq(v2.as_bigint().bigint()),
        // Null, Undefined, and Bool all have a single canonical bit representation for each value,
        // so the bits can be compared directly. For Objects and Symbols there is a single
        // representation for a unique pointer, so can directly compare bits as well.
        _ => v1.as_raw_bits() == v2.as_raw_bits(),
    }
}

// Specialization of SameValue for objects, checks object identity
#[inline]
pub fn same_object_value(value1: Gc<ObjectValue>, value2: Gc<ObjectValue>) -> bool {
    value1.ptr_eq(&value2)
}

// Specialization of SameValue for optional objects, checks object identity
#[inline]
pub fn same_opt_object_value(
    value1: Option<Gc<ObjectValue>>,
    value2: Option<Gc<ObjectValue>>,
) -> bool {
    match (value1, value2) {
        (None, None) => true,
        (Some(value1), Some(value2)) => value1.ptr_eq(&value2),
        _ => false,
    }
}

// 6.1.6.1.20 Number::toString
pub fn number_to_string(x: f64) -> String {
    // TODO: Implement Number::toString from spec
    if x.is_infinite() {
        if x == f64::INFINITY {
            return String::from("Infinity");
        } else {
            return String::from("-Infinity");
        }
    }

    x.to_string()
}
