use crate::maybe;

use super::{
    abstract_operations::{call_object, get, get_method},
    completion::EvalResult,
    error::type_error_,
    gc::Gc,
    intrinsics::{
        boolean_constructor::BooleanObject, number_constructor::NumberObject,
        string_constructor::StringObject,
    },
    object_value::ObjectValue,
    value::{
        StringValue, Value, BIGINT_TAG, BOOL_TAG, NULL_TAG, OBJECT_TAG, STRING_TAG, SYMBOL_TAG,
        UNDEFINED_TAG,
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
pub fn to_primitive(
    cx: &mut Context,
    value: Value,
    mut preferred_type: ToPrimitivePreferredType,
) -> EvalResult<Value> {
    if !value.is_object() {
        return value.into();
    }

    // TODO: Change to symbol once symbols are implemented
    let exotic_prim = maybe!(get_method(cx, value, "@@toPrimitive"));
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
        call_method!("toString");
        call_method!("valueOf");
    } else {
        call_method!("valueOf");
        call_method!("toString");
    }

    type_error_(cx, "object cannot be converted to primitive")
}

// 7.1.2 ToBoolean
pub fn to_boolean(value: Value) -> bool {
    if value.is_number() {
        return value.as_number() != 0.0 && !value.is_nan();
    }

    match value.get_tag() {
        BOOL_TAG => value.as_bool(),
        NULL_TAG => false,
        UNDEFINED_TAG => false,
        OBJECT_TAG => true,
        STRING_TAG => !value.as_string().is_empty(),
        SYMBOL_TAG => true,
        BIGINT_TAG => unimplemented!("BigInt"),
        _ => unreachable!(),
    }
}

// 7.1.3 ToNumeric
pub fn to_numeric(cx: &mut Context, value: Value) -> EvalResult<Value> {
    let prim_value = maybe!(to_primitive(cx, value, ToPrimitivePreferredType::Number));
    if prim_value.is_bigint() {
        return prim_value.into();
    }

    to_number(cx, value)
}

// 7.1.4 ToNumber
pub fn to_number(cx: &mut Context, value: Value) -> EvalResult<Value> {
    if value.is_number() {
        return value.into();
    }

    match value.get_tag() {
        NULL_TAG => Value::number(0.0).into(),
        UNDEFINED_TAG => Value::nan().into(),
        BOOL_TAG => {
            if value.as_bool() {
                Value::number(1.0).into()
            } else {
                Value::number(0.0).into()
            }
        }
        STRING_TAG => string_to_number(value).into(),
        OBJECT_TAG => {
            let primitive_value = maybe!(to_primitive(cx, value, ToPrimitivePreferredType::Number));
            to_number(cx, primitive_value)
        }
        SYMBOL_TAG => type_error_(cx, "symbol cannot be converted to number"),
        BIGINT_TAG => type_error_(cx, "BigInt cannot be converted to number"),
        _ => unreachable!(),
    }
}

// 7.1.4.1.1 StringToNumber
fn string_to_number(value: Value) -> Value {
    unimplemented!()
}

// 7.1.17 ToString
pub fn to_string(cx: &mut Context, value: Value) -> EvalResult<Gc<StringValue>> {
    if value.is_string() {
        return value.as_string().into();
    } else if value.is_number() {
        // TODO: Implement Number::toString from spec
        return cx.heap.alloc_string(value.as_number().to_string()).into();
    }

    match value.get_tag() {
        NULL_TAG => cx.heap.alloc_string("null".to_owned()).into(),
        UNDEFINED_TAG => cx.heap.alloc_string("undefined".to_owned()).into(),
        BOOL_TAG => {
            let str = if value.as_bool() { "true" } else { "false" };
            cx.heap.alloc_string(str.to_owned()).into()
        }
        OBJECT_TAG => {
            let primitive_value = maybe!(to_primitive(cx, value, ToPrimitivePreferredType::String));
            to_string(cx, primitive_value)
        }
        BIGINT_TAG => unimplemented!("BigInts"),
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
        SYMBOL_TAG => unimplemented!("symbol objects"),
        BIGINT_TAG => unimplemented!("BigInt objects"),
        _ => unreachable!(),
    }
}

// 7.2.2 IsArray
pub fn is_array(value: Value) -> bool {
    if !value.is_object() {
        return false;
    }

    let object_value = value.as_object();
    if object_value.is_array() {
        return true;
    }

    if object_value.is_proxy() {
        unimplemented!("proxy objects")
    }

    return false;
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

    value.as_object().is_constructor()
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

    let tag1 = v1.get_tag();
    if tag1 != v2.get_tag() {
        return false;
    }

    match tag1 {
        STRING_TAG => v1.as_string().as_ref() == v2.as_string().as_ref(),
        BIGINT_TAG => unimplemented!("BigInt"),
        // Null, Undefined, and Bool all have a single canonical bit representation for each value,
        // so the bits can be compared directly. For Objects and Symbols there is a single
        // representation for a unique pointer, so can directly compare bits as well.
        _ => v1.as_raw_bits() == v2.as_raw_bits(),
    }
}

// 7.1.14 StringToBigInt
fn string_to_big_int(value: Value) -> Value {
    unimplemented!()
}

// 7.1.19 ToPropertyKey
pub fn to_property_key(value: Value) -> Gc<StringValue> {
    if value.is_string() {
        return value.as_string();
    }

    unimplemented!("Non-string property keys")
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
                let number_v2 = string_to_number(v2);
                (v1.as_number() == number_v2.as_number()).into()
            }
            OBJECT_TAG => {
                let primitive_v2 = maybe!(to_primitive(cx, v2, ToPrimitivePreferredType::None));
                is_loosely_equal(cx, v1, primitive_v2)
            }
            BIGINT_TAG => unimplemented!("BigInt"),
            _ => false.into(),
        };
    }

    let tag1 = v1.get_tag();
    let tag2 = v2.get_tag();
    if tag1 == tag2 {
        return match tag1 {
            STRING_TAG => (v1.as_string().as_ref() == v2.as_string().as_ref()).into(),
            BIGINT_TAG => unimplemented!("BigInt"),
            // Null, Undefined, and Bool all have a single canonical bit representation for each value,
            // so the bits can be compared directly. For Objects and Symbols there is a single
            // representation for a unique pointer, so can directly compare bits as well.
            _ => (v1.as_raw_bits() == v2.as_raw_bits()).into(),
        };
    }

    match (tag1, tag2) {
        (NULL_TAG, UNDEFINED_TAG) | (UNDEFINED_TAG, NULL_TAG) => true.into(),
        (STRING_TAG, _) if v2.is_number() => {
            let v1_number = string_to_number(v1);
            (v1_number.as_number() == v2.as_number()).into()
        }
        (STRING_TAG, BIGINT_TAG) => {
            let v1_bigint = string_to_big_int(v1);
            if v1_bigint.is_undefined() {
                false.into()
            } else {
                unimplemented!("BigInt")
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
        (BIGINT_TAG, _) if v2.is_number() => unimplemented!("BigInt"),
        (BIGINT_TAG, STRING_TAG) => {
            let v2_bigint = string_to_big_int(v2);
            if v2_bigint.is_undefined() {
                false.into()
            } else {
                unimplemented!("BigInt")
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
        BIGINT_TAG => unimplemented!("BigInt"),
        // Null, Undefined, and Bool all have a single canonical bit representation for each value,
        // so the bits can be compared directly. For Objects and Symbols there is a single
        // representation for a unique pointer, so can directly compare bits as well.
        _ => v1.as_raw_bits() == v2.as_raw_bits(),
    }
}

// Specialization of SameValue for objects, checks object identity
#[inline]
pub fn same_object_value(value1: Gc<ObjectValue>, value2: Gc<ObjectValue>) -> bool {
    value1 == value2
}

// Specialization of SameValue for optional objects, checks object identity
#[inline]
pub fn same_opt_object_value(
    value1: Option<Gc<ObjectValue>>,
    value2: Option<Gc<ObjectValue>>,
) -> bool {
    match (value1, value2) {
        (None, None) => true,
        (Some(value1), Some(value2)) => value1 == value2,
        _ => false,
    }
}
