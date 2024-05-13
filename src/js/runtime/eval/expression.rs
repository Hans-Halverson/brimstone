use std::convert::TryInto;

use num_bigint::{BigInt, Sign};

use crate::{
    js::{
        parser::ast,
        runtime::{
            abstract_operations::{
                call_object, define_property_or_throw, get_method, has_property,
                ordinary_has_instance, set_integrity_level, IntegrityLevel,
            },
            array_object::array_create_in_realm,
            completion::EvalResult,
            error::{range_error, type_error},
            interned_strings::InternedStrings,
            numeric_operations::number_exponentiate,
            object_descriptor::ObjectKind,
            object_value::ObjectValue,
            property_descriptor::PropertyDescriptor,
            property_key::PropertyKey,
            string_value::StringValue,
            type_utilities::{
                is_less_than, to_boolean, to_int32, to_numeric, to_object, to_primitive,
                to_property_key, to_string, to_uint32, ToPrimitivePreferredType,
            },
            value::{BigIntValue, Value, BOOL_TAG, NULL_TAG, UNDEFINED_TAG},
            Context, Handle, Realm,
        },
    },
    maybe, must,
};
// 13.2.8.3 GetTemplateObject
pub fn generate_template_object(
    cx: Context,
    realm: Handle<Realm>,
    lit: &ast::TemplateLiteral,
) -> Handle<ObjectValue> {
    let num_strings = lit.quasis.len();
    let template_object: Handle<ObjectValue> =
        must!(array_create_in_realm(cx, realm, num_strings as u64, None)).into();
    let raw_object: Handle<ObjectValue> =
        must!(array_create_in_realm(cx, realm, num_strings as u64, None)).into();

    // Property key is shared between iterations
    let mut index_key = PropertyKey::uninit().to_handle(cx);

    for (i, quasi) in lit.quasis.iter().enumerate() {
        index_key.replace(PropertyKey::array_index(cx, i as u32));

        let cooked_value = match &quasi.cooked {
            None => cx.undefined(),
            Some(cooked) => InternedStrings::get_wtf8_str(cx, cooked).into(),
        };
        let cooked_desc = PropertyDescriptor::data(cooked_value, false, true, false);
        must!(define_property_or_throw(cx, template_object, index_key, cooked_desc));

        let raw_value = InternedStrings::get_wtf8_str(cx, &quasi.raw);
        let raw_desc = PropertyDescriptor::data(raw_value.into(), false, true, false);
        must!(define_property_or_throw(cx, raw_object, index_key, raw_desc));
    }

    must!(set_integrity_level(cx, raw_object.into(), IntegrityLevel::Frozen));

    let raw_object_desc = PropertyDescriptor::data(raw_object.into(), false, false, false);
    must!(define_property_or_throw(cx, template_object, cx.names.raw(), raw_object_desc));

    must!(set_integrity_level(cx, template_object, IntegrityLevel::Frozen));

    template_object
}

pub fn eval_delete_property(
    cx: Context,
    object: Handle<Value>,
    key: Handle<PropertyKey>,
    is_strict: bool,
) -> EvalResult<bool> {
    let mut base_object = maybe!(to_object(cx, object));
    let delete_status = maybe!(base_object.delete(cx, key));

    if !delete_status && is_strict {
        return type_error(cx, "cannot delete property");
    }

    delete_status.into()
}

pub fn eval_typeof(mut cx: Context, value: Handle<Value>) -> Handle<StringValue> {
    let type_string = if value.is_pointer() {
        let kind = value.as_pointer().descriptor().kind();
        match kind {
            ObjectKind::String => "string",
            ObjectKind::Symbol => "symbol",
            ObjectKind::BigInt => "bigint",
            // All other pointer values must be an object
            _ => {
                if value.as_object().is_callable() {
                    "function"
                } else {
                    "object"
                }
            }
        }
    } else {
        match value.get_tag() {
            NULL_TAG => "object",
            UNDEFINED_TAG => "undefined",
            BOOL_TAG => "boolean",
            // Otherwise must be a number - either a double or smi
            _ => "number",
        }
    };

    cx.alloc_string(type_string)
}

pub fn eval_negate(cx: Context, value: Handle<Value>) -> EvalResult<Handle<Value>> {
    let value = maybe!(to_numeric(cx, value));

    if value.is_bigint() {
        let neg_bignum = -value.as_bigint().bigint();
        BigIntValue::new(cx, neg_bignum).into()
    } else {
        Value::number(-value.as_number()).to_handle(cx).into()
    }
}

pub fn eval_bitwise_not(cx: Context, value: Handle<Value>) -> EvalResult<Handle<Value>> {
    let value = maybe!(to_numeric(cx, value));

    if value.is_bigint() {
        let not_bignum = !value.as_bigint().bigint();
        BigIntValue::new(cx, not_bignum).into()
    } else {
        let value = must!(to_int32(cx, value));
        Value::smi(!value).to_handle(cx).into()
    }
}

pub fn eval_add(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_prim = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::None));
    let right_prim = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::None));
    if left_prim.is_string() || right_prim.is_string() {
        let left_string = maybe!(to_string(cx, left_prim));
        let right_string = maybe!(to_string(cx, right_prim));

        return StringValue::concat(cx, left_string, right_string).into();
    }

    let left_num = maybe!(to_numeric(cx, left_prim));
    let right_num = maybe!(to_numeric(cx, right_prim));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() + right_num.as_bigint().bigint();
        return BigIntValue::new(cx, result).into();
    } else {
        return Value::number(left_num.as_number() + right_num.as_number())
            .to_handle(cx)
            .into();
    }
}

pub fn eval_subtract(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() - right_num.as_bigint().bigint();
        return BigIntValue::new(cx, result).into();
    } else {
        return Value::number(left_num.as_number() - right_num.as_number())
            .to_handle(cx)
            .into();
    }
}

pub fn eval_multiply(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() * right_num.as_bigint().bigint();
        return BigIntValue::new(cx, result).into();
    } else {
        return Value::number(left_num.as_number() * right_num.as_number())
            .to_handle(cx)
            .into();
    }
}

pub fn eval_divide(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let bigint_right = right_num.as_bigint().bigint();
        if bigint_right.eq(&BigInt::default()) {
            return range_error(cx, "BigInt division by zero");
        }

        let result = left_num.as_bigint().bigint() / bigint_right;
        return BigIntValue::new(cx, result).into();
    } else {
        return Value::number(left_num.as_number() / right_num.as_number())
            .to_handle(cx)
            .into();
    }
}

pub fn eval_remainder(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let bigint_right = right_num.as_bigint().bigint();
        if bigint_right.eq(&BigInt::default()) {
            return range_error(cx, "BigInt division by zero");
        }

        let bigint_left = left_num.as_bigint().bigint();
        if bigint_left.eq(&BigInt::default()) {
            return BigIntValue::new(cx, BigInt::default()).into();
        }

        let result = bigint_left % bigint_right;
        return BigIntValue::new(cx, result).into();
    } else {
        return Value::number(left_num.as_number() % right_num.as_number())
            .to_handle(cx)
            .into();
    }
}

pub fn eval_exponentiation(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let base_bignum = left_num.as_bigint().bigint();
        let exponent_bignum = right_num.as_bigint().bigint();

        if exponent_bignum.lt(&BigInt::default()) {
            return range_error(cx, "BigInt negative exponent");
        } else if exponent_bignum.eq(&BigInt::default()) && base_bignum.eq(&BigInt::default()) {
            return BigIntValue::new(cx, 1.into()).into();
        }

        if let Ok(exponent_u32) = exponent_bignum.try_into() {
            let result = base_bignum.pow(exponent_u32);
            return BigIntValue::new(cx, result).into();
        } else {
            // This guarantees a bigint that is too large
            return range_error(cx, "BigInt is too large");
        }
    } else {
        return Value::number(number_exponentiate(left_num.as_number(), right_num.as_number()))
            .to_handle(cx)
            .into();
    }
}

pub fn eval_less_than(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::Number));
    let right = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::Number));

    let result = maybe!(is_less_than(cx, left, right));
    if result.is_undefined() {
        cx.bool(false).into()
    } else {
        cx.bool(result.as_bool()).into()
    }
}

pub fn eval_greater_than(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::Number));
    let right = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::Number));

    // Intentionally flipped
    let result = maybe!(is_less_than(cx, right, left));
    if result.is_undefined() {
        cx.bool(false).into()
    } else {
        cx.bool(result.as_bool()).into()
    }
}

pub fn eval_less_than_or_equal(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::Number));
    let right = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::Number));

    // Intentionally flipped
    let result = maybe!(is_less_than(cx, right, left));
    cx.bool(result.is_false()).into()
}

pub fn eval_greater_than_or_equal(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left = maybe!(to_primitive(cx, left_value, ToPrimitivePreferredType::Number));
    let right = maybe!(to_primitive(cx, right_value, ToPrimitivePreferredType::Number));

    let result = maybe!(is_less_than(cx, left, right));
    cx.bool(result.is_false()).into()
}

pub fn eval_bitwise_and(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() & right_num.as_bigint().bigint();
        return BigIntValue::new(cx, result).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_smi = must!(to_int32(cx, right_value));

        return Value::smi(left_smi & right_smi).to_handle(cx).into();
    }
}

pub fn eval_bitwise_or(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() | right_num.as_bigint().bigint();
        return BigIntValue::new(cx, result).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_smi = must!(to_int32(cx, right_value));

        return Value::smi(left_smi | right_smi).to_handle(cx).into();
    }
}

pub fn eval_bitwise_xor(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() ^ right_num.as_bigint().bigint();
        return BigIntValue::new(cx, result).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_smi = must!(to_int32(cx, right_value));

        return Value::smi(left_smi ^ right_smi).to_handle(cx).into();
    }
}

pub fn eval_shift_left(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = maybe!(eval_bigint_left_shift(
            cx,
            &left_num.as_bigint().bigint(),
            &right_num.as_bigint().bigint()
        ));

        return BigIntValue::new(cx, result).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_u32 = must!(to_uint32(cx, right_value));

        // Shift modulus 32
        let shift = right_u32 & 0x1F;

        return Value::smi(left_smi << shift).to_handle(cx).into();
    }
}

pub fn eval_shift_right_arithmetic(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = maybe!(eval_bigint_left_shift(
            cx,
            &left_num.as_bigint().bigint(),
            &-right_num.as_bigint().bigint()
        ));

        return BigIntValue::new(cx, result).into();
    } else {
        let left_smi = must!(to_int32(cx, left_value));
        let right_u32 = must!(to_uint32(cx, right_value));

        // Shift modulus 32
        let shift = right_u32 & 0x1F;

        return Value::smi(left_smi >> shift).to_handle(cx).into();
    }
}

// 6.1.6.2.9 BigInt::leftShift
fn eval_bigint_left_shift(cx: Context, left: &BigInt, right: &BigInt) -> EvalResult<BigInt> {
    let bigint_2: BigInt = 2.into();

    if right.lt(&BigInt::default()) {
        let exponent: u32 = match (-right).try_into() {
            Ok(exponent) => exponent,
            // This guarantees a bigint that is zero, since no bigints that can be represented
            // that would be large enough for the result to be non-zero.
            Err(_) => return BigInt::default().into(),
        };

        let pow_of_2 = bigint_2.pow(exponent);

        // Division result must be rounded down, even for negative numbers. Detect this case and
        // force rounding down for negative numbers.
        if left.sign() == Sign::Minus {
            let result: BigInt = (left - &pow_of_2 + 1) / &pow_of_2;
            result.into()
        } else {
            (left / pow_of_2).into()
        }
    } else {
        let exponent: u32 = match right.try_into() {
            Ok(exponent) => exponent,
            // This guarantees a bigint that is too large
            Err(_) => {
                return range_error(cx, "BigInt is too large");
            }
        };

        (left * bigint_2.pow(exponent)).into()
    }
}

pub fn eval_shift_right_logical(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = maybe!(to_numeric(cx, left_value));
    let right_num = maybe!(to_numeric(cx, right_value));

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint || right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    let left_smi = must!(to_uint32(cx, left_value));
    let right_u32 = must!(to_uint32(cx, right_value));

    // Shift modulus 32
    let shift = right_u32 & 0x1F;

    return Value::from(left_smi >> shift).to_handle(cx).into();
}

// 13.10.2 InstanceofOperator
pub fn eval_instanceof_expression(
    cx: Context,
    value: Handle<Value>,
    target: Handle<Value>,
) -> EvalResult<bool> {
    if !target.is_object() {
        return type_error(cx, "invalid instanceof operand");
    }

    let has_instance_key = cx.well_known_symbols.has_instance();
    let instance_of_handler = maybe!(get_method(cx, target, has_instance_key));
    if let Some(instance_of_handler) = instance_of_handler {
        let result = maybe!(call_object(cx, instance_of_handler, target, &[value]));
        return to_boolean(result.get()).into();
    }

    let target_object = target.as_object();
    if !target_object.is_callable() {
        return type_error(cx, "invalid 'instanceof' operand");
    }

    let has_instance = maybe!(ordinary_has_instance(cx, target, value));
    has_instance.into()
}

pub fn eval_in_expression(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<bool> {
    if !right_value.is_object() {
        return type_error(cx, "right side of 'in' must be an object");
    }

    let property_key = maybe!(to_property_key(cx, left_value));
    has_property(cx, right_value.as_object(), property_key)
}
