use std::convert::TryInto;

use num_bigint::{BigInt, Sign};

use crate::{
    must, must_a,
    parser::ast,
    runtime::{
        abstract_operations::{
            call_object, define_property_or_throw, get_method, has_property, ordinary_has_instance,
            set_integrity_level, IntegrityLevel,
        },
        alloc_error::AllocResult,
        array_object::array_create_in_realm,
        error::{range_error, type_error},
        eval_result::EvalResult,
        heap_item_descriptor::HeapItemKind,
        numeric_operations::number_exponentiate,
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
};

/// GetTemplateObject (https://tc39.es/ecma262/#sec-gettemplateobject)
pub fn generate_template_object(
    mut cx: Context,
    realm: Handle<Realm>,
    lit: &ast::TemplateLiteral,
) -> AllocResult<Handle<ObjectValue>> {
    let num_strings = lit.quasis.len();
    let template_object =
        must_a!(array_create_in_realm(cx, realm, num_strings as u64, None)).as_object();
    let raw_object =
        must_a!(array_create_in_realm(cx, realm, num_strings as u64, None)).as_object();

    // Property key is shared between iterations
    let mut index_key = PropertyKey::uninit().to_handle(cx);

    for (i, quasi) in lit.quasis.iter().enumerate() {
        index_key.replace(PropertyKey::array_index(cx, i as u32)?);

        let cooked_value = match &quasi.cooked {
            None => cx.undefined(),
            Some(cooked) => cx.alloc_wtf8_str(cooked)?.as_value(),
        };
        let cooked_desc = PropertyDescriptor::data(cooked_value, false, true, false);
        must_a!(define_property_or_throw(cx, template_object, index_key, cooked_desc));

        let raw_value = cx.alloc_wtf8_str(quasi.raw)?;
        let raw_desc = PropertyDescriptor::data(raw_value.as_value(), false, true, false);
        must_a!(define_property_or_throw(cx, raw_object, index_key, raw_desc));
    }

    must_a!(set_integrity_level(cx, raw_object, IntegrityLevel::Frozen));

    let raw_object_desc = PropertyDescriptor::data(raw_object.into(), false, false, false);
    must_a!(define_property_or_throw(cx, template_object, cx.names.raw(), raw_object_desc));

    must_a!(set_integrity_level(cx, template_object, IntegrityLevel::Frozen));

    Ok(template_object)
}

pub fn eval_delete_property(
    cx: Context,
    object: Handle<Value>,
    key: Handle<PropertyKey>,
    is_strict: bool,
) -> EvalResult<bool> {
    let mut base_object = to_object(cx, object)?;
    let delete_status = base_object.delete(cx, key)?;

    if !delete_status && is_strict {
        return type_error(cx, "cannot delete property");
    }

    Ok(delete_status)
}

pub fn eval_typeof(mut cx: Context, value: Handle<Value>) -> AllocResult<Handle<StringValue>> {
    let type_string = if value.is_pointer() {
        let kind = value.as_pointer().descriptor().kind();
        match kind {
            HeapItemKind::String => "string",
            HeapItemKind::Symbol => "symbol",
            HeapItemKind::BigInt => "bigint",
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

    Ok(cx.alloc_string(type_string)?.as_string())
}

pub fn eval_negate(cx: Context, value: Handle<Value>) -> EvalResult<Handle<Value>> {
    let value = to_numeric(cx, value)?;

    if value.is_bigint() {
        let neg_bignum = -value.as_bigint().bigint();
        Ok(BigIntValue::new(cx, neg_bignum)?.into())
    } else {
        Ok(cx.number(-value.as_number()))
    }
}

pub fn eval_bitwise_not(cx: Context, value: Handle<Value>) -> EvalResult<Handle<Value>> {
    let value = to_numeric(cx, value)?;

    if value.is_bigint() {
        let not_bignum = !value.as_bigint().bigint();
        Ok(BigIntValue::new(cx, not_bignum)?.into())
    } else {
        let value = must!(to_int32(cx, value));
        Ok(cx.smi(!value))
    }
}

pub fn eval_add(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_prim = to_primitive(cx, left_value, ToPrimitivePreferredType::None)?;
    let right_prim = to_primitive(cx, right_value, ToPrimitivePreferredType::None)?;
    if left_prim.is_string() || right_prim.is_string() {
        let left_string = to_string(cx, left_prim)?;
        let right_string = to_string(cx, right_prim)?;

        return Ok(StringValue::concat(cx, left_string, right_string)?.as_value());
    }

    let left_num = to_numeric(cx, left_prim)?;
    let right_num = to_numeric(cx, right_prim)?;

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() + right_num.as_bigint().bigint();
        Ok(BigIntValue::new(cx, result)?.into())
    } else {
        Ok(cx.number(left_num.as_number() + right_num.as_number()))
    }
}

pub fn eval_subtract(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = to_numeric(cx, left_value)?;
    let right_num = to_numeric(cx, right_value)?;

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() - right_num.as_bigint().bigint();
        Ok(BigIntValue::new(cx, result)?.into())
    } else {
        Ok(cx.number(left_num.as_number() - right_num.as_number()))
    }
}

pub fn eval_multiply(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = to_numeric(cx, left_value)?;
    let right_num = to_numeric(cx, right_value)?;

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() * right_num.as_bigint().bigint();
        Ok(BigIntValue::new(cx, result)?.into())
    } else {
        Ok(cx.number(left_num.as_number() * right_num.as_number()))
    }
}

pub fn eval_divide(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = to_numeric(cx, left_value)?;
    let right_num = to_numeric(cx, right_value)?;

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
        Ok(BigIntValue::new(cx, result)?.into())
    } else {
        Ok(cx.number(left_num.as_number() / right_num.as_number()))
    }
}

pub fn eval_remainder(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = to_numeric(cx, left_value)?;
    let right_num = to_numeric(cx, right_value)?;

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
            return Ok(BigIntValue::new(cx, BigInt::default())?.into());
        }

        let result = bigint_left % bigint_right;
        Ok(BigIntValue::new(cx, result)?.into())
    } else {
        Ok(cx.number(left_num.as_number() % right_num.as_number()))
    }
}

pub fn eval_exponentiation(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = to_numeric(cx, left_value)?;
    let right_num = to_numeric(cx, right_value)?;

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
            return Ok(BigIntValue::new(cx, 1.into())?.into());
        }

        if let Ok(exponent_u32) = exponent_bignum.try_into() {
            let result = base_bignum.pow(exponent_u32);
            Ok(BigIntValue::new(cx, result)?.into())
        } else {
            // This guarantees a bigint that is too large
            range_error(cx, "BigInt is too large")
        }
    } else {
        Ok(cx.number(number_exponentiate(left_num.as_number(), right_num.as_number())))
    }
}

pub fn eval_less_than(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left = to_primitive(cx, left_value, ToPrimitivePreferredType::Number)?;
    let right = to_primitive(cx, right_value, ToPrimitivePreferredType::Number)?;

    let result = is_less_than(cx, left, right)?;
    if result.is_undefined() {
        Ok(cx.bool(false))
    } else {
        Ok(cx.bool(result.as_bool()))
    }
}

pub fn eval_greater_than(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left = to_primitive(cx, left_value, ToPrimitivePreferredType::Number)?;
    let right = to_primitive(cx, right_value, ToPrimitivePreferredType::Number)?;

    // Intentionally flipped
    let result = is_less_than(cx, right, left)?;
    if result.is_undefined() {
        Ok(cx.bool(false))
    } else {
        Ok(cx.bool(result.as_bool()))
    }
}

pub fn eval_less_than_or_equal(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left = to_primitive(cx, left_value, ToPrimitivePreferredType::Number)?;
    let right = to_primitive(cx, right_value, ToPrimitivePreferredType::Number)?;

    // Intentionally flipped
    let result = is_less_than(cx, right, left)?;
    Ok(cx.bool(result.is_false()))
}

pub fn eval_greater_than_or_equal(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left = to_primitive(cx, left_value, ToPrimitivePreferredType::Number)?;
    let right = to_primitive(cx, right_value, ToPrimitivePreferredType::Number)?;

    let result = is_less_than(cx, left, right)?;
    Ok(cx.bool(result.is_false()))
}

pub fn eval_bitwise_and(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = to_numeric(cx, left_value)?;
    let right_num = to_numeric(cx, right_value)?;

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() & right_num.as_bigint().bigint();
        Ok(BigIntValue::new(cx, result)?.into())
    } else {
        let left_smi = must!(to_int32(cx, left_num));
        let right_smi = must!(to_int32(cx, right_num));

        Ok(cx.smi(left_smi & right_smi))
    }
}

pub fn eval_bitwise_or(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = to_numeric(cx, left_value)?;
    let right_num = to_numeric(cx, right_value)?;

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() | right_num.as_bigint().bigint();
        Ok(BigIntValue::new(cx, result)?.into())
    } else {
        let left_smi = must!(to_int32(cx, left_num));
        let right_smi = must!(to_int32(cx, right_num));

        Ok(cx.smi(left_smi | right_smi))
    }
}

pub fn eval_bitwise_xor(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = to_numeric(cx, left_value)?;
    let right_num = to_numeric(cx, right_value)?;

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = left_num.as_bigint().bigint() ^ right_num.as_bigint().bigint();
        Ok(BigIntValue::new(cx, result)?.into())
    } else {
        let left_smi = must!(to_int32(cx, left_num));
        let right_smi = must!(to_int32(cx, right_num));

        Ok(cx.smi(left_smi ^ right_smi))
    }
}

pub fn eval_shift_left(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = to_numeric(cx, left_value)?;
    let right_num = to_numeric(cx, right_value)?;

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = eval_bigint_left_shift(
            cx,
            &left_num.as_bigint().bigint(),
            &right_num.as_bigint().bigint(),
        )?;

        Ok(BigIntValue::new(cx, result)?.into())
    } else {
        let left_smi = must!(to_int32(cx, left_num));
        let right_u32 = must!(to_uint32(cx, right_num));

        // Shift modulus 32
        let shift = right_u32 & 0x1F;

        Ok(cx.smi(left_smi << shift))
    }
}

pub fn eval_shift_right_arithmetic(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = to_numeric(cx, left_value)?;
    let right_num = to_numeric(cx, right_value)?;

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint != right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    if left_is_bigint {
        let result = eval_bigint_left_shift(
            cx,
            &left_num.as_bigint().bigint(),
            &-right_num.as_bigint().bigint(),
        )?;

        Ok(BigIntValue::new(cx, result)?.into())
    } else {
        let left_smi = must!(to_int32(cx, left_num));
        let right_u32 = must!(to_uint32(cx, right_num));

        // Shift modulus 32
        let shift = right_u32 & 0x1F;

        Ok(cx.smi(left_smi >> shift))
    }
}

/// BigInt::leftShift (https://tc39.es/ecma262/#sec-numeric-types-bigint-leftShift)
fn eval_bigint_left_shift(cx: Context, left: &BigInt, right: &BigInt) -> EvalResult<BigInt> {
    let bigint_2: BigInt = 2.into();

    if right.lt(&BigInt::default()) {
        let exponent: u32 = match (-right).try_into() {
            Ok(exponent) => exponent,
            // This guarantees a bigint that is zero, since no bigints that can be represented
            // that would be large enough for the result to be non-zero.
            Err(_) => return Ok(BigInt::default()),
        };

        let pow_of_2 = bigint_2.pow(exponent);

        // Division result must be rounded down, even for negative numbers. Detect this case and
        // force rounding down for negative numbers.
        if left.sign() == Sign::Minus {
            let result: BigInt = (left - &pow_of_2 + 1) / &pow_of_2;
            Ok(result)
        } else {
            Ok(left / pow_of_2)
        }
    } else {
        let exponent: u32 = match right.try_into() {
            Ok(exponent) => exponent,
            // This guarantees a bigint that is too large
            Err(_) => {
                return range_error(cx, "BigInt is too large");
            }
        };

        Ok(left * bigint_2.pow(exponent))
    }
}

pub fn eval_shift_right_logical(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_num = to_numeric(cx, left_value)?;
    let right_num = to_numeric(cx, right_value)?;

    let left_is_bigint = left_num.is_bigint();
    if left_is_bigint || right_num.is_bigint() {
        return type_error(cx, "BigInt cannot be converted to number");
    }

    let left_smi = must!(to_uint32(cx, left_num));
    let right_u32 = must!(to_uint32(cx, right_num));

    // Shift modulus 32
    let shift = right_u32 & 0x1F;

    Ok(Value::from(left_smi >> shift).to_handle(cx))
}

/// InstanceofOperator (https://tc39.es/ecma262/#sec-instanceofoperator)
pub fn eval_instanceof_expression(
    cx: Context,
    value: Handle<Value>,
    target: Handle<Value>,
) -> EvalResult<bool> {
    if !target.is_object() {
        return type_error(cx, "invalid instanceof operand");
    }

    let has_instance_key = cx.well_known_symbols.has_instance();
    let instance_of_handler = get_method(cx, target, has_instance_key)?;
    if let Some(instance_of_handler) = instance_of_handler {
        let result = call_object(cx, instance_of_handler, target, &[value])?;
        return Ok(to_boolean(*result));
    }

    let target_object = target.as_object();
    if !target_object.is_callable() {
        return type_error(cx, "invalid 'instanceof' operand");
    }

    let has_instance = ordinary_has_instance(cx, target, value)?;
    Ok(has_instance)
}

pub fn eval_in_expression(
    cx: Context,
    left_value: Handle<Value>,
    right_value: Handle<Value>,
) -> EvalResult<bool> {
    if !right_value.is_object() {
        return type_error(cx, "right side of 'in' must be an object");
    }

    let property_key = to_property_key(cx, left_value)?;
    has_property(cx, right_value.as_object(), property_key)
}
