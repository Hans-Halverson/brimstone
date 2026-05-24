use std::convert::TryInto;

use icu_collections::codepointtrie::TrieValue;
use num_bigint::BigInt;

use crate::{
    must,
    runtime::{
        alloc_error::AllocResult,
        error::range_error,
        eval::expression::eval_bigint_left_shift,
        numeric_operations::number_exponentiate,
        string_value::StringValue,
        type_utilities::{to_int32, to_uint32},
        value::BigIntValue,
        Context, EvalResult, Handle, Value,
    },
};

/// Common operations that are used by IC Stubs and the interpreter

/// Check if a value is a string
#[inline]
pub fn is_string(val: Handle<Value>) -> bool {
    val.is_string()
}

/// Adds two strings
#[inline]
pub fn add_string_fast(
    cx: Context,
    left: Handle<StringValue>,
    right: Handle<StringValue>,
) -> AllocResult<Handle<Value>> {
    return Ok(StringValue::concat(cx, left, right)?.as_value());
}

/// Checks if a value is a big int
#[inline]
pub fn is_bigint(val: Handle<Value>) -> bool {
    val.is_bigint()
}

/// Adds two big ints
#[inline]
pub fn add_bigint_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> AllocResult<Handle<Value>> {
    let result = left_num.as_bigint().bigint() + right_num.as_bigint().bigint();
    Ok(BigIntValue::new(cx, result)?.into())
}

/// Checks if a value is a number
#[inline]
pub fn is_number(val: Handle<Value>) -> bool {
    val.is_number()
}

/// Adds two numbers
#[inline]
pub fn add_number_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    cx.number(left_num.as_number() + right_num.as_number())
}

/// Check if a value is an SMI
#[inline]
pub fn is_smi(val: Handle<Value>) -> bool {
    val.is_smi()
}

/// Adds two smi's
#[inline]
pub fn add_smi_fast(
    cx: Context,
    left_smi: Handle<Value>,
    right_smi: Handle<Value>,
) -> Handle<Value> {
    match left_smi.as_smi().checked_add(right_smi.as_smi()) {
        Some(sum) => cx.smi(sum),
        None => add_number_fast(cx, left_smi, right_smi),
    }
}

/// Subs two numbers
#[inline]
pub fn sub_number_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    cx.number(left_num.as_number() - right_num.as_number())
}

/// Subs two smis
#[inline]
pub fn sub_smi_fast(
    cx: Context,
    left_smi: Handle<Value>,
    right_smi: Handle<Value>,
) -> Handle<Value> {
    match left_smi.as_smi().checked_sub(right_smi.as_smi()) {
        Some(r) => cx.smi(r),
        None => sub_number_fast(cx, left_smi, right_smi),
    }
}

/// Subs two big ints
#[inline]
pub fn sub_bigint_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> AllocResult<Handle<Value>> {
    let result = left_num.as_bigint().bigint() - right_num.as_bigint().bigint();
    Ok(BigIntValue::new(cx, result)?.into())
}

/// Muls two smis
#[inline]
pub fn mul_smi_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    match left_num.as_smi().checked_mul(right_num.as_smi()) {
        Some(r) => cx.smi(r),
        None => mul_number_fast(cx, left_num, right_num),
    }
}

/// Muls two numbers
#[inline]
pub fn mul_number_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    cx.number(left_num.as_number() * right_num.as_number())
}

/// Muls two bigints
#[inline]
pub fn mul_bigint_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> AllocResult<Handle<Value>> {
    let result = left_num.as_bigint().bigint() * right_num.as_bigint().bigint();
    Ok(BigIntValue::new(cx, result)?.into())
}

/// Divs two smis
#[inline]
pub fn div_smi_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    let l = left_num.as_smi();
    let r = right_num.as_smi();
    if r != 0 && l % r == 0 {
        if let Some(result) = l.checked_div(r) {
            return cx.smi(result);
        }
    }
    div_number_fast(cx, left_num, right_num)
}

/// Divs two numbers
#[inline]
pub fn div_number_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    cx.number(left_num.as_number() / right_num.as_number())
}

/// Divs two smis
#[inline]
pub fn div_bigint_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let bigint_right = right_num.as_bigint().bigint();
    if bigint_right.eq(&BigInt::default()) {
        return range_error(cx, "BigInt division by zero");
    }

    let result = left_num.as_bigint().bigint() / bigint_right;
    Ok(BigIntValue::new(cx, result)?.into())
}

/// rems two smis
#[inline]
pub fn rem_smi_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    match left_num.as_smi().checked_rem(right_num.as_smi()) {
        Some(r) => cx.smi(r),
        None => rem_number_fast(cx, left_num, right_num),
    }
}

/// rems two numbers
#[inline]
pub fn rem_number_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    cx.number(left_num.as_number() % right_num.as_number())
}

/// rems two bigints
#[inline]
pub fn rem_bigint_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
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
}

/// exps two smis
#[inline]
pub fn exp_smi_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    let base = left_num.as_smi();
    let exp = right_num.as_smi();

    // Negative exponent: fractional result, bail to number
    if exp < 0 {
        return exp_number_fast(cx, left_num, right_num);
    }

    match base.checked_pow(exp.to_u32()) {
        Some(r) => cx.smi(r),
        None => exp_number_fast(cx, left_num, right_num),
    }
}

/// exps two numbers
#[inline]
pub fn exp_number_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    // NOTE: Can probably make more fast paths for number_exponentiate
    cx.number(number_exponentiate(left_num.as_number(), right_num.as_number()))
}

/// exps two bigints
#[inline]
pub fn exp_bigint_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
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
}

/// bitwise_ands two smis
#[inline]
pub fn bitwise_and_smi_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    cx.smi(left_num.as_smi() & right_num.as_smi())
}

/// bitwise_ands two numbers
#[inline]
pub fn bitwise_and_number_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_smi = must!(to_int32(cx, left_num));
    let right_smi = must!(to_int32(cx, right_num));

    Ok(cx.smi(left_smi & right_smi))
}

/// bitwise_ands two bigints
#[inline]
pub fn bitwise_and_bigint_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let result = left_num.as_bigint().bigint() & right_num.as_bigint().bigint();
    Ok(BigIntValue::new(cx, result)?.into())
}

/// bitwise_ors two smis
#[inline]
pub fn bitwise_or_smi_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    cx.smi(left_num.as_smi() | right_num.as_smi())
}

/// bitwise_ors two numbers
#[inline]
pub fn bitwise_or_number_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_smi = must!(to_int32(cx, left_num));
    let right_smi = must!(to_int32(cx, right_num));

    Ok(cx.smi(left_smi | right_smi))
}

/// bitwise_ors two bigints
#[inline]
pub fn bitwise_or_bigint_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let result = left_num.as_bigint().bigint() | right_num.as_bigint().bigint();
    Ok(BigIntValue::new(cx, result)?.into())
}

/// bitwise_xors two smis
#[inline]
pub fn bitwise_xor_smi_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> Handle<Value> {
    cx.smi(left_num.as_smi() ^ right_num.as_smi())
}

/// bitwise_xors two numbers
#[inline]
pub fn bitwise_xor_number_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_smi = must!(to_int32(cx, left_num));
    let right_smi = must!(to_int32(cx, right_num));

    Ok(cx.smi(left_smi ^ right_smi))
}

/// bitwise_xors two bigints
#[inline]
pub fn bitwise_xor_bigint_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let result = left_num.as_bigint().bigint() ^ right_num.as_bigint().bigint();
    Ok(BigIntValue::new(cx, result)?.into())
}

/// shift_lefts two smis
#[inline]
pub fn shift_left_smi_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_smi = left_num.as_smi();
    let right_smi = right_num.as_smi();

    if right_smi < 0 {
        shift_left_number_fast(cx, left_num, right_num)
    } else {
        let shift = (right_smi as u32) & 0x1F;
        Ok(cx.smi(left_smi.wrapping_shl(shift)))
    }
}

/// shift_lefts two numbers
#[inline]
pub fn shift_left_number_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_smi = must!(to_int32(cx, left_num));
    let right_u32 = must!(to_uint32(cx, right_num));

    // Shift modulus 32
    let shift = right_u32 & 0x1F;
    Ok(cx.smi(left_smi << shift))
}

/// shift_lefts two bigints
#[inline]
pub fn shift_left_bigint_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let result = eval_bigint_left_shift(
        cx,
        &left_num.as_bigint().bigint(),
        &right_num.as_bigint().bigint(),
    )?;

    Ok(BigIntValue::new(cx, result)?.into())
}

/// shift_right_ariths two smis
#[inline]
pub fn shift_right_arith_smi_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_smi = left_num.as_smi();
    let right_smi = right_num.as_smi();

    if right_smi < 0 {
        shift_right_arith_number_fast(cx, left_num, right_num)
    } else {
        let shift = (right_smi as u32) & 0x1F;
        Ok(cx.smi(left_smi >> shift))
    }
}

/// shift_right_ariths two numbers
#[inline]
pub fn shift_right_arith_number_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_smi = must!(to_int32(cx, left_num));
    let right_u32 = must!(to_uint32(cx, right_num));

    // Shift modulus 32
    let shift = right_u32 & 0x1F;

    Ok(cx.smi(left_smi >> shift))
}

/// shift_right_ariths two bigints
#[inline]
pub fn shift_right_arith_bigint_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let result = eval_bigint_left_shift(
        cx,
        &left_num.as_bigint().bigint(),
        &-right_num.as_bigint().bigint(),
    )?;

    Ok(BigIntValue::new(cx, result)?.into())
}

/// shift_right_logicals two smis
#[inline]
pub fn shift_right_logical_smi_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_smi = left_num.as_smi();
    let right_smi = right_num.as_smi();

    if right_smi < 0 || left_smi < 0 {
        shift_right_logical_number_fast(cx, left_num, right_num)
    } else {
        let shift = (right_smi as u32) & 0x1F;
        let result = (left_smi as u32) >> shift;
        Ok(cx.number(result as f64))
    }
}

/// shift_right_logicals two numbers
#[inline]
pub fn shift_right_logical_number_fast(
    cx: Context,
    left_num: Handle<Value>,
    right_num: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let left_smi = must!(to_uint32(cx, left_num));
    let right_u32 = must!(to_uint32(cx, right_num));

    // Shift modulus 32
    let shift = right_u32 & 0x1F;

    Ok(Value::from(left_smi >> shift).to_handle(cx))
}

/// smi equal fast
#[inline]
pub fn strict_equal_smi_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> Handle<Value> {
    cx.bool(left.as_smi() == right.as_smi())
}

/// number equal fast
#[inline]
pub fn strict_equal_number_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> Handle<Value> {
    cx.bool(left.as_number() == right.as_number())
}

/// bigint equal fast
#[inline]
pub fn strict_equal_bigint_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> Handle<Value> {
    cx.bool(left.as_bigint().bigint().eq(&right.as_bigint().bigint()))
}

/// string equal fast
#[inline]
pub fn strict_equal_string_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    Ok(cx.bool(left.as_string().equals(&right.as_string())?))
}

/// smi not equal fast
#[inline]
pub fn strict_not_equal_smi_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> Handle<Value> {
    let res = strict_equal_smi_fast(cx, left, right);
    cx.bool(res.is_false())
}

/// number not equal fast
#[inline]
pub fn strict_not_equal_number_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> Handle<Value> {
    let res = strict_equal_number_fast(cx, left, right);
    cx.bool(res.is_false())
}

/// bigint not equal fast
#[inline]
pub fn strict_not_equal_bigint_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> Handle<Value> {
    let res = strict_equal_bigint_fast(cx, left, right);
    cx.bool(res.is_false())
}

/// string not equal fast
#[inline]
pub fn strict_not_equal_string_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let res = strict_equal_string_fast(cx, left, right)?;
    Ok(cx.bool(res.is_false()))
}

/// smi less than fast
#[inline]
pub fn less_than_smi_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    Ok(cx.bool(left.as_smi() < right.as_smi()))
}

/// number less than fast
#[inline]
pub fn less_than_number_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    if left.is_nan() || right.is_nan() {
        return Ok(cx.undefined());
    }

    Ok(cx.bool(left.as_number() < right.as_number()))
}

/// bigint less than fast
#[inline]
pub fn less_than_bigint_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    Ok(cx.bool(left.as_bigint().bigint().lt(&right.as_bigint().bigint())))
}

/// string less than fast
#[inline]
pub fn less_than_string_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    Ok(cx.bool(
        left.as_string()
            .compare(&right.as_string())
            .map(|o| o.is_lt())?,
    ))
}

/// smi greater than fast
#[inline]
pub fn greater_than_smi_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    less_than_smi_fast(cx, right, left)
}

/// number greater than fast
#[inline]
pub fn greater_than_number_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    // Can return undefined in case of NaN
    let res = less_than_number_fast(cx, right, left)?;
    if res.is_undefined() {
        Ok(cx.bool(false))
    } else {
        Ok(cx.bool(res.as_bool()))
    }
}

/// bigint greater than fast
#[inline]
pub fn greater_than_bigint_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    less_than_bigint_fast(cx, right, left)
}

/// string greater than fast
#[inline]
pub fn greater_than_string_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    less_than_string_fast(cx, right, left)
}

/// smi less than or equal fast
#[inline]
pub fn less_than_or_equal_smi_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let res = less_than_smi_fast(cx, right, left)?;
    Ok(cx.bool(res.is_false()))
}

/// number less than or equal fast
#[inline]
pub fn less_than_or_equal_number_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let res = less_than_number_fast(cx, right, left)?;
    Ok(cx.bool(res.is_false()))
}

/// bigint less than or equal fast
#[inline]
pub fn less_than_or_equal_bigint_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let res = less_than_bigint_fast(cx, right, left)?;
    Ok(cx.bool(res.is_false()))
}

/// string less than or equal fast
#[inline]
pub fn less_than_or_equal_string_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let res = less_than_string_fast(cx, right, left)?;
    Ok(cx.bool(res.is_false()))
}

/// smi greater than or equal fast
#[inline]
pub fn greater_than_or_equal_smi_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let res = less_than_smi_fast(cx, left, right)?;
    Ok(cx.bool(res.is_false()))
}

/// number greater than or equal fast
#[inline]
pub fn greater_than_or_equal_number_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let res = less_than_number_fast(cx, left, right)?;
    Ok(cx.bool(res.is_false()))
}

/// bigint greater than or equal fast
#[inline]
pub fn greater_than_or_equal_bigint_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let res = less_than_bigint_fast(cx, left, right)?;
    Ok(cx.bool(res.is_false()))
}

/// string greater than or equal fast
#[inline]
pub fn greater_than_or_equal_string_fast(
    cx: Context,
    left: Handle<Value>,
    right: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let res = less_than_bigint_fast(cx, left, right)?;
    Ok(cx.bool(res.is_false()))
}
