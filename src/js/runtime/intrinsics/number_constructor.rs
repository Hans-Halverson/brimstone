use num_traits::ToPrimitive;

use crate::{
    common::numeric::{MAX_SAFE_INTEGER_F64, MIN_POSITIVE_SUBNORMAL_F64, MIN_SAFE_INTEGER_F64},
    intrinsic_methods,
    runtime::{
        Context,
        alloc_error::AllocResult,
        gc::Handle,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic, number_object::NumberObject, rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::{is_integral_number, to_numeric},
    },
    runtime_fn,
};

pub struct NumberConstructor;

impl NumberConstructor {
    /// Properties of the Number Constructor (https://tc39.es/ecma262/#sec-properties-of-the-number-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::NumberConstructor_construct,
            1,
            cx.names.number(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::NumberPrototype)?;

        intrinsic_methods!(cx, builder, {
            is_finite       NumberConstructor_is_finite       (1),
            is_integer      NumberConstructor_is_integer      (1),
            is_nan          NumberConstructor_is_nan          (1),
            is_safe_integer NumberConstructor_is_safe_integer (1),
        });

        builder.frozen(cx.names.epsilon(), cx.number(f64::EPSILON))?;
        builder.frozen(cx.names.max_safe_integer(), cx.number(MAX_SAFE_INTEGER_F64))?;
        builder.frozen(cx.names.max_value(), cx.number(f64::MAX))?;
        builder.frozen(cx.names.min_safe_integer(), cx.number(MIN_SAFE_INTEGER_F64))?;
        builder.frozen(cx.names.min_value(), cx.number(MIN_POSITIVE_SUBNORMAL_F64))?;
        builder.frozen(cx.names.nan(), cx.nan())?;
        builder.frozen(cx.names.negative_infinity(), cx.number(f64::NEG_INFINITY))?;
        builder.frozen(cx.names.positive_infinity(), cx.number(f64::INFINITY))?;

        let parse_float = realm.get_intrinsic(Intrinsic::ParseFloat);
        builder.data(cx.names.parse_float(), parse_float.into())?;

        let parse_int = realm.get_intrinsic(Intrinsic::ParseInt);
        builder.data(cx.names.parse_int(), parse_int.into())?;

        builder.build()
    }

    runtime_fn! {
    /// Number (https://tc39.es/ecma262/#sec-number-constructor-number-value)
    fn construct(cx, _, arguments) {
        let number_value = if arguments.is_empty() {
            0.0
        } else {
            let argument = arguments.get(cx, 0);
            let numeric_value = to_numeric(cx, argument)?;
            if numeric_value.is_bigint() {
                // Safe since BigInt::to_f64 never returns None
                numeric_value.as_bigint().bigint().to_f64().unwrap()
            } else {
                numeric_value.as_number()
            }
        };

        match cx.current_new_target() {
            None => Ok(cx.number(number_value)),
            Some(new_target) => {
                Ok(NumberObject::new_from_constructor(cx, new_target, number_value)?.as_value())
            }
        }
    }}

    runtime_fn! {
    /// Number.isFinite (https://tc39.es/ecma262/#sec-number.isfinite)
    fn is_finite(cx, _, arguments) {
        let value = arguments.get(cx, 0);
        if !value.is_number() {
            return Ok(cx.bool(false));
        }

        Ok(cx.bool(!value.is_nan() && !value.is_infinity()))
    }}

    runtime_fn! {
    /// Number.isInteger (https://tc39.es/ecma262/#sec-number.isinteger)
    fn is_integer(cx, _, arguments) {
        let value = arguments.get(cx, 0);
        Ok(cx.bool(is_integral_number(*value)))
    }}

    runtime_fn! {
    /// Number.isNaN (https://tc39.es/ecma262/#sec-number.isnan)
    fn is_nan(cx, _, arguments) {
        let value = arguments.get(cx, 0);
        if !value.is_number() {
            return Ok(cx.bool(false));
        }

        Ok(cx.bool(value.is_nan()))
    }}

    runtime_fn! {
    /// Number.isSafeInteger (https://tc39.es/ecma262/#sec-number.issafeinteger)
    fn is_safe_integer(cx, _, arguments) {
        let value = arguments.get(cx, 0);
        if !is_integral_number(*value) {
            return Ok(cx.bool(false));
        }

        Ok(cx.bool(value.as_number().abs() <= MAX_SAFE_INTEGER_F64))
    }}
}
