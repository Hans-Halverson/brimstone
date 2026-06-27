use std::mem::size_of;

use num_traits::ToPrimitive;

use crate::{
    common::numeric::{MAX_SAFE_INTEGER_F64, MIN_POSITIVE_SUBNORMAL_F64, MIN_SAFE_INTEGER_F64},
    extend_object,
    runtime::{
        Context, HeapPtr,
        alloc_error::AllocResult,
        builtin_function::BuiltinFunction,
        eval_result::EvalResult,
        gc::{Handle, HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemKind,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction},
        object_value::ObjectValue,
        ordinary_object::{
            object_create, object_create_from_constructor, object_create_with_proto,
        },
        realm::Realm,
        type_utilities::{is_integral_number, to_numeric},
    },
    runtime_fn, set_uninit,
};

// Number Objects (https://tc39.es/ecma262/#sec-number-objects)
extend_object! {
    pub struct NumberObject {
        // The number value wrapped by this object
        number_data: f64,
    }
}

impl NumberObject {
    pub fn new(cx: Context, number_data: f64) -> AllocResult<Handle<NumberObject>> {
        let mut object = object_create::<NumberObject>(
            cx,
            HeapItemKind::NumberObject,
            Intrinsic::NumberPrototype,
        )?;

        set_uninit!(object.number_data, number_data);

        Ok(object.to_handle())
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        number_data: f64,
    ) -> EvalResult<Handle<NumberObject>> {
        let mut object = object_create_from_constructor::<NumberObject>(
            cx,
            constructor,
            HeapItemKind::NumberObject,
            Intrinsic::NumberPrototype,
        )?;

        set_uninit!(object.number_data, number_data);

        Ok(object.to_handle())
    }

    pub fn new_with_proto(
        cx: Context,
        proto: Handle<ObjectValue>,
        number_data: f64,
    ) -> AllocResult<Handle<NumberObject>> {
        let mut object =
            object_create_with_proto::<NumberObject>(cx, HeapItemKind::NumberObject, proto)?;

        set_uninit!(object.number_data, number_data);

        Ok(object.to_handle())
    }

    pub fn number_data(&self) -> f64 {
        self.number_data
    }

    pub fn set_number_data(&mut self, number_data: f64) {
        self.number_data = number_data;
    }
}

pub struct NumberConstructor;

impl NumberConstructor {
    /// Properties of the Number Constructor (https://tc39.es/ecma262/#sec-properties-of-the-number-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            RuntimeFunction::NumberConstructor_construct,
            1,
            cx.names.number(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::NumberPrototype).into(),
        )?;

        let epsilon_value = cx.number(f64::EPSILON);
        func.intrinsic_frozen_property(cx, cx.names.epsilon(), epsilon_value)?;

        let max_safe_integer_value = cx.number(MAX_SAFE_INTEGER_F64);
        func.intrinsic_frozen_property(cx, cx.names.max_safe_integer(), max_safe_integer_value)?;

        let max_value = cx.number(f64::MAX);
        func.intrinsic_frozen_property(cx, cx.names.max_value(), max_value)?;

        let min_safe_integer_value = cx.number(MIN_SAFE_INTEGER_F64);
        func.intrinsic_frozen_property(cx, cx.names.min_safe_integer(), min_safe_integer_value)?;

        let min_value = cx.number(MIN_POSITIVE_SUBNORMAL_F64);
        func.intrinsic_frozen_property(cx, cx.names.min_value(), min_value)?;

        let nan_value = cx.nan();
        func.intrinsic_frozen_property(cx, cx.names.nan(), nan_value)?;

        let neg_infinity_value = cx.number(f64::NEG_INFINITY);
        func.intrinsic_frozen_property(cx, cx.names.negative_infinity(), neg_infinity_value)?;

        let infinity_value = cx.number(f64::INFINITY);
        func.intrinsic_frozen_property(cx, cx.names.positive_infinity(), infinity_value)?;

        func.intrinsic_func(
            cx,
            cx.names.is_finite(),
            RuntimeFunction::NumberConstructor_is_finite,
            1,
            realm,
        )?;
        func.intrinsic_func(
            cx,
            cx.names.is_integer(),
            RuntimeFunction::NumberConstructor_is_integer,
            1,
            realm,
        )?;
        func.intrinsic_func(
            cx,
            cx.names.is_nan(),
            RuntimeFunction::NumberConstructor_is_nan,
            1,
            realm,
        )?;
        func.intrinsic_func(
            cx,
            cx.names.is_safe_integer(),
            RuntimeFunction::NumberConstructor_is_safe_integer,
            1,
            realm,
        )?;

        let parse_float = realm.get_intrinsic(Intrinsic::ParseFloat);
        func.intrinsic_data_prop(cx, cx.names.parse_float(), parse_float.into())?;

        let parse_int = realm.get_intrinsic(Intrinsic::ParseInt);
        func.intrinsic_data_prop(cx, cx.names.parse_int(), parse_int.into())?;

        Ok(func)
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

impl HeapItem for HeapPtr<NumberObject> {
    fn byte_size(&self) -> usize {
        size_of::<NumberObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
    }
}
