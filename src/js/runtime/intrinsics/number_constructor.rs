use std::mem::size_of;

use num_traits::ToPrimitive;

use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        gc::{Handle, HeapObject, HeapVisitor},
        numeric_constants::{
            MAX_SAFE_INTEGER_F64, MIN_POSITIVE_SUBNORMAL_F64, MIN_SAFE_INTEGER_F64,
        },
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{
            object_create, object_create_from_constructor, object_create_with_proto,
        },
        realm::Realm,
        type_utilities::{is_integral_number, to_numeric},
        value::Value,
        Context, HeapPtr,
    },
    maybe, set_uninit,
};

use super::intrinsics::Intrinsic;

// 21.1 Number Objects
extend_object! {
    pub struct NumberObject {
        // The number value wrapped by this object
        number_data: f64,
    }
}

impl NumberObject {
    pub fn new(cx: Context, number_data: f64) -> Handle<NumberObject> {
        let mut object =
            object_create::<NumberObject>(cx, ObjectKind::NumberObject, Intrinsic::NumberPrototype);

        set_uninit!(object.number_data, number_data);

        object.to_handle()
    }

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
        number_data: f64,
    ) -> EvalResult<Handle<NumberObject>> {
        let mut object = maybe!(object_create_from_constructor::<NumberObject>(
            cx,
            constructor,
            ObjectKind::NumberObject,
            Intrinsic::NumberPrototype
        ));

        set_uninit!(object.number_data, number_data);

        object.to_handle().into()
    }

    pub fn new_with_proto(
        cx: Context,
        proto: Handle<ObjectValue>,
        number_data: f64,
    ) -> Handle<NumberObject> {
        let mut object =
            object_create_with_proto::<NumberObject>(cx, ObjectKind::NumberObject, proto);

        set_uninit!(object.number_data, number_data);

        object.to_handle()
    }

    pub fn number_data(&self) -> f64 {
        self.number_data
    }
}

pub struct NumberConstructor;

impl NumberConstructor {
    // 21.1.2 Properties of the Number Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.number(),
            Some(realm),
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::NumberPrototype).into(),
        );

        let epsilon_value = Value::number(f64::EPSILON).to_handle(cx);
        func.intrinsic_frozen_property(cx, cx.names.epsilon(), epsilon_value);

        let max_safe_integer_value = Value::number(MAX_SAFE_INTEGER_F64).to_handle(cx);
        func.intrinsic_frozen_property(cx, cx.names.max_safe_integer(), max_safe_integer_value);

        let max_value = Value::number(f64::MAX).to_handle(cx);
        func.intrinsic_frozen_property(cx, cx.names.max_value(), max_value);

        let min_safe_integer_value = Value::number(MIN_SAFE_INTEGER_F64).to_handle(cx);
        func.intrinsic_frozen_property(cx, cx.names.min_safe_integer(), min_safe_integer_value);

        let min_value = Value::number(MIN_POSITIVE_SUBNORMAL_F64).to_handle(cx);
        func.intrinsic_frozen_property(cx, cx.names.min_value(), min_value);

        let nan_value = Value::nan().to_handle(cx);
        func.intrinsic_frozen_property(cx, cx.names.nan(), nan_value);

        let neg_infinity_value = Value::number(f64::NEG_INFINITY).to_handle(cx);
        func.intrinsic_frozen_property(cx, cx.names.negative_infinity(), neg_infinity_value);

        let infinity_value = Value::number(f64::INFINITY).to_handle(cx);
        func.intrinsic_frozen_property(cx, cx.names.positive_infinity(), infinity_value);

        func.intrinsic_func(cx, cx.names.is_finite(), Self::is_finite, 1, realm);
        func.intrinsic_func(cx, cx.names.is_integer(), Self::is_integer, 1, realm);
        func.intrinsic_func(cx, cx.names.is_nan(), Self::is_nan, 1, realm);
        func.intrinsic_func(cx, cx.names.is_safe_integer(), Self::is_safe_integer, 1, realm);

        let parse_float = realm.get_intrinsic(Intrinsic::ParseFloat);
        func.intrinsic_data_prop(cx, cx.names.parse_float(), parse_float.into());

        let parse_int = realm.get_intrinsic(Intrinsic::ParseInt);
        func.intrinsic_data_prop(cx, cx.names.parse_int(), parse_int.into());

        func
    }

    // 21.1.1.1 Number
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let number_value = if arguments.is_empty() {
            0.0
        } else {
            let argument = get_argument(cx, arguments, 0);
            let numeric_value = maybe!(to_numeric(cx, argument));
            if numeric_value.is_bigint() {
                // Safe since BigInt::to_f64 never returns None
                numeric_value.as_bigint().bigint().to_f64().unwrap()
            } else {
                numeric_value.as_number()
            }
        };

        match new_target {
            None => Value::from(number_value).to_handle(cx).into(),
            Some(new_target) => {
                maybe!(NumberObject::new_from_constructor(cx, new_target, number_value)).into()
            }
        }
    }

    // 21.1.2.2 Number.isFinite
    pub fn is_finite(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        if !value.is_number() {
            return cx.bool(false).into();
        }

        cx.bool(!value.is_nan() && !value.is_infinity()).into()
    }

    // 21.1.2.3 Number.isInteger
    pub fn is_integer(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        cx.bool(is_integral_number(value.get())).into()
    }

    // 21.1.2.4 Number.isNaN
    pub fn is_nan(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        if !value.is_number() {
            return cx.bool(false).into();
        }

        cx.bool(value.is_nan()).into()
    }

    // 21.1.2.5 Number.isSafeInteger
    pub fn is_safe_integer(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        if !is_integral_number(value.get()) {
            return cx.bool(false).into();
        }

        cx.bool(value.as_number().abs() <= MAX_SAFE_INTEGER_F64)
            .into()
    }
}

impl HeapObject for HeapPtr<NumberObject> {
    fn byte_size(&self) -> usize {
        size_of::<NumberObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
    }
}
