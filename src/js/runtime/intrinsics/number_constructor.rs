use std::str::FromStr;

use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        gc::Gc,
        numeric_constants::{
            MAX_SAFE_INTEGER_F64, MIN_POSITIVE_SUBNORMAL_F64, MIN_SAFE_INTEGER_F64,
        },
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{object_ordinary_init, object_ordinary_init_from_constructor},
        property::Property,
        realm::Realm,
        type_utilities::{is_integral_number, to_numeric},
        value::Value,
        Context,
    },
    maybe,
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
    pub fn new_with_proto(
        cx: &mut Context,
        proto: Gc<ObjectValue>,
        number_data: f64,
    ) -> Gc<NumberObject> {
        let mut object = cx.heap.alloc_uninit::<NumberObject>();
        object.descriptor = cx.base_descriptors.get(ObjectKind::NumberObject);

        object_ordinary_init(cx, object.object(), proto);

        object.number_data = number_data;

        object
    }

    pub fn new_from_value(cx: &mut Context, number_data: f64) -> Gc<NumberObject> {
        let proto = cx.current_realm().get_intrinsic(Intrinsic::NumberPrototype);

        Self::new_with_proto(cx, proto, number_data)
    }

    pub fn new_from_constructor(
        cx: &mut Context,
        constructor: Gc<ObjectValue>,
        number_data: f64,
    ) -> EvalResult<Gc<NumberObject>> {
        let mut object = cx.heap.alloc_uninit::<NumberObject>();
        object.descriptor = cx.base_descriptors.get(ObjectKind::NumberObject);

        maybe!(object_ordinary_init_from_constructor(
            cx,
            object.object(),
            constructor,
            Intrinsic::NumberPrototype
        ));

        object.number_data = number_data;

        object.into()
    }

    pub fn number_data(&self) -> f64 {
        self.number_data
    }
}

pub struct NumberConstructor;

impl NumberConstructor {
    // 21.1.2 Properties of the Number Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            &cx.names.number(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            cx,
            &cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::NumberPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func.set_property(
            cx,
            &cx.names.epsilon(),
            Property::data(Value::number(f64::EPSILON), false, false, false),
        );
        func.set_property(
            cx,
            &cx.names.max_safe_integer(),
            Property::data(Value::number(MAX_SAFE_INTEGER_F64), false, false, false),
        );
        func.set_property(
            cx,
            &cx.names.max_value(),
            Property::data(Value::number(f64::MAX), false, false, false),
        );
        func.set_property(
            cx,
            &cx.names.min_safe_integer(),
            Property::data(Value::number(MIN_SAFE_INTEGER_F64), false, false, false),
        );
        func.set_property(
            cx,
            &cx.names.min_value(),
            Property::data(Value::number(MIN_POSITIVE_SUBNORMAL_F64), false, false, false),
        );
        func.set_property(cx, &cx.names.nan(), Property::data(Value::nan(), false, false, false));
        func.set_property(
            cx,
            &cx.names.negative_infinity(),
            Property::data(Value::number(f64::NEG_INFINITY), false, false, false),
        );
        func.set_property(
            cx,
            &cx.names.positive_infinity(),
            Property::data(Value::number(f64::INFINITY), false, false, false),
        );

        func.intrinsic_func(cx, &cx.names.is_finite(), Self::is_finite, 1, realm);
        func.intrinsic_func(cx, &cx.names.is_integer(), Self::is_integer, 1, realm);
        func.intrinsic_func(cx, &cx.names.is_nan(), Self::is_nan, 1, realm);
        func.intrinsic_func(cx, &cx.names.is_safe_integer(), Self::is_safe_integer, 1, realm);

        func
    }

    // 21.1.1.1 Number
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let number_value = if arguments.is_empty() {
            0.0
        } else {
            let numeric_value = maybe!(to_numeric(cx, get_argument(arguments, 0)));
            if numeric_value.is_bigint() {
                // TODO: Create better conversion directly from BigInt to f64 instead of through string
                let bigint_string = numeric_value.as_bigint().bigint().to_string();
                f64::from_str(&bigint_string).unwrap().into()
            } else {
                numeric_value.as_number()
            }
        };

        match new_target {
            None => number_value.into(),
            Some(new_target) => {
                maybe!(NumberObject::new_from_constructor(cx, new_target, number_value)).into()
            }
        }
    }

    // 21.1.2.2 Number.isFinite
    fn is_finite(
        _: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let value = get_argument(arguments, 0);
        if !value.is_number() {
            return false.into();
        }

        (!value.is_nan() && !value.is_infinity()).into()
    }

    // 21.1.2.3 Number.isInteger
    fn is_integer(
        _: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let value = get_argument(arguments, 0);
        is_integral_number(value).into()
    }

    // 21.1.2.4 Number.isNaN
    fn is_nan(
        _: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let value = get_argument(arguments, 0);
        if !value.is_number() {
            return false.into();
        }

        value.is_nan().into()
    }

    // 21.1.2.5 Number.isSafeInteger
    fn is_safe_integer(
        _: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let value = get_argument(arguments, 0);
        if !is_integral_number(value) {
            return false.into();
        }

        (value.as_number().abs() <= MAX_SAFE_INTEGER_F64).into()
    }
}
