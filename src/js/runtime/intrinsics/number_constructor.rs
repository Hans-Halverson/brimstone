use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        gc::{Gc, GcDeref},
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::{
            ordinary_create_from_constructor, ordinary_object_create, OrdinaryObject,
        },
        property::Property,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        type_utilities::to_numeric,
        value::Value,
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 21.1 Number Objects
#[repr(C)]
pub struct NumberObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    // The number value wrapped by this object
    number_data: f64,
}

impl GcDeref for NumberObject {}

impl_gc_into!(NumberObject, ObjectValue);

impl NumberObject {
    const VTABLE: *const () = extract_object_vtable::<NumberObject>();

    pub fn new(object: OrdinaryObject, number_data: f64) -> NumberObject {
        NumberObject {
            _vtable: Self::VTABLE,
            object,
            number_data,
        }
    }

    pub fn new_from_value(cx: &mut Context, number_data: f64) -> Gc<NumberObject> {
        let proto = cx.current_realm().get_intrinsic(Intrinsic::NumberPrototype);
        let object = ordinary_object_create(proto);

        cx.heap.alloc(NumberObject::new(object, number_data))
    }

    pub fn number_data(&self) -> f64 {
        self.number_data
    }

    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }
}

#[wrap_ordinary_object]
impl Object for NumberObject {
    fn is_number_object(&self) -> bool {
        true
    }
}

pub struct NumberConstructor;

impl NumberConstructor {
    // 21.1.2 Properties of the Number Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func =
            BuiltinFunction::create(cx, Self::construct, 1, "Number", Some(realm), None, None);

        func.set_is_constructor();
        func.set_property(
            "prototype".to_owned(),
            Property::data(
                realm.get_intrinsic(Intrinsic::NumberPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func.set_property(
            "EPSILON".to_owned(),
            Property::data(Value::number(f64::EPSILON), false, false, false),
        );
        func.set_property(
            "MAX_SAFE_INTEGER".to_owned(),
            Property::data(Value::number(9007199254740991.0), false, false, false),
        );
        func.set_property(
            "MAX_VALUE".to_owned(),
            Property::data(Value::number(f64::MAX), false, false, false),
        );
        func.set_property(
            "MIN_SAFE_INTEGER".to_owned(),
            Property::data(Value::number(-9007199254740991.0), false, false, false),
        );
        func.set_property(
            "MIN_VALUE".to_owned(),
            Property::data(Value::number(f64::MIN), false, false, false),
        );
        func.set_property(
            "NaN".to_owned(),
            Property::data(Value::nan(), false, false, false),
        );
        func.set_property(
            "NEGATIVE_INFINITY".to_owned(),
            Property::data(Value::number(f64::NEG_INFINITY), false, false, false),
        );
        func.set_property(
            "POSITIVE_INFINITY".to_owned(),
            Property::data(Value::number(f64::INFINITY), false, false, false),
        );

        func.intrinsic_func(cx, "isFinite", Self::is_finite, 1, realm);
        func.intrinsic_func(cx, "isNaN", Self::is_nan, 1, realm);

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
                unimplemented!("BigInt")
            } else {
                numeric_value.as_number()
            }
        };

        match new_target {
            None => number_value.into(),
            Some(new_target) => {
                let object = maybe!(ordinary_create_from_constructor(
                    cx,
                    new_target,
                    Intrinsic::NumberPrototype
                ));

                cx.heap
                    .alloc(NumberObject::new(object, number_value))
                    .into()
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
}
