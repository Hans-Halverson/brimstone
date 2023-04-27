use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::{range_error_, type_error_},
        function::get_argument,
        gc::Gc,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_ordinary_init,
        property::Property,
        realm::Realm,
        type_utilities::{is_integral_number, to_bigint, to_primitive, ToPrimitivePreferredType},
        value::{BigIntValue, Value},
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 21.2 BigInt Objects
extend_object! {
    pub struct BigIntObject {
        // The BigInt value wrapped by this object
        bigint_data: Gc<BigIntValue>,
    }
}

impl BigIntObject {
    pub fn new_from_value(cx: &mut Context, bigint_data: Gc<BigIntValue>) -> Gc<BigIntObject> {
        let proto = cx.current_realm().get_intrinsic(Intrinsic::BigIntPrototype);

        let mut object = cx.heap.alloc_uninit::<BigIntObject>();
        object_ordinary_init(cx, object.object(), ObjectKind::BigIntObject, proto);

        object.bigint_data = bigint_data;

        object
    }

    pub fn bigint_data(&self) -> Gc<BigIntValue> {
        self.bigint_data
    }
}

pub struct BigIntConstructor;

impl BigIntConstructor {
    // 21.2.1 The BigInt Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            cx.names.bigint(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            cx,
            cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::BigIntPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func
    }

    // 21.2.1.1 BigInt
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if new_target.is_some() {
            return type_error_(cx, "BigInt is not a constructor");
        }

        let value = get_argument(arguments, 0);
        let primitive = maybe!(to_primitive(cx, value, ToPrimitivePreferredType::Number));

        if primitive.is_number() {
            if !is_integral_number(primitive) {
                return range_error_(cx, "number is not an integer");
            }

            if primitive.is_smi() {
                BigIntValue::new(cx, primitive.as_smi().into()).into()
            } else {
                // TODO: This conversion is lossy
                BigIntValue::new(cx, (primitive.as_double() as u64).into()).into()
            }
        } else {
            maybe!(to_bigint(cx, value)).into()
        }
    }
}
