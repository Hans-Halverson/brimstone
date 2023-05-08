use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::{range_error_, type_error_},
        function::get_argument,
        gc::{Gc, HandleValue},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        property::Property,
        realm::Realm,
        type_utilities::{is_integral_number, to_bigint, to_primitive, ToPrimitivePreferredType},
        value::BigIntValue,
        Context, Handle, HeapPtr,
    },
    maybe, set_uninit,
};

use super::intrinsics::Intrinsic;

// 21.2 BigInt Objects
extend_object! {
    pub struct BigIntObject {
        // The BigInt value wrapped by this object
        bigint_data: HeapPtr<BigIntValue>,
    }
}

impl BigIntObject {
    pub fn new_from_value(
        cx: &mut Context,
        bigint_data: Handle<BigIntValue>,
    ) -> Handle<BigIntObject> {
        let mut object =
            object_create::<BigIntObject>(cx, ObjectKind::BigIntObject, Intrinsic::BigIntPrototype);

        set_uninit!(object.bigint_data, bigint_data.get_());

        Handle::from_heap(object)
    }

    pub fn bigint_data(&self) -> Handle<BigIntValue> {
        Handle::from_heap(self.bigint_data)
    }
}

pub struct BigIntConstructor;

impl BigIntConstructor {
    // 21.2.1 The BigInt Constructor
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
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
        _: HandleValue,
        arguments: &[HandleValue],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        if new_target.is_some() {
            return type_error_(cx, "BigInt is not a constructor");
        }

        let value = get_argument(cx, arguments, 0);
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
