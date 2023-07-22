use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::{range_error_, type_error_},
        function::get_argument,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        property::Property,
        realm::Realm,
        type_utilities::{is_integral_number, to_bigint, to_primitive, ToPrimitivePreferredType},
        value::BigIntValue,
        Context, Handle, HeapPtr, Value,
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

        object.to_handle()
    }

    pub fn bigint_data(&self) -> Handle<BigIntValue> {
        self.bigint_data.to_handle()
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
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if new_target.is_some() {
            return type_error_(cx, "BigInt is not a constructor");
        }

        let value = get_argument(cx, arguments, 0);
        let primitive = maybe!(to_primitive(cx, value, ToPrimitivePreferredType::Number));

        if primitive.is_number() {
            if !is_integral_number(primitive.get()) {
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

impl HeapObject for HeapPtr<BigIntObject> {
    fn byte_size(&self) -> usize {
        size_of::<BigIntObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.bigint_data);
    }
}
