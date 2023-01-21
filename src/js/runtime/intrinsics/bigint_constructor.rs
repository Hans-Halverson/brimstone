use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        error::{range_error_, type_error_},
        function::get_argument,
        gc::{Gc, GcDeref},
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::{ordinary_object_create, OrdinaryObject},
        property::{PrivateProperty, Property},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::{is_integral_number, to_bigint, to_primitive, ToPrimitivePreferredType},
        value::{BigIntValue, Value},
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 21.2 BigInt Objects
#[repr(C)]
pub struct BigIntObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    // The BigInt value wrapped by this object
    bigint_data: Gc<BigIntValue>,
}

impl GcDeref for BigIntObject {}

impl_gc_into!(BigIntObject, ObjectValue);

impl BigIntObject {
    const VTABLE: *const () = extract_object_vtable::<BigIntObject>();

    pub fn new_from_value(cx: &mut Context, bigint_data: Gc<BigIntValue>) -> Gc<BigIntObject> {
        let proto = cx.current_realm().get_intrinsic(Intrinsic::BigIntPrototype);
        let object = ordinary_object_create(proto);

        let bigint_object = BigIntObject { _vtable: Self::VTABLE, object, bigint_data };
        cx.heap.alloc(bigint_object)
    }

    pub fn bigint_data(&self) -> Gc<BigIntValue> {
        self.bigint_data
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
impl Object for BigIntObject {
    fn is_bigint_object(&self) -> bool {
        true
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
            &cx.names.bigint(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            &cx.names.prototype(),
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
                cx.heap.alloc_bigint(primitive.as_smi().into()).into()
            } else {
                // TODO: This conversion is lossy
                cx.heap
                    .alloc_bigint((primitive.as_double() as u64).into())
                    .into()
            }
        } else {
            maybe!(to_bigint(cx, value)).into()
        }
    }
}
