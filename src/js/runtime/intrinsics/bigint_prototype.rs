use crate::{
    js::runtime::{
        completion::EvalResult,
        error::{range_error_, type_error_},
        function::get_argument,
        gc::Gc,
        object_value::{Object, ObjectValue},
        ordinary_object::OrdinaryObject,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::to_integer_or_infinity,
        value::{BigIntValue, Value},
        Context,
    },
    maybe,
};

use super::{bigint_constructor::BigIntObject, intrinsics::Intrinsic};

pub struct BigIntPrototype;

impl BigIntPrototype {
    // 21.2.3 Properties of the BigInt Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once BigIntConstructor has been created
        object.intrinsic_func(cx, &cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_func(cx, &cx.names.value_of(), Self::value_of, 0, realm);

        // 21.2.3.5 BigInt.prototype [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        object.set_property(
            &to_string_tag_key,
            Property::data(cx.names.bigint().as_string().into(), false, false, true),
        );

        object.into()
    }

    // 21.2.3.3 BigInt.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let bigint_value = maybe!(this_bigint_value(cx, this_value));

        let radix = get_argument(arguments, 0);
        let radix = if radix.is_undefined() {
            10
        } else {
            let radix_int = maybe!(to_integer_or_infinity(cx, radix));
            if radix_int < 2.0 || radix_int > 36.0 {
                return range_error_(cx, "radix must be an integer between 2 and 36");
            }

            radix_int as u32
        };

        cx.heap
            .alloc_string(bigint_value.bigint().to_str_radix(radix))
            .into()
    }

    // 21.2.3.4 BigInt.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        maybe!(this_bigint_value(cx, this_value)).into()
    }
}

fn this_bigint_value(cx: &mut Context, value: Value) -> EvalResult<Gc<BigIntValue>> {
    if value.is_bigint() {
        return value.as_bigint().into();
    }

    if value.is_object() {
        let object_value = value.as_object();
        if object_value.is_bigint_object() {
            return object_value.cast::<BigIntObject>().bigint_data().into();
        }
    }

    type_error_(cx, "value cannot be converted to BigInt")
}
