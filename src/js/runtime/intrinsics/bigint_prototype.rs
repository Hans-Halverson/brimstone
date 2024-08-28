use crate::{
    js::runtime::{
        completion::EvalResult,
        error::{range_error, type_error},
        function::get_argument,
        object_value::ObjectValue,
        property::Property,
        realm::Realm,
        type_utilities::to_integer_or_infinity,
        value::BigIntValue,
        Context, Handle, Value,
    },
    maybe,
};

use super::{bigint_constructor::BigIntObject, intrinsics::Intrinsic};

pub struct BigIntPrototype;

impl BigIntPrototype {
    // 21.2.3 Properties of the BigInt Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once BigIntConstructor has been created
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_func(cx, cx.names.value_of(), Self::value_of, 0, realm);

        // 21.2.3.5 BigInt.prototype [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.bigint().as_string().into(), false, false, true),
        );

        object
    }

    // 21.2.3.3 BigInt.prototype.toString
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let bigint_value = maybe!(this_bigint_value(cx, this_value));

        let radix = get_argument(cx, arguments, 0);
        let radix = if radix.is_undefined() {
            10
        } else {
            let radix_int = maybe!(to_integer_or_infinity(cx, radix));
            if !(2.0..=36.0).contains(&radix_int) {
                return range_error(cx, "radix must be an integer between 2 and 36");
            }

            radix_int as u32
        };

        cx.alloc_string(&bigint_value.bigint().to_str_radix(radix))
            .into()
    }

    // 21.2.3.4 BigInt.prototype.valueOf
    pub fn value_of(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        maybe!(this_bigint_value(cx, this_value)).into()
    }
}

fn this_bigint_value(cx: Context, value: Handle<Value>) -> EvalResult<Handle<BigIntValue>> {
    if value.is_bigint() {
        return value.as_bigint().into();
    }

    if value.is_object() {
        let object_value = value.as_object();
        if object_value.is_bigint_object() {
            return object_value.cast::<BigIntObject>().bigint_data().into();
        }
    }

    type_error(cx, "value cannot be converted to BigInt")
}
