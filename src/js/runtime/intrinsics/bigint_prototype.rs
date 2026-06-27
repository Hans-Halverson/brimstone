use crate::{
    runtime::{
        Context, Handle, Value,
        alloc_error::AllocResult,
        error::{range_error, type_error},
        eval_result::EvalResult,
        intrinsics::{
            bigint_constructor::BigIntObject, intrinsics::Intrinsic, rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        property::Property,
        realm::Realm,
        type_utilities::to_integer_or_infinity,
        value::BigIntValue,
    },
    runtime_fn,
};

pub struct BigIntPrototype;

impl BigIntPrototype {
    /// Properties of the BigInt Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-bigint-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once BigIntConstructor has been created
        object.intrinsic_func(
            cx,
            cx.names.to_string(),
            RuntimeFunction::BigIntPrototype_to_string,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.value_of(),
            RuntimeFunction::BigIntPrototype_value_of,
            0,
            realm,
        )?;

        // BigInt.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-bigint.prototype-%symbol.tostringtag%)
        let to_string_tag_key = cx.symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.bigint().as_string().into(), false, false, true),
        )?;

        Ok(object)
    }

    runtime_fn! {
    /// BigInt.prototype.toString (https://tc39.es/ecma262/#sec-bigint.prototype.tostring)
    fn to_string(cx, this_value, arguments) {
        let bigint_value = this_bigint_value(cx, this_value, "toString")?;

        let radix = arguments.get(cx, 0);
        let radix = if radix.is_undefined() {
            10
        } else {
            let radix_int = to_integer_or_infinity(cx, radix)?;
            if !(2.0..=36.0).contains(&radix_int) {
                return range_error(
                    cx,
                    "BigInt.prototype.toString radix must be an integer between 2 and 36",
                );
            }

            radix_int as u32
        };

        Ok(cx
            .alloc_string(&bigint_value.bigint().to_str_radix(radix))?
            .as_value())
    }}

    runtime_fn! {
    /// BigInt.prototype.valueOf (https://tc39.es/ecma262/#sec-bigint.prototype.valueof)
    fn value_of(cx, this_value, _) {
        Ok(this_bigint_value(cx, this_value, "valueOf")?.into())
    }}
}

fn this_bigint_value(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<BigIntValue>> {
    if value.is_bigint() {
        return Ok(value.as_bigint());
    }

    if value.is_object() {
        let object_value = value.as_object();
        if object_value.is_bigint_object() {
            return Ok(object_value.cast::<BigIntObject>().bigint_data());
        }
    }

    type_error(cx, &format!("BigInt.prototype.{method_name} must be called on a BigInt"))
}
