use std::mem::size_of;

use num_bigint::BigInt;
use num_traits::FromPrimitive;

use crate::{
    extend_object, intrinsic_methods,
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr, Value,
        alloc_error::AllocResult,
        error::{range_error, type_error},
        eval_result::EvalResult,
        gc::{HeapItem, HeapVisitor},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction},
        object_value::ObjectValue,
        ordinary_object::object_create,
        realm::Realm,
        type_utilities::{
            ToPrimitivePreferredType, is_integral_number, to_bigint, to_index, to_primitive,
        },
        value::BigIntValue,
    },
    runtime_fn, set_uninit,
};

// BigInt Objects (https://tc39.es/ecma262/#sec-bigint-objects)
extend_object! {
    pub struct BigIntObject {
        // The BigInt value wrapped by this object
        bigint_data: HeapPtr<BigIntValue>,
    }
}

impl BigIntObject {
    pub fn new_from_value(
        cx: Context,
        bigint_data: Handle<BigIntValue>,
    ) -> AllocResult<Handle<BigIntObject>> {
        let mut object = object_create::<BigIntObject>(
            cx,
            HeapItemKind::BigIntObject,
            Intrinsic::BigIntPrototype,
        )?;

        set_uninit!(object.bigint_data, *bigint_data);

        Ok(object.to_handle())
    }

    pub fn bigint_data(&self) -> Handle<BigIntValue> {
        self.bigint_data.to_handle()
    }
}

pub struct BigIntConstructor;

impl BigIntConstructor {
    //// The BigInt Constructor (https://tc39.es/ecma262/#sec-bigint-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::BigIntConstructor_construct,
            1,
            cx.names.bigint(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::BigIntPrototype)?;

        intrinsic_methods!(cx, builder, {
            as_int_n  BigIntConstructor_as_int_n  (2),
            as_uint_n BigIntConstructor_as_uint_n (2),
        });

        builder.build()
    }

    runtime_fn! {
    /// BigInt (https://tc39.es/ecma262/#sec-bigint-constructor-number-value)
    fn construct(cx, _, arguments) {
        if cx.current_new_target().is_some() {
            return type_error(cx, "BigInt constructor cannot be used with new");
        }

        let value = arguments.get(cx, 0);
        let primitive = to_primitive(cx, value, ToPrimitivePreferredType::Number)?;

        if primitive.is_number() {
            Ok(number_to_bigint(cx, *primitive, "BigInt constructor")?.into())
        } else {
            Ok(to_bigint(cx, primitive)?.into())
        }
    }}

    runtime_fn! {
    /// BigInt.asIntN (https://tc39.es/ecma262/#sec-bigint.asintn)
    fn as_int_n(cx, _, arguments) {
        let bits_arg = arguments.get(cx, 0);
        let bits = to_index(cx, bits_arg)?;

        let bigint_arg = arguments.get(cx, 1);
        let bigint = to_bigint(cx, bigint_arg)?;

        // Applying 2^n modulus is equivalent to masking by 2^n - 1
        let modulus = BigInt::from(1) << bits;
        let mask = &modulus - BigInt::from(1);
        let unsigned_bigint = bigint.bigint() & &mask;

        // Unsigned values >= 2^(n - 1) should be negative signed values
        let signed_bigint = if bits > 0 && unsigned_bigint.bit(bits as u64 - 1) {
            unsigned_bigint - &modulus
        } else {
            unsigned_bigint
        };

        Ok(BigIntValue::new(cx, signed_bigint)?.into())
    }}

    runtime_fn! {
    /// BigInt.asUintN (https://tc39.es/ecma262/#sec-bigint.asuintn)
    fn as_uint_n(cx, _, arguments) {
        let bits_arg = arguments.get(cx, 0);
        let bits = to_index(cx, bits_arg)?;

        let bigint_arg = arguments.get(cx, 1);
        let bigint = to_bigint(cx, bigint_arg)?;

        // Applying 2^n modulus is equivalent to masking by 2^n - 1
        let mask = (BigInt::from(1) << bits) - BigInt::from(1);
        let new_bigint = bigint.bigint() & &mask;

        Ok(BigIntValue::new(cx, new_bigint)?.into())
    }}
}

/// NumberToBigInt (https://tc39.es/ecma262/#sec-numbertobigint)
pub fn number_to_bigint(
    cx: Context,
    number: Value,
    method_name: &str,
) -> EvalResult<Handle<BigIntValue>> {
    debug_assert!(number.is_number());

    if !is_integral_number(number) {
        return range_error(cx, &format!("{method_name} argument is not an integer"));
    }

    if number.is_smi() {
        Ok(BigIntValue::new(cx, number.as_smi().into())?)
    } else {
        // Safe to unwrap since we know the number is finite
        let bigint = BigInt::from_f64(number.as_double()).unwrap();
        Ok(BigIntValue::new(cx, bigint)?)
    }
}

impl HeapItem for BigIntObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<BigIntObject>()
    }

    fn visit_pointers(mut bigint_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        bigint_object.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut bigint_object.bigint_data);
    }
}
