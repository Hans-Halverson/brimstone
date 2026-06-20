use std::mem::size_of;

use num_bigint::BigInt;
use num_traits::FromPrimitive;

use crate::{
    extend_object,
    runtime::{
        Context, Handle, HeapPtr, Value,
        alloc_error::AllocResult,
        builtin_function::BuiltinFunction,
        error::{range_error, type_error},
        eval_result::EvalResult,
        function::get_argument,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemKind,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction},
        object_value::ObjectValue,
        ordinary_object::object_create,
        realm::Realm,
        type_utilities::{
            ToPrimitivePreferredType, is_integral_number, to_bigint, to_index, to_primitive,
        },
        value::BigIntValue,
    },
    set_uninit,
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
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            RuntimeFunction::BigIntConstructor_construct,
            1,
            cx.names.bigint(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::BigIntPrototype).into(),
        )?;

        func.intrinsic_func(
            cx,
            cx.names.as_int_n(),
            RuntimeFunction::BigIntConstructor_as_int_n,
            2,
            realm,
        )?;
        func.intrinsic_func(
            cx,
            cx.names.as_uint_n(),
            RuntimeFunction::BigIntConstructor_as_uint_n,
            2,
            realm,
        )?;

        Ok(func)
    }

    /// BigInt (https://tc39.es/ecma262/#sec-bigint-constructor-number-value)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        if cx.current_new_target().is_some() {
            return type_error(cx, "BigInt constructor cannot be used with new");
        }

        let value = get_argument(cx, arguments, 0);
        let primitive = to_primitive(cx, value, ToPrimitivePreferredType::Number)?;

        if primitive.is_number() {
            if !is_integral_number(*primitive) {
                return range_error(cx, "BigInt constructor argument is not an integer");
            }

            if primitive.is_smi() {
                Ok(BigIntValue::new(cx, primitive.as_smi().into())?.into())
            } else {
                // Safe to unwrap since we know the primitive is finite
                let bigint = BigInt::from_f64(primitive.as_double()).unwrap();
                Ok(BigIntValue::new(cx, bigint)?.into())
            }
        } else {
            Ok(to_bigint(cx, primitive)?.into())
        }
    }

    /// BigInt.asIntN (https://tc39.es/ecma262/#sec-bigint.asintn)
    pub fn as_int_n(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let bits_arg = get_argument(cx, arguments, 0);
        let bits = to_index(cx, bits_arg)?;

        let bigint_arg = get_argument(cx, arguments, 1);
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
    }

    /// BigInt.asUintN (https://tc39.es/ecma262/#sec-bigint.asuintn)
    pub fn as_uint_n(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let bits_arg = get_argument(cx, arguments, 0);
        let bits = to_index(cx, bits_arg)?;

        let bigint_arg = get_argument(cx, arguments, 1);
        let bigint = to_bigint(cx, bigint_arg)?;

        // Applying 2^n modulus is equivalent to masking by 2^n - 1
        let mask = (BigInt::from(1) << bits) - BigInt::from(1);
        let new_bigint = bigint.bigint() & &mask;

        Ok(BigIntValue::new(cx, new_bigint)?.into())
    }
}

impl HeapItem for HeapPtr<BigIntObject> {
    fn byte_size(&self) -> usize {
        size_of::<BigIntObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.bigint_data);
    }
}
