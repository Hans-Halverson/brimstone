use std::mem::size_of;

use num_bigint::{BigInt, Sign};
use num_traits::FromPrimitive;

use crate::{
    extend_object,
    js::runtime::{
        builtin_function::BuiltinFunction,
        error::{range_error, type_error},
        eval_result::EvalResult,
        function::get_argument,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        realm::Realm,
        type_utilities::{
            is_integral_number, to_bigint, to_index, to_primitive, ToPrimitivePreferredType,
        },
        value::BigIntValue,
        Context, Handle, HeapPtr, Value,
    },
    set_uninit,
};

use super::intrinsics::Intrinsic;

// BigInt Objects (https://tc39.es/ecma262/#sec-bigint-objects)
extend_object! {
    pub struct BigIntObject {
        // The BigInt value wrapped by this object
        bigint_data: HeapPtr<BigIntValue>,
    }
}

impl BigIntObject {
    pub fn new_from_value(cx: Context, bigint_data: Handle<BigIntValue>) -> Handle<BigIntObject> {
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
    //// The BigInt Constructor (https://tc39.es/ecma262/#sec-bigint-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.bigint(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::BigIntPrototype).into(),
        );

        func.intrinsic_func(cx, cx.names.as_int_n(), Self::as_int_n, 2, realm);
        func.intrinsic_func(cx, cx.names.as_uint_n(), Self::as_uint_n, 2, realm);

        func
    }

    /// BigInt (https://tc39.es/ecma262/#sec-bigint-constructor-number-value)
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if new_target.is_some() {
            return type_error(cx, "BigInt is not a constructor");
        }

        let value = get_argument(cx, arguments, 0);
        let primitive = to_primitive(cx, value, ToPrimitivePreferredType::Number)?;

        if primitive.is_number() {
            if !is_integral_number(primitive.get()) {
                return range_error(cx, "number is not an integer");
            }

            if primitive.is_smi() {
                Ok(BigIntValue::new(cx, primitive.as_smi().into()).into())
            } else {
                // Safe to unwrap since we know the primitive is finite
                let bigint = BigInt::from_f64(primitive.as_double()).unwrap();
                Ok(BigIntValue::new(cx, bigint).into())
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
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let bits_arg = get_argument(cx, arguments, 0);
        let bits = to_index(cx, bits_arg)?;

        let bigint_arg = get_argument(cx, arguments, 1);
        let bigint = to_bigint(cx, bigint_arg)?;

        // Convert BigInt to its representation as bits
        let mut bytes = bigint.bigint().to_signed_bytes_le();

        // Keep all the bytes that have some bits in the range to keep
        let num_full_bytes = bits / 8;
        bytes.truncate(num_full_bytes + 1);

        // Clear the unused bits in the last byte that was kept
        let num_bits_to_remove = 8 - bits % 8;
        if num_bits_to_remove == 8 {
            bytes.pop();
        } else if !bytes.is_empty() {
            // Be sure to sign extend from the new highest bit
            let last_byte = bytes.last_mut().unwrap();
            *last_byte <<= num_bits_to_remove;
            *last_byte = ((*last_byte as i8) >> num_bits_to_remove) as u8;
        }

        let new_bigint = BigInt::from_signed_bytes_le(&bytes);
        Ok(BigIntValue::new(cx, new_bigint).into())
    }

    /// BigInt.asUintN (https://tc39.es/ecma262/#sec-bigint.asuintn)
    pub fn as_uint_n(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let bits_arg = get_argument(cx, arguments, 0);
        let bits = to_index(cx, bits_arg)?;

        let bigint_arg = get_argument(cx, arguments, 1);
        let bigint = to_bigint(cx, bigint_arg)?;

        // Convert BigInt to its representation as bits
        let mut bytes = bigint.bigint().to_signed_bytes_le();

        // Keep all the bytes that have some bits in the range to keep
        let num_full_bytes = bits / 8;
        bytes.truncate(num_full_bytes + 1);

        // Clear the unused bits in the last byte that was kept
        let num_bits_to_remove = 8 - bits % 8;
        if num_bits_to_remove == 8 {
            bytes.pop();
        } else if !bytes.is_empty() {
            let last_byte = bytes.last_mut().unwrap();
            *last_byte <<= num_bits_to_remove;
            *last_byte >>= num_bits_to_remove;
        }

        let new_bigint = BigInt::from_bytes_le(Sign::Plus, &bytes);
        Ok(BigIntValue::new(cx, new_bigint).into())
    }
}

impl HeapObject for HeapPtr<BigIntObject> {
    fn byte_size(&self) -> usize {
        size_of::<BigIntObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.bigint_data);
    }
}
