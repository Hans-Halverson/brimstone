use std::mem::size_of;

use brimstone_macros::wrap_ordinary_object;
use half::f16;
use num_bigint::{BigInt, Sign};

use crate::{
    common::math::f64_to_f16,
    create_typed_array_constructor, create_typed_array_prototype, extend_object, heap_trait_object,
    runtime::{
        abstract_operations::{get, get_method, length_of_array_like, set},
        builtin_function::BuiltinFunction,
        error::{range_error, type_error},
        eval_result::EvalResult,
        function::get_argument,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::HeapItemKind,
        iterator::iter_iterator_method_values,
        object_value::{ObjectValue, VirtualObject},
        ordinary_object::{
            get_prototype_from_constructor, object_create_with_proto, ordinary_define_own_property,
            ordinary_delete, ordinary_get, ordinary_get_own_property, ordinary_has_property,
            ordinary_own_string_symbol_property_keys, ordinary_set,
        },
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        realm::Realm,
        rust_vtables::{extract_typed_array_vtable, extract_virtual_object_vtable},
        string_value::StringValue,
        type_utilities::{
            to_big_int64, to_big_uint64, to_index, to_int16, to_int32, to_int8, to_number,
            to_uint16, to_uint32, to_uint8, to_uint8_clamp,
        },
        value::{BigIntValue, Value},
        Context, Handle, HeapPtr,
    },
    set_uninit,
};

use super::{
    array_buffer_constructor::{clone_array_buffer, ArrayBufferObject},
    intrinsics::Intrinsic,
    typed_array_constructor::{canonical_numeric_index_string, CanonicalIndexType},
    typed_array_prototype::{
        is_typed_array_out_of_bounds, make_typed_array_with_buffer_witness_record,
        typed_array_length,
    },
};

#[derive(PartialEq)]
pub enum ContentType {
    Number,
    BigInt,
}

/// Abstraction over typed arrays, allowing for generic access of properties.
pub trait TypedArray {
    /// Length of the array. None represents a value of AUTO.
    fn array_length(&self) -> Option<usize>;

    /// Byte length of the array. None represents a value of AUTO.
    fn byte_length(&self) -> Option<usize>;

    fn byte_offset(&self) -> usize;

    fn viewed_array_buffer_ptr(&self) -> HeapPtr<ArrayBufferObject>;

    fn viewed_array_buffer(&self) -> Handle<ArrayBufferObject>;

    fn name(&self, cx: Context) -> Handle<StringValue>;

    fn content_type(&self) -> ContentType;

    fn kind(&self) -> TypedArrayKind;

    fn element_size(&self) -> usize;

    fn read_element_value(
        &self,
        cx: Context,
        array_buffer: HeapPtr<ArrayBufferObject>,
        byte_index: usize,
    ) -> Handle<Value>;

    /// Write the value at a particular byte index. Only valid to use on a value that will not
    /// invoke user code when converted to an array element (such as values read directly from
    /// another array).
    fn write_element_value(
        &mut self,
        cx: Context,
        byte_index: usize,
        value: Handle<Value>,
    ) -> EvalResult<()>;

    /// Write the value at a particular array index. Do not check that the index is in bounds.
    fn write_element_value_unchecked(
        &mut self,
        cx: Context,
        index: u64,
        value: Handle<Value>,
    ) -> EvalResult<()>;
}

heap_trait_object!(
    TypedArray,
    DynTypedArray,
    HeapDynTypedArray,
    into_dyn_typed_array,
    extract_typed_array_vtable
);

impl DynTypedArray {
    pub fn into_object_value(self) -> Handle<ObjectValue> {
        self.data.cast()
    }
}

#[derive(PartialEq)]
#[allow(clippy::enum_variant_names)]
pub enum TypedArrayKind {
    Int8Array,
    UInt8Array,
    UInt8ClampedArray,
    Int16Array,
    UInt16Array,
    Int32Array,
    UInt32Array,
    BigInt64Array,
    BigUInt64Array,
    Float16Array,
    Float32Array,
    Float64Array,
}

macro_rules! create_typed_array {
    ($typed_array:ident, $rust_name:ident, $element_type:ident, $content_type:expr, $prototype:ident, $constructor:ident, $to_element:ident, $from_element:ident) => {
        create_typed_array_constructor!(
            $typed_array,
            $rust_name,
            $element_type,
            $content_type,
            $prototype,
            $constructor,
            $to_element,
            $from_element
        );
        create_typed_array_prototype!(
            $typed_array,
            $rust_name,
            $element_type,
            $prototype,
            $constructor
        );
    };
}

#[inline]
pub fn to_int8_element(cx: Context, value: Handle<Value>) -> EvalResult<i8> {
    to_int8(cx, value)
}

#[inline]
pub fn from_int8_element(cx: Context, element: i8) -> Handle<Value> {
    Value::from(element).to_handle(cx)
}

create_typed_array!(
    Int8Array,
    int8_array,
    i8,
    ContentType::Number,
    Int8ArrayPrototype,
    Int8ArrayConstructor,
    to_int8_element,
    from_int8_element
);

#[inline]
pub fn to_uint8_element(cx: Context, value: Handle<Value>) -> EvalResult<u8> {
    to_uint8(cx, value)
}

#[inline]
pub fn from_uint8_element(cx: Context, element: u8) -> Handle<Value> {
    Value::from(element).to_handle(cx)
}

create_typed_array!(
    UInt8Array,
    uint8_array,
    u8,
    ContentType::Number,
    UInt8ArrayPrototype,
    UInt8ArrayConstructor,
    to_uint8_element,
    from_uint8_element
);

#[inline]
pub fn to_uint8_clamped_element(cx: Context, value: Handle<Value>) -> EvalResult<u8> {
    to_uint8_clamp(cx, value)
}

#[inline]
pub fn from_uint8_clamped_element(cx: Context, element: u8) -> Handle<Value> {
    Value::from(element).to_handle(cx)
}

create_typed_array!(
    UInt8ClampedArray,
    uint8_clamped_array,
    u8,
    ContentType::Number,
    UInt8ClampedArrayPrototype,
    UInt8ClampedArrayConstructor,
    to_uint8_clamped_element,
    from_uint8_clamped_element
);

#[inline]
pub fn to_int16_element(cx: Context, value: Handle<Value>) -> EvalResult<i16> {
    to_int16(cx, value)
}

#[inline]
pub fn from_int16_element(cx: Context, element: i16) -> Handle<Value> {
    Value::from(element).to_handle(cx)
}

create_typed_array!(
    Int16Array,
    int16_array,
    i16,
    ContentType::Number,
    Int16ArrayPrototype,
    Int16ArrayConstructor,
    to_int16_element,
    from_int16_element
);

#[inline]
pub fn to_uint16_element(cx: Context, value: Handle<Value>) -> EvalResult<u16> {
    to_uint16(cx, value)
}

#[inline]
pub fn from_uint16_element(cx: Context, element: u16) -> Handle<Value> {
    Value::from(element).to_handle(cx)
}

create_typed_array!(
    UInt16Array,
    uint16_array,
    u16,
    ContentType::Number,
    UInt16ArrayPrototype,
    UInt16ArrayConstructor,
    to_uint16_element,
    from_uint16_element
);

#[inline]
pub fn to_int32_element(cx: Context, value: Handle<Value>) -> EvalResult<i32> {
    to_int32(cx, value)
}

#[inline]
pub fn from_int32_element(cx: Context, element: i32) -> Handle<Value> {
    Value::from(element).to_handle(cx)
}

create_typed_array!(
    Int32Array,
    int32_array,
    i32,
    ContentType::Number,
    Int32ArrayPrototype,
    Int32ArrayConstructor,
    to_int32_element,
    from_int32_element
);

#[inline]
pub fn to_uint32_element(cx: Context, value: Handle<Value>) -> EvalResult<u32> {
    to_uint32(cx, value)
}

#[inline]
pub fn from_uint32_element(cx: Context, element: u32) -> Handle<Value> {
    Value::from(element).to_handle(cx)
}

create_typed_array!(
    UInt32Array,
    uint32_array,
    u32,
    ContentType::Number,
    UInt32ArrayPrototype,
    UInt32ArrayConstructor,
    to_uint32_element,
    from_uint32_element
);

#[inline]
pub fn to_big_int64_element(cx: Context, value: Handle<Value>) -> EvalResult<i64> {
    let bigint = to_big_int64(cx, value)?;

    // Guaranteed to have a single u64 component in i64 range from checks in to_big_int64
    let (sign, digits) = bigint.to_u64_digits();

    match sign {
        Sign::Plus => Ok(digits[0] as i64),
        Sign::NoSign => Ok(0),
        Sign::Minus => Ok(-(digits[0] as i64)),
    }
}

#[inline]
pub fn from_big_int64_element(cx: Context, element: i64) -> Handle<Value> {
    let bigint = BigInt::from(element);
    BigIntValue::new(cx, bigint).into()
}

create_typed_array!(
    BigInt64Array,
    big_int64_array,
    i64,
    ContentType::BigInt,
    BigInt64ArrayPrototype,
    BigInt64ArrayConstructor,
    to_big_int64_element,
    from_big_int64_element
);

#[inline]
pub fn to_big_uint64_element(cx: Context, value: Handle<Value>) -> EvalResult<u64> {
    let bigint = to_big_uint64(cx, value)?;

    // Guaranteed to have a single u64 component from checks in to_big_uint64
    let (sign, digits) = bigint.to_u64_digits();

    if sign == Sign::NoSign {
        Ok(0)
    } else {
        Ok(digits[0])
    }
}

#[inline]
pub fn from_big_uint64_element(cx: Context, element: u64) -> Handle<Value> {
    let bigint = BigInt::from(element);
    BigIntValue::new(cx, bigint).into()
}

create_typed_array!(
    BigUInt64Array,
    big_uint64_array,
    u64,
    ContentType::BigInt,
    BigUInt64ArrayPrototype,
    BigUInt64ArrayConstructor,
    to_big_uint64_element,
    from_big_uint64_element
);

#[inline]
pub fn to_float16_element(cx: Context, value: Handle<Value>) -> EvalResult<f16> {
    let number = to_number(cx, value)?;
    Ok(f64_to_f16(number.as_number()))
}

#[inline]
pub fn from_float16_element(cx: Context, element: f16) -> Handle<Value> {
    Value::from(element.to_f64()).to_handle(cx)
}

create_typed_array!(
    Float16Array,
    float16_array,
    f16,
    ContentType::Number,
    Float16ArrayPrototype,
    Float16ArrayConstructor,
    to_float16_element,
    from_float16_element
);

#[inline]
pub fn to_float32_element(cx: Context, value: Handle<Value>) -> EvalResult<f32> {
    let number = to_number(cx, value)?;
    Ok(number.as_number() as f32)
}

#[inline]
pub fn from_float32_element(cx: Context, element: f32) -> Handle<Value> {
    Value::from(element).to_handle(cx)
}

create_typed_array!(
    Float32Array,
    float32_array,
    f32,
    ContentType::Number,
    Float32ArrayPrototype,
    Float32ArrayConstructor,
    to_float32_element,
    from_float32_element
);

#[inline]
pub fn to_float64_element(cx: Context, value: Handle<Value>) -> EvalResult<f64> {
    let number = to_number(cx, value)?;
    Ok(number.as_number())
}

#[inline]
pub fn from_float64_element(cx: Context, element: f64) -> Handle<Value> {
    Value::from(element).to_handle(cx)
}

create_typed_array!(
    Float64Array,
    float64_array,
    f64,
    ContentType::Number,
    Float64ArrayPrototype,
    Float64ArrayConstructor,
    to_float64_element,
    from_float64_element
);
