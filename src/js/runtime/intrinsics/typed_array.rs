use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    create_typed_array_constructor, create_typed_array_prototype, impl_gc_into,
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        error::type_error_,
        function::get_argument,
        gc::{Gc, GcDeref},
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::{get_prototype_from_constructor, ordinary_object_create, OrdinaryObject},
        property::{PrivateProperty, Property},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        realm::Realm,
        string_value::StringValue,
        type_utilities::to_index,
        value::Value,
        Context,
    },
    maybe,
};

use super::{array_buffer_constructor::ArrayBufferObject, intrinsics::Intrinsic};

pub enum ContentType {
    Number,
    BigInt,
}

/// Abstraction over typed arrays, allowing for generic access of properties.
pub trait TypedArray {
    fn array_length(&self) -> usize;

    fn byte_length(&self) -> usize;

    fn byte_offset(&self) -> usize;

    fn viewed_array_buffer(&self) -> Gc<ArrayBufferObject>;

    fn name(&self, cx: &mut Context) -> Gc<StringValue>;

    fn content_type(&self) -> ContentType;
}

impl GcDeref for dyn TypedArray {}

macro_rules! create_typed_array {
    ($typed_array:ident, $rust_name:ident, $element_type:ident, $prototype:ident, $constructor:ident) => {
        create_typed_array_constructor!(
            $typed_array,
            $rust_name,
            $element_type,
            $prototype,
            $constructor
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

create_typed_array!(Int8Array, int8_array, i8, Int8ArrayPrototype, Int8ArrayConstructor);

create_typed_array!(UInt8Array, uint8_array, u8, UInt8ArrayPrototype, UInt8ArrayConstructor);

create_typed_array!(
    UInt8ClampedArray,
    uint8_clamped_array,
    u8,
    UInt8ClampedArrayPrototype,
    UInt8ClampedArrayConstructor
);

create_typed_array!(Int16Array, int16_array, i16, Int16ArrayPrototype, Int16ArrayConstructor);

create_typed_array!(UInt16Array, uint16_array, u16, UInt16ArrayPrototype, UInt16ArrayConstructor);

create_typed_array!(Int32Array, int32_array, i32, Int32ArrayPrototype, Int32ArrayConstructor);

create_typed_array!(UInt32Array, uint32_array, u32, UInt32ArrayPrototype, UInt32ArrayConstructor);

create_typed_array!(
    BigInt64Array,
    big_int64_array,
    i64,
    BigInt64ArrayPrototype,
    BigInt64ArrayConstructor
);

create_typed_array!(
    BigUInt64Array,
    big_uint64_array,
    u64,
    BigUInt64ArrayPrototype,
    BigUInt64ArrayConstructor
);

create_typed_array!(
    Float32Array,
    float32_array,
    f32,
    Float32ArrayPrototype,
    Float32ArrayConstructor
);

create_typed_array!(
    Float64Array,
    float64_array,
    f64,
    Float64ArrayPrototype,
    Float64ArrayConstructor
);
