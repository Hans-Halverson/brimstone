use crate::{
    js::runtime::{
        error::{range_error_, type_error_},
        function::get_argument,
        object_value::ObjectValue,
        property::Property,
        realm::Realm,
        type_utilities::{to_bigint, to_boolean, to_index, to_number},
        Context, EvalResult, Handle, Value,
    },
    maybe,
};

use super::{
    data_view_constructor::DataViewObject,
    intrinsics::Intrinsic,
    typed_array::{
        from_big_int64_element, from_big_uint64_element, from_float32_element,
        from_float64_element, from_int16_element, from_int32_element, from_int8_element,
        from_uint16_element, from_uint32_element, from_uint8_element, to_big_int64_element,
        to_big_uint64_element, to_float32_element, to_float64_element, to_int16_element,
        to_int32_element, to_int8_element, to_uint16_element, to_uint32_element, to_uint8_element,
        ContentType,
    },
};

pub struct DataViewPrototype;

impl DataViewPrototype {
    // 25.3.4 Properties of the DataView Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once DataViewConstructor has been created
        object.intrinsic_getter(cx, cx.names.buffer(), Self::get_buffer, realm);
        object.intrinsic_getter(cx, cx.names.byte_length(), Self::get_byte_length, realm);
        object.intrinsic_getter(cx, cx.names.byte_offset(), Self::get_byte_offset, realm);
        object.intrinsic_func(cx, cx.names.get_big_int64(), Self::get_big_int64, 1, realm);
        object.intrinsic_func(cx, cx.names.get_big_uint64(), Self::get_big_uint64, 1, realm);
        object.intrinsic_func(cx, cx.names.get_float32(), Self::get_float32, 1, realm);
        object.intrinsic_func(cx, cx.names.get_float64(), Self::get_float64, 1, realm);
        object.intrinsic_func(cx, cx.names.get_int8(), Self::get_int8, 1, realm);
        object.intrinsic_func(cx, cx.names.get_int16(), Self::get_int16, 1, realm);
        object.intrinsic_func(cx, cx.names.get_int32(), Self::get_int32, 1, realm);
        object.intrinsic_func(cx, cx.names.get_uint8(), Self::get_uint8, 1, realm);
        object.intrinsic_func(cx, cx.names.get_uint16(), Self::get_uint16, 1, realm);
        object.intrinsic_func(cx, cx.names.get_uint32(), Self::get_uint32, 1, realm);
        object.intrinsic_func(cx, cx.names.set_big_int64(), Self::set_big_int64, 2, realm);
        object.intrinsic_func(cx, cx.names.set_big_uint64(), Self::set_big_uint64, 2, realm);
        object.intrinsic_func(cx, cx.names.set_float32(), Self::set_float32, 2, realm);
        object.intrinsic_func(cx, cx.names.set_float64(), Self::set_float64, 2, realm);
        object.intrinsic_func(cx, cx.names.set_int8(), Self::set_int8, 2, realm);
        object.intrinsic_func(cx, cx.names.set_int16(), Self::set_int16, 2, realm);
        object.intrinsic_func(cx, cx.names.set_int32(), Self::set_int32, 2, realm);
        object.intrinsic_func(cx, cx.names.set_uint8(), Self::set_uint8, 2, realm);
        object.intrinsic_func(cx, cx.names.set_uint16(), Self::set_uint16, 2, realm);
        object.intrinsic_func(cx, cx.names.set_uint32(), Self::set_uint32, 2, realm);

        // 25.3.4.25 DataView.prototype [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.data_view().as_string().into(), false, false, true),
        );

        object
    }

    // 25.3.4.1 get DataView.prototype.buffer
    fn get_buffer(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let data_view = maybe!(require_is_data_view(cx, this_value));
        data_view.viewed_array_buffer().into()
    }

    // 25.3.4.2 get DataView.prototype.byteLength
    fn get_byte_length(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let data_view = maybe!(require_is_data_view(cx, this_value));

        if data_view.viewed_array_buffer_ptr().is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        Value::from(data_view.byte_length()).to_handle(cx).into()
    }

    // 25.3.4.3 get DataView.prototype.byteOffset
    fn get_byte_offset(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let data_view = maybe!(require_is_data_view(cx, this_value));

        if data_view.viewed_array_buffer_ptr().is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        Value::from(data_view.byte_offset()).to_handle(cx).into()
    }

    // 25.3.4.5 DataView.prototype.getBigInt64
    fn get_big_int64(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_big_int64_element, i64::swap_bytes)
    }

    // 25.3.4.6 DataView.prototype.getBigUint64
    fn get_big_uint64(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_big_uint64_element, u64::swap_bytes)
    }

    // 25.3.4.7 DataView.prototype.getFloat32
    fn get_float32(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_float32_element, |element| {
            let bits = f32::to_bits(element);
            f32::from_bits(bits.swap_bytes())
        })
    }

    // 25.3.4.8 DataView.prototype.getFloat64
    fn get_float64(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_float64_element, |element| {
            let bits = f64::to_bits(element);
            f64::from_bits(bits.swap_bytes())
        })
    }

    // 25.3.4.9 DataView.prototype.getInt8
    fn get_int8(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_int8_element, |element| element)
    }

    // 25.3.4.10 DataView.prototype.getInt16
    fn get_int16(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_int16_element, i16::swap_bytes)
    }

    // 25.3.4.11 DataView.prototype.getInt32
    fn get_int32(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_int32_element, i32::swap_bytes)
    }

    // 25.3.4.12 DataView.prototype.getUint8
    fn get_uint8(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_uint8_element, |element| element)
    }

    // 25.3.4.13 DataView.prototype.getUint16
    fn get_uint16(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_uint16_element, u16::swap_bytes)
    }

    // 25.3.4.14 DataView.prototype.getUint32
    fn get_uint32(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_uint32_element, u32::swap_bytes)
    }

    // 25.3.4.15 DataView.prototype.setBigInt64
    fn set_big_int64(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        set_view_value(
            cx,
            this_value,
            arguments,
            ContentType::BigInt,
            to_big_int64_element,
            i64::swap_bytes,
        )
    }

    // 25.3.4.16 DataView.prototype.setBigUint64
    fn set_big_uint64(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        set_view_value(
            cx,
            this_value,
            arguments,
            ContentType::BigInt,
            to_big_uint64_element,
            u64::swap_bytes,
        )
    }

    // 25.3.4.17 DataView.prototype.setFloat32
    fn set_float32(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        set_view_value(
            cx,
            this_value,
            arguments,
            ContentType::Number,
            to_float32_element,
            |element| {
                let bits = f32::to_bits(element);
                f32::from_bits(bits.swap_bytes())
            },
        )
    }

    // 25.3.4.18 DataView.prototype.setFloat64
    fn set_float64(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        set_view_value(
            cx,
            this_value,
            arguments,
            ContentType::Number,
            to_float64_element,
            |element| {
                let bits = f64::to_bits(element);
                f64::from_bits(bits.swap_bytes())
            },
        )
    }

    // 25.3.4.19 DataView.prototype.setInt8
    fn set_int8(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        set_view_value(cx, this_value, arguments, ContentType::Number, to_int8_element, |element| {
            element
        })
    }

    // 25.3.4.20 DataView.prototype.setInt16
    fn set_int16(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        set_view_value(
            cx,
            this_value,
            arguments,
            ContentType::Number,
            to_int16_element,
            i16::swap_bytes,
        )
    }

    // 25.3.4.21 DataView.prototype.setInt32
    fn set_int32(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        set_view_value(
            cx,
            this_value,
            arguments,
            ContentType::Number,
            to_int32_element,
            i32::swap_bytes,
        )
    }

    // 25.3.4.22 DataView.prototype.setUint8
    fn set_uint8(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        set_view_value(
            cx,
            this_value,
            arguments,
            ContentType::Number,
            to_uint8_element,
            |element| element,
        )
    }

    // 25.3.4.23 DataView.prototype.setUint16
    fn set_uint16(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        set_view_value(
            cx,
            this_value,
            arguments,
            ContentType::Number,
            to_uint16_element,
            u16::swap_bytes,
        )
    }

    // 25.3.4.24 DataView.prototype.setUint32
    fn set_uint32(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        set_view_value(
            cx,
            this_value,
            arguments,
            ContentType::Number,
            to_uint32_element,
            u32::swap_bytes,
        )
    }
}

#[inline]
fn require_is_data_view(cx: Context, value: Handle<Value>) -> EvalResult<Handle<DataViewObject>> {
    if !value.is_object() {
        return type_error_(cx, "expected data view");
    }

    let object = value.as_object();
    if !object.is_data_view() {
        return type_error_(cx, "expected data view");
    }

    object.cast::<DataViewObject>().into()
}

// 25.3.1.1 GetViewValue
#[inline]
fn get_view_value<T>(
    cx: Context,
    this_value: Handle<Value>,
    arguments: &[Handle<Value>],
    from_element_fn: fn(Context, T) -> Handle<Value>,
    swap_element_bytes_fn: fn(T) -> T,
) -> EvalResult<Handle<Value>> {
    let data_view = maybe!(require_is_data_view(cx, this_value));

    let get_index_arg = get_argument(cx, arguments, 0);
    let get_index = maybe!(to_index(cx, get_index_arg));
    let is_little_endian = to_boolean(get_argument(cx, arguments, 1).get());

    // Does not allocate past this point, so safe to keep pointer to array buffer
    let mut buffer = data_view.viewed_array_buffer_ptr();

    if buffer.is_detached() {
        return type_error_(cx, "array buffer is detached");
    }

    let view_offset = data_view.byte_offset();

    let element_size = std::mem::size_of::<T>();

    if get_index + element_size > data_view.byte_length() {
        return range_error_(cx, "byte offset is too large");
    }

    let buffer_index = get_index + view_offset;

    // Read element bytes with correct endianness
    let element_bytes = unsafe { buffer.data().as_ptr().add(buffer_index).cast::<T>().read() };

    let element = if cfg!(target_endian = "little") {
        if is_little_endian {
            element_bytes
        } else {
            swap_element_bytes_fn(element_bytes)
        }
    } else {
        if is_little_endian {
            swap_element_bytes_fn(element_bytes)
        } else {
            element_bytes
        }
    };

    from_element_fn(cx, element).into()
}

// 25.3.1.2 SetViewValue
#[inline]
fn set_view_value<T>(
    cx: Context,
    this_value: Handle<Value>,
    arguments: &[Handle<Value>],
    content_type: ContentType,
    to_element_fn: fn(Context, Handle<Value>) -> EvalResult<T>,
    swap_element_bytes_fn: fn(T) -> T,
) -> EvalResult<Handle<Value>> {
    let data_view = maybe!(require_is_data_view(cx, this_value));

    let get_index_arg = get_argument(cx, arguments, 0);
    let get_index = maybe!(to_index(cx, get_index_arg));
    let is_little_endian = to_boolean(get_argument(cx, arguments, 2).get());

    let value_arg = get_argument(cx, arguments, 1);
    let value = if content_type == ContentType::BigInt {
        maybe!(to_bigint(cx, value_arg)).into()
    } else {
        maybe!(to_number(cx, value_arg))
    };

    // Does not allocate past this point, so safe to keep pointer to array buffer
    let mut buffer = data_view.viewed_array_buffer_ptr();

    if buffer.is_detached() {
        return type_error_(cx, "array buffer is detached");
    }

    let view_offset = data_view.byte_offset();

    let element_size = std::mem::size_of::<T>();
    if get_index + element_size > data_view.byte_length() {
        return range_error_(cx, "byte offset is too large");
    }

    // Convert number to bytes with correct endianness
    let element = maybe!(to_element_fn(cx, value));

    let element_bytes = if cfg!(target_endian = "little") {
        if is_little_endian {
            element
        } else {
            swap_element_bytes_fn(element)
        }
    } else {
        if is_little_endian {
            swap_element_bytes_fn(element)
        } else {
            element
        }
    };

    // Write bytes back into buffer
    let buffer_index = get_index + view_offset;

    unsafe {
        let element_ptr = buffer.data().as_mut_ptr().add(buffer_index).cast::<T>();
        element_ptr.write(element_bytes)
    }

    cx.undefined().into()
}
