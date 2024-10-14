use crate::js::runtime::{
    error::{range_error, type_error},
    function::get_argument,
    object_value::ObjectValue,
    property::Property,
    realm::Realm,
    type_utilities::{to_bigint, to_boolean, to_index, to_number},
    Context, EvalResult, Handle, Value,
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
    /// Properties of the DataView Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-dataview-prototype-object)
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

        // DataView.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-dataview.prototype-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.data_view().as_string().into(), false, false, true),
        );

        object
    }

    /// get DataView.prototype.buffer (https://tc39.es/ecma262/#sec-get-dataview.prototype.buffer)
    pub fn get_buffer(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let data_view = require_is_data_view(cx, this_value)?;
        Ok(data_view.viewed_array_buffer().as_value())
    }

    /// get DataView.prototype.byteLength (https://tc39.es/ecma262/#sec-get-dataview.prototype.bytelength)
    pub fn get_byte_length(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let data_view = require_is_data_view(cx, this_value)?;
        let data_view_record = make_data_view_with_buffer_witness_record(data_view);

        if is_view_out_of_bounds(&data_view_record) {
            return type_error(cx, "DataView is out of bounds");
        }

        let byte_length = get_view_byte_length(&data_view_record);

        Ok(Value::from(byte_length).to_handle(cx))
    }

    /// get DataView.prototype.byteOffset (https://tc39.es/ecma262/#sec-get-dataview.prototype.byteoffset)
    pub fn get_byte_offset(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let data_view = require_is_data_view(cx, this_value)?;
        let data_view_record = make_data_view_with_buffer_witness_record(data_view);

        if is_view_out_of_bounds(&data_view_record) {
            return type_error(cx, "DataView is out of bounds");
        }

        Ok(Value::from(data_view.byte_offset()).to_handle(cx))
    }

    /// DataView.prototype.getBigInt64 (https://tc39.es/ecma262/#sec-dataview.prototype.getbigint64)
    pub fn get_big_int64(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_big_int64_element, i64::swap_bytes)
    }

    /// DataView.prototype.getBigUint64 (https://tc39.es/ecma262/#sec-dataview.prototype.getbiguint64)
    pub fn get_big_uint64(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_big_uint64_element, u64::swap_bytes)
    }

    /// DataView.prototype.getFloat32 (https://tc39.es/ecma262/#sec-dataview.prototype.getfloat32)
    pub fn get_float32(
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

    /// DataView.prototype.getFloat64 (https://tc39.es/ecma262/#sec-dataview.prototype.getfloat64)
    pub fn get_float64(
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

    /// DataView.prototype.getInt8 (https://tc39.es/ecma262/#sec-dataview.prototype.getint8)
    pub fn get_int8(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_int8_element, |element| element)
    }

    /// DataView.prototype.getInt16 (https://tc39.es/ecma262/#sec-dataview.prototype.getint16)
    pub fn get_int16(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_int16_element, i16::swap_bytes)
    }

    /// DataView.prototype.getInt32 (https://tc39.es/ecma262/#sec-dataview.prototype.getint32)
    pub fn get_int32(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_int32_element, i32::swap_bytes)
    }

    /// DataView.prototype.getUint8 (https://tc39.es/ecma262/#sec-dataview.prototype.getuint8)
    pub fn get_uint8(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_uint8_element, |element| element)
    }

    /// DataView.prototype.getUint16 (https://tc39.es/ecma262/#sec-dataview.prototype.getuint16)
    pub fn get_uint16(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_uint16_element, u16::swap_bytes)
    }

    /// DataView.prototype.getUint32 (https://tc39.es/ecma262/#sec-dataview.prototype.getuint32)
    pub fn get_uint32(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        get_view_value(cx, this_value, arguments, from_uint32_element, u32::swap_bytes)
    }

    /// DataView.prototype.setBigInt64 (https://tc39.es/ecma262/#sec-dataview.prototype.setbigint64)
    pub fn set_big_int64(
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

    /// DataView.prototype.setBigUint64 (https://tc39.es/ecma262/#sec-dataview.prototype.setbiguint64)
    pub fn set_big_uint64(
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

    /// DataView.prototype.setFloat32 (https://tc39.es/ecma262/#sec-dataview.prototype.setfloat32)
    pub fn set_float32(
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

    /// DataView.prototype.setFloat64 (https://tc39.es/ecma262/#sec-dataview.prototype.setfloat64)
    pub fn set_float64(
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

    /// DataView.prototype.setInt8 (https://tc39.es/ecma262/#sec-dataview.prototype.setint8)
    pub fn set_int8(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        set_view_value(cx, this_value, arguments, ContentType::Number, to_int8_element, |element| {
            element
        })
    }

    /// DataView.prototype.setInt16 (https://tc39.es/ecma262/#sec-dataview.prototype.setint16)
    pub fn set_int16(
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

    /// DataView.prototype.setInt32 (https://tc39.es/ecma262/#sec-dataview.prototype.setint32)
    pub fn set_int32(
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

    /// DataView.prototype.setUint8 (https://tc39.es/ecma262/#sec-dataview.prototype.setuint8)
    pub fn set_uint8(
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

    /// DataView.prototype.setUint16 (https://tc39.es/ecma262/#sec-dataview.prototype.setuint16)
    pub fn set_uint16(
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

    /// DataView.prototype.setUint32 (https://tc39.es/ecma262/#sec-dataview.prototype.setuint32)
    pub fn set_uint32(
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
        return type_error(cx, "expected data view");
    }

    let object = value.as_object();
    if let Some(data_view) = object.as_data_view() {
        return Ok(data_view);
    }

    type_error(cx, "expected data view")
}

/// GetViewValue (https://tc39.es/ecma262/#sec-getviewvalue)
#[inline]
fn get_view_value<T>(
    cx: Context,
    this_value: Handle<Value>,
    arguments: &[Handle<Value>],
    from_element_fn: fn(Context, T) -> Handle<Value>,
    swap_element_bytes_fn: fn(T) -> T,
) -> EvalResult<Handle<Value>> {
    let data_view = require_is_data_view(cx, this_value)?;

    let get_index_arg = get_argument(cx, arguments, 0);
    let get_index = to_index(cx, get_index_arg)?;
    let is_little_endian = to_boolean(get_argument(cx, arguments, 1).get());

    let data_view_record = make_data_view_with_buffer_witness_record(data_view);
    if is_view_out_of_bounds(&data_view_record) {
        return type_error(cx, "DataView is out of bounds");
    }

    let view_size = get_view_byte_length(&data_view_record);
    let view_offset = data_view.byte_offset();

    let element_size = std::mem::size_of::<T>();

    if get_index + element_size > view_size {
        return range_error(cx, "byte offset is too large");
    }

    let buffer_index = get_index + view_offset;

    // Read element bytes with correct endianness
    let mut buffer = data_view.viewed_array_buffer_ptr();
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

    Ok(from_element_fn(cx, element))
}

/// SetViewValue (https://tc39.es/ecma262/#sec-setviewvalue)
#[inline]
fn set_view_value<T>(
    cx: Context,
    this_value: Handle<Value>,
    arguments: &[Handle<Value>],
    content_type: ContentType,
    to_element_fn: fn(Context, Handle<Value>) -> EvalResult<T>,
    swap_element_bytes_fn: fn(T) -> T,
) -> EvalResult<Handle<Value>> {
    let data_view = require_is_data_view(cx, this_value)?;

    let get_index_arg = get_argument(cx, arguments, 0);
    let get_index = to_index(cx, get_index_arg)?;
    let is_little_endian = to_boolean(get_argument(cx, arguments, 2).get());

    let value_arg = get_argument(cx, arguments, 1);
    let value = if content_type == ContentType::BigInt {
        to_bigint(cx, value_arg)?.into()
    } else {
        to_number(cx, value_arg)?
    };

    let data_view_record = make_data_view_with_buffer_witness_record(data_view);
    if is_view_out_of_bounds(&data_view_record) {
        return type_error(cx, "DataView is out of bounds");
    }

    let view_size = get_view_byte_length(&data_view_record);
    let view_offset = data_view.byte_offset();

    let element_size = std::mem::size_of::<T>();
    if get_index + element_size > view_size {
        return range_error(cx, "byte offset is too large");
    }

    // Convert number to bytes with correct endianness
    let element = to_element_fn(cx, value)?;

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
    let mut buffer = data_view.viewed_array_buffer_ptr();

    unsafe {
        let element_ptr = buffer.data().as_mut_ptr().add(buffer_index).cast::<T>();
        element_ptr.write(element_bytes)
    }

    Ok(cx.undefined())
}

pub struct DataViewWithBufferWitnessRecord {
    pub data_view: Handle<DataViewObject>,
    pub cached_buffer_byte_length: Option<usize>,
}

/// MakeDataViewWithBufferWitnessRecord (https://tc39.es/ecma262/#sec-makedataviewwithbufferwitnessrecord)
fn make_data_view_with_buffer_witness_record(
    data_view: Handle<DataViewObject>,
) -> DataViewWithBufferWitnessRecord {
    let array_buffer = data_view.viewed_array_buffer_ptr();

    let byte_length = if array_buffer.is_detached() {
        None
    } else {
        Some(array_buffer.byte_length())
    };

    DataViewWithBufferWitnessRecord { data_view, cached_buffer_byte_length: byte_length }
}

/// GetViewByteLength (https://tc39.es/ecma262/#sec-getviewbytelength)
fn get_view_byte_length(data_view_record: &DataViewWithBufferWitnessRecord) -> usize {
    let data_view = data_view_record.data_view;

    if let Some(byte_length) = data_view.byte_length() {
        byte_length
    } else {
        let buffer_byte_length = data_view_record.cached_buffer_byte_length.unwrap();
        buffer_byte_length - data_view.byte_offset()
    }
}

/// IsViewOutOfBounds (https://tc39.es/ecma262/#sec-isviewoutofbounds)
fn is_view_out_of_bounds(data_view_record: &DataViewWithBufferWitnessRecord) -> bool {
    let data_view = data_view_record.data_view;
    let buffer_byte_length = data_view_record.cached_buffer_byte_length;

    if buffer_byte_length.is_none() {
        return true;
    }
    let buffer_byte_length = buffer_byte_length.unwrap();

    let byte_offset_start = data_view.byte_offset();

    let byte_offset_end = match data_view.byte_length() {
        None => buffer_byte_length,
        Some(byte_length) => byte_offset_start + byte_length,
    };

    byte_offset_start > buffer_byte_length || byte_offset_end > buffer_byte_length
}
