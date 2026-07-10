use half::f16;

use crate::{
    intrinsic_getter_methods, intrinsic_methods,
    runtime::{
        Arguments, Context, EvalResult, Handle, Value,
        alloc_error::AllocResult,
        error::{range_error, type_error},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            data_view_object::DataViewObject,
            intrinsics::Intrinsic,
            typed_array::{
                ContentType, from_big_int64_element, from_big_uint64_element, from_float16_element,
                from_float32_element, from_float64_element, from_int8_element, from_int16_element,
                from_int32_element, from_uint8_element, from_uint16_element, from_uint32_element,
                to_big_int64_element, to_big_uint64_element, to_float16_element,
                to_float32_element, to_float64_element, to_int8_element, to_int16_element,
                to_int32_element, to_uint8_element, to_uint16_element, to_uint32_element,
            },
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::{to_bigint, to_boolean, to_index, to_number},
    },
    runtime_fn,
};

pub struct DataViewPrototype;

impl DataViewPrototype {
    /// Properties of the DataView Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-dataview-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::new_object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once DataViewConstructor has been created
        intrinsic_methods!(cx, builder, {
            get_big_int64  DataViewPrototype_get_big_int64  (1),
            get_big_uint64 DataViewPrototype_get_big_uint64 (1),
            get_float16    DataViewPrototype_get_float16    (1),
            get_float32    DataViewPrototype_get_float32    (1),
            get_float64    DataViewPrototype_get_float64    (1),
            get_int8       DataViewPrototype_get_int8       (1),
            get_int16      DataViewPrototype_get_int16      (1),
            get_int32      DataViewPrototype_get_int32      (1),
            get_uint8      DataViewPrototype_get_uint8      (1),
            get_uint16     DataViewPrototype_get_uint16     (1),
            get_uint32     DataViewPrototype_get_uint32     (1),
            set_big_int64  DataViewPrototype_set_big_int64  (2),
            set_big_uint64 DataViewPrototype_set_big_uint64 (2),
            set_float16    DataViewPrototype_set_float16    (2),
            set_float32    DataViewPrototype_set_float32    (2),
            set_float64    DataViewPrototype_set_float64    (2),
            set_int8       DataViewPrototype_set_int8       (2),
            set_int16      DataViewPrototype_set_int16      (2),
            set_int32      DataViewPrototype_set_int32      (2),
            set_uint8      DataViewPrototype_set_uint8      (2),
            set_uint16     DataViewPrototype_set_uint16     (2),
            set_uint32     DataViewPrototype_set_uint32     (2),
        });

        intrinsic_getter_methods!(cx, builder, {
            buffer      DataViewPrototype_get_buffer,
            byte_length DataViewPrototype_get_byte_length,
            byte_offset DataViewPrototype_get_byte_offset,
        });

        // DataView.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-dataview.prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.data_view())?;

        builder.build()
    }

    runtime_fn! {
    /// get DataView.prototype.buffer (https://tc39.es/ecma262/#sec-get-dataview.prototype.buffer)
    fn get_buffer(cx, this_value, _) {
        let data_view = this_data_view(cx, this_value, "buffer")?;
        Ok(data_view.viewed_array_buffer().as_value())
    }}

    runtime_fn! {
    /// get DataView.prototype.byteLength (https://tc39.es/ecma262/#sec-get-dataview.prototype.bytelength)
    fn get_byte_length(cx, this_value, _) {
        let data_view = this_data_view(cx, this_value, "byteLength")?;
        let data_view_record = make_data_view_with_buffer_witness_record(data_view);

        if is_view_out_of_bounds(&data_view_record) {
            return type_error(cx, "DataView.prototype.byteLength DataView is out of bounds");
        }

        let byte_length = get_view_byte_length(&data_view_record);

        Ok(cx.number(byte_length))
    }}

    runtime_fn! {
    /// get DataView.prototype.byteOffset (https://tc39.es/ecma262/#sec-get-dataview.prototype.byteoffset)
    fn get_byte_offset(cx, this_value, _) {
        let data_view = this_data_view(cx, this_value, "byteOffset")?;
        let data_view_record = make_data_view_with_buffer_witness_record(data_view);

        if is_view_out_of_bounds(&data_view_record) {
            return type_error(cx, "DataView.prototype.byteOffset DataView is out of bounds");
        }

        Ok(cx.number(data_view.byte_offset()))
    }}

    runtime_fn! {
    /// DataView.prototype.getBigInt64 (https://tc39.es/ecma262/#sec-dataview.prototype.getbigint64)
    fn get_big_int64(cx, this_value, arguments) {
        get_view_value(
            cx,
            this_value,
            arguments,
            "getBigInt64",
            from_big_int64_element,
            i64::swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.getBigUint64 (https://tc39.es/ecma262/#sec-dataview.prototype.getbiguint64)
    fn get_big_uint64(cx, this_value, arguments) {
        get_view_value(
            cx,
            this_value,
            arguments,
            "getBigUint64",
            from_big_uint64_element,
            u64::swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.getFloat16 (https://tc39.es/ecma262/#sec-dataview.prototype.getfloat16)
    fn get_float16(cx, this_value, arguments) {
        get_view_value(
            cx,
            this_value,
            arguments,
            "getFloat16",
            from_float16_element,
            f16_swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.getFloat32 (https://tc39.es/ecma262/#sec-dataview.prototype.getfloat32)
    fn get_float32(cx, this_value, arguments) {
        get_view_value(
            cx,
            this_value,
            arguments,
            "getFloat32",
            from_float32_element,
            f32_swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.getFloat64 (https://tc39.es/ecma262/#sec-dataview.prototype.getfloat64)
    fn get_float64(cx, this_value, arguments) {
        get_view_value(
            cx,
            this_value,
            arguments,
            "getFloat64",
            from_float64_element,
            f64_swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.getInt8 (https://tc39.es/ecma262/#sec-dataview.prototype.getint8)
    fn get_int8(cx, this_value, arguments) {
        get_view_value(cx, this_value, arguments, "getInt8", from_int8_element, |element| element)
    }}

    runtime_fn! {
    /// DataView.prototype.getInt16 (https://tc39.es/ecma262/#sec-dataview.prototype.getint16)
    fn get_int16(cx, this_value, arguments) {
        get_view_value(cx, this_value, arguments, "getInt16", from_int16_element, i16::swap_bytes)
    }}

    runtime_fn! {
    /// DataView.prototype.getInt32 (https://tc39.es/ecma262/#sec-dataview.prototype.getint32)
    fn get_int32(cx, this_value, arguments) {
        get_view_value(cx, this_value, arguments, "getInt32", from_int32_element, i32::swap_bytes)
    }}

    runtime_fn! {
    /// DataView.prototype.getUint8 (https://tc39.es/ecma262/#sec-dataview.prototype.getuint8)
    fn get_uint8(cx, this_value, arguments) {
        get_view_value(cx, this_value, arguments, "getUint8", from_uint8_element, |element| element)
    }}

    runtime_fn! {
    /// DataView.prototype.getUint16 (https://tc39.es/ecma262/#sec-dataview.prototype.getuint16)
    fn get_uint16(cx, this_value, arguments) {
        get_view_value(cx, this_value, arguments, "getUint16", from_uint16_element, u16::swap_bytes)
    }}

    runtime_fn! {
    /// DataView.prototype.getUint32 (https://tc39.es/ecma262/#sec-dataview.prototype.getuint32)
    fn get_uint32(cx, this_value, arguments) {
        get_view_value(cx, this_value, arguments, "getUint32", from_uint32_element, u32::swap_bytes)
    }}

    runtime_fn! {
    /// DataView.prototype.setBigInt64 (https://tc39.es/ecma262/#sec-dataview.prototype.setbigint64)
    fn set_big_int64(cx, this_value, arguments) {
        set_view_value(
            cx,
            this_value,
            arguments,
            "setBigInt64",
            ContentType::BigInt,
            to_big_int64_element,
            i64::swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.setBigUint64 (https://tc39.es/ecma262/#sec-dataview.prototype.setbiguint64)
    fn set_big_uint64(cx, this_value, arguments) {
        set_view_value(
            cx,
            this_value,
            arguments,
            "setBigUint64",
            ContentType::BigInt,
            to_big_uint64_element,
            u64::swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.setFloat16 (https://tc39.es/ecma262/#sec-dataview.prototype.setfloat16)
    fn set_float16(cx, this_value, arguments) {
        set_view_value(
            cx,
            this_value,
            arguments,
            "setFloat16",
            ContentType::Number,
            to_float16_element,
            f16_swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.setFloat32 (https://tc39.es/ecma262/#sec-dataview.prototype.setfloat32)
    fn set_float32(cx, this_value, arguments) {
        set_view_value(
            cx,
            this_value,
            arguments,
            "setFloat32",
            ContentType::Number,
            to_float32_element,
            f32_swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.setFloat64 (https://tc39.es/ecma262/#sec-dataview.prototype.setfloat64)
    fn set_float64(cx, this_value, arguments) {
        set_view_value(
            cx,
            this_value,
            arguments,
            "setFloat64",
            ContentType::Number,
            to_float64_element,
            f64_swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.setInt8 (https://tc39.es/ecma262/#sec-dataview.prototype.setint8)
    fn set_int8(cx, this_value, arguments) {
        set_view_value(
            cx,
            this_value,
            arguments,
            "setInt8",
            ContentType::Number,
            to_int8_element,
            |element| element,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.setInt16 (https://tc39.es/ecma262/#sec-dataview.prototype.setint16)
    fn set_int16(cx, this_value, arguments) {
        set_view_value(
            cx,
            this_value,
            arguments,
            "setInt16",
            ContentType::Number,
            to_int16_element,
            i16::swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.setInt32 (https://tc39.es/ecma262/#sec-dataview.prototype.setint32)
    fn set_int32(cx, this_value, arguments) {
        set_view_value(
            cx,
            this_value,
            arguments,
            "setInt32",
            ContentType::Number,
            to_int32_element,
            i32::swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.setUint8 (https://tc39.es/ecma262/#sec-dataview.prototype.setuint8)
    fn set_uint8(cx, this_value, arguments) {
        set_view_value(
            cx,
            this_value,
            arguments,
            "setUint8",
            ContentType::Number,
            to_uint8_element,
            |element| element,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.setUint16 (https://tc39.es/ecma262/#sec-dataview.prototype.setuint16)
    fn set_uint16(cx, this_value, arguments) {
        set_view_value(
            cx,
            this_value,
            arguments,
            "setUint16",
            ContentType::Number,
            to_uint16_element,
            u16::swap_bytes,
        )
    }}

    runtime_fn! {
    /// DataView.prototype.setUint32 (https://tc39.es/ecma262/#sec-dataview.prototype.setuint32)
    fn set_uint32(cx, this_value, arguments) {
        set_view_value(
            cx,
            this_value,
            arguments,
            "setUint32",
            ContentType::Number,
            to_uint32_element,
            u32::swap_bytes,
        )
    }}
}

#[inline]
fn this_data_view(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<DataViewObject>> {
    if !value.is_object() {
        return type_error(
            cx,
            &format!("DataView.prototype.{method_name} must be called on a DataView"),
        );
    }

    if let Some(data_view) = value.as_opt::<DataViewObject>() {
        return Ok(data_view);
    }

    type_error(cx, &format!("DataView.prototype.{method_name} must be called on a DataView"))
}

/// GetViewValue (https://tc39.es/ecma262/#sec-getviewvalue)
#[inline]
fn get_view_value<T>(
    cx: Context,
    this_value: Handle<Value>,
    arguments: Arguments,
    method_name: &str,
    from_element_fn: fn(Context, T) -> AllocResult<Handle<Value>>,
    swap_element_bytes_fn: fn(T) -> T,
) -> EvalResult<Handle<Value>> {
    let data_view = this_data_view(cx, this_value, method_name)?;

    let get_index_arg = arguments.get(cx, 0);
    let get_index = to_index(cx, get_index_arg)?;
    let is_little_endian = to_boolean(*arguments.get(cx, 1));

    let data_view_record = make_data_view_with_buffer_witness_record(data_view);
    if is_view_out_of_bounds(&data_view_record) {
        return type_error(
            cx,
            &format!("DataView.prototype.{method_name} DataView is out of bounds"),
        );
    }

    let view_size = get_view_byte_length(&data_view_record);
    let view_offset = data_view.byte_offset();

    let element_size = std::mem::size_of::<T>();

    if get_index + element_size > view_size {
        return range_error(
            cx,
            &format!("DataView.prototype.{method_name} byte offset is too large"),
        );
    }

    let buffer_index = get_index + view_offset;

    // Read element bytes with correct endianness
    let buffer = data_view.viewed_array_buffer_ptr();
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

    Ok(from_element_fn(cx, element)?)
}

/// SetViewValue (https://tc39.es/ecma262/#sec-setviewvalue)
#[inline]
fn set_view_value<T>(
    cx: Context,
    this_value: Handle<Value>,
    arguments: Arguments,
    method_name: &str,
    content_type: ContentType,
    to_element_fn: fn(Context, Handle<Value>) -> EvalResult<T>,
    swap_element_bytes_fn: fn(T) -> T,
) -> EvalResult<Handle<Value>> {
    let data_view = this_data_view(cx, this_value, method_name)?;

    let get_index_arg = arguments.get(cx, 0);
    let get_index = to_index(cx, get_index_arg)?;
    let is_little_endian = to_boolean(*arguments.get(cx, 2));

    let value_arg = arguments.get(cx, 1);
    let value = if content_type == ContentType::BigInt {
        to_bigint(cx, value_arg)?.into()
    } else {
        to_number(cx, value_arg)?
    };

    let data_view_record = make_data_view_with_buffer_witness_record(data_view);
    if is_view_out_of_bounds(&data_view_record) {
        return type_error(
            cx,
            &format!("DataView.prototype.{method_name} DataView is out of bounds"),
        );
    }

    let view_size = get_view_byte_length(&data_view_record);
    let view_offset = data_view.byte_offset();

    let element_size = std::mem::size_of::<T>();
    if get_index + element_size > view_size {
        return range_error(
            cx,
            &format!("DataView.prototype.{method_name} byte offset is too large"),
        );
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
        let element_ptr = buffer.data_mut().as_mut_ptr().add(buffer_index).cast::<T>();
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

#[inline]
fn f16_swap_bytes(element: f16) -> f16 {
    unsafe {
        let bits = std::mem::transmute::<f16, u16>(element);
        std::mem::transmute::<u16, f16>(bits.swap_bytes())
    }
}

#[inline]
fn f32_swap_bytes(element: f32) -> f32 {
    let bits = f32::to_bits(element);
    f32::from_bits(bits.swap_bytes())
}

#[inline]
fn f64_swap_bytes(element: f64) -> f64 {
    let bits = f64::to_bits(element);
    f64::from_bits(bits.swap_bytes())
}
