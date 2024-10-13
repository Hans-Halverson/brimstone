use crate::{
    js::runtime::{
        abstract_operations::{construct, species_constructor},
        collections::BsArray,
        error::{range_error, type_error},
        function::get_argument,
        intrinsics::array_buffer_constructor::throw_if_detached,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        property::Property,
        realm::Realm,
        type_utilities::{to_index, to_integer_or_infinity},
        Context, EvalResult, Handle, Value,
    },
    maybe,
};

use super::{
    array_buffer_constructor::{array_buffer_copy_and_detach, ArrayBufferObject},
    intrinsics::Intrinsic,
};

pub struct ArrayBufferPrototype;

impl ArrayBufferPrototype {
    /// Properties of the ArrayBuffer Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once ArrayBufferConstructor has been created
        object.intrinsic_getter(cx, cx.names.byte_length(), Self::get_byte_length, realm);
        object.intrinsic_getter(cx, cx.names.detached(), Self::get_detached, realm);
        object.intrinsic_getter(cx, cx.names.max_byte_length(), Self::get_max_byte_length, realm);
        object.intrinsic_func(cx, cx.names.resize(), Self::resize, 1, realm);
        object.intrinsic_getter(cx, cx.names.resizable(), Self::get_resizable, realm);
        object.intrinsic_func(cx, cx.names.slice(), Self::slice, 2, realm);
        object.intrinsic_func(cx, cx.names.transfer(), Self::transfer, 0, realm);
        object.intrinsic_func(
            cx,
            cx.names.transfer_to_fixed_length(),
            Self::transfer_to_fixed_length,
            0,
            realm,
        );

        // ArrayBuffer.prototype [ %Symbol.toStringTag% ] (https://tc39.es/ecma262/#sec-arraybuffer.prototype-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.array_buffer().as_string().into(), false, false, true),
        );

        object
    }

    /// get ArrayBuffer.prototype.byteLength (https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.bytelength)
    pub fn get_byte_length(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = maybe!(require_array_buffer(cx, this_value, "byteLength"));

        // Detached array buffers have byte length set to 0
        Value::from(array_buffer.byte_length()).to_handle(cx).into()
    }

    /// get ArrayBuffer.prototype.detached (https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.detached)
    pub fn get_detached(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = maybe!(require_array_buffer(cx, this_value, "detached"));
        cx.bool(array_buffer.is_detached()).into()
    }

    /// get ArrayBuffer.prototype.maxByteLength (https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.maxbytelength)
    pub fn get_max_byte_length(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = maybe!(require_array_buffer(cx, this_value, "maxByteLength"));

        // Detached array buffers have max byte length set to 0
        let max_byte_length = array_buffer
            .max_byte_length()
            .unwrap_or(array_buffer.byte_length());

        Value::from(max_byte_length).to_handle(cx).into()
    }

    /// get ArrayBuffer.prototype.resizable (https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.resizable)
    pub fn get_resizable(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = maybe!(require_array_buffer(cx, this_value, "resizable"));
        cx.bool(!array_buffer.is_fixed_length()).into()
    }

    /// ArrayBuffer.prototype.resize (https://tc39.es/ecma262/#sec-arraybuffer.prototype.resize)
    pub fn resize(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let mut array_buffer = maybe!(require_array_buffer(cx, this_value, "resize"));

        let max_byte_length = if let Some(max_byte_length) = array_buffer.max_byte_length() {
            max_byte_length
        } else {
            return type_error(cx, "array buffer is not resizable");
        };

        let new_length_arg = get_argument(cx, arguments, 0);
        let new_byte_length = maybe!(to_index(cx, new_length_arg));

        maybe!(throw_if_detached(cx, array_buffer.get_()));

        if new_byte_length > max_byte_length {
            return range_error(cx, "new length exceeds max byte length");
        }

        // Create new data block with copy of old data at start
        let mut new_data = BsArray::<u8>::new_uninit(cx, ObjectKind::ByteArray, new_byte_length);
        let old_byte_length = array_buffer.byte_length();

        unsafe {
            std::ptr::copy(
                array_buffer.data().as_ptr(),
                new_data.as_mut_slice().as_mut_ptr(),
                old_byte_length.min(new_byte_length),
            )
        }

        // Initialize rest of array to all zeros
        if new_byte_length > old_byte_length {
            unsafe {
                std::ptr::write_bytes(
                    new_data.as_mut_slice().as_mut_ptr().add(old_byte_length),
                    0,
                    new_byte_length - old_byte_length,
                )
            }
        }

        array_buffer.set_data(new_data);
        array_buffer.set_byte_length(new_byte_length);

        cx.undefined().into()
    }

    /// ArrayBuffer.prototype.slice (https://tc39.es/ecma262/#sec-arraybuffer.prototype.slice)
    pub fn slice(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let mut array_buffer = maybe!(require_array_buffer(cx, this_value, "slice"));

        maybe!(throw_if_detached(cx, array_buffer.get_()));

        let length = array_buffer.byte_length() as u64;

        // Calculate the start index of the slice
        let start_arg = get_argument(cx, arguments, 0);
        let relative_start = maybe!(to_integer_or_infinity(cx, start_arg));
        let start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_start as i64, 0) as u64
            }
        } else {
            u64::min(relative_start as u64, length)
        };

        // Calculate the end index of the slice
        let end_argument = get_argument(cx, arguments, 1);
        let end_index = if !end_argument.is_undefined() {
            let relative_end = maybe!(to_integer_or_infinity(cx, end_argument));

            if relative_end < 0.0 {
                if relative_end == f64::NEG_INFINITY {
                    0
                } else {
                    i64::max(length as i64 + relative_end as i64, 0) as u64
                }
            } else {
                u64::min(relative_end as u64, length)
            }
        } else {
            length
        };

        let new_length = end_index.saturating_sub(start_index);
        let new_length_value = Value::from(new_length).to_handle(cx);

        // Call species constructor to create new array buffer with the given length
        let constructor =
            maybe!(species_constructor(cx, array_buffer.into(), Intrinsic::ArrayBufferConstructor));
        let new_object = maybe!(construct(cx, constructor, &[new_length_value], None));

        // Check type of object returned from constructor
        let mut new_array_buffer = if new_object.is_array_buffer() {
            new_object.cast::<ArrayBufferObject>()
        } else if new_object.is_shared_array_buffer() {
            return type_error(cx, "constructor cannot return SharedArrayBuffer");
        } else {
            return type_error(cx, "expected array buffer");
        };

        maybe!(throw_if_detached(cx, new_array_buffer.get_()));

        if new_array_buffer.ptr_eq(&array_buffer) {
            return type_error(cx, "constructor cannot return same array buffer");
        } else if (new_array_buffer.byte_length() as u64) < new_length {
            return type_error(cx, "new array buffer is too small");
        }

        // Original array buffer may have become detached during previous calls
        maybe!(throw_if_detached(cx, array_buffer.get_()));

        // Copy data from original array buffer to new array buffer
        unsafe {
            let source = array_buffer.data().as_ptr().add(start_index as usize);
            let target = new_array_buffer.data().as_mut_ptr();

            std::ptr::copy_nonoverlapping(source, target, new_length as usize)
        }

        new_array_buffer.into()
    }

    /// ArrayBuffer.prototype.transfer (https://tc39.es/ecma262/#sec-arraybuffer.prototype.transfer)
    pub fn transfer(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = maybe!(require_array_buffer(cx, this_value, "transfer"));
        let new_length = get_argument(cx, arguments, 0);

        let new_array_buffer = maybe!(array_buffer_copy_and_detach(
            cx,
            array_buffer,
            new_length,
            /* to_fixed */ false
        ));

        new_array_buffer.into()
    }

    /// ArrayBuffer.prototype.transferToFixedLength (https://tc39.es/ecma262/#sec-arraybuffer.prototype.transfertofixedlength)
    pub fn transfer_to_fixed_length(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = maybe!(require_array_buffer(cx, this_value, "transferToFixedLength"));
        let new_length = get_argument(cx, arguments, 0);

        let new_array_buffer = maybe!(array_buffer_copy_and_detach(
            cx,
            array_buffer,
            new_length,
            /* to_fixed */ true
        ));

        new_array_buffer.into()
    }
}

fn require_array_buffer(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<ArrayBufferObject>> {
    if value.is_object() {
        let object = value.as_object();
        if object.is_array_buffer() {
            return object.cast::<ArrayBufferObject>().into();
        }
    }

    type_error(cx, &format!("ArrayBuffer.prototype.{} expected ArrayBuffer", method_name))
}
