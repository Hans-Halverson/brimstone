use crate::runtime::{
    abstract_operations::{construct, species_constructor},
    alloc_error::AllocResult,
    collections::BsArray,
    error::{range_error, type_error},
    function::get_argument,
    heap_item_descriptor::HeapItemKind,
    intrinsics::{
        array_buffer_constructor::{
            array_buffer_copy_and_detach, throw_if_detached, ArrayBufferObject,
        },
        intrinsics::Intrinsic,
        rust_runtime::RuntimeFunction,
    },
    object_value::ObjectValue,
    property::Property,
    realm::Realm,
    type_utilities::{resolve_relative_index_argument, to_index},
    Context, EvalResult, Handle, Value,
};

pub struct ArrayBufferPrototype;

impl ArrayBufferPrototype {
    /// Properties of the ArrayBuffer Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-arraybuffer-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Constructor property is added once ArrayBufferConstructor has been created
        object.intrinsic_getter(
            cx,
            cx.names.byte_length(),
            RuntimeFunction::ArrayBufferPrototype_get_byte_length,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.detached(),
            RuntimeFunction::ArrayBufferPrototype_get_detached,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.max_byte_length(),
            RuntimeFunction::ArrayBufferPrototype_get_max_byte_length,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.resize(),
            RuntimeFunction::ArrayBufferPrototype_resize,
            1,
            realm,
        )?;
        object.intrinsic_getter(
            cx,
            cx.names.resizable(),
            RuntimeFunction::ArrayBufferPrototype_get_resizable,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.slice(),
            RuntimeFunction::ArrayBufferPrototype_slice,
            2,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.transfer(),
            RuntimeFunction::ArrayBufferPrototype_transfer,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.transfer_to_fixed_length(),
            RuntimeFunction::ArrayBufferPrototype_transfer_to_fixed_length,
            0,
            realm,
        )?;

        // ArrayBuffer.prototype [ %Symbol.toStringTag% ] (https://tc39.es/ecma262/#sec-arraybuffer.prototype-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.array_buffer().as_string().into(), false, false, true),
        )?;

        Ok(object)
    }

    /// get ArrayBuffer.prototype.byteLength (https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.bytelength)
    pub fn get_byte_length(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = require_array_buffer(cx, this_value, "byteLength")?;

        // Detached array buffers have byte length set to 0
        Ok(Value::from(array_buffer.byte_length()).to_handle(cx))
    }

    /// get ArrayBuffer.prototype.detached (https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.detached)
    pub fn get_detached(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = require_array_buffer(cx, this_value, "detached")?;
        Ok(cx.bool(array_buffer.is_detached()))
    }

    /// get ArrayBuffer.prototype.maxByteLength (https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.maxbytelength)
    pub fn get_max_byte_length(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = require_array_buffer(cx, this_value, "maxByteLength")?;

        // Detached array buffers have max byte length set to 0
        let max_byte_length = array_buffer
            .max_byte_length()
            .unwrap_or(array_buffer.byte_length());

        Ok(Value::from(max_byte_length).to_handle(cx))
    }

    /// get ArrayBuffer.prototype.resizable (https://tc39.es/ecma262/#sec-get-arraybuffer.prototype.resizable)
    pub fn get_resizable(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = require_array_buffer(cx, this_value, "resizable")?;
        Ok(cx.bool(!array_buffer.is_fixed_length()))
    }

    /// ArrayBuffer.prototype.resize (https://tc39.es/ecma262/#sec-arraybuffer.prototype.resize)
    pub fn resize(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let mut array_buffer = require_array_buffer(cx, this_value, "resize")?;

        let max_byte_length = if let Some(max_byte_length) = array_buffer.max_byte_length() {
            max_byte_length
        } else {
            return type_error(
                cx,
                "ArrayBuffer.prototype.resize cannot be used on a fixed-length ArrayBuffer",
            );
        };

        let new_length_arg = get_argument(cx, arguments, 0);
        let new_byte_length = to_index(cx, new_length_arg)?;

        throw_if_detached(cx, *array_buffer)?;

        if new_byte_length > max_byte_length {
            return range_error(
                cx,
                "ArrayBuffer.prototype.resize new length exceeds max byte length",
            );
        }

        // Create new data block with copy of old data at start
        let mut new_data = BsArray::<u8>::new_uninit(cx, HeapItemKind::ByteArray, new_byte_length)?;
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

        Ok(cx.undefined())
    }

    /// ArrayBuffer.prototype.slice (https://tc39.es/ecma262/#sec-arraybuffer.prototype.slice)
    pub fn slice(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = require_array_buffer(cx, this_value, "slice")?;

        throw_if_detached(cx, *array_buffer)?;

        let length = array_buffer.byte_length() as u64;

        // Calculate the start index of the slice
        let start_arg = get_argument(cx, arguments, 0);
        let start_index = resolve_relative_index_argument(cx, start_arg, length)?;

        // Calculate the end index of the slice
        let end_argument = get_argument(cx, arguments, 1);
        let end_index = if !end_argument.is_undefined() {
            resolve_relative_index_argument(cx, end_argument, length)?
        } else {
            length
        };

        let new_length = end_index.saturating_sub(start_index);
        let new_length_value = Value::from(new_length).to_handle(cx);

        // Call species constructor to create new array buffer with the given length
        let constructor =
            species_constructor(cx, array_buffer.into(), Intrinsic::ArrayBufferConstructor)?;
        let new_object = construct(cx, constructor, &[new_length_value], None)?;

        // Check type of object returned from constructor
        let mut new_array_buffer = if let Some(array_buffer) = new_object.as_array_buffer() {
            array_buffer
        } else {
            // Includes case where constructor returns a shared array buffer
            return type_error(
                cx,
                "ArrayBuffer.prototype.slice species constructor must return an ArrayBuffer",
            );
        };

        throw_if_detached(cx, *new_array_buffer)?;

        if new_array_buffer.ptr_eq(&array_buffer) {
            return type_error(
                cx,
                "ArrayBuffer.prototype.slice species constructor cannot return the same ArrayBuffer",
            );
        } else if (new_array_buffer.byte_length() as u64) < new_length {
            return type_error(
                cx,
                "ArrayBuffer.prototype.slice species constructor returned an ArrayBuffer that is too small",
            );
        }

        // Original array buffer may have become detached during previous calls
        throw_if_detached(cx, *array_buffer)?;

        // Original array buffer may have become resized during previous calls. Make sure to adjust
        // copied length accordingly.
        let current_length = array_buffer.byte_length() as u64;
        if start_index < current_length {
            let copied_length = u64::min(new_length, current_length - start_index);

            // Copy data from original array buffer to new array buffer
            unsafe {
                let source = array_buffer.data().as_ptr().add(start_index as usize);
                let target = new_array_buffer.data_mut().as_mut_ptr();

                std::ptr::copy_nonoverlapping(source, target, copied_length as usize);
            }
        }

        Ok(new_array_buffer.as_value())
    }

    /// ArrayBuffer.prototype.transfer (https://tc39.es/ecma262/#sec-arraybuffer.prototype.transfer)
    pub fn transfer(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = require_array_buffer(cx, this_value, "transfer")?;
        let new_length = get_argument(cx, arguments, 0);

        let new_array_buffer =
            array_buffer_copy_and_detach(cx, array_buffer, new_length, /* to_fixed */ false)?;

        Ok(new_array_buffer.as_value())
    }

    /// ArrayBuffer.prototype.transferToFixedLength (https://tc39.es/ecma262/#sec-arraybuffer.prototype.transfertofixedlength)
    pub fn transfer_to_fixed_length(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let array_buffer = require_array_buffer(cx, this_value, "transferToFixedLength")?;
        let new_length = get_argument(cx, arguments, 0);

        let new_array_buffer =
            array_buffer_copy_and_detach(cx, array_buffer, new_length, /* to_fixed */ true)?;

        Ok(new_array_buffer.as_value())
    }
}

fn require_array_buffer(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<ArrayBufferObject>> {
    if value.is_object() {
        let object = value.as_object();
        if let Some(array_buffer) = object.as_array_buffer() {
            return Ok(array_buffer);
        }
    }

    type_error(
        cx,
        &format!("ArrayBuffer.prototype.{method_name} must be called on an ArrayBuffer"),
    )
}
