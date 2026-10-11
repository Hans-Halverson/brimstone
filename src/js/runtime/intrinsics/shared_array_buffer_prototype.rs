use crate::{
    intrinsic_getter_methods, intrinsic_methods,
    runtime::{
        Context, EvalResult, Handle, Value,
        abstract_operations::{construct, species_constructor},
        alloc_error::AllocResult,
        collections::{ArrayInstance, array::ByteArray},
        error::{range_error, type_error},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{array_buffer_object::ArrayBufferObject, intrinsics::Intrinsic},
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::{resolve_relative_index_argument, to_index},
    },
    runtime_fn,
};

pub struct SharedArrayBufferPrototype;

impl SharedArrayBufferPrototype {
    /// Properties of the SharedArrayBuffer Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-sharedarraybuffer-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::new_object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once SharedArrayBufferConstructor has been created
        intrinsic_methods!(cx, builder, {
            grow  SharedArrayBufferPrototype_grow  (1),
            slice SharedArrayBufferPrototype_slice (2),
        });

        intrinsic_getter_methods!(cx, builder, {
            byte_length     SharedArrayBufferPrototype_get_byte_length,
            growable        SharedArrayBufferPrototype_get_growable,
            max_byte_length SharedArrayBufferPrototype_get_max_byte_length,
        });

        // SharedArrayBuffer.prototype [ %Symbol.toStringTag% ] (https://tc39.es/ecma262/#sec-sharedarraybuffer.prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.shared_array_buffer())?;

        builder.build()
    }

    runtime_fn! {
    /// get SharedArrayBuffer.prototype.byteLength (https://tc39.es/ecma262/#sec-get-sharedarraybuffer.prototype.bytelength)
    fn get_byte_length(cx, this_value, _) {
        let shared_array_buffer = require_shared_array_buffer(cx, this_value, "byteLength")?;

        Ok(cx.number(shared_array_buffer.byte_length()))
    }}

    runtime_fn! {
    /// get SharedArrayBuffer.prototype.growable (https://tc39.es/ecma262/#sec-get-sharedarraybuffer.prototype.growable)
    fn get_growable(cx, this_value, _) {
        let shared_array_buffer = require_shared_array_buffer(cx, this_value, "growable")?;
        Ok(cx.bool(!shared_array_buffer.is_fixed_length()))
    }}

    runtime_fn! {
    /// get SharedArrayBuffer.prototype.maxByteLength (https://tc39.es/ecma262/#sec-get-sharedarraybuffer.prototype.maxbytelength)
    fn get_max_byte_length(cx, this_value, _) {
        let shared_array_buffer = require_shared_array_buffer(cx, this_value, "maxByteLength")?;

        let max_byte_length = shared_array_buffer
            .max_byte_length()
            .unwrap_or(shared_array_buffer.byte_length());

        Ok(cx.number(max_byte_length))
    }}

    runtime_fn! {
    /// SharedArrayBuffer.prototype.grow (https://tc39.es/ecma262/#sec-sharedarraybuffer.prototype.grow)
    fn grow(cx, this_value, arguments) {
        let mut shared_array_buffer = require_shared_array_buffer(cx, this_value, "grow")?;

        let max_byte_length = if let Some(max_byte_length) = shared_array_buffer.max_byte_length() {
            max_byte_length
        } else {
            return type_error(
                cx,
                "SharedArrayBuffer.prototype.grow cannot be used on a fixed-length SharedArrayBuffer",
            );
        };

        let new_length_arg = arguments.get(cx, 0);
        let new_byte_length = to_index(cx, new_length_arg)?;

        if new_byte_length == shared_array_buffer.byte_length() {
            return Ok(cx.undefined());
        }

        if new_byte_length < shared_array_buffer.byte_length() {
            return range_error(cx, "SharedArrayBuffer.prototype.grow can only grow");
        }

        if new_byte_length > max_byte_length {
            return range_error(
                cx,
                "SharedArrayBuffer.prototype.grow new length exceeds max byte length",
            );
        }

        // TODO Reallocates the data block. Once memory is shared between threads this must grow in
        // place, since other agents may hold references to the existing data.
        // Create new data block with copy of old data at start
        let mut new_data = ByteArray::new_uninit(cx, new_byte_length)?;
        let new_slice = new_data.as_mut_slice();
        let copied_length = shared_array_buffer.byte_length();
        new_slice[..copied_length].copy_from_slice(&shared_array_buffer.data()[..copied_length]);

        // Initialize rest of array to all zeros
        new_slice[copied_length..].fill(0);

        shared_array_buffer.set_data(new_data);
        shared_array_buffer.set_byte_length(new_byte_length);

        Ok(cx.undefined())
    }}

    runtime_fn! {
    /// SharedArrayBuffer.prototype.slice (https://tc39.es/ecma262/#sec-sharedarraybuffer.prototype.slice)
    fn slice(cx, this_value, arguments) {
        let shared_array_buffer = require_shared_array_buffer(cx, this_value, "slice")?;

        let length = shared_array_buffer.byte_length() as u64;

        // Calculate the start index of the slice
        let start_arg = arguments.get(cx, 0);
        let start_index = resolve_relative_index_argument(cx, start_arg, length)?;

        // Calculate the end index of the slice
        let end_argument = arguments.get(cx, 1);
        let end_index = if !end_argument.is_undefined() {
            resolve_relative_index_argument(cx, end_argument, length)?
        } else {
            length
        };

        let new_length = end_index.saturating_sub(start_index);
        let new_length_value = cx.number(new_length);

        // Call species constructor to create new array buffer with the given length
        let constructor = species_constructor(
            cx,
            shared_array_buffer.into(),
            Intrinsic::SharedArrayBufferConstructor,
        )?;
        let new_object = construct(cx, constructor, &[new_length_value], None)?;

        // Check type of object returned from constructor
        let mut new_array_buffer = if let Some(array_buffer) =
            new_object.as_opt::<ArrayBufferObject>()
            && array_buffer.is_shared()
        {
            array_buffer
        } else {
            return type_error(
                cx,
                "SharedArrayBuffer.prototype.slice species constructor must return a SharedArrayBuffer",
            );
        };

        if new_array_buffer.ptr_eq(&shared_array_buffer) {
            return type_error(
                cx,
                "SharedArrayBuffer.prototype.slice species constructor cannot return the same SharedArrayBuffer",
            );
        } else if (new_array_buffer.byte_length() as u64) < new_length {
            return type_error(
                cx,
                "SharedArrayBuffer.prototype.slice species constructor returned a SharedArrayBuffer that is too small",
            );
        }

        if start_index < length {
            let copied_length = u64::min(new_length, length - start_index) as usize;
            let start_index = start_index as usize;

            // Copy data from original array buffer to new array buffer
            let source_slice =
                &shared_array_buffer.data()[start_index..(start_index + copied_length)];
            let dest_slice = &mut new_array_buffer.data_mut()[..copied_length];
            dest_slice.copy_from_slice(source_slice);
        }

        Ok(new_array_buffer.as_value())
    }}
}

fn require_shared_array_buffer(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<ArrayBufferObject>> {
    if let Some(shared_array_buffer) = value.as_opt::<ArrayBufferObject>()
        && shared_array_buffer.is_shared()
    {
        return Ok(shared_array_buffer);
    }

    type_error(
        cx,
        &format!("SharedArrayBuffer.prototype.{method_name} must be called on a SharedArrayBuffer"),
    )
}
