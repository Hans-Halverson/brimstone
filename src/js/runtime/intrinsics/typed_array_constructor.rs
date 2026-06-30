use crate::{
    eval_err, intrinsic_methods, must,
    runtime::{
        Context, Handle, PropertyKey, Realm,
        abstract_operations::{call_object, get_method, length_of_array_like, set},
        alloc_error::AllocResult,
        error::type_error,
        eval_result::EvalResult,
        get,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            encodings::{
                decode_base64, decode_hex, get_base64_alphabet_option,
                get_base64_last_chunk_handling_option, get_base64_options_argument,
            },
            intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
            typed_array::UInt8ArrayConstructor,
            typed_array_prototype::{
                typed_array_create_from_constructor, typed_array_create_from_constructor_object,
            },
        },
        iterator::iter_iterator_method_values,
        object_value::ObjectValue,
        type_utilities::{is_callable, is_constructor_value, to_object},
        value::Value,
    },
    runtime_fn,
};

/// The %TypedArray% Intrinsic Object (https://tc39.es/ecma262/#sec-%typedarray%-intrinsic-object)
pub struct TypedArrayConstructor;

impl TypedArrayConstructor {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::TypedArrayConstructor_construct,
            0,
            cx.names.typed_array(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::TypedArrayPrototype)?;

        intrinsic_methods!(cx, builder, {
            from TypedArrayConstructor_from (1),
            of   TypedArrayConstructor_of   (0),
        });

        // get %TypedArray% [ @@species ] (https://tc39.es/ecma262/#sec-get-%typedarray%-%symbol.species%)
        builder.getter(cx.symbols.species(), RuntimeFunction::ReturnThis)?;

        builder.build()
    }

    pub fn install_uint8_array_methods(cx: Context, realm: Handle<Realm>) -> AllocResult<()> {
        let constructor = realm.get_intrinsic(Intrinsic::UInt8ArrayConstructor);
        let mut builder = IntrinsicBuilder::new(cx, realm, constructor);

        intrinsic_methods!(cx, builder, {
            from_base64 TypedArrayConstructor_from_base64 (1),
            from_hex    TypedArrayConstructor_from_hex    (1),
        });

        builder.build()?;

        Ok(())
    }

    runtime_fn! {
    /// %TypedArray% (https://tc39.es/ecma262/#sec-%typedarray%)
    fn construct(cx, _, _) {
        type_error(cx, "TypedArray constructor is abstract and cannot be called")
    }}

    runtime_fn! {
    /// %TypedArray%.from (https://tc39.es/ecma262/#sec-%typedarray%.from)
    fn from(cx, this_value, arguments) {
        if !is_constructor_value(this_value) {
            return type_error(cx, "TypedArray.from must be called on a constructor");
        }

        let this_constructor = this_value.as_object();

        let map_function = {
            let argument = arguments.get(cx, 1);
            if argument.is_undefined() {
                None
            } else if !is_callable(argument) {
                return type_error(cx, "TypedArray.from map function must be a function");
            } else {
                Some(argument.as_object())
            }
        };

        let source = arguments.get(cx, 0);
        let this_argument = arguments.get(cx, 2);

        let iterator_key = cx.symbols.iterator();
        let iterator = get_method(cx, source, iterator_key)?;

        // If source is iterable then add all values from iterator
        if let Some(iterator) = iterator {
            // Collect all values from iterator
            let mut values = vec![];
            iter_iterator_method_values(cx, source, iterator, &mut |_, value| {
                values.push(value);
                None
            })?;

            let length = values.len();

            let length_value = cx.number(length);
            let target_object = typed_array_create_from_constructor_object(
                cx,
                this_constructor,
                &[length_value],
                "TypedArray.from",
            )?;

            // Shared between iterations
            let mut index_key = PropertyKey::uninit().to_handle(cx);
            let mut index_value = Value::uninit().to_handle(cx);

            for (i, value) in values.into_iter().enumerate() {
                index_key.replace(PropertyKey::from_u64(cx, i as u64)?);

                let value = if let Some(map_function) = map_function {
                    index_value.replace(Value::number(i));
                    call_object(cx, map_function, this_argument, &[value, index_value])?
                } else {
                    value
                };

                set(cx, target_object, index_key, value, true)?;
            }

            return Ok(target_object.as_value());
        }

        // Otherwise treat source like an array and add its values
        let array_like = must!(to_object(cx, source));
        let length = length_of_array_like(cx, array_like)? as usize;

        let length_value = cx.number(length);
        let target_object = typed_array_create_from_constructor_object(
            cx,
            this_constructor,
            &[length_value],
            "TypedArray.from",
        )?;

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i as u64)?);

            let value = get(cx, array_like, index_key)?;

            let value = if let Some(map_function) = map_function {
                index_value.replace(Value::number(i));
                call_object(cx, map_function, this_argument, &[value, index_value])?
            } else {
                value
            };

            set(cx, target_object, index_key, value, true)?;
        }

        Ok(target_object.as_value())
    }}

    runtime_fn! {
    /// Uint8Array.fromBase64 (https://tc39.es/ecma262/#sec-uint8array.frombase64)
    fn from_base64(cx, _, arguments) {
        let string_arg = arguments.get(cx, 0);
        if !string_arg.is_string() {
            return type_error(cx, "Uint8Array.fromBase64 argument must be a string");
        }

        let options_arg = arguments.get(cx, 1);
        let options = get_base64_options_argument(cx, options_arg, "Uint8Array.fromBase64")?;

        let alphabet = get_base64_alphabet_option(cx, options, "Uint8Array.fromBase64")?;
        let last_chunk_handling =
            get_base64_last_chunk_handling_option(cx, options, "Uint8Array.fromBase64")?;

        let decode_result = decode_base64(
            cx,
            string_arg.as_string(),
            alphabet,
            last_chunk_handling,
            None,
            "Uint8Array.fromBase64",
        )?;
        if let Some(error) = decode_result.error {
            return eval_err!(error);
        }

        Self::new_uint8_array_from_bytes(cx, &decode_result.bytes)
    }}

    runtime_fn! {
    /// Uint8Array.fromHex (https://tc39.es/ecma262/#sec-uint8array.fromhex)
    fn from_hex(cx, _, arguments) {
        let string_arg = arguments.get(cx, 0);
        if !string_arg.is_string() {
            return type_error(cx, "Uint8Array.fromHex argument must be a string");
        }

        let decode_result = decode_hex(cx, string_arg.as_string(), None, "Uint8Array.fromHex")?;
        if let Some(error) = decode_result.error {
            return eval_err!(error);
        }

        Self::new_uint8_array_from_bytes(cx, &decode_result.bytes)
    }}

    fn new_uint8_array_from_bytes(cx: Context, bytes: &[u8]) -> EvalResult<Handle<Value>> {
        // Create an uninitialized Uint8Array to hold the bytes
        let uint8_array_constructor = cx.get_intrinsic(Intrinsic::UInt8ArrayConstructor);
        let array_value =
            UInt8ArrayConstructor::allocate_with_length(cx, uint8_array_constructor, bytes.len())?;

        // Guaranteed to be a Uint8Array
        debug_assert!(array_value.is_object() && array_value.as_object().is_typed_array());
        let array = array_value.as_object().as_typed_array();

        // Write the encoded bytes into the backing ArrayBuffer
        array
            .viewed_array_buffer_ptr()
            .data_mut()
            .copy_from_slice(bytes);

        Ok(array_value)
    }

    runtime_fn! {
    /// %TypedArray%.of (https://tc39.es/ecma262/#sec-%typedarray%.of)
    fn of(cx, this_value, arguments) {
        if !is_constructor_value(this_value) {
            return type_error(cx, "TypedArray.of must be called on a constructor");
        }

        let this_constructor = this_value.as_object();
        let length = arguments.len();
        let length_value = cx.number(length);

        let typed_array = typed_array_create_from_constructor(
            cx,
            this_constructor,
            &[length_value],
            "TypedArray.of",
        )?;
        let object = typed_array.into_object_value();

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for (i, value) in arguments.iter().enumerate() {
            key.replace(PropertyKey::from_u64(cx, i as u64)?);
            set(cx, object, key, *value, true)?;
        }

        Ok(object.as_value())
    }}
}

#[macro_export]
macro_rules! create_typed_array_constructor {
    ($typed_array:ident, $kind:ident, $rust_name:ident, $element_type:ident, $content_type:expr, $prototype:ident, $constructor:ident, $to_element:ident, $construct_fn:expr) => {
        macro_rules! element_size {
            () => {
                std::mem::size_of::<$element_type>()
            };
        }

        pub struct $constructor;

        impl $constructor {
            /// Properties of the TypedArray Constructors (https://tc39.es/ecma262/#sec-properties-of-the-typedarray-constructors)
            pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
                let mut builder = IntrinsicBuilder::constructor(
                    cx,
                    realm,
                    $construct_fn,
                    3,
                    cx.names.$rust_name(),
                    Intrinsic::TypedArrayConstructor,
                )?;

                builder.prototype(Intrinsic::$prototype)?;

                builder.frozen(cx.names.bytes_per_element(), cx.smi(element_size!() as u8))?;

                builder.build()
            }

            $crate::runtime_fn! {
            /// TypedArray (https://tc39.es/ecma262/#sec-typedarray)
            fn construct(cx, _, arguments) {
                let new_target = if let Some(new_target) = cx.current_new_target() {
                    new_target
                } else {
                    return type_error(
                        cx,
                        &format!(
                            "{} constructor must be called with new",
                            cx.names.$rust_name().format()?
                        ),
                    );
                };

                if arguments.is_empty() {
                    return Self::allocate_with_length(cx, new_target, 0);
                }

                let argument = arguments.get(cx, 0);
                if !argument.is_object() {
                    let length = to_index(cx, argument)?;
                    return Self::allocate_with_length(cx, new_target, length);
                }

                let proto = get_prototype_from_constructor(cx, new_target, Intrinsic::$prototype)?;

                let argument = argument.as_object();
                if argument.is_typed_array() {
                    return Self::initialize_typed_array_from_typed_array(
                        cx,
                        proto,
                        argument.as_typed_array(),
                    );
                } else if let Some(argument) = argument.as_opt::<ArrayBufferObject>() {
                    let byte_offset = arguments.get(cx, 1);
                    let length = arguments.get(cx, 2);

                    return Self::initialize_typed_array_from_array_buffer(
                        cx,
                        proto,
                        argument,
                        byte_offset,
                        length,
                    );
                }

                let iterator_key = cx.symbols.iterator();
                let iterator = get_method(cx, argument.into(), iterator_key)?;

                if let Some(iterator) = iterator {
                    Self::initialize_typed_array_from_list(cx, proto, argument.into(), iterator)
                } else {
                    Self::initialize_typed_array_from_array_like(cx, proto, argument)
                }
            }}

            /// AllocateTypedArray (https://tc39.es/ecma262/#sec-allocatetypedarray)
            /// AllocateTypedArrayBuffer (https://tc39.es/ecma262/#sec-allocatetypedarraybuffer)
            pub fn allocate_with_length(
                cx: Context,
                new_target: Handle<ObjectValue>,
                length: usize,
            ) -> EvalResult<Handle<Value>> {
                let proto = get_prototype_from_constructor(cx, new_target, Intrinsic::$prototype)?;

                Ok(Self::allocate_from_object_with_length(cx, proto, length)?.as_value())
            }

            #[inline]
            fn allocate_from_object_with_length(
                cx: Context,
                proto: Handle<ObjectValue>,
                length: usize,
            ) -> EvalResult<Handle<ObjectValue>> {
                let byte_length = element_size!() * length;

                let array_buffer_constructor = cx.get_intrinsic(Intrinsic::ArrayBufferConstructor);
                let array_buffer = ArrayBufferObject::new(
                    cx,
                    array_buffer_constructor,
                    byte_length,
                    /* max_byte_length */ None,
                    /* data */ None,
                )?;

                let typed_array = $typed_array::new_with_proto(
                    cx,
                    proto,
                    array_buffer,
                    Some(byte_length),
                    0,
                    Some(length),
                )?;

                Ok(typed_array)
            }

            /// InitializeTypedArrayFromTypedArray (https://tc39.es/ecma262/#sec-initializetypedarrayfromtypedarray)
            fn initialize_typed_array_from_typed_array(
                cx: Context,
                proto: Handle<ObjectValue>,
                source_typed_array: DynTypedArray,
            ) -> EvalResult<Handle<Value>> {
                let source_data = source_typed_array.viewed_array_buffer();
                let source_element_size = source_typed_array.element_size();
                let source_byte_offset = source_typed_array.byte_offset();

                let target_element_size = element_size!();

                let source_record = make_typed_array_with_buffer_witness_record(source_typed_array);
                if is_typed_array_out_of_bounds(&source_record) {
                    return type_error(
                        cx,
                        &format!(
                            "{} constructor source array is out of bounds",
                            cx.names.$rust_name().format()?
                        ),
                    );
                }

                let source_array_length = typed_array_length(&source_record);
                let byte_length = source_array_length * element_size!();

                if source_typed_array.kind() == TypedArrayKind::$kind {
                    // If arrays have the same type then directly copy array buffer
                    let data =
                        clone_array_buffer(cx, source_data, source_byte_offset, byte_length)?;

                    let typed_array = $typed_array::new_with_proto(
                        cx,
                        proto,
                        data,
                        Some(byte_length),
                        0,
                        Some(source_array_length),
                    )?;

                    Ok(typed_array.as_value())
                } else {
                    // Otherwise arrays have different type, so allocate buffer that holds the same
                    // number of elements as the source array.
                    let buffer_constructor = cx.get_intrinsic(Intrinsic::ArrayBufferConstructor);
                    let data = ArrayBufferObject::new(
                        cx,
                        buffer_constructor,
                        byte_length,
                        /* max_byte_length */ None,
                        /* data */ None,
                    )?;

                    if source_typed_array.content_type() != $content_type {
                        return type_error(
                            cx,
                            &format!(
                                "{} constructor source array must contain {}",
                                cx.names.$rust_name().format()?,
                                $content_type.format()
                            ),
                        );
                    }

                    // Copy elements one at a time from source to target array, converting types
                    let mut source_byte_index = source_byte_offset;
                    let mut target_byte_index = 0;

                    for _ in 0..source_array_length {
                        let value = source_typed_array.read_element_value(
                            cx,
                            *source_data,
                            source_byte_index,
                        )?;

                        // Convert element to target type. May allocate but may does not invoke
                        // user code.
                        let element_value = $to_element(cx, value)?;

                        $typed_array::write_element(*data, target_byte_index, element_value);

                        source_byte_index += source_element_size;
                        target_byte_index += target_element_size;
                    }

                    let typed_array = $typed_array::new_with_proto(
                        cx,
                        proto,
                        data,
                        Some(byte_length),
                        0,
                        Some(source_array_length),
                    )?;

                    Ok(typed_array.as_value())
                }
            }

            /// InitializeTypedArrayFromArrayBuffer (https://tc39.es/ecma262/#sec-initializetypedarrayfromarraybuffer)
            fn initialize_typed_array_from_array_buffer(
                cx: Context,
                proto: Handle<ObjectValue>,
                array_buffer: Handle<ArrayBufferObject>,
                byte_offset: Handle<Value>,
                length: Handle<Value>,
            ) -> EvalResult<Handle<Value>> {
                let offset = to_index(cx, byte_offset)?;
                if offset % element_size!() != 0 {
                    return range_error(
                        cx,
                        &format!(
                            "{} constructor byte offset must be a multiple of {} but found {}",
                            cx.names.$rust_name().format()?,
                            element_size!(),
                            offset
                        ),
                    );
                }

                let buffer_is_fixed_length = array_buffer.is_fixed_length();

                let mut new_length = 0;
                if !length.is_undefined() {
                    new_length = to_index(cx, length)?;
                }

                if array_buffer.is_detached() {
                    return type_error(
                        cx,
                        &format!(
                            "{} constructor source cannot be a detached array buffer",
                            cx.names.$rust_name().format()?,
                        ),
                    );
                }

                let byte_length = array_buffer.byte_length();
                let result_new_byte_length;
                let result_new_array_length;

                if length.is_undefined() && !buffer_is_fixed_length {
                    if offset > byte_length {
                        return range_error(
                            cx,
                            &format!(
                                "{} constructor byte offset must not be larger than array buffer length",
                                cx.names.$rust_name().format()?
                            ),
                        );
                    }

                    result_new_byte_length = None;
                    result_new_array_length = None;
                } else if length.is_undefined() {
                    if byte_length % element_size!() != 0 {
                        return range_error(
                            cx,
                            &format!(
                                "{} constructor source length must be a multiple of {} but found {}",
                                cx.names.$rust_name().format()?,
                                element_size!(),
                                byte_length
                            ),
                        );
                    }

                    let maybe_negative_new_byte_length = byte_length as i64 - offset as i64;

                    if maybe_negative_new_byte_length < 0 {
                        return range_error(
                            cx,
                            &format!(
                                "{} constructor byte offset must not be larger than array buffer length",
                                cx.names.$rust_name().format()?
                            ),
                        );
                    }

                    let new_byte_length = maybe_negative_new_byte_length as usize;

                    result_new_byte_length = Some(new_byte_length);
                    result_new_array_length = Some(new_byte_length / element_size!());
                } else {
                    let new_byte_length = new_length * element_size!();

                    if offset + new_byte_length as usize > byte_length {
                        return range_error(
                            cx,
                            &format!(
                                "{} constructor byte offset must not be larger than array buffer length",
                                cx.names.$rust_name().format()?
                            ),
                        );
                    }

                    result_new_byte_length = Some(new_byte_length);
                    result_new_array_length = Some(new_length);
                };

                let typed_array = $typed_array::new_with_proto(
                    cx,
                    proto,
                    array_buffer,
                    result_new_byte_length,
                    offset,
                    result_new_array_length,
                )?;

                Ok(typed_array.as_value())
            }

            /// InitializeTypedArrayFromList (https://tc39.es/ecma262/#sec-initializetypedarrayfromlist)
            fn initialize_typed_array_from_list(
                cx: Context,
                proto: Handle<ObjectValue>,
                iterable: Handle<Value>,
                iterator: Handle<ObjectValue>,
            ) -> EvalResult<Handle<Value>> {
                // Collect all values from iterator
                let mut values = vec![];
                iter_iterator_method_values(cx, iterable, iterator, &mut |_, value| {
                    values.push(value);
                    None
                })?;

                // Allocated typed array
                let length = values.len();
                let typed_array_object = Self::allocate_from_object_with_length(cx, proto, length)?;

                // Shared between iterations
                let mut key = PropertyKey::uninit().to_handle(cx);

                // Add each value from iterator into typed array
                for (i, value) in values.into_iter().enumerate() {
                    key.replace(PropertyKey::from_u64(cx, i as u64)?);
                    set(cx, typed_array_object, key, value, true)?;
                }

                Ok(typed_array_object.as_value())
            }

            /// InitializeTypedArrayFromArrayLike (https://tc39.es/ecma262/#sec-initializetypedarrayfromarraylike)
            fn initialize_typed_array_from_array_like(
                cx: Context,
                proto: Handle<ObjectValue>,
                array_like: Handle<ObjectValue>,
            ) -> EvalResult<Handle<Value>> {
                // Allocated typed array
                let length = length_of_array_like(cx, array_like)?;
                let typed_array_object =
                    Self::allocate_from_object_with_length(cx, proto, length as usize)?;

                // Shared between iterations
                let mut key = PropertyKey::uninit().to_handle(cx);

                // Add each value from array into typed array
                for i in 0..length {
                    key.replace(PropertyKey::from_u64(cx, i)?);
                    let value = get(cx, array_like, key)?;
                    set(cx, typed_array_object, key, value, true)?;
                }

                Ok(typed_array_object.as_value())
            }
        }
    };
}
