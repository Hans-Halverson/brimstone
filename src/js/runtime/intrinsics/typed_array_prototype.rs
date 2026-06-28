use std::cmp::Ordering;

use num_bigint::{BigUint, Sign};

use crate::{
    eval_err, intrinsic_getter_methods, intrinsic_methods, must,
    runtime::{
        Context, EvalResult, Handle, PropertyKey, Realm, Value,
        abstract_operations::{
            call_object, construct, create_data_property_or_throw, has_property, invoke,
            length_of_array_like, set, species_constructor,
        },
        alloc_error::AllocResult,
        error::{range_error, type_error},
        get,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            array_buffer_constructor::clone_array_buffer,
            array_iterator::{ArrayIterator, ArrayIteratorKind},
            array_prototype::{
                INCLUDE_HOLES, TYPED_ARRAY, find_via_predicate, sort_indexed_properties,
            },
            encodings::{
                DecodeResult, decode_base64, decode_hex, encode_base64, encode_hex,
                get_base64_alphabet_option, get_base64_last_chunk_handling_option,
                get_base64_omit_padding_option, get_base64_options_argument,
            },
            intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
            typed_array::{ContentType, DynTypedArray, TypedArrayKind},
        },
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create,
        string_value::{FlatString, StringValue},
        to_string,
        type_utilities::{
            is_callable, is_strictly_equal, resolve_relative_index_argument, same_object_value,
            same_value_zero, to_bigint, to_boolean, to_integer_or_infinity, to_number, to_object,
        },
    },
    runtime_fn,
};

pub struct TypedArrayPrototype;

impl TypedArrayPrototype {
    /// Properties of the %TypedArray% Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-%typedarrayprototype%-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once TypedArrayConstructor has been created
        intrinsic_methods!(cx, builder, {
            at               TypedArrayPrototype_at               (1),
            copy_within      TypedArrayPrototype_copy_within      (2),
            entries          TypedArrayPrototype_entries          (0),
            every            TypedArrayPrototype_every            (1),
            fill             TypedArrayPrototype_fill             (1),
            filter           TypedArrayPrototype_filter           (1),
            find             TypedArrayPrototype_find             (1),
            find_index       TypedArrayPrototype_find_index       (1),
            find_last        TypedArrayPrototype_find_last        (1),
            find_last_index  TypedArrayPrototype_find_last_index  (1),
            for_each         TypedArrayPrototype_for_each         (1),
            includes         TypedArrayPrototype_includes         (1),
            index_of         TypedArrayPrototype_index_of         (1),
            join             TypedArrayPrototype_join             (1),
            keys             TypedArrayPrototype_keys             (0),
            last_index_of    TypedArrayPrototype_last_index_of    (1),
            map_             TypedArrayPrototype_map              (1),
            reduce           TypedArrayPrototype_reduce           (1),
            reduce_right     TypedArrayPrototype_reduce_right     (1),
            reverse          TypedArrayPrototype_reverse          (0),
            set_             TypedArrayPrototype_set              (1),
            slice            TypedArrayPrototype_slice            (2),
            some             TypedArrayPrototype_some             (1),
            sort             TypedArrayPrototype_sort             (1),
            subarray         TypedArrayPrototype_subarray         (2),
            to_locale_string TypedArrayPrototype_to_locale_string (0),
            to_reversed      TypedArrayPrototype_to_reversed      (0),
            to_sorted        TypedArrayPrototype_to_sorted        (1),
            values           TypedArrayPrototype_values           (0),
            with             TypedArrayPrototype_with             (2),
        });

        intrinsic_getter_methods!(cx, builder, {
            buffer      TypedArrayPrototype_buffer,
            byte_length TypedArrayPrototype_byte_length,
            byte_offset TypedArrayPrototype_byte_offset,
            length      TypedArrayPrototype_length,
        });

        // Use Array.prototype.toString directly
        builder.data(
            cx.names.to_string(),
            realm
                .get_intrinsic(Intrinsic::ArrayPrototypeToString)
                .into(),
        )?;

        // %TypedArray%.prototype [ @@iterator ] (https://tc39.es/ecma262/#sec-%typedarray%.prototype-%symbol.iterator%)
        builder.alias(cx.names.values(), cx.symbols.iterator())?;

        // get %TypedArray%.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-get-%typedarray%.prototype-%symbol.tostringtag%)
        builder.getter(
            cx.symbols.to_string_tag(),
            RuntimeFunction::TypedArrayPrototype_get_to_string_tag,
        )?;

        builder.build()
    }

    pub fn install_uint8_array_methods(cx: Context, realm: Handle<Realm>) -> AllocResult<()> {
        let prototype = realm.get_intrinsic(Intrinsic::UInt8ArrayPrototype);
        let mut builder = IntrinsicBuilder::new(cx, realm, prototype);

        intrinsic_methods!(cx, builder, {
            to_base64       TypedArrayPrototype_to_base64       (0),
            to_hex          TypedArrayPrototype_to_hex          (0),
            set_from_base64 TypedArrayPrototype_set_from_base64 (1),
            set_from_hex    TypedArrayPrototype_set_from_hex    (1),
        });

        builder.build()?;

        Ok(())
    }

    runtime_fn! {
    /// %TypedArray%.prototype.at (https://tc39.es/ecma262/#sec-%typedarray%.prototype.at)
    fn at(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "at")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let index_arg = arguments.get(cx, 0);
        let relative_index = to_integer_or_infinity(cx, index_arg)?;

        let key = if relative_index >= 0.0 {
            if relative_index >= length as f64 {
                return Ok(cx.undefined());
            }

            PropertyKey::from_u64_handle(cx, relative_index as u64)?
        } else {
            if -relative_index > length as f64 {
                return Ok(cx.undefined());
            }

            PropertyKey::from_u64_handle(cx, (length as i64 + relative_index as i64) as u64)?
        };

        get(cx, object, key)
    }}

    runtime_fn! {
    /// get %TypedArray%.prototype.buffer (https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.buffer)
    fn buffer(cx, this_value, _) {
        let typed_array = this_typed_array(cx, this_value, "buffer")?;
        Ok(typed_array.viewed_array_buffer().as_value())
    }}

    runtime_fn! {
    /// get %TypedArray%.prototype.byteLength (https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.bytelength)
    fn byte_length(cx, this_value, _) {
        let typed_array = this_typed_array(cx, this_value, "byteLength")?;

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return Ok(cx.zero());
        }

        let byte_length = typed_array_byte_length(&typed_array_record);

        Ok(cx.number(byte_length))
    }}

    runtime_fn! {
    /// get %TypedArray%.prototype.byteOffset (https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.byteoffset)
    fn byte_offset(cx, this_value, _) {
        let typed_array = this_typed_array(cx, this_value, "byteOffset")?;

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return Ok(cx.zero());
        }

        Ok(cx.number(typed_array.byte_offset()))
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.copyWithin (https://tc39.es/ecma262/#sec-%typedarray%.prototype.copywithin)
    fn copy_within(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "copyWithin")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let target_arg = arguments.get(cx, 0);
        let to_index = resolve_relative_index_argument(cx, target_arg, length)?;

        let start_arg = arguments.get(cx, 1);
        let from_start_index = resolve_relative_index_argument(cx, start_arg, length)?;

        let end_argument = arguments.get(cx, 2);
        let from_end_index = if !end_argument.is_undefined() {
            resolve_relative_index_argument(cx, end_argument, length)?
        } else {
            length
        };

        let count = i64::min(
            from_end_index as i64 - from_start_index as i64,
            length as i64 - to_index as i64,
        );
        if count <= 0 {
            return Ok(object.as_value());
        }

        let byte_offset = typed_array.byte_offset() as u64;
        let element_size = typed_array.element_size() as u64;

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return type_error(cx, "TypedArray.prototype.copyWithin typed array is out of bounds");
        }

        let length = typed_array_length(&typed_array_record) as u64;
        let count = (count as u64)
            .min(length - to_index)
            .min(length - from_start_index);

        let buffer_byte_limit = length * element_size + byte_offset;

        let to_byte_index = to_index * element_size + byte_offset;
        let from_byte_index = from_start_index * element_size + byte_offset;
        let mut count_bytes = count as u64 * element_size;

        let data_ptr = typed_array
            .viewed_array_buffer_ptr()
            .data_mut()
            .as_mut_ptr();

        // Copy bytes one at a time from from_ptr to to_ptr
        unsafe {
            if from_byte_index < to_byte_index && to_byte_index < from_byte_index + count_bytes {
                let from_byte_index = from_byte_index + count_bytes - 1;
                let to_byte_index = to_byte_index + count_bytes - 1;

                let mut from_ptr = data_ptr.add(from_byte_index as usize);
                let mut to_ptr = data_ptr.add(to_byte_index as usize);

                // Copy backwards, so only need to check against `buffer_byte_limit` once at start
                // of loop.
                if from_byte_index < buffer_byte_limit && to_byte_index < buffer_byte_limit {
                    while count_bytes > 0 {
                        let byte = from_ptr.read();
                        to_ptr.write(byte);

                        from_ptr = from_ptr.sub(1);
                        to_ptr = to_ptr.sub(1);
                        count_bytes -= 1;
                    }
                }
            } else {
                let mut from_ptr = data_ptr.add(from_byte_index as usize);
                let mut to_ptr = data_ptr.add(to_byte_index as usize);

                // Calculate the number of bytes that should be left if the from or to byte indices
                // reach the buffer byte limit.
                let count_bytes_left = i64::min(
                    buffer_byte_limit as i64 - from_byte_index as i64,
                    buffer_byte_limit as i64 - to_byte_index as i64,
                )
                .max(0) as u64;

                // We can only copy bytes up until the buffer byte limit is reached.
                count_bytes = count_bytes.min(count_bytes_left);

                while count_bytes > 0 {
                    let byte = from_ptr.read();
                    to_ptr.write(byte);

                    from_ptr = from_ptr.add(1);
                    to_ptr = to_ptr.add(1);
                    count_bytes -= 1;
                }
            }
        }

        Ok(object.as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.entries (https://tc39.es/ecma262/#sec-%typedarray%.prototype.entries)
    fn entries(cx, this_value, _) {
        let typed_array_record = this_typed_array_record(cx, this_value, "entries")?;
        let typed_array_object = typed_array_record.typed_array.into_object_value();

        Ok(ArrayIterator::new(cx, typed_array_object, ArrayIteratorKind::KeyAndValue)?.as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.every (https://tc39.es/ecma262/#sec-%typedarray%.prototype.every)
    fn every(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "every")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let callback_function = arguments.get(cx, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "TypedArray.prototype.every callback must be a function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = arguments.get(cx, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i)?);
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::number(i));
            let arguments = [value, index_value, object.into()];

            let test_result = call_object(cx, callback_function, this_arg, &arguments)?;
            if !to_boolean(*test_result) {
                return Ok(cx.bool(false));
            }
        }

        Ok(cx.bool(true))
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.fill (https://tc39.es/ecma262/#sec-%typedarray%.prototype.fill)
    fn fill(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "fill")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let value_arg = arguments.get(cx, 0);
        let value = match typed_array.content_type() {
            ContentType::Number => to_number(cx, value_arg)?,
            ContentType::BigInt => to_bigint(cx, value_arg)?.into(),
        };

        let start_arg = arguments.get(cx, 1);
        let start_index = resolve_relative_index_argument(cx, start_arg, length)?;

        let end_argument = arguments.get(cx, 2);
        let end_index = if !end_argument.is_undefined() {
            resolve_relative_index_argument(cx, end_argument, length)?
        } else {
            length
        };

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return type_error(cx, "TypedArray.prototype.fill typed array is out of bounds");
        }

        let end_index = u64::min(end_index, typed_array_length(&typed_array_record) as u64);

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..end_index {
            key.replace(PropertyKey::from_u64(cx, i)?);
            must!(set(cx, object, key, value, true));
        }

        Ok(object.as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.filter (https://tc39.es/ecma262/#sec-%typedarray%.prototype.filter)
    fn filter(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "filter")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let callback_function = arguments.get(cx, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "TypedArray.prototype.filter callback must be a function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = arguments.get(cx, 1);

        let mut kept_values = vec![];

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        // First collect all values that pass the predicate
        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i)?);
            let value = get(cx, object, index_key)?;

            index_value.replace(Value::number(i));
            let arguments = [value, index_value, object.into()];

            let is_selected = call_object(cx, callback_function, this_arg, &arguments)?;

            if to_boolean(*is_selected) {
                kept_values.push(value)
            }
        }

        // Then create a new array that contains the kept values
        let num_kept_values = kept_values.len();
        let num_kept_values_value = cx.number(num_kept_values);
        let array =
            typed_array_species_create_object(cx, typed_array, &[num_kept_values_value], "filter")?;

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);

        for (i, value) in kept_values.into_iter().enumerate() {
            index_key.replace(PropertyKey::from_u64(cx, i as u64)?);
            must!(set(cx, array, index_key, value, true));
        }

        Ok(array.as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.find (https://tc39.es/ecma262/#sec-%typedarray%.prototype.find)
    fn find(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "find")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let predicate_function = arguments.get(cx, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "TypedArray.prototype.find callback must be a function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = arguments.get(cx, 1);

        let find_result = find_via_predicate(cx, object, 0..length, predicate_function, this_arg)?;

        match find_result {
            Some((value, _)) => Ok(value),
            None => Ok(cx.undefined()),
        }
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.findIndex (https://tc39.es/ecma262/#sec-%typedarray%.prototype.findindex)
    fn find_index(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "findIndex")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let predicate_function = arguments.get(cx, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "TypedArray.prototype.findIndex callback must be a function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = arguments.get(cx, 1);

        let find_result = find_via_predicate(cx, object, 0..length, predicate_function, this_arg)?;

        match find_result {
            Some((_, index_value)) => Ok(index_value),
            None => Ok(cx.negative_one()),
        }
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.findLast (https://tc39.es/ecma262/#sec-%typedarray%.prototype.findlast)
    fn find_last(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "findLast")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let predicate_function = arguments.get(cx, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "TypedArray.prototype.findLast callback must be a function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = arguments.get(cx, 1);

        let find_result =
            find_via_predicate(cx, object, (0..length).rev(), predicate_function, this_arg)?;

        match find_result {
            Some((value, _)) => Ok(value),
            None => Ok(cx.undefined()),
        }
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.findLastIndex (https://tc39.es/ecma262/#sec-%typedarray%.prototype.findlastindex)
    fn find_last_index(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "findLastIndex")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let predicate_function = arguments.get(cx, 0);
        if !is_callable(predicate_function) {
            return type_error(
                cx,
                "TypedArray.prototype.findLastIndex callback must be a function",
            );
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = arguments.get(cx, 1);

        let find_result =
            find_via_predicate(cx, object, (0..length).rev(), predicate_function, this_arg)?;

        match find_result {
            Some((_, index_value)) => Ok(index_value),
            None => Ok(cx.negative_one()),
        }
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.forEach (https://tc39.es/ecma262/#sec-%typedarray%.prototype.foreach)
    fn for_each(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "forEach")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let callback_function = arguments.get(cx, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "TypedArray.prototype.forEach callback must be a function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = arguments.get(cx, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i)?);
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::number(i));
            let arguments = [value, index_value, object.into()];

            call_object(cx, callback_function, this_arg, &arguments)?;
        }

        Ok(cx.undefined())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.includes (https://tc39.es/ecma262/#sec-%typedarray%.prototype.includes)
    fn includes(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "includes")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        if length == 0 {
            return Ok(cx.bool(false));
        }

        let search_element = arguments.get(cx, 0);

        let n_arg = arguments.get(cx, 1);
        let start_index = resolve_relative_index_argument(cx, n_arg, length)?;

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..length {
            key.replace(PropertyKey::from_u64(cx, i)?);
            let element = must!(get(cx, object, key));

            if same_value_zero(search_element, element)? {
                return Ok(cx.bool(true));
            }
        }

        Ok(cx.bool(false))
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.indexOf (https://tc39.es/ecma262/#sec-%typedarray%.prototype.indexof)
    fn index_of(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "indexOf")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        if length == 0 {
            return Ok(cx.negative_one());
        }

        let search_element = arguments.get(cx, 0);

        let n_arg = arguments.get(cx, 1);
        let start_index = resolve_relative_index_argument(cx, n_arg, length)?;

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..length {
            key.replace(PropertyKey::from_u64(cx, i)?);
            if must!(has_property(cx, object, key)) {
                let element = must!(get(cx, object, key));
                if is_strictly_equal(search_element, element)? {
                    return Ok(cx.number(i));
                }
            }
        }

        Ok(cx.negative_one())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.join (https://tc39.es/ecma262/#sec-%typedarray%.prototype.join)
    fn join(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "join")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let separator = arguments.get(cx, 0);
        let separator = if separator.is_undefined() {
            cx.names.comma().as_string()
        } else {
            to_string(cx, separator)?
        };

        let mut joined = cx.names.empty_string().as_string();

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in 0..length {
            if i > 0 {
                joined = StringValue::concat(cx, joined, separator)?;
            }

            key.replace(PropertyKey::from_u64(cx, i as u64)?);
            let element = must!(get(cx, object, key));

            if !element.is_undefined() {
                let next = to_string(cx, element)?;
                joined = StringValue::concat(cx, joined, next)?;
            }
        }

        Ok(joined.into())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.keys (https://tc39.es/ecma262/#sec-%typedarray%.prototype.keys)
    fn keys(cx, this_value, _) {
        let typed_array_record = this_typed_array_record(cx, this_value, "keys")?;
        let typed_array_object = typed_array_record.typed_array.into_object_value();

        Ok(ArrayIterator::new(cx, typed_array_object, ArrayIteratorKind::Key)?.as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.lastIndexOf (https://tc39.es/ecma262/#sec-%typedarray%.prototype.lastindexof)
    fn last_index_of(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "lastIndexOf")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        if length == 0 {
            return Ok(cx.negative_one());
        }

        let search_element = arguments.get(cx, 0);

        let start_index = if arguments.len() >= 2 {
            let start_arg = arguments.get(cx, 1);
            let n = to_integer_or_infinity(cx, start_arg)?;
            if n == f64::NEG_INFINITY {
                return Ok(cx.negative_one());
            }

            if n >= 0.0 {
                u64::min(n as u64, length - 1)
            } else {
                let start_index = length as i64 + n as i64;

                if start_index < 0 {
                    return Ok(cx.negative_one());
                }

                start_index as u64
            }
        } else {
            length - 1
        };

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in (0..=start_index).rev() {
            key.replace(PropertyKey::from_u64(cx, i)?);
            if must!(has_property(cx, object, key)) {
                let element = must!(get(cx, object, key));
                if is_strictly_equal(search_element, element)? {
                    return Ok(cx.number(i));
                }
            }
        }

        Ok(cx.negative_one())
    }}

    runtime_fn! {
    /// get %TypedArray%.prototype.length (https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.length)
    fn length(cx, this_value, _) {
        let typed_array = this_typed_array(cx, this_value, "length")?;

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return Ok(cx.zero());
        }

        let length = typed_array_length(&typed_array_record);

        Ok(cx.number(length))
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.map (https://tc39.es/ecma262/#sec-%typedarray%.prototype.map)
    fn map(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "map")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let callback_function = arguments.get(cx, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "TypedArray.prototype.map mapper must be a function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = arguments.get(cx, 1);

        let length_value = cx.number(length);
        let array = typed_array_species_create_object(cx, typed_array, &[length_value], "map")?;

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i as u64)?);
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::number(i));
            let arguments = [value, index_value, object.into()];

            let mapped_value = call_object(cx, callback_function, this_arg, &arguments)?;
            set(cx, array, index_key, mapped_value, true)?;
        }

        Ok(array.as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.reduce (https://tc39.es/ecma262/#sec-%typedarray%.prototype.reduce)
    fn reduce(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "reduce")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let callback_function = arguments.get(cx, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "TypedArray.prototype.reduce callback must be a function");
        }

        let callback_function = callback_function.as_object();
        let mut initial_index = 0;

        let mut accumulator = if arguments.len() >= 2 {
            arguments.get(cx, 1)
        } else if length == 0 {
            return type_error(cx, "TypedArray.prototype.reduce does not have an initial value");
        } else {
            initial_index = 1;
            let first_index_key = PropertyKey::array_index_handle(cx, 0)?;
            must!(get(cx, object, first_index_key))
        };

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in initial_index..length {
            index_key.replace(PropertyKey::from_u64(cx, i)?);
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::number(i));
            let arguments = [accumulator, value, index_value, object.into()];

            accumulator = call_object(cx, callback_function, cx.undefined(), &arguments)?;
        }

        Ok(accumulator)
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.reduceRight (https://tc39.es/ecma262/#sec-%typedarray%.prototype.reduceright)
    fn reduce_right(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "reduceRight")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let callback_function = arguments.get(cx, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "TypedArray.prototype.reduceRight callback must be a function");
        }

        let callback_function = callback_function.as_object();
        let mut initial_index = length as i64 - 1;

        let mut accumulator = if arguments.len() >= 2 {
            arguments.get(cx, 1)
        } else if length == 0 {
            return type_error(
                cx,
                "TypedArray.prototype.reduceRight does not have an initial value",
            );
        } else {
            let last_index_key = PropertyKey::from_u64_handle(cx, initial_index as u64)?;
            initial_index -= 1;
            must!(get(cx, object, last_index_key))
        };

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in (0..=initial_index).rev() {
            index_key.replace(PropertyKey::from_u64(cx, i as u64)?);
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::number(i));
            let arguments = [accumulator, value, index_value, object.into()];

            accumulator = call_object(cx, callback_function, cx.undefined(), &arguments)?;
        }

        Ok(accumulator)
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.reverse (https://tc39.es/ecma262/#sec-%typedarray%.prototype.reverse)
    fn reverse(cx, this_value, _) {
        let typed_array_record = this_typed_array_record(cx, this_value, "reverse")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let middle = length / 2;
        let mut lower = 0;
        // Safe to wrap as this only occurs when length is 0 and loop will be skipped
        let mut upper = length.wrapping_sub(1);

        // Shared between iterations
        let mut lower_key = PropertyKey::uninit().to_handle(cx);
        let mut upper_key = PropertyKey::uninit().to_handle(cx);

        while lower != middle {
            lower_key.replace(PropertyKey::from_u64(cx, lower)?);
            upper_key.replace(PropertyKey::from_u64(cx, upper)?);

            let lower_value = must!(get(cx, object, lower_key));
            let upper_value = must!(get(cx, object, upper_key));

            must!(set(cx, object, lower_key, upper_value, true));
            must!(set(cx, object, upper_key, lower_value, true));

            lower += 1;
            upper -= 1;
        }

        Ok(object.as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.set (https://tc39.es/ecma262/#sec-%typedarray%.prototype.set)
    fn set(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "set")?;
        let typed_array = typed_array_record.typed_array;

        let offset_arg = arguments.get(cx, 1);
        let offset = to_integer_or_infinity(cx, offset_arg)?;
        if offset < 0.0 {
            return range_error(cx, "TypedArray.prototype.set offset is negative");
        }

        let source_arg = arguments.get(cx, 0);
        if source_arg.is_object() && source_arg.as_object().is_typed_array() {
            Self::set_typed_array_from_typed_array(
                cx,
                typed_array,
                offset,
                source_arg.as_object().as_typed_array(),
            )?;
        } else {
            Self::set_typed_array_from_array_like(cx, typed_array, offset, source_arg)?;
        }

        Ok(cx.undefined())
    }}

    /// SetTypedArrayFromTypedArray (https://tc39.es/ecma262/#sec-settypedarrayfromtypedarray)
    pub fn set_typed_array_from_typed_array(
        cx: Context,
        mut target: DynTypedArray,
        target_offset: f64,
        source: DynTypedArray,
    ) -> EvalResult<()> {
        let mut target_buffer = target.viewed_array_buffer();

        let target_record = make_typed_array_with_buffer_witness_record(target);
        if is_typed_array_out_of_bounds(&target_record) {
            return type_error(cx, "TypedArray.prototype.set target typed array is out of bounds");
        }
        let target_length = typed_array_length(&target_record) as u64;

        let mut source_buffer = source.viewed_array_buffer();

        let source_record = make_typed_array_with_buffer_witness_record(source);
        if is_typed_array_out_of_bounds(&source_record) {
            return type_error(cx, "TypedArray.prototype.set source typed array is out of bounds");
        }
        let source_length = typed_array_length(&source_record);

        let target_byte_offset = target.byte_offset();
        let target_element_size = target.element_size();

        let source_byte_offset = source.byte_offset();
        let source_element_size = source.element_size();

        if target_offset == f64::INFINITY
            || (source_length as u64).saturating_add(target_offset as u64) > target_length
        {
            return range_error(cx, "TypedArray.prototype.set offset is out of range");
        }

        let source_byte_index =
            if same_object_value(*source_buffer.as_object(), *target_buffer.as_object()) {
                let source_byte_length = typed_array_byte_length(&source_record);
                source_buffer =
                    clone_array_buffer(cx, source_buffer, source_byte_offset, source_byte_length)?;
                0
            } else {
                source_byte_offset
            };

        let target_byte_index = (target_offset as usize * target_element_size) + target_byte_offset;
        let limit = target_byte_index + (target_element_size * source_length);

        unsafe {
            let mut from_ptr = source_buffer.data().as_ptr().add(source_byte_index);
            let mut to_ptr = target_buffer.data_mut().as_mut_ptr().add(target_byte_index);
            let limit_ptr = target_buffer.data_mut().as_mut_ptr().add(limit);

            let mut from_byte_index = source_byte_index;
            let mut to_byte_index = target_byte_index;

            if source.kind() != target.kind() {
                // If types are different then can access bytes directly but must convert
                while to_byte_index < limit {
                    // Convert between types. May allocate but does not invoke user code.
                    let element_value =
                        source.read_element_value(cx, *source_buffer, from_byte_index)?;

                    target.write_element_value(cx, to_byte_index, element_value)?;

                    from_byte_index += source_element_size;
                    to_byte_index += target_element_size;
                }
            } else {
                // Otherwise copy bytes directly instead of performing any conversions
                while to_ptr < limit_ptr {
                    let byte = from_ptr.read();
                    to_ptr.write(byte);

                    from_ptr = from_ptr.add(1);
                    to_ptr = to_ptr.add(1);
                }
            }
        }

        Ok(())
    }

    fn set_typed_array_from_array_like(
        cx: Context,
        mut target: DynTypedArray,
        offset: f64,
        source: Handle<Value>,
    ) -> EvalResult<()> {
        let target_record = make_typed_array_with_buffer_witness_record(target);
        if is_typed_array_out_of_bounds(&target_record) {
            return type_error(cx, "TypedArray.prototype.set target typed array is out of bounds");
        }

        let target_length = typed_array_length(&target_record) as u64;

        let source = to_object(cx, source)?;
        let source_length = length_of_array_like(cx, source)?;

        if offset == f64::INFINITY || source_length.saturating_add(offset as u64) > target_length {
            return range_error(cx, "TypedArray.prototype.set offset is out of range");
        }
        let offset = offset as u64;

        // Keys are shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in 0..source_length {
            key.replace(PropertyKey::from_u64(cx, i)?);
            let value = get(cx, source, key)?;

            let target_index = offset + i;

            target.write_element_value_unchecked(cx, target_index, value)?;
        }

        Ok(())
    }

    runtime_fn! {
    /// Uint8Array.prototype.setFromBase64 (https://tc39.es/ecma262/#sec-uint8array.prototype.setfrombase64)
    fn set_from_base64(cx, this_value, arguments) {
        let typed_array = validate_uint8_array(cx, this_value, "setFromBase64")?;

        let string_arg = arguments.get(cx, 0);
        if !string_arg.is_string() {
            return type_error(cx, "Uint8Array.prototype.setFromBase64 argument must be a string");
        }

        let options_arg = arguments.get(cx, 1);
        let options =
            get_base64_options_argument(cx, options_arg, "Uint8Array.prototype.setFromBase64")?;

        let alphabet =
            get_base64_alphabet_option(cx, options, "Uint8Array.prototype.setFromBase64")?;
        let last_chunk_handling = get_base64_last_chunk_handling_option(
            cx,
            options,
            "Uint8Array.prototype.setFromBase64",
        )?;

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return type_error(
                cx,
                "Uint8Array.prototype.setFromBase64 typed array is out of bounds",
            );
        }

        let byte_length = typed_array_length(&typed_array_record);

        let decode_result = decode_base64(
            cx,
            string_arg.as_string(),
            alphabet,
            last_chunk_handling,
            Some(byte_length as u64),
            "Uint8Array.prototype.setFromBase64",
        )?;

        Self::set_from_decode_result(cx, typed_array, decode_result)
    }}

    runtime_fn! {
    /// Uint8Array.prototype.setFromHex (https://tc39.es/ecma262/#sec-uint8array.prototype.setfromhex)
    fn set_from_hex(cx, this_value, arguments) {
        let typed_array = validate_uint8_array(cx, this_value, "setFromHex")?;

        let string_arg = arguments.get(cx, 0);
        if !string_arg.is_string() {
            return type_error(cx, "Uint8Array.prototype.setFromHex argument must be a string");
        }

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return type_error(cx, "Uint8Array.prototype.setFromHex typed array is out of bounds");
        }

        let byte_length = typed_array_length(&typed_array_record);

        let decode_result = decode_hex(
            cx,
            string_arg.as_string(),
            Some(byte_length as u64),
            "Uint8Array.prototype.setFromHex",
        )?;

        Self::set_from_decode_result(cx, typed_array, decode_result)
    }}

    fn set_from_decode_result(
        cx: Context,
        mut typed_array: DynTypedArray,
        decode_result: DecodeResult,
    ) -> EvalResult<Handle<Value>> {
        let num_bytes_read = cx.number(decode_result.read);
        let num_bytes_written = cx.number(decode_result.bytes.len());

        // Write the encoded bytes into the backing slice of the ArrayBuffer. If there was a
        // decoding error then still write all successfully decoded bytes until the error.
        let data = &mut typed_array.data_mut()[0..decode_result.bytes.len()];
        data.copy_from_slice(&decode_result.bytes);

        if let Some(error) = decode_result.error {
            return eval_err!(error);
        }

        // Output object with number of bytes read and written
        let result_object = ordinary_object_create(cx)?;
        must!(create_data_property_or_throw(
            cx,
            result_object,
            cx.names.read(),
            num_bytes_read
        ));
        must!(create_data_property_or_throw(
            cx,
            result_object,
            cx.names.written(),
            num_bytes_written
        ));

        Ok(result_object.as_value())
    }

    runtime_fn! {
    /// %TypedArray%.prototype.slice (https://tc39.es/ecma262/#sec-%typedarray%.prototype.slice)
    fn slice(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "slice")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let start_arg = arguments.get(cx, 0);
        let start_index = resolve_relative_index_argument(cx, start_arg, length)?;

        let end_argument = arguments.get(cx, 1);
        let end_index = if !end_argument.is_undefined() {
            resolve_relative_index_argument(cx, end_argument, length)?
        } else {
            length
        };

        let count = end_index.saturating_sub(start_index);
        let count_value = cx.number(count);
        let new_typed_array = typed_array_species_create(cx, typed_array, &[count_value], "slice")?;
        let array = new_typed_array.into_object_value();

        if count == 0 {
            return Ok(array.as_value());
        }

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return type_error(cx, "TypedArray.prototype.slice typed array is out of bounds");
        }

        let end_index = u64::min(end_index, typed_array_length(&typed_array_record) as u64);
        let count = end_index.saturating_sub(start_index);

        // If types are different then must call get and set and convert types
        if typed_array.kind() != new_typed_array.kind() {
            // Shared between iterations
            let mut from_key = PropertyKey::uninit().to_handle(cx);
            let mut to_key = PropertyKey::uninit().to_handle(cx);

            let mut current_index = start_index;
            for i in 0..count {
                from_key.replace(PropertyKey::from_u64(cx, current_index)?);
                to_key.replace(PropertyKey::from_u64(cx, i)?);

                let value = get(cx, object, from_key)?;
                set(cx, array, to_key, value, true)?;

                current_index += 1;
            }
        } else {
            // Otherwise copy bytes directly instead of performing any conversions
            let source_buffer = typed_array.viewed_array_buffer();
            let mut target_buffer = new_typed_array.viewed_array_buffer();
            let element_size = typed_array.element_size();

            let source_byte_offset = typed_array.byte_offset();
            let source_byte_index = (start_index as usize) * element_size + source_byte_offset;
            let target_byte_index = new_typed_array.byte_offset();

            unsafe {
                let mut from_ptr = source_buffer.data().as_ptr().add(source_byte_index);
                let mut to_ptr = target_buffer.data_mut().as_mut_ptr().add(target_byte_index);

                for _ in 0..(count as usize * element_size) {
                    let byte = from_ptr.read();
                    to_ptr.write(byte);

                    from_ptr = from_ptr.add(1);
                    to_ptr = to_ptr.add(1);
                }
            }
        }

        Ok(array.as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.some (https://tc39.es/ecma262/#sec-%typedarray%.prototype.some)
    fn some(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "some")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let callback_function = arguments.get(cx, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "TypedArray.prototype.some callback must be a function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = arguments.get(cx, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i as u64)?);
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::number(i));
            let arguments = [value, index_value, object.into()];

            let test_result = call_object(cx, callback_function, this_arg, &arguments)?;
            if to_boolean(*test_result) {
                return Ok(cx.bool(true));
            }
        }

        Ok(cx.bool(false))
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.sort (https://tc39.es/ecma262/#sec-%typedarray%.prototype.sort)
    fn sort(cx, this_value, arguments) {
        let compare_function_arg = arguments.get(cx, 0);
        if !compare_function_arg.is_undefined() && !is_callable(compare_function_arg) {
            return type_error(cx, "TypedArray.prototype.sort comparator must be a function");
        };

        let typed_array_record = this_typed_array_record(cx, this_value, "sort")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let sorted_values = sort_indexed_properties::<INCLUDE_HOLES, TYPED_ARRAY>(
            cx,
            object,
            length,
            compare_function_arg,
        )?;

        // Reuse handle between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);

        // Copy sorted values into array
        for (i, value) in sorted_values.iter().enumerate() {
            index_key.replace(PropertyKey::from_u64(cx, i as u64)?);
            set(cx, object, index_key, *value, true)?;
        }

        Ok(object.as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.subarray (https://tc39.es/ecma262/#sec-%typedarray%.prototype.subarray)
    fn subarray(cx, this_value, arguments) {
        let typed_array = this_typed_array(cx, this_value, "subarray")?;
        let buffer = typed_array.viewed_array_buffer();

        let source_record = make_typed_array_with_buffer_witness_record(typed_array);
        let source_length = if is_typed_array_out_of_bounds(&source_record) {
            0
        } else {
            typed_array_length(&source_record) as u64
        };

        let start_arg = arguments.get(cx, 0);
        let start_index = resolve_relative_index_argument(cx, start_arg, source_length)?;

        let end_argument = arguments.get(cx, 1);
        let end_index = if !end_argument.is_undefined() {
            resolve_relative_index_argument(cx, end_argument, source_length)?
        } else {
            source_length
        };

        let new_length = end_index.saturating_sub(start_index);

        let element_size = typed_array.element_size();
        let source_byte_offset = typed_array.byte_offset();
        let begin_byte_offset = source_byte_offset + (start_index as usize) * element_size;
        let begin_byte_offset_value = cx.number(begin_byte_offset);

        let subarray = if typed_array.array_length().is_none() && end_argument.is_undefined() {
            typed_array_species_create_object(
                cx,
                typed_array,
                &[buffer.into(), begin_byte_offset_value],
                "subarray",
            )?
        } else {
            let new_length_value = cx.number(new_length);
            typed_array_species_create_object(
                cx,
                typed_array,
                &[buffer.into(), begin_byte_offset_value, new_length_value],
                "subarray",
            )?
        };

        Ok(subarray.as_value())
    }}

    runtime_fn! {
    /// Uint8Array.prototype.toBase64 (https://tc39.es/ecma262/#sec-uint8array.prototype.tobase64)
    fn to_base64(cx, this_value, arguments) {
        let typed_array = validate_uint8_array(cx, this_value, "toBase64")?;

        let options_arg = arguments.get(cx, 0);
        let options =
            get_base64_options_argument(cx, options_arg, "Uint8Array.prototype.toBase64")?;

        let alphabet = get_base64_alphabet_option(cx, options, "Uint8Array.prototype.toBase64")?;
        let omit_padding = get_base64_omit_padding_option(cx, options)?;

        // Inlined validation from GetUint8ArrayBytes
        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return type_error(cx, "Uint8Array.prototype.toBase64 typed array is out of bounds");
        }

        // Encode the slice of the backing ArrayBuffer that corresponds to this typed array
        let length = typed_array_length(&typed_array_record);
        let data = &typed_array.data()[0..length];

        let base64_code_points = encode_base64(data, alphabet, omit_padding);
        let base64_string = FlatString::from_one_byte_slice(cx, &base64_code_points)?.to_handle();

        Ok(base64_string.as_string().as_value())
    }}

    runtime_fn! {
    /// Uint8Array.prototype.toHex (https://tc39.es/ecma262/#sec-uint8array.prototype.tohex)
    fn to_hex(cx, this_value, _) {
        let typed_array = validate_uint8_array(cx, this_value, "toHex")?;

        // Inlined validation from GetUint8ArrayBytes
        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return type_error(cx, "Uint8Array.prototype.toHex typed array is out of bounds");
        }

        // Get the slice of the backing ArrayBuffer that corresponds to this typed array
        let length = typed_array_length(&typed_array_record);
        let data = &typed_array.data()[0..length];

        let hex_code_points = encode_hex(data);
        let hex_string = FlatString::from_one_byte_slice(cx, &hex_code_points)?.to_handle();

        Ok(hex_string.as_string().as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.toLocaleString (https://tc39.es/ecma262/#sec-%typedarray%.prototype.tolocalestring)
    fn to_locale_string(cx, this_value, _) {
        let typed_array_record = this_typed_array_record(cx, this_value, "toLocaleString")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let mut result = cx.names.empty_string().as_string();
        let separator = cx.names.comma().as_string();

        for i in 0..length {
            if i > 0 {
                result = StringValue::concat(cx, result, separator)?;
            }

            let key = PropertyKey::from_u64(cx, i as u64)?.to_handle(cx);
            let next_element = must!(get(cx, object, key));

            if !next_element.is_nullish() {
                let string_result = invoke(cx, next_element, cx.names.to_locale_string(), &[])?;
                let string_result = to_string(cx, string_result)?;

                result = StringValue::concat(cx, result, string_result)?;
            }
        }

        Ok(result.into())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.toReversed (https://tc39.es/ecma262/#sec-%typedarray%.prototype.toreversed)
    fn to_reversed(cx, this_value, _) {
        let typed_array_record = this_typed_array_record(cx, this_value, "toReversed")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let array = typed_array_create_same_type(cx, typed_array, length, "toReversed")?;

        // Keys are shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);
        let mut to_key = PropertyKey::uninit().to_handle(cx);

        for i in 0..length {
            from_key.replace(PropertyKey::from_u64(cx, length - i - 1)?);
            to_key.replace(PropertyKey::from_u64(cx, i)?);

            let value = must!(get(cx, object, from_key));
            must!(set(cx, array, to_key, value, true));
        }

        Ok(array.as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.toSorted (https://tc39.es/ecma262/#sec-%typedarray%.prototype.tosorted)
    fn to_sorted(cx, this_value, arguments) {
        let compare_function_arg = arguments.get(cx, 0);
        if !compare_function_arg.is_undefined() && !is_callable(compare_function_arg) {
            return type_error(cx, "TypedArray.prototype.toSorted comparator must be a function");
        };

        let typed_array_record = this_typed_array_record(cx, this_value, "toSorted")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let sorted_array = typed_array_create_same_type(cx, typed_array, length, "toSorted")?;

        let sorted_values = sort_indexed_properties::<INCLUDE_HOLES, TYPED_ARRAY>(
            cx,
            object,
            length,
            compare_function_arg,
        )?;

        // Reuse handle between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);

        // Copy sorted values into array
        for (i, value) in sorted_values.iter().enumerate() {
            index_key.replace(PropertyKey::from_u64(cx, i as u64)?);
            set(cx, sorted_array, index_key, *value, true)?;
        }

        Ok(sorted_array.as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.values (https://tc39.es/ecma262/#sec-%typedarray%.prototype.values)
    fn values(cx, this_value, _) {
        let typed_array_record = this_typed_array_record(cx, this_value, "values")?;
        let typed_array_object = typed_array_record.typed_array.into_object_value();

        Ok(ArrayIterator::new(cx, typed_array_object, ArrayIteratorKind::Value)?.as_value())
    }}

    runtime_fn! {
    /// %TypedArray%.prototype.with (https://tc39.es/ecma262/#sec-%typedarray%.prototype.with)
    fn with(cx, this_value, arguments) {
        let typed_array_record = this_typed_array_record(cx, this_value, "with")?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let index_arg = arguments.get(cx, 0);
        let relative_index = to_integer_or_infinity(cx, index_arg)?;

        // Convert from relative to actual index, making sure index is in range
        let actual_index = if relative_index >= 0.0 {
            relative_index
        } else {
            relative_index + length as f64
        };

        // Convert new value to correct typed
        let new_value = arguments.get(cx, 1);
        let new_value = match typed_array.content_type() {
            ContentType::BigInt => to_bigint(cx, new_value)?.into(),
            ContentType::Number => to_number(cx, new_value)?,
        };

        // User code may have been invoked during conversions before this point, which may resize
        // the underlying ArrayBuffer. Now we can check bounds.
        let is_valid_integer_index = {
            // Refetch typed array length in case it has changed, but only use the updated length
            // for bounds checks.
            let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);

            if actual_index.is_infinite() || is_typed_array_out_of_bounds(&typed_array_record) {
                false
            } else {
                let actual_index = actual_index as i64;
                let new_length = typed_array_length(&typed_array_record);

                (actual_index >= 0) && ((actual_index as usize) < new_length)
            }
        };

        if !is_valid_integer_index {
            return range_error(cx, "TypedArray.prototype.with index is out of range");
        }
        let actual_index = actual_index as u64;

        let array = typed_array_create_same_type(cx, typed_array, length as u64, "with")?;

        // Key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in 0..(length as u64) {
            key.replace(PropertyKey::from_u64(cx, i)?);

            // Replace the i'th value with the new value
            let value = if i == actual_index {
                new_value
            } else {
                must!(get(cx, object, key))
            };

            must!(set(cx, array, key, value, true));
        }

        Ok(array.as_value())
    }}

    runtime_fn! {
    /// get %TypedArray%.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-get-%typedarray%.prototype-%symbol.tostringtag%)
    fn get_to_string_tag(cx, this_value, _) {
        if !this_value.is_object() {
            return Ok(cx.undefined());
        }

        let this_object = this_value.as_object();
        if !this_object.is_typed_array() {
            return Ok(cx.undefined());
        }

        Ok(this_object.as_typed_array().name(cx).into())
    }}
}

#[macro_export]
macro_rules! create_typed_array_prototype {
    ($typed_array:ident, $rust_name:ident, $element_type:ident, $prototype:ident, $constructor:ident) => {
        pub struct $prototype;

        impl $prototype {
            /// Properties of the TypedArray Prototype Objects (https://tc39.es/ecma262/#sec-properties-of-typedarray-prototype-objects)
            pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
                let mut builder =
                    IntrinsicBuilder::object(cx, realm, Intrinsic::TypedArrayPrototype)?;

                // Constructor property is added once TypedArrayConstructor has been created
                builder.frozen(
                    cx.names.bytes_per_element(),
                    cx.smi(std::mem::size_of::<$element_type>() as u8),
                )?;

                builder.build()
            }
        }
    };
}

#[inline]
fn this_typed_array(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<DynTypedArray> {
    if value.is_object() {
        let object = value.as_object();
        if object.is_typed_array() {
            return Ok(object.as_typed_array());
        }
    }

    type_error(
        cx,
        &format!("TypedArray.prototype.{method_name} must be called on a TypedArray"),
    )
}

/// TypedArraySpeciesCreate (https://tc39.es/ecma262/#typedarray-species-create)
fn typed_array_species_create_object(
    cx: Context,
    exemplar: DynTypedArray,
    arguments: &[Handle<Value>],
    method_name: &str,
) -> EvalResult<Handle<ObjectValue>> {
    let result = typed_array_species_create(cx, exemplar, arguments, method_name)?;
    Ok(result.into_object_value())
}

fn typed_array_species_create(
    cx: Context,
    exemplar: DynTypedArray,
    arguments: &[Handle<Value>],
    method_name: &str,
) -> EvalResult<DynTypedArray> {
    let intrinsic = match exemplar.kind() {
        TypedArrayKind::Int8Array => Intrinsic::Int8ArrayConstructor,
        TypedArrayKind::UInt8Array => Intrinsic::UInt8ArrayConstructor,
        TypedArrayKind::UInt8ClampedArray => Intrinsic::UInt8ClampedArrayConstructor,
        TypedArrayKind::Int16Array => Intrinsic::Int16ArrayConstructor,
        TypedArrayKind::UInt16Array => Intrinsic::UInt16ArrayConstructor,
        TypedArrayKind::Int32Array => Intrinsic::Int32ArrayConstructor,
        TypedArrayKind::UInt32Array => Intrinsic::UInt32ArrayConstructor,
        TypedArrayKind::BigInt64Array => Intrinsic::BigInt64ArrayConstructor,
        TypedArrayKind::BigUInt64Array => Intrinsic::BigUInt64ArrayConstructor,
        TypedArrayKind::Float16Array => Intrinsic::Float16ArrayConstructor,
        TypedArrayKind::Float32Array => Intrinsic::Float32ArrayConstructor,
        TypedArrayKind::Float64Array => Intrinsic::Float64ArrayConstructor,
    };

    let constructor = species_constructor(cx, exemplar.into_object_value(), intrinsic)?;

    let result = typed_array_create_from_constructor(
        cx,
        constructor,
        arguments,
        &format!("TypedArray.prototype.{method_name}"),
    )?;

    if result.content_type() != exemplar.content_type() {
        return type_error(
            cx,
            &format!(
                "TypedArray.prototype.{method_name} species constructor must return a typed array that contains {}",
                exemplar.content_type().format()
            ),
        );
    }

    Ok(result)
}

/// TypedArrayCreateFromConstructor (https://tc39.es/ecma262/#sec-typedarraycreatefromconstructor)
pub fn typed_array_create_from_constructor_object(
    cx: Context,
    constructor: Handle<ObjectValue>,
    arguments: &[Handle<Value>],
    full_method_name: &str,
) -> EvalResult<Handle<ObjectValue>> {
    let result = typed_array_create_from_constructor(cx, constructor, arguments, full_method_name)?;
    Ok(result.into_object_value())
}

pub fn typed_array_create_from_constructor(
    cx: Context,
    constructor: Handle<ObjectValue>,
    arguments: &[Handle<Value>],
    full_method_name: &str,
) -> EvalResult<DynTypedArray> {
    let new_typed_array = construct(cx, constructor, arguments, None)?;

    let new_typed_array_record = species_constructor_result_typed_array_record(
        cx,
        new_typed_array.into(),
        full_method_name,
    )?;

    if arguments.len() == 1 && arguments[0].is_number() {
        if is_typed_array_out_of_bounds(&new_typed_array_record) {
            return type_error(
                cx,
                &format!(
                    "{full_method_name} species constructor returned a typed array that is out of bounds"
                ),
            );
        }

        let new_typed_array_length = typed_array_length(&new_typed_array_record);

        if (new_typed_array_length as f64) < arguments[0].as_number() {
            return type_error(
                cx,
                &format!(
                    "{full_method_name} species constructor returned a typed array that does not have the expected length"
                ),
            );
        }
    }

    Ok(new_typed_array_record.typed_array)
}

/// TypedArrayCreateSameType (https://tc39.es/ecma262/#sec-typedarray-create-same-type)
fn typed_array_create_same_type(
    cx: Context,
    exemplar: DynTypedArray,
    length: u64,
    method_name: &str,
) -> EvalResult<Handle<ObjectValue>> {
    let constructor_intrinsic = match exemplar.kind() {
        TypedArrayKind::Int8Array => Intrinsic::Int8ArrayConstructor,
        TypedArrayKind::UInt8Array => Intrinsic::UInt8ArrayConstructor,
        TypedArrayKind::UInt8ClampedArray => Intrinsic::UInt8ClampedArrayConstructor,
        TypedArrayKind::Int16Array => Intrinsic::Int16ArrayConstructor,
        TypedArrayKind::UInt16Array => Intrinsic::UInt16ArrayConstructor,
        TypedArrayKind::Int32Array => Intrinsic::Int32ArrayConstructor,
        TypedArrayKind::UInt32Array => Intrinsic::UInt32ArrayConstructor,
        TypedArrayKind::BigInt64Array => Intrinsic::BigInt64ArrayConstructor,
        TypedArrayKind::BigUInt64Array => Intrinsic::BigUInt64ArrayConstructor,
        TypedArrayKind::Float16Array => Intrinsic::Float16ArrayConstructor,
        TypedArrayKind::Float32Array => Intrinsic::Float32ArrayConstructor,
        TypedArrayKind::Float64Array => Intrinsic::Float64ArrayConstructor,
    };

    let constructor = cx.get_intrinsic(constructor_intrinsic);
    let length_value = cx.number(length);

    typed_array_create_from_constructor_object(
        cx,
        constructor,
        &[length_value],
        &format!("TypedArray.prototype.{method_name}"),
    )
}

/// ValidateTypedArray (https://tc39.es/ecma262/#sec-validatetypedarray)
#[inline]
fn this_typed_array_record(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<TypedArrayWithBufferWitnessRecord> {
    let typed_array = this_typed_array(cx, value, method_name)?;
    let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);

    if is_typed_array_out_of_bounds(&typed_array_record) {
        return type_error(
            cx,
            &format!("TypedArray.prototype.{method_name} typed array is out of bounds"),
        );
    }

    Ok(typed_array_record)
}

/// ValidateUint8Array (https://tc39.es/ecma262/#sec-validateuint8array)
fn validate_uint8_array(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<DynTypedArray> {
    let typed_array = this_typed_array(cx, value, method_name)?;

    if typed_array.kind() != TypedArrayKind::UInt8Array {
        return type_error(
            cx,
            &format!("Uint8Array.prototype.{method_name} must be called on a Uint8Array"),
        );
    }

    Ok(typed_array)
}

fn species_constructor_result_typed_array_record(
    cx: Context,
    value: Handle<Value>,
    full_method_name: &str,
) -> EvalResult<TypedArrayWithBufferWitnessRecord> {
    if !value.is_object() || !value.as_object().is_typed_array() {
        return type_error(
            cx,
            &format!("{full_method_name} species constructor must return a typed array"),
        );
    }

    let typed_array = value.as_object().as_typed_array();
    let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);

    if is_typed_array_out_of_bounds(&typed_array_record) {
        return type_error(
            cx,
            &format!(
                "{full_method_name} species constructor returned a typed array that is out of bounds"
            ),
        );
    }

    Ok(typed_array_record)
}

pub struct TypedArrayWithBufferWitnessRecord {
    pub typed_array: DynTypedArray,
    pub cached_buffer_byte_length: Option<usize>,
}

/// MakeTypedArrayWithBufferWitnessRecord (https://tc39.es/ecma262/#sec-maketypedarraywithbufferwitnessrecord)
pub fn make_typed_array_with_buffer_witness_record(
    typed_array: DynTypedArray,
) -> TypedArrayWithBufferWitnessRecord {
    let array_buffer = typed_array.viewed_array_buffer_ptr();

    let byte_length = if array_buffer.is_detached() {
        None
    } else {
        Some(array_buffer.byte_length())
    };

    TypedArrayWithBufferWitnessRecord { typed_array, cached_buffer_byte_length: byte_length }
}

/// TypedArrayByteLength (https://tc39.es/ecma262/#sec-typedarraybytelength)
fn typed_array_byte_length(typed_array_record: &TypedArrayWithBufferWitnessRecord) -> usize {
    if is_typed_array_out_of_bounds(typed_array_record) {
        return 0;
    }

    let length = typed_array_length(typed_array_record);
    if length == 0 {
        return 0;
    }

    // Fixed length typed arrays return known byte length
    let typed_array = typed_array_record.typed_array;
    if let Some(byte_length) = typed_array.byte_length() {
        return byte_length;
    }

    // Resizable typed arrays must calculate the current length
    length * typed_array.element_size()
}

/// TypedArrayLength (https://tc39.es/ecma262/#sec-typedarraylength)
pub fn typed_array_length(typed_array_record: &TypedArrayWithBufferWitnessRecord) -> usize {
    // Fixed length typed arrays return known array length
    let typed_array = typed_array_record.typed_array;
    if let Some(array_length) = typed_array.array_length() {
        return array_length;
    }

    // Resizable typed arrays must calculate the current length
    let byte_offset = typed_array.byte_offset();
    let element_size = typed_array.element_size();
    let byte_length = typed_array_record.cached_buffer_byte_length.unwrap();

    (byte_length - byte_offset) / element_size
}

/// IsTypedArrayOutOfBounds (https://tc39.es/ecma262/#sec-istypedarrayoutofbounds)
pub fn is_typed_array_out_of_bounds(
    typed_array_record: &TypedArrayWithBufferWitnessRecord,
) -> bool {
    let typed_array = typed_array_record.typed_array;
    let buffer_byte_length = typed_array_record.cached_buffer_byte_length;

    if buffer_byte_length.is_none() {
        return true;
    }
    let buffer_byte_length = buffer_byte_length.unwrap();

    let byte_offset_start = typed_array.byte_offset();

    let byte_offset_end = match typed_array.array_length() {
        None => buffer_byte_length,
        Some(array_length) => byte_offset_start + array_length * typed_array.element_size(),
    };

    byte_offset_start > buffer_byte_length || byte_offset_end > buffer_byte_length
}

/// CompareTypedArrayElements (https://tc39.es/ecma262/#sec-comparetypedarrayelements)
///
/// Will only be called on numbers and BigInts.
pub fn compare_typed_array_elements(
    cx: Context,
    v1: Handle<Value>,
    v2: Handle<Value>,
    compare_function: Handle<Value>,
) -> EvalResult<Ordering> {
    // Use the compare function if provided
    if !compare_function.is_undefined() {
        let result_value =
            call_object(cx, compare_function.as_object(), cx.undefined(), &[v1, v2])?;
        if result_value.is_nan() {
            return Ok(Ordering::Equal);
        }

        let result_number = to_number(cx, result_value)?;
        let result_number = result_number.as_number();

        // Convert from positive/negative/equal number result to Ordering
        return if result_number == 0.0 {
            Ok(Ordering::Equal)
        } else if result_number < 0.0 {
            Ok(Ordering::Less)
        } else {
            Ok(Ordering::Greater)
        };
    }

    // Treat NaN as the lowest value
    let v1_is_nan = v1.is_nan();
    let v2_is_nan = v2.is_nan();
    if v1_is_nan && v2_is_nan {
        return Ok(Ordering::Equal);
    } else if v1_is_nan {
        return Ok(Ordering::Greater);
    } else if v2_is_nan {
        return Ok(Ordering::Less);
    }

    // Both values must have the same type - number or BigInt
    if v1.is_bigint() {
        debug_assert!(v2.is_bigint());

        let v1_bigint = v1.as_bigint().bigint();
        let v2_bigint = v2.as_bigint().bigint();

        if v1_bigint < v2_bigint {
            return Ok(Ordering::Less);
        } else if v1_bigint > v2_bigint {
            return Ok(Ordering::Greater);
        }

        if v1_bigint.magnitude().eq(&BigUint::default())
            && v2_bigint.magnitude().eq(&BigUint::default())
        {
            if v1_bigint.sign() == Sign::Minus && v2_bigint.sign() == Sign::Plus {
                return Ok(Ordering::Less);
            } else if v1_bigint.sign() == Sign::Plus && v2_bigint.sign() == Sign::Minus {
                return Ok(Ordering::Greater);
            }
        }
    } else {
        debug_assert!(v1.is_number() && v2.is_number());

        let v1_number = v1.as_number();
        let v2_number = v2.as_number();

        if v1_number < v2_number {
            return Ok(Ordering::Less);
        } else if v1_number > v2_number {
            return Ok(Ordering::Greater);
        }

        if v1_number == 0.0 && v2_number == 0.0 {
            if v1_number.is_sign_negative() && v2_number.is_sign_negative() {
                return Ok(Ordering::Less);
            } else if v1_number.is_sign_positive() && v2_number.is_sign_negative() {
                return Ok(Ordering::Greater);
            }
        }
    }

    Ok(Ordering::Equal)
}
