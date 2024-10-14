use std::cmp::Ordering;

use num_bigint::{BigUint, Sign};

use crate::{
    js::runtime::{
        abstract_operations::{
            call_object, construct, has_property, invoke, length_of_array_like, set,
            species_constructor,
        },
        builtin_function::BuiltinFunction,
        error::{range_error, type_error},
        function::get_argument,
        get,
        interned_strings::InternedStrings,
        intrinsics::{
            array_buffer_constructor::clone_array_buffer,
            array_iterator::{ArrayIterator, ArrayIteratorKind},
        },
        object_value::ObjectValue,
        property::Property,
        string_value::StringValue,
        to_string,
        type_utilities::{
            is_callable, is_strictly_equal, same_object_value, same_value_zero, to_bigint,
            to_boolean, to_integer_or_infinity, to_number, to_object,
        },
        Context, EvalResult, Handle, PropertyKey, Realm, Value,
    },
    must,
};

use super::{
    array_prototype::{find_via_predicate, sort_indexed_properties, INCLUDE_HOLES, TYPED_ARRAY},
    intrinsics::Intrinsic,
    typed_array::{ContentType, DynTypedArray, TypedArrayKind},
};

pub struct TypedArrayPrototype;

impl TypedArrayPrototype {
    /// Properties of the %TypedArray% Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-%typedarrayprototype%-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once TypedArrayConstructor has been created

        // Create values function as it is referenced by multiple properties
        let values_function =
            BuiltinFunction::create(cx, Self::values, 0, cx.names.values(), realm, None, None)
                .into();

        object.intrinsic_func(cx, cx.names.at(), Self::at, 1, realm);
        object.intrinsic_getter(cx, cx.names.buffer(), Self::buffer, realm);
        object.intrinsic_getter(cx, cx.names.byte_length(), Self::byte_length, realm);
        object.intrinsic_getter(cx, cx.names.byte_offset(), Self::byte_offset, realm);
        object.intrinsic_func(cx, cx.names.copy_within(), Self::copy_within, 2, realm);
        object.intrinsic_func(cx, cx.names.entries(), Self::entries, 0, realm);
        object.intrinsic_func(cx, cx.names.every(), Self::every, 1, realm);
        object.intrinsic_func(cx, cx.names.fill(), Self::fill, 1, realm);
        object.intrinsic_func(cx, cx.names.filter(), Self::filter, 1, realm);
        object.intrinsic_func(cx, cx.names.find(), Self::find, 1, realm);
        object.intrinsic_func(cx, cx.names.find_index(), Self::find_index, 1, realm);
        object.intrinsic_func(cx, cx.names.find_last(), Self::find_last, 1, realm);
        object.intrinsic_func(cx, cx.names.find_last_index(), Self::find_last_index, 1, realm);
        object.intrinsic_func(cx, cx.names.for_each(), Self::for_each, 1, realm);
        object.intrinsic_func(cx, cx.names.includes(), Self::includes, 1, realm);
        object.intrinsic_func(cx, cx.names.index_of(), Self::index_of, 1, realm);
        object.intrinsic_func(cx, cx.names.join(), Self::join, 1, realm);
        object.intrinsic_func(cx, cx.names.keys(), Self::keys, 0, realm);
        object.intrinsic_func(cx, cx.names.last_index_of(), Self::last_index_of, 1, realm);
        object.intrinsic_getter(cx, cx.names.length(), Self::length, realm);
        object.intrinsic_func(cx, cx.names.map_(), Self::map, 1, realm);
        object.intrinsic_func(cx, cx.names.reduce(), Self::reduce, 1, realm);
        object.intrinsic_func(cx, cx.names.reduce_right(), Self::reduce_right, 1, realm);
        object.intrinsic_func(cx, cx.names.reverse(), Self::reverse, 0, realm);
        object.intrinsic_func(cx, cx.names.set_(), Self::set, 1, realm);
        object.intrinsic_func(cx, cx.names.slice(), Self::slice, 2, realm);
        object.intrinsic_func(cx, cx.names.some(), Self::some, 1, realm);
        object.intrinsic_func(cx, cx.names.sort(), Self::sort, 1, realm);
        object.intrinsic_func(cx, cx.names.subarray(), Self::subarray, 2, realm);
        object.intrinsic_func(cx, cx.names.to_locale_string(), Self::to_locale_string, 0, realm);
        object.intrinsic_func(cx, cx.names.to_reversed(), Self::to_reversed, 0, realm);
        object.intrinsic_func(cx, cx.names.to_sorted(), Self::to_sorted, 1, realm);
        // Use Array.prototype.toString directly
        object.intrinsic_data_prop(
            cx,
            cx.names.to_string(),
            realm
                .get_intrinsic(Intrinsic::ArrayPrototypeToString)
                .into(),
        );
        object.intrinsic_data_prop(cx, cx.names.values(), values_function);
        object.intrinsic_func(cx, cx.names.with(), Self::with, 2, realm);

        // %TypedArray%.prototype [ @@iterator ] (https://tc39.es/ecma262/#sec-%typedarray%.prototype-%symbol.iterator%)
        let iterator_key = cx.well_known_symbols.iterator();
        object.set_property(cx, iterator_key, Property::data(values_function, true, false, true));

        // get %TypedArray%.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-get-%typedarray%.prototype-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.intrinsic_getter(cx, to_string_tag_key, Self::get_to_string_tag, realm);

        object
    }

    /// %TypedArray%.prototype.at (https://tc39.es/ecma262/#sec-%typedarray%.prototype.at)
    pub fn at(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let index_arg = get_argument(cx, arguments, 0);
        let relative_index = to_integer_or_infinity(cx, index_arg)?;

        let key = if relative_index >= 0.0 {
            if relative_index >= length as f64 {
                return Ok(cx.undefined());
            }

            PropertyKey::from_u64(cx, relative_index as u64).to_handle(cx)
        } else {
            if -relative_index > length as f64 {
                return Ok(cx.undefined());
            }

            PropertyKey::from_u64(cx, (length as i64 + relative_index as i64) as u64).to_handle(cx)
        };

        get(cx, object, key)
    }

    /// get %TypedArray%.prototype.buffer (https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.buffer)
    pub fn buffer(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array = require_typed_array(cx, this_value)?;
        Ok(typed_array.viewed_array_buffer().as_value())
    }

    /// get %TypedArray%.prototype.byteLength (https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.bytelength)
    pub fn byte_length(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array = require_typed_array(cx, this_value)?;

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return Ok(cx.zero());
        }

        let byte_length = typed_array_byte_length(&typed_array_record);

        Ok(Value::from(byte_length).to_handle(cx))
    }

    /// get %TypedArray%.prototype.byteOffset (https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.byteoffset)
    pub fn byte_offset(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array = require_typed_array(cx, this_value)?;

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return Ok(cx.zero());
        }

        Ok(Value::from(typed_array.byte_offset()).to_handle(cx))
    }

    /// %TypedArray%.prototype.copyWithin (https://tc39.es/ecma262/#sec-%typedarray%.prototype.copywithin)
    pub fn copy_within(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let target_arg = get_argument(cx, arguments, 0);
        let relative_target = to_integer_or_infinity(cx, target_arg)?;
        let to_index = if relative_target < 0.0 {
            if relative_target == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_target as i64, 0) as u64
            }
        } else {
            u64::min(relative_target as u64, length)
        };

        let start_arg = get_argument(cx, arguments, 1);
        let relative_start = to_integer_or_infinity(cx, start_arg)?;
        let from_start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_start as i64, 0) as u64
            }
        } else {
            u64::min(relative_start as u64, length)
        };

        let end_argument = get_argument(cx, arguments, 2);
        let from_end_index = if !end_argument.is_undefined() {
            let relative_end = to_integer_or_infinity(cx, end_argument)?;

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
            return type_error(cx, "typed array is out of bounds");
        }

        let buffer_byte_limit =
            typed_array_length(&typed_array_record) as u64 * element_size + byte_offset;

        let to_byte_index = to_index * element_size + byte_offset;
        let from_byte_index = from_start_index * element_size + byte_offset;
        let mut count_bytes = count as u64 * element_size;

        let data_ptr = typed_array.viewed_array_buffer_ptr().data().as_mut_ptr();

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
    }

    /// %TypedArray%.prototype.entries (https://tc39.es/ecma262/#sec-%typedarray%.prototype.entries)
    pub fn entries(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array_object = typed_array_record.typed_array.into_object_value();

        Ok(ArrayIterator::new(cx, typed_array_object, ArrayIteratorKind::KeyAndValue).as_value())
    }

    /// %TypedArray%.prototype.every (https://tc39.es/ecma262/#sec-%typedarray%.prototype.every)
    pub fn every(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::from(i));
            let arguments = [value, index_value, object.into()];

            let test_result = call_object(cx, callback_function, this_arg, &arguments)?;
            if !to_boolean(test_result.get()) {
                return Ok(cx.bool(false));
            }
        }

        Ok(cx.bool(true))
    }

    /// %TypedArray%.prototype.fill (https://tc39.es/ecma262/#sec-%typedarray%.prototype.fill)
    pub fn fill(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let value_arg = get_argument(cx, arguments, 0);
        let value = match typed_array.content_type() {
            ContentType::Number => to_number(cx, value_arg)?,
            ContentType::BigInt => to_bigint(cx, value_arg)?.into(),
        };

        let start_arg = get_argument(cx, arguments, 1);
        let relative_start = to_integer_or_infinity(cx, start_arg)?;
        let start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_start as i64, 0) as u64
            }
        } else {
            u64::min(relative_start as u64, length)
        };

        let end_argument = get_argument(cx, arguments, 2);
        let end_index = if !end_argument.is_undefined() {
            let relative_end = to_integer_or_infinity(cx, end_argument)?;

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

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return type_error(cx, "typed array is out of bounds");
        }

        let end_index = u64::min(end_index, typed_array_length(&typed_array_record) as u64);

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..end_index {
            key.replace(PropertyKey::from_u64(cx, i));
            must!(set(cx, object, key, value, true));
        }

        Ok(object.as_value())
    }

    /// %TypedArray%.prototype.filter (https://tc39.es/ecma262/#sec-%typedarray%.prototype.filter)
    pub fn filter(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let mut kept_values = vec![];

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        // First collect all values that pass the predicate
        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            let value = get(cx, object, index_key)?;

            index_value.replace(Value::from(i));
            let arguments = [value, index_value, object.into()];

            let is_selected = call_object(cx, callback_function, this_arg, &arguments)?;

            if to_boolean(is_selected.get()) {
                kept_values.push(value)
            }
        }

        // Then create a new array that contains the kept values
        let num_kept_values = kept_values.len();
        let num_kept_values_value = Value::from(num_kept_values).to_handle(cx);
        let array = typed_array_species_create_object(cx, typed_array, &[num_kept_values_value])?;

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);

        for (i, value) in kept_values.into_iter().enumerate() {
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            must!(set(cx, array, index_key, value, true));
        }

        Ok(array.as_value())
    }

    /// %TypedArray%.prototype.find (https://tc39.es/ecma262/#sec-%typedarray%.prototype.find)
    pub fn find(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let predicate_function = get_argument(cx, arguments, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "TypedArray.prototype.find expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let find_result = find_via_predicate(cx, object, 0..length, predicate_function, this_arg)?;

        match find_result {
            Some((value, _)) => Ok(value),
            None => Ok(cx.undefined()),
        }
    }

    /// %TypedArray%.prototype.findIndex (https://tc39.es/ecma262/#sec-%typedarray%.prototype.findindex)
    pub fn find_index(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let predicate_function = get_argument(cx, arguments, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "TypedArray.prototype.findIndex expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let find_result = find_via_predicate(cx, object, 0..length, predicate_function, this_arg)?;

        match find_result {
            Some((_, index_value)) => Ok(index_value),
            None => Ok(cx.negative_one()),
        }
    }

    /// %TypedArray%.prototype.findLast (https://tc39.es/ecma262/#sec-%typedarray%.prototype.findlast)
    pub fn find_last(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let predicate_function = get_argument(cx, arguments, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "TypedArray.prototype.findLast expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let find_result =
            find_via_predicate(cx, object, (0..length).rev(), predicate_function, this_arg)?;

        match find_result {
            Some((value, _)) => Ok(value),
            None => Ok(cx.undefined()),
        }
    }

    /// %TypedArray%.prototype.findLastIndex (https://tc39.es/ecma262/#sec-%typedarray%.prototype.findlastindex)
    pub fn find_last_index(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let predicate_function = get_argument(cx, arguments, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "TypedArray.prototype.findLastIndex expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let find_result =
            find_via_predicate(cx, object, (0..length).rev(), predicate_function, this_arg)?;

        match find_result {
            Some((_, index_value)) => Ok(index_value),
            None => Ok(cx.negative_one()),
        }
    }

    /// %TypedArray%.prototype.forEach (https://tc39.es/ecma262/#sec-%typedarray%.prototype.foreach)
    pub fn for_each(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::from(i));
            let arguments = [value, index_value, object.into()];

            call_object(cx, callback_function, this_arg, &arguments)?;
        }

        Ok(cx.undefined())
    }

    /// %TypedArray%.prototype.includes (https://tc39.es/ecma262/#sec-%typedarray%.prototype.includes)
    pub fn includes(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        if length == 0 {
            return Ok(cx.bool(false));
        }

        let search_element = get_argument(cx, arguments, 0);

        let n_arg = get_argument(cx, arguments, 1);
        let mut n = to_integer_or_infinity(cx, n_arg)?;
        if n == f64::INFINITY {
            return Ok(cx.bool(false));
        } else if n == f64::NEG_INFINITY {
            n = 0.0;
        }

        let start_index = if n >= 0.0 {
            n as u64
        } else {
            i64::max(length as i64 + n as i64, 0) as u64
        };

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..length {
            key.replace(PropertyKey::from_u64(cx, i));
            let element = must!(get(cx, object, key));

            if same_value_zero(search_element, element) {
                return Ok(cx.bool(true));
            }
        }

        Ok(cx.bool(false))
    }

    /// %TypedArray%.prototype.indexOf (https://tc39.es/ecma262/#sec-%typedarray%.prototype.indexof)
    pub fn index_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        if length == 0 {
            return Ok(cx.negative_one());
        }

        let search_element = get_argument(cx, arguments, 0);

        let n_arg = get_argument(cx, arguments, 1);
        let mut n = to_integer_or_infinity(cx, n_arg)?;
        if n == f64::INFINITY {
            return Ok(cx.negative_one());
        } else if n == f64::NEG_INFINITY {
            n = 0.0;
        }

        let start_index = if n >= 0.0 {
            n as u64
        } else {
            i64::max(length as i64 + n as i64, 0) as u64
        };

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..length {
            key.replace(PropertyKey::from_u64(cx, i));
            if must!(has_property(cx, object, key)) {
                let element = must!(get(cx, object, key));
                if is_strictly_equal(search_element, element) {
                    return Ok(Value::from(i).to_handle(cx));
                }
            }
        }

        Ok(cx.negative_one())
    }

    /// %TypedArray%.prototype.join (https://tc39.es/ecma262/#sec-%typedarray%.prototype.join)
    pub fn join(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let separator = get_argument(cx, arguments, 0);
        let separator = if separator.is_undefined() {
            InternedStrings::get_str(cx, ",")
        } else {
            to_string(cx, separator)?
        };

        let mut joined = cx.names.empty_string().as_string();

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in 0..length {
            if i > 0 {
                joined = StringValue::concat(cx, joined, separator);
            }

            key.replace(PropertyKey::from_u64(cx, i as u64));
            let element = must!(get(cx, object, key));

            if !element.is_undefined() {
                let next = to_string(cx, element)?;
                joined = StringValue::concat(cx, joined, next);
            }
        }

        Ok(joined.into())
    }

    /// %TypedArray%.prototype.keys (https://tc39.es/ecma262/#sec-%typedarray%.prototype.keys)
    pub fn keys(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array_object = typed_array_record.typed_array.into_object_value();

        Ok(ArrayIterator::new(cx, typed_array_object, ArrayIteratorKind::Key).as_value())
    }

    /// %TypedArray%.prototype.lastIndexOf (https://tc39.es/ecma262/#sec-%typedarray%.prototype.lastindexof)
    pub fn last_index_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        if length == 0 {
            return Ok(cx.negative_one());
        }

        let search_element = get_argument(cx, arguments, 0);

        let start_index = if arguments.len() >= 2 {
            let start_arg = get_argument(cx, arguments, 1);
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
            key.replace(PropertyKey::from_u64(cx, i));
            if must!(has_property(cx, object, key)) {
                let element = must!(get(cx, object, key));
                if is_strictly_equal(search_element, element) {
                    return Ok(Value::from(i).to_handle(cx));
                }
            }
        }

        Ok(cx.negative_one())
    }

    /// get %TypedArray%.prototype.length (https://tc39.es/ecma262/#sec-get-%typedarray%.prototype.length)
    pub fn length(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array = require_typed_array(cx, this_value)?;

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return Ok(cx.zero());
        }

        let length = typed_array_length(&typed_array_record);

        Ok(Value::from(length).to_handle(cx))
    }

    /// %TypedArray%.prototype.map (https://tc39.es/ecma262/#sec-%typedarray%.prototype.map)
    pub fn map(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let length_value = Value::from(length).to_handle(cx);
        let array = typed_array_species_create_object(cx, typed_array, &[length_value])?;

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::from(i));
            let arguments = [value, index_value, object.into()];

            let mapped_value = call_object(cx, callback_function, this_arg, &arguments)?;
            set(cx, array, index_key, mapped_value, true)?;
        }

        Ok(array.as_value())
    }

    /// %TypedArray%.prototype.reduce (https://tc39.es/ecma262/#sec-%typedarray%.prototype.reduce)
    pub fn reduce(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let mut initial_index = 0;

        let mut accumulator = if arguments.len() >= 2 {
            get_argument(cx, arguments, 1)
        } else if length == 0 {
            return type_error(cx, "reduce does not have initial value");
        } else {
            initial_index = 1;
            let first_index_key = PropertyKey::array_index(cx, 0).to_handle(cx);
            must!(get(cx, object, first_index_key))
        };

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in initial_index..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::from(i));
            let arguments = [accumulator, value, index_value, object.into()];

            accumulator = call_object(cx, callback_function, cx.undefined(), &arguments)?;
        }

        Ok(accumulator)
    }

    /// %TypedArray%.prototype.reduceRight (https://tc39.es/ecma262/#sec-%typedarray%.prototype.reduceright)
    pub fn reduce_right(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let mut initial_index = length as i64 - 1;

        let mut accumulator = if arguments.len() >= 2 {
            get_argument(cx, arguments, 1)
        } else if length == 0 {
            return type_error(cx, "reduceRight does not have initial value");
        } else {
            let last_index_key = PropertyKey::from_u64(cx, initial_index as u64).to_handle(cx);
            initial_index -= 1;
            must!(get(cx, object, last_index_key))
        };

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in (0..=initial_index).rev() {
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::from(i));
            let arguments = [accumulator, value, index_value, object.into()];

            accumulator = call_object(cx, callback_function, cx.undefined(), &arguments)?;
        }

        Ok(accumulator)
    }

    /// %TypedArray%.prototype.reverse (https://tc39.es/ecma262/#sec-%typedarray%.prototype.reverse)
    pub fn reverse(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
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
            lower_key.replace(PropertyKey::from_u64(cx, lower));
            upper_key.replace(PropertyKey::from_u64(cx, upper));

            let lower_value = must!(get(cx, object, lower_key));
            let upper_value = must!(get(cx, object, upper_key));

            must!(set(cx, object, lower_key, upper_value, true));
            must!(set(cx, object, upper_key, lower_value, true));

            lower += 1;
            upper -= 1;
        }

        Ok(object.as_value())
    }

    /// %TypedArray%.prototype.set (https://tc39.es/ecma262/#sec-%typedarray%.prototype.set)
    pub fn set(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let offset_arg = get_argument(cx, arguments, 1);
        let offset = to_integer_or_infinity(cx, offset_arg)?;
        if offset < 0.0 {
            return range_error(cx, "TypedArray.prototype.set offset is negative");
        }

        let source_arg = get_argument(cx, arguments, 0);
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
    }

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
            return type_error(cx, "typed array is out of bounds");
        }
        let target_length = typed_array_length(&target_record) as u64;

        let mut source_buffer = source.viewed_array_buffer();

        let source_record = make_typed_array_with_buffer_witness_record(source);
        if is_typed_array_out_of_bounds(&source_record) {
            return type_error(cx, "typed array is out of bounds");
        }
        let source_length = typed_array_length(&source_record);

        let target_byte_offset = target.byte_offset();
        let target_element_size = target.element_size();

        let source_byte_offset = source.byte_offset();
        let source_element_size = source.element_size();

        if target_offset == f64::INFINITY
            || source_length as u64 + target_offset as u64 > target_length
        {
            return range_error(cx, "TypedArray.prototype.set offset is out of range");
        }

        let source_byte_index =
            if same_object_value(source_buffer.get_().into(), target_buffer.get_().into()) {
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
            let mut from_ptr = source_buffer.data().as_mut_ptr().add(source_byte_index);
            let mut to_ptr = target_buffer.data().as_mut_ptr().add(target_byte_index);
            let limit_ptr = target_buffer.data().as_mut_ptr().add(limit);

            let mut from_byte_index = source_byte_index;
            let mut to_byte_index = target_byte_index;

            if source.kind() != target.kind() {
                // If types are different then can access bytes directly but must convert
                while to_byte_index < limit {
                    // Convert between types. May allocate but does not invoke user code.
                    let element_value =
                        source.read_element_value(cx, source_buffer.get_(), from_byte_index);

                    target.write_element_value(cx, to_byte_index, element_value)?;

                    from_byte_index += source_element_size;
                    to_byte_index += target_element_size;
                }
            } else {
                // Otherwse copy bytes directly instead of performing any conversions
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
            return type_error(cx, "typed array is out of bounds");
        }

        let target_length = typed_array_length(&target_record) as u64;

        let source = to_object(cx, source)?;
        let source_length = length_of_array_like(cx, source)?;

        if offset == f64::INFINITY || source_length + offset as u64 > target_length {
            return range_error(cx, "TypedArray.prototype.set offset is out of range");
        }
        let offset = offset as u64;

        // Keys are shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in 0..source_length {
            key.replace(PropertyKey::from_u64(cx, i));
            let value = get(cx, source, key)?;

            let target_index = offset + i;

            target.write_element_value_unchecked(cx, target_index, value)?;
        }

        Ok(())
    }

    /// %TypedArray%.prototype.slice (https://tc39.es/ecma262/#sec-%typedarray%.prototype.slice)
    pub fn slice(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let start_arg = get_argument(cx, arguments, 0);
        let relative_start = to_integer_or_infinity(cx, start_arg)?;
        let start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_start as i64, 0) as u64
            }
        } else {
            u64::min(relative_start as u64, length)
        };

        let end_argument = get_argument(cx, arguments, 1);
        let end_index = if !end_argument.is_undefined() {
            let relative_end = to_integer_or_infinity(cx, end_argument)?;

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

        let count = end_index.saturating_sub(start_index);
        let count_value = Value::from(count).to_handle(cx);
        let new_typed_array = typed_array_species_create(cx, typed_array, &[count_value])?;
        let array = new_typed_array.into_object_value();

        if count == 0 {
            return Ok(array.as_value());
        }

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return type_error(cx, "typed array is out of bounds");
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
                from_key.replace(PropertyKey::from_u64(cx, current_index));
                to_key.replace(PropertyKey::from_u64(cx, i));

                let value = get(cx, object, from_key)?;
                set(cx, array, to_key, value, true)?;

                current_index += 1;
            }
        } else {
            // Otherwse copy bytes directly instead of performing any conversions
            let mut source_buffer = typed_array.viewed_array_buffer();
            let mut target_buffer = new_typed_array.viewed_array_buffer();
            let element_size = typed_array.element_size();

            let source_byte_offset = typed_array.byte_offset();
            let source_byte_index = (start_index as usize) * element_size + source_byte_offset;
            let target_byte_index = new_typed_array.byte_offset();

            unsafe {
                let mut from_ptr = source_buffer.data().as_mut_ptr().add(source_byte_index);
                let mut to_ptr = target_buffer.data().as_mut_ptr().add(target_byte_index);

                for _ in 0..(count as usize * element_size) {
                    let byte = from_ptr.read();
                    to_ptr.write(byte);

                    from_ptr = from_ptr.add(1);
                    to_ptr = to_ptr.add(1);
                }
            }
        }

        Ok(array.as_value())
    }

    /// %TypedArray%.prototype.some (https://tc39.es/ecma262/#sec-%typedarray%.prototype.some)
    pub fn some(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::from(i));
            let arguments = [value, index_value, object.into()];

            let test_result = call_object(cx, callback_function, this_arg, &arguments)?;
            if to_boolean(test_result.get()) {
                return Ok(cx.bool(true));
            }
        }

        Ok(cx.bool(false))
    }

    /// %TypedArray%.prototype.sort (https://tc39.es/ecma262/#sec-%typedarray%.prototype.sort)
    pub fn sort(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let compare_function_arg = get_argument(cx, arguments, 0);
        if !compare_function_arg.is_undefined() && !is_callable(compare_function_arg) {
            return type_error(cx, "Sort comparator must be a function");
        };

        let typed_array_record = validate_typed_array(cx, this_value)?;
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
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            set(cx, object, index_key, *value, true)?;
        }

        Ok(object.as_value())
    }

    /// %TypedArray%.prototype.subarray (https://tc39.es/ecma262/#sec-%typedarray%.prototype.subarray)
    pub fn subarray(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array = require_typed_array(cx, this_value)?;
        let buffer = typed_array.viewed_array_buffer();

        let source_record = make_typed_array_with_buffer_witness_record(typed_array);
        let source_length = if is_typed_array_out_of_bounds(&source_record) {
            0
        } else {
            typed_array_length(&source_record) as u64
        };

        let start_arg = get_argument(cx, arguments, 0);
        let relative_start = to_integer_or_infinity(cx, start_arg)?;
        let start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(source_length as i64 + relative_start as i64, 0) as u64
            }
        } else {
            u64::min(relative_start as u64, source_length)
        };

        let end_argument = get_argument(cx, arguments, 1);
        let end_index = if !end_argument.is_undefined() {
            let relative_end = to_integer_or_infinity(cx, end_argument)?;

            if relative_end < 0.0 {
                if relative_end == f64::NEG_INFINITY {
                    0
                } else {
                    i64::max(source_length as i64 + relative_end as i64, 0) as u64
                }
            } else {
                u64::min(relative_end as u64, source_length)
            }
        } else {
            source_length
        };

        let new_length = end_index.saturating_sub(start_index);

        let element_size = typed_array.element_size();
        let source_byte_offset = typed_array.byte_offset();
        let begin_byte_offset = source_byte_offset + (start_index as usize) * element_size;
        let begin_byte_offset_value = Value::from(begin_byte_offset).to_handle(cx);

        let subarray = if typed_array.array_length().is_none() && end_argument.is_undefined() {
            typed_array_species_create_object(
                cx,
                typed_array,
                &[buffer.into(), begin_byte_offset_value],
            )?
        } else {
            let new_length_value = Value::from(new_length).to_handle(cx);
            typed_array_species_create_object(
                cx,
                typed_array,
                &[buffer.into(), begin_byte_offset_value, new_length_value],
            )?
        };

        Ok(subarray.as_value())
    }

    /// %TypedArray%.prototype.toLocaleString (https://tc39.es/ecma262/#sec-%typedarray%.prototype.tolocalestring)
    pub fn to_locale_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let mut result = cx.names.empty_string().as_string();
        let separator = InternedStrings::get_str(cx, ",");

        for i in 0..length {
            if i > 0 {
                result = StringValue::concat(cx, result, separator);
            }

            let key = PropertyKey::from_u64(cx, i as u64).to_handle(cx);
            let next_element = must!(get(cx, object, key));

            if !next_element.is_nullish() {
                let string_result = invoke(cx, next_element, cx.names.to_locale_string(), &[])?;
                let string_result = to_string(cx, string_result)?;

                result = StringValue::concat(cx, result, string_result);
            }
        }

        Ok(result.into())
    }

    /// %TypedArray%.prototype.toReversed (https://tc39.es/ecma262/#sec-%typedarray%.prototype.toreversed)
    pub fn to_reversed(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let array = typed_array_create_same_type(cx, typed_array, length)?;

        // Keys are shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);
        let mut to_key = PropertyKey::uninit().to_handle(cx);

        for i in 0..length {
            from_key.replace(PropertyKey::from_u64(cx, length - i - 1));
            to_key.replace(PropertyKey::from_u64(cx, i));

            let value = must!(get(cx, object, from_key));
            must!(set(cx, array, to_key, value, true));
        }

        Ok(array.as_value())
    }

    /// %TypedArray%.prototype.toSorted (https://tc39.es/ecma262/#sec-%typedarray%.prototype.tosorted)
    pub fn to_sorted(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let compare_function_arg = get_argument(cx, arguments, 0);
        if !compare_function_arg.is_undefined() && !is_callable(compare_function_arg) {
            return type_error(cx, "Sort comparator must be a function");
        };

        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let sorted_array = typed_array_create_same_type(cx, typed_array, length)?;

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
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            set(cx, sorted_array, index_key, *value, true)?;
        }

        Ok(sorted_array.as_value())
    }

    /// %TypedArray%.prototype.values (https://tc39.es/ecma262/#sec-%typedarray%.prototype.values)
    pub fn values(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array_object = typed_array_record.typed_array.into_object_value();

        Ok(ArrayIterator::new(cx, typed_array_object, ArrayIteratorKind::Value).as_value())
    }

    /// %TypedArray%.prototype.with (https://tc39.es/ecma262/#sec-%typedarray%.prototype.with)
    pub fn with(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = validate_typed_array(cx, this_value)?;
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let index_arg = get_argument(cx, arguments, 0);
        let relative_index = to_integer_or_infinity(cx, index_arg)?;

        // Convert from relative to actual index, making sure index is in range
        let actual_index = if relative_index >= 0.0 {
            relative_index
        } else {
            relative_index + length as f64
        };

        // Convert new value to correct typed
        let new_value = get_argument(cx, arguments, 1);
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

        let array = typed_array_create_same_type(cx, typed_array, length as u64)?;

        // Key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in 0..(length as u64) {
            key.replace(PropertyKey::from_u64(cx, i));

            // Replace the i'th value with the new value
            let value = if i == actual_index {
                new_value
            } else {
                must!(get(cx, object, key))
            };

            must!(set(cx, array, key, value, true));
        }

        Ok(array.as_value())
    }

    /// get %TypedArray%.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-get-%typedarray%.prototype-%symbol.tostringtag%)
    pub fn get_to_string_tag(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return Ok(cx.undefined());
        }

        let this_object = this_value.as_object();
        if !this_object.is_typed_array() {
            return Ok(cx.undefined());
        }

        Ok(this_object.as_typed_array().name(cx).into())
    }
}

#[macro_export]
macro_rules! create_typed_array_prototype {
    ($typed_array:ident, $rust_name:ident, $element_type:ident, $prototype:ident, $constructor:ident) => {
        pub struct $prototype;

        impl $prototype {
            /// Properties of the TypedArray Prototype Objects (https://tc39.es/ecma262/#sec-properties-of-typedarray-prototype-objects)
            pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
                let mut object = ObjectValue::new(
                    cx,
                    Some(realm.get_intrinsic(Intrinsic::TypedArrayPrototype)),
                    true,
                );

                // Constructor property is added once TypedArrayConstructor has been created
                let element_size_value = cx.smi(std::mem::size_of::<$element_type>() as i32);
                object.intrinsic_frozen_property(
                    cx,
                    cx.names.bytes_per_element(),
                    element_size_value,
                );

                object.into()
            }
        }
    };
}

#[inline]
fn require_typed_array(cx: Context, value: Handle<Value>) -> EvalResult<DynTypedArray> {
    if !value.is_object() {
        return type_error(cx, "expected typed array");
    }

    let object = value.as_object();
    if !object.is_typed_array() {
        return type_error(cx, "expected typed array");
    }

    Ok(object.as_typed_array())
}

/// TypedArraySpeciesCreate (https://tc39.es/ecma262/#typedarray-species-create)
fn typed_array_species_create_object(
    cx: Context,
    exemplar: DynTypedArray,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<ObjectValue>> {
    let result = typed_array_species_create(cx, exemplar, arguments)?;
    Ok(result.into_object_value())
}

fn typed_array_species_create(
    cx: Context,
    exemplar: DynTypedArray,
    arguments: &[Handle<Value>],
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
        TypedArrayKind::Float32Array => Intrinsic::Float32ArrayConstructor,
        TypedArrayKind::Float64Array => Intrinsic::Float64ArrayConstructor,
    };

    let constructor = species_constructor(cx, exemplar.into_object_value(), intrinsic)?;

    let result = typed_array_create_from_constructor(cx, constructor, arguments)?;

    if result.content_type() != exemplar.content_type() {
        return type_error(cx, "typed arrays must both contain either numbers or BigInts");
    }

    Ok(result)
}

/// TypedArrayCreateFromConstructor (https://tc39.es/ecma262/#sec-typedarraycreatefromconstructor)
pub fn typed_array_create_from_constructor_object(
    cx: Context,
    constructor: Handle<ObjectValue>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<ObjectValue>> {
    let result = typed_array_create_from_constructor(cx, constructor, arguments)?;
    Ok(result.into_object_value())
}

pub fn typed_array_create_from_constructor(
    cx: Context,
    constructor: Handle<ObjectValue>,
    arguments: &[Handle<Value>],
) -> EvalResult<DynTypedArray> {
    let new_typed_array = construct(cx, constructor, arguments, None)?;

    let new_typed_array_record = validate_typed_array(cx, new_typed_array.into())?;

    if arguments.len() == 1 && arguments[0].is_number() {
        if is_typed_array_out_of_bounds(&new_typed_array_record) {
            return type_error(cx, "typed array is out of bounds");
        }

        let new_typed_array_length = typed_array_length(&new_typed_array_record);

        if (new_typed_array_length as f64) < arguments[0].as_number() {
            return type_error(cx, "typed array does not have expected length");
        }
    }

    Ok(new_typed_array_record.typed_array)
}

/// TypedArrayCreateSameType (https://tc39.es/ecma262/#sec-typedarray-create-same-type)
fn typed_array_create_same_type(
    cx: Context,
    exemplar: DynTypedArray,
    length: u64,
) -> EvalResult<Handle<ObjectValue>> {
    let constructor_instrinsic = match exemplar.kind() {
        TypedArrayKind::Int8Array => Intrinsic::Int8ArrayConstructor,
        TypedArrayKind::UInt8Array => Intrinsic::UInt8ArrayConstructor,
        TypedArrayKind::UInt8ClampedArray => Intrinsic::UInt8ClampedArrayConstructor,
        TypedArrayKind::Int16Array => Intrinsic::Int16ArrayConstructor,
        TypedArrayKind::UInt16Array => Intrinsic::UInt16ArrayConstructor,
        TypedArrayKind::Int32Array => Intrinsic::Int32ArrayConstructor,
        TypedArrayKind::UInt32Array => Intrinsic::UInt32ArrayConstructor,
        TypedArrayKind::BigInt64Array => Intrinsic::BigInt64ArrayConstructor,
        TypedArrayKind::BigUInt64Array => Intrinsic::BigUInt64ArrayConstructor,
        TypedArrayKind::Float32Array => Intrinsic::Float32ArrayConstructor,
        TypedArrayKind::Float64Array => Intrinsic::Float64ArrayConstructor,
    };

    let constructor = cx.get_intrinsic(constructor_instrinsic);
    let length_value = Value::from(length).to_handle(cx);

    typed_array_create_from_constructor_object(cx, constructor, &[length_value])
}

/// ValidateTypedArray (https://tc39.es/ecma262/#sec-validatetypedarray)
#[inline]
fn validate_typed_array(
    cx: Context,
    value: Handle<Value>,
) -> EvalResult<TypedArrayWithBufferWitnessRecord> {
    let typed_array = require_typed_array(cx, value)?;
    let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);

    if is_typed_array_out_of_bounds(&typed_array_record) {
        return type_error(cx, "typed array is out of bounds");
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

        // Covert from positive/negative/equal number result to Ordering
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
