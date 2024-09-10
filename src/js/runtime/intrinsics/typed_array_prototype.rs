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
    maybe, must,
};

use super::{
    array_prototype::{find_via_predicate, sort_indexed_properties, INCLUDE_HOLES, TYPED_ARRAY},
    intrinsics::Intrinsic,
    typed_array::{ContentType, DynTypedArray, TypedArrayKind},
};

pub struct TypedArrayPrototype;

impl TypedArrayPrototype {
    // 23.2.3 Properties of the %TypedArray% Prototype Object
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

        // 23.2.3.37 %TypedArray%.prototype [ @@iterator ]
        let iterator_key = cx.well_known_symbols.iterator();
        object.set_property(cx, iterator_key, Property::data(values_function, true, false, true));

        // 23.2.3.38 get %TypedArray%.prototype [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.intrinsic_getter(cx, to_string_tag_key, Self::get_to_string_tag, realm);

        object
    }

    // 23.2.3.1 %TypedArray%.prototype.at
    pub fn at(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let index_arg = get_argument(cx, arguments, 0);
        let relative_index = maybe!(to_integer_or_infinity(cx, index_arg));

        let key = if relative_index >= 0.0 {
            if relative_index >= length as f64 {
                return cx.undefined().into();
            }

            PropertyKey::from_u64(cx, relative_index as u64).to_handle(cx)
        } else {
            if -relative_index > length as f64 {
                return cx.undefined().into();
            }

            PropertyKey::from_u64(cx, (length as i64 + relative_index as i64) as u64).to_handle(cx)
        };

        get(cx, object, key)
    }

    // 23.2.3.2 get %TypedArray%.prototype.buffer
    pub fn buffer(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array = maybe!(require_typed_array(cx, this_value));
        typed_array.viewed_array_buffer().into()
    }

    // 23.2.3.3 get %TypedArray%.prototype.byteLength
    pub fn byte_length(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array = maybe!(require_typed_array(cx, this_value));

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return Value::smi(0).to_handle(cx).into();
        }

        let byte_length = typed_array_byte_length(&typed_array_record);

        Value::from(byte_length).to_handle(cx).into()
    }

    // 23.2.3.4 get %TypedArray%.prototype.byteOffset
    pub fn byte_offset(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array = maybe!(require_typed_array(cx, this_value));

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return Value::smi(0).to_handle(cx).into();
        }

        Value::from(typed_array.byte_offset()).to_handle(cx).into()
    }

    // 23.2.3.6 %TypedArray%.prototype.copyWithin
    pub fn copy_within(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let target_arg = get_argument(cx, arguments, 0);
        let relative_target = maybe!(to_integer_or_infinity(cx, target_arg));
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
        let relative_start = maybe!(to_integer_or_infinity(cx, start_arg));
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

        let count = i64::min(
            from_end_index as i64 - from_start_index as i64,
            length as i64 - to_index as i64,
        );
        if count <= 0 {
            return object.into();
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

        object.into()
    }

    // 23.2.3.7 %TypedArray%.prototype.entries
    pub fn entries(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array_object = typed_array_record.typed_array.into_object_value();

        ArrayIterator::new(cx, typed_array_object, ArrayIteratorKind::KeyAndValue).into()
    }

    // 23.2.3.8 %TypedArray%.prototype.every
    pub fn every(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
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
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::from(i));
            let arguments = [value, index_value, object.into()];

            let test_result = maybe!(call_object(cx, callback_function, this_arg, &arguments));
            if !to_boolean(test_result.get()) {
                return cx.bool(false).into();
            }
        }

        cx.bool(true).into()
    }

    // 23.2.3.9 %TypedArray%.prototype.fill
    pub fn fill(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let value_arg = get_argument(cx, arguments, 0);
        let value = match typed_array.content_type() {
            ContentType::Number => maybe!(to_number(cx, value_arg)),
            ContentType::BigInt => maybe!(to_bigint(cx, value_arg)).into(),
        };

        let start_arg = get_argument(cx, arguments, 1);
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

        let end_argument = get_argument(cx, arguments, 2);
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

        object.into()
    }

    // 23.2.3.10 %TypedArray%.prototype.filter
    pub fn filter(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
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
            let value = maybe!(get(cx, object, index_key));

            index_value.replace(Value::from(i));
            let arguments = [value, index_value, object.into()];

            let is_selected = maybe!(call_object(cx, callback_function, this_arg, &arguments));

            if to_boolean(is_selected.get()) {
                kept_values.push(value)
            }
        }

        // Then create a new array that contains the kept values
        let num_kept_values = kept_values.len();
        let num_kept_values_value = Value::from(num_kept_values).to_handle(cx);
        let array =
            maybe!(typed_array_species_create_object(cx, typed_array, &[num_kept_values_value],));

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);

        for (i, value) in kept_values.into_iter().enumerate() {
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            must!(set(cx, array, index_key, value, true));
        }

        array.into()
    }

    // 23.2.3.11 %TypedArray%.prototype.find
    pub fn find(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let predicate_function = get_argument(cx, arguments, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "TypedArray.prototype.find expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let find_result =
            maybe!(find_via_predicate(cx, object, 0..length, predicate_function, this_arg));

        match find_result {
            Some((value, _)) => value.into(),
            None => cx.undefined().into(),
        }
    }

    // 23.2.3.12 %TypedArray%.prototype.findIndex
    pub fn find_index(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let predicate_function = get_argument(cx, arguments, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "TypedArray.prototype.findIndex expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let find_result =
            maybe!(find_via_predicate(cx, object, 0..length, predicate_function, this_arg));

        match find_result {
            Some((_, index_value)) => index_value.into(),
            None => Value::smi(-1).to_handle(cx).into(),
        }
    }

    // 23.2.3.13 %TypedArray%.prototype.findLast
    pub fn find_last(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
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
            maybe!(find_via_predicate(cx, object, (0..length).rev(), predicate_function, this_arg));

        match find_result {
            Some((value, _)) => value.into(),
            None => cx.undefined().into(),
        }
    }

    // 23.2.3.14 %TypedArray%.prototype.findLastIndex
    pub fn find_last_index(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
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
            maybe!(find_via_predicate(cx, object, (0..length).rev(), predicate_function, this_arg));

        match find_result {
            Some((_, index_value)) => index_value.into(),
            None => Value::smi(-1).to_handle(cx).into(),
        }
    }

    // 23.2.3.15 %TypedArray%.prototype.forEach
    pub fn for_each(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
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
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::from(i));
            let arguments = [value, index_value, object.into()];

            maybe!(call_object(cx, callback_function, this_arg, &arguments));
        }

        cx.undefined().into()
    }

    // 23.2.3.16 %TypedArray%.prototype.includes
    pub fn includes(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        if length == 0 {
            return cx.bool(false).into();
        }

        let search_element = get_argument(cx, arguments, 0);

        let n_arg = get_argument(cx, arguments, 1);
        let mut n = maybe!(to_integer_or_infinity(cx, n_arg));
        if n == f64::INFINITY {
            return cx.bool(false).into();
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
                return cx.bool(true).into();
            }
        }

        cx.bool(false).into()
    }

    // 23.2.3.17 %TypedArray%.prototype.indexOf
    pub fn index_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        if length == 0 {
            return Value::smi(-1).to_handle(cx).into();
        }

        let search_element = get_argument(cx, arguments, 0);

        let n_arg = get_argument(cx, arguments, 1);
        let mut n = maybe!(to_integer_or_infinity(cx, n_arg));
        if n == f64::INFINITY {
            return Value::smi(-1).to_handle(cx).into();
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
                    return Value::from(i).to_handle(cx).into();
                }
            }
        }

        Value::smi(-1).to_handle(cx).into()
    }

    // 23.2.3.18 %TypedArray%.prototype.join
    pub fn join(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let separator = get_argument(cx, arguments, 0);
        let separator = if separator.is_undefined() {
            InternedStrings::get_str(cx, ",")
        } else {
            maybe!(to_string(cx, separator))
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
                let next = maybe!(to_string(cx, element));
                joined = StringValue::concat(cx, joined, next);
            }
        }

        joined.into()
    }

    // 23.2.3.19 %TypedArray%.prototype.keys
    pub fn keys(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array_object = typed_array_record.typed_array.into_object_value();

        ArrayIterator::new(cx, typed_array_object, ArrayIteratorKind::Key).into()
    }

    // 23.2.3.20 %TypedArray%.prototype.lastIndexOf
    pub fn last_index_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        if length == 0 {
            return Value::smi(-1).to_handle(cx).into();
        }

        let search_element = get_argument(cx, arguments, 0);

        let start_index = if arguments.len() >= 2 {
            let start_arg = get_argument(cx, arguments, 1);
            let n = maybe!(to_integer_or_infinity(cx, start_arg));
            if n == f64::NEG_INFINITY {
                return Value::smi(-1).to_handle(cx).into();
            }

            if n >= 0.0 {
                u64::min(n as u64, length - 1)
            } else {
                let start_index = length as i64 + n as i64;

                if start_index < 0 {
                    return Value::smi(-1).to_handle(cx).into();
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
                    return Value::from(i).to_handle(cx).into();
                }
            }
        }

        Value::smi(-1).to_handle(cx).into()
    }

    // 23.2.3.21 get %TypedArray%.prototype.length
    pub fn length(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array = maybe!(require_typed_array(cx, this_value));

        let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);
        if is_typed_array_out_of_bounds(&typed_array_record) {
            return Value::smi(0).to_handle(cx).into();
        }

        let length = typed_array_length(&typed_array_record);

        Value::from(length).to_handle(cx).into()
    }

    // 23.2.3.22 %TypedArray%.prototype.map
    pub fn map(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
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
        let array = maybe!(typed_array_species_create_object(cx, typed_array, &[length_value]));

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            let value = must!(get(cx, object, index_key));

            index_value.replace(Value::from(i));
            let arguments = [value, index_value, object.into()];

            let mapped_value = maybe!(call_object(cx, callback_function, this_arg, &arguments));
            maybe!(set(cx, array, index_key, mapped_value, true));
        }

        array.into()
    }

    // 23.2.3.23 %TypedArray%.prototype.reduce
    pub fn reduce(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
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

            accumulator = maybe!(call_object(cx, callback_function, cx.undefined(), &arguments));
        }

        accumulator.into()
    }

    // 23.2.3.24 %TypedArray%.prototype.reduceRight
    pub fn reduce_right(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
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

            accumulator = maybe!(call_object(cx, callback_function, cx.undefined(), &arguments));
        }

        accumulator.into()
    }

    // 23.2.3.25 %TypedArray%.prototype.reverse
    pub fn reverse(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
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

        object.into()
    }

    // 23.2.3.26 %TypedArray%.prototype.set
    pub fn set(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let offset_arg = get_argument(cx, arguments, 1);
        let offset = maybe!(to_integer_or_infinity(cx, offset_arg));
        if offset < 0.0 {
            return range_error(cx, "TypedArray.prototype.set offset is negative");
        }

        let source_arg = get_argument(cx, arguments, 0);
        if source_arg.is_object() && source_arg.as_object().is_typed_array() {
            maybe!(Self::set_typed_array_from_typed_array(
                cx,
                typed_array,
                offset,
                source_arg.as_object().as_typed_array()
            ));
        } else {
            maybe!(Self::set_typed_array_from_array_like(cx, typed_array, offset, source_arg));
        }

        cx.undefined().into()
    }

    // 23.2.3.26.1 SetTypedArrayFromTypedArray
    pub fn set_typed_array_from_typed_array(
        cx: Context,
        target: DynTypedArray,
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
                source_buffer = maybe!(clone_array_buffer(
                    cx,
                    source_buffer,
                    source_byte_offset,
                    source_byte_length,
                ));
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

            if source.kind() != target.kind() {
                // If types are different then can access bytes directly but must convert
                while to_ptr < limit_ptr {
                    let value = source.read_element_ptr(cx, from_ptr);
                    target.write_element_ptr(cx, to_ptr, value);

                    from_ptr = from_ptr.add(source_element_size);
                    to_ptr = to_ptr.add(target_element_size);
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

        ().into()
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

        let source = maybe!(to_object(cx, source));
        let source_length = maybe!(length_of_array_like(cx, source));

        if offset == f64::INFINITY || source_length + offset as u64 > target_length {
            return range_error(cx, "TypedArray.prototype.set offset is out of range");
        }
        let offset = offset as u64;

        // Keys are shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in 0..source_length {
            key.replace(PropertyKey::from_u64(cx, i));
            let value = maybe!(get(cx, source, key));

            let target_index = offset + i;

            maybe!(target.write_element_value_unchecked(cx, target_index, value));
        }

        ().into()
    }

    // 23.2.3.27 %TypedArray%.prototype.slice
    pub fn slice(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

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

        let count = end_index.saturating_sub(start_index);
        let count_value = Value::from(count).to_handle(cx);
        let new_typed_array = maybe!(typed_array_species_create(cx, typed_array, &[count_value],));
        let array = new_typed_array.into_object_value();

        if count == 0 {
            return array.into();
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

                let value = maybe!(get(cx, object, from_key));
                maybe!(set(cx, array, to_key, value, true));

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

        array.into()
    }

    // 23.2.3.28 %TypedArray%.prototype.some
    pub fn some(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
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

            let test_result = maybe!(call_object(cx, callback_function, this_arg, &arguments));
            if to_boolean(test_result.get()) {
                return cx.bool(true).into();
            }
        }

        cx.bool(false).into()
    }

    // 23.2.3.29 %TypedArray%.prototype.sort
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

        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let sorted_values = maybe!(sort_indexed_properties::<INCLUDE_HOLES, TYPED_ARRAY>(
            cx,
            object,
            length,
            compare_function_arg
        ));

        // Reuse handle between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);

        // Copy sorted values into array
        for (i, value) in sorted_values.iter().enumerate() {
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            maybe!(set(cx, object, index_key, *value, true));
        }

        object.into()
    }

    // 23.2.3.30 %TypedArray%.prototype.subarray
    pub fn subarray(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array = maybe!(require_typed_array(cx, this_value));
        let buffer = typed_array.viewed_array_buffer();

        let source_record = make_typed_array_with_buffer_witness_record(typed_array);
        let source_length = if is_typed_array_out_of_bounds(&source_record) {
            0
        } else {
            typed_array_length(&source_record) as u64
        };

        let start_arg = get_argument(cx, arguments, 0);
        let relative_start = maybe!(to_integer_or_infinity(cx, start_arg));
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
            let relative_end = maybe!(to_integer_or_infinity(cx, end_argument));

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
            maybe!(typed_array_species_create_object(
                cx,
                typed_array,
                &[buffer.into(), begin_byte_offset_value],
            ))
        } else {
            let new_length_value = Value::from(new_length).to_handle(cx);
            maybe!(typed_array_species_create_object(
                cx,
                typed_array,
                &[buffer.into(), begin_byte_offset_value, new_length_value],
            ))
        };

        subarray.into()
    }

    // 23.2.3.31 %TypedArray%.prototype.toLocaleString
    pub fn to_locale_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
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
                let string_result =
                    maybe!(invoke(cx, next_element, cx.names.to_locale_string(), &[]));
                let string_result = maybe!(to_string(cx, string_result));

                result = StringValue::concat(cx, result, string_result);
            }
        }
        result.into()
    }

    // 23.2.3.32 %TypedArray%.prototype.toReversed
    pub fn to_reversed(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let array = maybe!(typed_array_create_same_type(cx, typed_array, length));

        // Keys are shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);
        let mut to_key = PropertyKey::uninit().to_handle(cx);

        for i in 0..length {
            from_key.replace(PropertyKey::from_u64(cx, length - i - 1));
            to_key.replace(PropertyKey::from_u64(cx, i));

            let value = must!(get(cx, object, from_key));
            must!(set(cx, array, to_key, value, true));
        }

        array.into()
    }

    // 23.2.3.33 %TypedArray%.prototype.toSorted
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

        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record) as u64;

        let sorted_array = maybe!(typed_array_create_same_type(cx, typed_array, length));

        let sorted_values = maybe!(sort_indexed_properties::<INCLUDE_HOLES, TYPED_ARRAY>(
            cx,
            object,
            length,
            compare_function_arg
        ));

        // Reuse handle between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);

        // Copy sorted values into array
        for (i, value) in sorted_values.iter().enumerate() {
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            maybe!(set(cx, sorted_array, index_key, *value, true));
        }

        sorted_array.into()
    }

    // 23.2.3.35 %TypedArray%.prototype.values
    pub fn values(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array_object = typed_array_record.typed_array.into_object_value();

        ArrayIterator::new(cx, typed_array_object, ArrayIteratorKind::Value).into()
    }

    // 23.2.3.36 %TypedArray%.prototype.with
    pub fn with(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let typed_array_record = maybe!(validate_typed_array(cx, this_value));
        let typed_array = typed_array_record.typed_array;

        let object = typed_array.into_object_value();
        let length = typed_array_length(&typed_array_record);

        let index_arg = get_argument(cx, arguments, 0);
        let relative_index = maybe!(to_integer_or_infinity(cx, index_arg));

        // Convert from relative to actual index, making sure index is in range
        let actual_index = if relative_index >= 0.0 {
            if relative_index >= length as f64 {
                return range_error(cx, "TypedArray.prototype.with index is out of range");
            }

            relative_index as u64
        } else {
            let actual_index = relative_index + length as f64;
            if actual_index < 0.0 {
                return range_error(cx, "TypedArray.prototype.with index is out of range");
            }

            actual_index as u64
        };

        // Convert new value to correct typed
        let new_value = get_argument(cx, arguments, 1);
        let new_value = match typed_array.content_type() {
            ContentType::BigInt => maybe!(to_bigint(cx, new_value)).into(),
            ContentType::Number => maybe!(to_number(cx, new_value)),
        };

        let array = maybe!(typed_array_create_same_type(cx, typed_array, length as u64));

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

        array.into()
    }

    // 23.2.3.38 get %TypedArray%.prototype [ @@toStringTag ]
    pub fn get_to_string_tag(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return cx.undefined().into();
        }

        let this_object = this_value.as_object();
        if !this_object.is_typed_array() {
            return cx.undefined().into();
        }

        this_object.as_typed_array().name(cx).into()
    }
}

#[macro_export]
macro_rules! create_typed_array_prototype {
    ($typed_array:ident, $rust_name:ident, $element_type:ident, $prototype:ident, $constructor:ident) => {
        pub struct $prototype;

        impl $prototype {
            // 23.2.7 Properties of the TypedArray Prototype Objects
            pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
                let mut object = ObjectValue::new(
                    cx,
                    Some(realm.get_intrinsic(Intrinsic::TypedArrayPrototype)),
                    true,
                );

                // Constructor property is added once TypedArrayConstructor has been created
                let element_size_value =
                    Value::smi(std::mem::size_of::<$element_type>() as i32).to_handle(cx);
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

    object.as_typed_array().into()
}

// 23.2.4.1 TypedArraySpeciesCreate
fn typed_array_species_create_object(
    cx: Context,
    exemplar: DynTypedArray,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<ObjectValue>> {
    let result = maybe!(typed_array_species_create(cx, exemplar, arguments));
    result.into_object_value().into()
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

    let constructor = maybe!(species_constructor(cx, exemplar.into_object_value(), intrinsic));

    let result = maybe!(typed_array_create_from_constructor(cx, constructor, arguments));

    if result.content_type() != exemplar.content_type() {
        return type_error(cx, "typed arrays must both contain either numbers or BigInts");
    }

    result.into()
}

// 23.2.4.2 TypedArrayCreateFromConstructor
pub fn typed_array_create_from_constructor_object(
    cx: Context,
    constructor: Handle<ObjectValue>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<ObjectValue>> {
    let result = maybe!(typed_array_create_from_constructor(cx, constructor, arguments));
    result.into_object_value().into()
}

pub fn typed_array_create_from_constructor(
    cx: Context,
    constructor: Handle<ObjectValue>,
    arguments: &[Handle<Value>],
) -> EvalResult<DynTypedArray> {
    let new_typed_array = maybe!(construct(cx, constructor, arguments, None));

    let new_typed_array_record = maybe!(validate_typed_array(cx, new_typed_array.into()));

    if arguments.len() == 1 && arguments[0].is_number() {
        if is_typed_array_out_of_bounds(&new_typed_array_record) {
            return type_error(cx, "typed array is out of bounds");
        }

        let new_typed_array_length = typed_array_length(&new_typed_array_record);

        if (new_typed_array_length as f64) < arguments[0].as_number() {
            return type_error(cx, "typed array does not have expected length");
        }
    }

    new_typed_array_record.typed_array.into()
}

// 23.2.4.3 TypedArrayCreateSameType
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

// 23.2.4.4 ValidateTypedArray
#[inline]
fn validate_typed_array(
    cx: Context,
    value: Handle<Value>,
) -> EvalResult<TypedArrayWithBufferWitnessRecord> {
    let typed_array = maybe!(require_typed_array(cx, value));
    let typed_array_record = make_typed_array_with_buffer_witness_record(typed_array);

    if is_typed_array_out_of_bounds(&typed_array_record) {
        return type_error(cx, "typed array is out of bounds");
    }

    typed_array_record.into()
}

pub struct TypedArrayWithBufferWitnessRecord {
    pub typed_array: DynTypedArray,
    pub cached_buffer_byte_length: Option<usize>,
}

// 10.4.5.9 MakeTypedArrayWithBufferWitnessRecord
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

// 10.4.5.11 TypedArrayByteLength
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

// 10.4.5.12 TypedArrayLength
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

// 10.4.5.13 IsTypedArrayOutOfBounds
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

// 23.2.4.7 CompareTypedArrayElements
//
// Will only be called on numbers and BigInts.
pub fn compare_typed_array_elements(
    cx: Context,
    v1: Handle<Value>,
    v2: Handle<Value>,
    compare_function: Handle<Value>,
) -> EvalResult<Ordering> {
    // Use the compare function if provided
    if !compare_function.is_undefined() {
        let result_value =
            maybe!(call_object(cx, compare_function.as_object(), cx.undefined(), &[v1, v2]));
        if result_value.is_nan() {
            return Ordering::Equal.into();
        }

        let result_number = maybe!(to_number(cx, result_value));
        let result_number = result_number.as_number();

        // Covert from positive/negative/equal number result to Ordering
        return if result_number == 0.0 {
            Ordering::Equal.into()
        } else if result_number < 0.0 {
            Ordering::Less.into()
        } else {
            Ordering::Greater.into()
        };
    }

    // Treat NaN as the lowest value
    let v1_is_nan = v1.is_nan();
    let v2_is_nan = v2.is_nan();
    if v1_is_nan && v2_is_nan {
        return Ordering::Equal.into();
    } else if v1_is_nan {
        return Ordering::Greater.into();
    } else if v2_is_nan {
        return Ordering::Less.into();
    }

    // Both values must have the same type - number or BigInt
    if v1.is_bigint() {
        debug_assert!(v2.is_bigint());

        let v1_bigint = v1.as_bigint().bigint();
        let v2_bigint = v2.as_bigint().bigint();

        if v1_bigint < v2_bigint {
            return Ordering::Less.into();
        } else if v1_bigint > v2_bigint {
            return Ordering::Greater.into();
        }

        if v1_bigint.magnitude().eq(&BigUint::default())
            && v2_bigint.magnitude().eq(&BigUint::default())
        {
            if v1_bigint.sign() == Sign::Minus && v2_bigint.sign() == Sign::Plus {
                return Ordering::Less.into();
            } else if v1_bigint.sign() == Sign::Plus && v2_bigint.sign() == Sign::Minus {
                return Ordering::Greater.into();
            }
        }
    } else {
        debug_assert!(v1.is_number() && v2.is_number());

        let v1_number = v1.as_number();
        let v2_number = v2.as_number();

        if v1_number < v2_number {
            return Ordering::Less.into();
        } else if v1_number > v2_number {
            return Ordering::Greater.into();
        }

        if v1_number == 0.0 && v2_number == 0.0 {
            if v1_number.is_sign_negative() && v2_number.is_sign_negative() {
                return Ordering::Less.into();
            } else if v1_number.is_sign_positive() && v2_number.is_sign_negative() {
                return Ordering::Greater.into();
            }
        }
    }

    Ordering::Equal.into()
}
