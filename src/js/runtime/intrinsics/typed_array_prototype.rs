use crate::{
    js::runtime::{
        abstract_operations::{
            call_object, construct, has_property, invoke, set, species_constructor,
        },
        builtin_function::BuiltinFunction,
        error::type_error_,
        function::get_argument,
        get,
        interned_strings::InternedStrings,
        intrinsics::array_iterator::{ArrayIterator, ArrayIteratorKind},
        object_value::ObjectValue,
        property::Property,
        string_value::StringValue,
        to_string,
        type_utilities::{
            is_callable, is_strictly_equal, same_value_zero, to_bigint, to_boolean,
            to_integer_or_infinity, to_number,
        },
        Context, EvalResult, Gc, PropertyKey, Realm, Value,
    },
    maybe, must,
};

use super::{
    intrinsics::Intrinsic,
    typed_array::{ContentType, DynTypedArray, TypedArrayKind},
};

pub struct TypedArrayPrototype;

impl TypedArrayPrototype {
    // 23.2.3 Properties of the %TypedArray% Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once TypedArrayConstructor has been created

        // Create values function as it is referenced by multiple properties
        let values_function = BuiltinFunction::create(
            cx,
            Self::values,
            0,
            cx.names.values(),
            Some(realm),
            None,
            None,
        )
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
        object.intrinsic_func(cx, cx.names.slice(), Self::slice, 2, realm);
        object.intrinsic_func(cx, cx.names.some(), Self::some, 1, realm);
        object.intrinsic_func(cx, cx.names.subarray(), Self::subarray, 2, realm);
        object.intrinsic_func(cx, cx.names.to_locale_string(), Self::to_locale_string, 0, realm);
        // Use Array.prototype.toString directly
        object.intrinsic_data_prop(
            cx,
            cx.names.to_string(),
            realm
                .get_intrinsic(Intrinsic::ArrayPrototypeToString)
                .into(),
        );
        object.intrinsic_data_prop(cx, cx.names.values(), values_function);

        // 23.2.3.32 %TypedArray%.prototype [ @@iterator ]
        let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
        object.set_property(cx, iterator_key, Property::data(values_function, true, false, true));

        // 23.2.3.33 get %TypedArray%.prototype [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        object.intrinsic_getter(cx, to_string_tag_key, Self::get_to_string_tag, realm);

        object.into()
    }

    // 23.2.3.1 %TypedArray%.prototype.at
    fn at(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length();

        let relative_index = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 0)));

        let key = if relative_index >= 0.0 {
            if relative_index >= length as f64 {
                return Value::undefined().into();
            }

            PropertyKey::from_u64(cx, relative_index as u64)
        } else {
            if -relative_index > length as f64 {
                return Value::undefined().into();
            }

            PropertyKey::from_u64(cx, (length as i64 + relative_index as i64) as u64)
        };

        get(cx, object, key)
    }

    // 23.2.3.2 get %TypedArray%.prototype.buffer
    fn buffer(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(require_typed_array(cx, this_value));
        typed_array.viewed_array_buffer().into()
    }

    // 23.2.3.3 get %TypedArray%.prototype.byteLength
    fn byte_length(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(require_typed_array(cx, this_value));

        if typed_array.viewed_array_buffer().is_detached() {
            return Value::smi(0).into();
        }

        Value::from(typed_array.byte_length()).into()
    }

    // 23.2.3.4 get %TypedArray%.prototype.byteOffset
    fn byte_offset(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(require_typed_array(cx, this_value));

        if typed_array.viewed_array_buffer().is_detached() {
            return Value::smi(0).into();
        }

        Value::from(typed_array.byte_offset()).into()
    }

    // 23.2.3.6 %TypedArray%.prototype.copyWithin
    fn copy_within(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length() as u64;

        let relative_target = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 0)));
        let to_index = if relative_target < 0.0 {
            if relative_target == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_target as i64, 0) as u64
            }
        } else {
            u64::min(relative_target as u64, length)
        };

        let relative_start = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 1)));
        let from_start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_start as i64, 0) as u64
            }
        } else {
            u64::min(relative_start as u64, length)
        };

        let end_argument = get_argument(arguments, 2);
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

        let to_byte_index = to_index * element_size + byte_offset;
        let from_byte_index = from_start_index * element_size + byte_offset;
        let mut count_bytes = count as u64 * element_size;

        let mut array_buffer = typed_array.viewed_array_buffer();
        if array_buffer.is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        let data_ptr = array_buffer.data().as_mut_ptr();

        // Copy bytes one at a time from from_ptr to to_ptr
        unsafe {
            if from_byte_index < to_byte_index && to_byte_index < from_byte_index + count_bytes {
                let mut from_ptr = data_ptr.add((from_byte_index + count_bytes - 1) as usize);
                let mut to_ptr = data_ptr.add((to_byte_index + count_bytes - 1) as usize);

                while count_bytes > 0 {
                    let byte = from_ptr.read();
                    to_ptr.write(byte);

                    from_ptr = from_ptr.sub(1);
                    to_ptr = to_ptr.sub(1);
                    count_bytes -= 1;
                }
            } else {
                let mut from_ptr = data_ptr.add(from_byte_index as usize);
                let mut to_ptr = data_ptr.add(to_byte_index as usize);

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
    fn entries(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value)).into_object_value();
        ArrayIterator::new(cx, typed_array, ArrayIteratorKind::KeyAndValue).into()
    }

    // 23.2.3.8 %TypedArray%.prototype.every
    fn every(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length();

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(arguments, 1);

        for i in 0..length {
            let index_key = PropertyKey::from_u64(cx, i as u64);
            let value = must!(get(cx, object, index_key));

            let index_value = Value::from(i);
            let arguments = [value, index_value, object.into()];

            let test_result = maybe!(call_object(cx, callback_function, this_arg, &arguments));
            if !to_boolean(test_result) {
                return false.into();
            }
        }

        true.into()
    }

    // 23.2.3.9 %TypedArray%.prototype.fill
    fn fill(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length() as u64;

        let value = match typed_array.content_type() {
            ContentType::Number => maybe!(to_number(cx, get_argument(arguments, 0))),
            ContentType::BigInt => maybe!(to_bigint(cx, get_argument(arguments, 0))).into(),
        };

        let relative_start = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 1)));
        let start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_start as i64, 0) as u64
            }
        } else {
            u64::min(relative_start as u64, length)
        };

        let end_argument = get_argument(arguments, 2);
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

        if typed_array.viewed_array_buffer().is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        for i in start_index..end_index {
            let key = PropertyKey::from_u64(cx, i);
            must!(set(cx, object, key, value, true));
        }

        object.into()
    }

    // 23.2.3.10 %TypedArray%.prototype.filter
    fn filter(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length() as u64;

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(arguments, 1);

        let mut kept_values = vec![];

        // First collect all values that pass the predicate
        for i in 0..length {
            let index_key = PropertyKey::from_u64(cx, i);
            let value = maybe!(get(cx, object, index_key));

            let index_value = Value::from(i);
            let arguments = [value, index_value, object.into()];

            let is_selected = maybe!(call_object(cx, callback_function, this_arg, &arguments));

            if to_boolean(is_selected) {
                kept_values.push(value)
            }
        }

        // Then create a new array that contains the kept values
        let num_kept_values = kept_values.len();
        let array = maybe!(typed_array_species_create_object(
            cx,
            typed_array,
            &[Value::from(num_kept_values)],
            Some(num_kept_values)
        ));

        for (i, value) in kept_values.into_iter().enumerate() {
            let index_key = PropertyKey::from_u64(cx, i as u64);
            must!(set(cx, array, index_key, value, true));
        }

        array.into()
    }

    // 23.2.3.11 %TypedArray%.prototype.find
    fn find(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length();

        let predicate_function = get_argument(arguments, 0);
        if !is_callable(predicate_function) {
            return type_error_(cx, "expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(arguments, 1);

        for i in 0..length {
            let index_key = PropertyKey::from_u64(cx, i as u64);
            let value = must!(get(cx, object, index_key));

            let index_value = Value::from(i);
            let arguments = [value, index_value, object.into()];

            let test_result = maybe!(call_object(cx, predicate_function, this_arg, &arguments));
            if to_boolean(test_result) {
                return value.into();
            }
        }

        Value::undefined().into()
    }

    // 23.2.3.12 %TypedArray%.prototype.findIndex
    fn find_index(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length();

        let predicate_function = get_argument(arguments, 0);
        if !is_callable(predicate_function) {
            return type_error_(cx, "expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(arguments, 1);

        for i in 0..length {
            let index_key = PropertyKey::from_u64(cx, i as u64);
            let value = must!(get(cx, object, index_key));

            let index_value = Value::from(i);
            let arguments = [value, index_value, object.into()];

            let test_result = maybe!(call_object(cx, predicate_function, this_arg, &arguments));
            if to_boolean(test_result) {
                return index_value.into();
            }
        }

        Value::number(-1.0).into()
    }

    // 23.2.3.13 %TypedArray%.prototype.forEach
    fn for_each(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length();

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(arguments, 1);

        for i in 0..length {
            let index_key = PropertyKey::from_u64(cx, i as u64);
            let value = must!(get(cx, object, index_key));

            let index_value = Value::from(i);
            let arguments = [value, index_value, object.into()];

            maybe!(call_object(cx, callback_function, this_arg, &arguments));
        }

        Value::undefined().into()
    }

    // 23.2.3.14 %TypedArray%.prototype.includes
    fn includes(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length() as u64;

        if length == 0 {
            return false.into();
        }

        let search_element = get_argument(arguments, 0);

        let mut n = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 1)));
        if n == f64::INFINITY {
            return false.into();
        } else if n == f64::NEG_INFINITY {
            n = 0.0;
        }

        let start_index = if n >= 0.0 {
            n as u64
        } else {
            i64::max(length as i64 + n as i64, 0) as u64
        };

        for i in start_index..length {
            let key = PropertyKey::from_u64(cx, i);
            let element = must!(get(cx, object, key));

            if same_value_zero(search_element, element) {
                return true.into();
            }
        }

        false.into()
    }

    // 23.2.3.15 %TypedArray%.prototype.indexOf
    fn index_of(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length() as u64;

        if length == 0 {
            return Value::smi(-1).into();
        }

        let search_element = get_argument(arguments, 0);

        let mut n = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 1)));
        if n == f64::INFINITY {
            return Value::smi(-1).into();
        } else if n == f64::NEG_INFINITY {
            n = 0.0;
        }

        let start_index = if n >= 0.0 {
            n as u64
        } else {
            i64::max(length as i64 + n as i64, 0) as u64
        };

        for i in start_index..length {
            let key = PropertyKey::from_u64(cx, i);
            if must!(has_property(cx, object, key)) {
                let element = must!(get(cx, object, key));
                if is_strictly_equal(search_element, element) {
                    return Value::from(i).into();
                }
            }
        }

        Value::smi(-1).into()
    }

    // 23.2.3.16 %TypedArray%.prototype.join
    fn join(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length();

        let separator = get_argument(arguments, 0);
        let separator = if separator.is_undefined() {
            InternedStrings::get_str(cx, ",")
        } else {
            maybe!(to_string(cx, separator))
        };

        let mut joined = cx.names.empty_string().as_string();

        for i in 0..length {
            if i > 0 {
                joined = StringValue::concat(cx, joined, separator);
            }

            let key = PropertyKey::from_u64(cx, i as u64);
            let element = must!(get(cx, object, key));

            if !element.is_undefined() {
                let next = maybe!(to_string(cx, element));
                joined = StringValue::concat(cx, joined, next);
            }
        }

        joined.into()
    }

    // 23.2.3.17 %TypedArray%.prototype.keys
    fn keys(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value)).into_object_value();
        ArrayIterator::new(cx, typed_array, ArrayIteratorKind::Key).into()
    }

    // 23.2.3.18 %TypedArray%.prototype.lastIndexOf
    fn last_index_of(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length() as u64;

        if length == 0 {
            return Value::smi(-1).into();
        }

        let search_element = get_argument(arguments, 0);

        let start_index = if arguments.len() >= 2 {
            let n = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 1)));
            if n == f64::NEG_INFINITY {
                return Value::smi(-1).into();
            }

            if n >= 0.0 {
                u64::min(n as u64, length - 1)
            } else {
                let start_index = length as i64 + n as i64;

                if start_index < 0 {
                    return Value::smi(-1).into();
                }

                start_index as u64
            }
        } else {
            length - 1
        };

        for i in (0..=start_index).rev() {
            let key = PropertyKey::from_u64(cx, i);
            if must!(has_property(cx, object, key)) {
                let element = must!(get(cx, object, key));
                if is_strictly_equal(search_element, element) {
                    return Value::from(i).into();
                }
            }
        }

        Value::smi(-1).into()
    }

    // 23.2.3.19 get %TypedArray%.prototype.length
    fn length(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(require_typed_array(cx, this_value));

        if typed_array.viewed_array_buffer().is_detached() {
            return Value::smi(0).into();
        }

        Value::from(typed_array.array_length()).into()
    }

    // 23.2.3.20 %TypedArray%.prototype.map
    fn map(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length();

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(arguments, 1);

        let array = maybe!(typed_array_species_create_object(
            cx,
            typed_array,
            &[Value::from(length)],
            Some(length)
        ));

        for i in 0..length {
            let index_key = PropertyKey::from_u64(cx, i as u64);
            let value = must!(get(cx, object, index_key));

            let index_value = Value::from(i);
            let arguments = [value, index_value, object.into()];

            let mapped_value = maybe!(call_object(cx, callback_function, this_arg, &arguments));
            maybe!(set(cx, array, index_key, mapped_value, true));
        }

        array.into()
    }

    // 23.2.3.21 %TypedArray%.prototype.reduce
    fn reduce(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length() as u64;

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let mut initial_index = 0;

        let mut accumulator = if arguments.len() >= 2 {
            get_argument(arguments, 1)
        } else if length == 0 {
            return type_error_(cx, "reduce does not have initial value");
        } else {
            initial_index = 1;
            let first_index_key = PropertyKey::array_index(cx, 0);
            must!(get(cx, object, first_index_key))
        };

        for i in initial_index..length {
            let index_key = PropertyKey::from_u64(cx, i);
            let value = must!(get(cx, object, index_key));

            let index_value = Value::from(i);
            let arguments = [accumulator, value, index_value, object.into()];

            accumulator =
                maybe!(call_object(cx, callback_function, Value::undefined(), &arguments));
        }

        accumulator.into()
    }

    // 23.2.3.22 %TypedArray%.prototype.reduceRight
    fn reduce_right(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length() as u64;

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let mut initial_index = length as i64 - 1;

        let mut accumulator = if arguments.len() >= 2 {
            get_argument(arguments, 1)
        } else if length == 0 {
            return type_error_(cx, "reduceRight does not have initial value");
        } else {
            let last_index_key = PropertyKey::from_u64(cx, initial_index as u64);
            initial_index -= 1;
            must!(get(cx, object, last_index_key))
        };

        for i in (0..=initial_index).rev() {
            let index_key = PropertyKey::from_u64(cx, i as u64);
            let value = must!(get(cx, object, index_key));

            let index_value = Value::from(i);
            let arguments = [accumulator, value, index_value, object.into()];

            accumulator =
                maybe!(call_object(cx, callback_function, Value::undefined(), &arguments));
        }

        accumulator.into()
    }

    // 23.2.3.23 %TypedArray%.prototype.reverse
    fn reverse(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length() as u64;

        let middle = length / 2;
        let mut lower = 0;
        // Safe to wrap as this only occurs when length is 0 and loop will be skipped
        let mut upper = length.wrapping_sub(1);

        while lower != middle {
            let lower_key = PropertyKey::from_u64(cx, lower);
            let upper_key = PropertyKey::from_u64(cx, upper);

            let lower_value = must!(get(cx, object, lower_key));
            let upper_value = must!(get(cx, object, upper_key));

            must!(set(cx, object, lower_key, upper_value, true));
            must!(set(cx, object, upper_key, lower_value, true));

            lower += 1;
            upper -= 1;
        }

        object.into()
    }

    // 23.2.3.25 %TypedArray%.prototype.slice
    fn slice(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length() as u64;

        let relative_start = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 0)));
        let start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_start as i64, 0) as u64
            }
        } else {
            u64::min(relative_start as u64, length)
        };

        let end_argument = get_argument(arguments, 1);
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
        let new_typed_array = maybe!(typed_array_species_create(
            cx,
            typed_array,
            &[Value::from(count)],
            Some(count as usize)
        ));
        let array = new_typed_array.into_object_value();

        if count == 0 {
            return array.into();
        }

        let array_buffer = typed_array.viewed_array_buffer();
        if array_buffer.is_detached() {
            return type_error_(cx, "array buffer is detached");
        }

        // If types are different then must call get and set and convert types
        if typed_array.kind() != new_typed_array.kind() {
            let mut current_index = start_index;
            for i in 0..count {
                let from_key = PropertyKey::from_u64(cx, current_index);
                let to_key = PropertyKey::from_u64(cx, i);

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

    // 23.2.3.26 %TypedArray%.prototype.some
    fn some(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length();

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(arguments, 1);

        for i in 0..length {
            let index_key = PropertyKey::from_u64(cx, i as u64);
            let value = must!(get(cx, object, index_key));

            let index_value = Value::from(i);
            let arguments = [value, index_value, object.into()];

            let test_result = maybe!(call_object(cx, callback_function, this_arg, &arguments));
            if to_boolean(test_result) {
                return true.into();
            }
        }

        false.into()
    }

    // 23.2.3.28 %TypedArray%.prototype.subarray
    fn subarray(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(require_typed_array(cx, this_value));
        let length = typed_array.array_length() as u64;
        let buffer = typed_array.viewed_array_buffer();

        let relative_start = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 0)));
        let start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_start as i64, 0) as u64
            }
        } else {
            u64::min(relative_start as u64, length)
        };

        let end_argument = get_argument(arguments, 1);
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

        let element_size = typed_array.element_size();
        let source_byte_offset = typed_array.byte_offset();
        let begin_byte_offset = source_byte_offset + (start_index as usize) * element_size;

        let subarray = maybe!(typed_array_species_create_object(
            cx,
            typed_array,
            &[
                buffer.into(),
                Value::from(begin_byte_offset),
                Value::from(new_length),
            ],
            None,
        ));

        subarray.into()
    }

    // 23.2.3.29 %TypedArray%.prototype.toLocaleString
    fn to_locale_string(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value));
        let object = typed_array.into_object_value();
        let length = typed_array.array_length();

        let mut result = cx.names.empty_string().as_string();
        let separator = InternedStrings::get_str(cx, ", ");

        for i in 0..length {
            if i > 0 {
                result = StringValue::concat(cx, result, separator);
            }

            let key = PropertyKey::from_u64(cx, i as u64);
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

    // 23.2.3.31 %TypedArray%.prototype.values
    fn values(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let typed_array = maybe!(validate_typed_array(cx, this_value)).into_object_value();
        ArrayIterator::new(cx, typed_array, ArrayIteratorKind::Value).into()
    }

    // 23.2.3.33 get %TypedArray%.prototype [ @@toStringTag ]
    pub fn get_to_string_tag(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if !this_value.is_object() {
            return Value::undefined().into();
        }

        let this_object = this_value.as_object();
        if !this_object.is_typed_array() {
            return Value::undefined().into();
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
            pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
                let mut object = ObjectValue::new(
                    cx,
                    Some(realm.get_intrinsic(Intrinsic::TypedArrayPrototype)),
                    true,
                );

                // Constructor property is added once TypedArrayConstructor has been created
                object.set_property(
                    cx,
                    cx.names.bytes_per_element(),
                    Property::data(
                        Value::smi(std::mem::size_of::<$element_type>() as i32),
                        false,
                        false,
                        false,
                    ),
                );

                object.into()
            }
        }
    };
}

#[inline]
fn require_typed_array(cx: &mut Context, value: Value) -> EvalResult<DynTypedArray> {
    if !value.is_object() {
        return type_error_(cx, "expected typed array");
    }

    let object = value.as_object();
    if !object.is_typed_array() {
        return type_error_(cx, "expected typed array");
    }

    object.as_typed_array().into()
}

// 23.2.4.1 TypedArraySpeciesCreate
fn typed_array_species_create_object(
    cx: &mut Context,
    exemplar: DynTypedArray,
    arguments: &[Value],
    length: Option<usize>,
) -> EvalResult<Gc<ObjectValue>> {
    let result = maybe!(typed_array_species_create(cx, exemplar, arguments, length));
    result.into_object_value().into()
}
fn typed_array_species_create(
    cx: &mut Context,
    exemplar: DynTypedArray,
    arguments: &[Value],
    length: Option<usize>,
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

    let default_constructor = cx.get_intrinsic(intrinsic);
    let constructor =
        maybe!(species_constructor(cx, exemplar.into_object_value(), default_constructor));

    let result = maybe!(typed_array_create(cx, constructor, arguments, length));

    if result.content_type() != exemplar.content_type() {
        return type_error_(cx, "typed arrays must both contain either numbers or BigInts");
    }

    result.into()
}

// 23.2.4.2 TypedArrayCreate
pub fn typed_array_create_object(
    cx: &mut Context,
    constructor: Gc<ObjectValue>,
    arguments: &[Value],
    length: Option<usize>,
) -> EvalResult<Gc<ObjectValue>> {
    let result = maybe!(typed_array_create(cx, constructor, arguments, length));
    result.into_object_value().into()
}

pub fn typed_array_create(
    cx: &mut Context,
    constructor: Gc<ObjectValue>,
    arguments: &[Value],
    length: Option<usize>,
) -> EvalResult<DynTypedArray> {
    let new_typed_array = maybe!(construct(cx, constructor, arguments, None));

    let new_typed_array = maybe!(validate_typed_array(cx, new_typed_array.into()));

    if let Some(length) = length {
        if new_typed_array.array_length() < length {
            return type_error_(cx, "typed array does not have expected length");
        }
    }

    new_typed_array.into()
}

// 23.2.4.3 ValidateTypedArray
#[inline]
fn validate_typed_array(cx: &mut Context, value: Value) -> EvalResult<DynTypedArray> {
    let typed_array = maybe!(require_typed_array(cx, value));

    if typed_array.viewed_array_buffer().is_detached() {
        return type_error_(cx, "array buffer is detached");
    }

    typed_array.into()
}
