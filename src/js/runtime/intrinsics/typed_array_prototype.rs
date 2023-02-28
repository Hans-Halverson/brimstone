use crate::{
    js::runtime::{
        abstract_operations::{call_object, has_property, invoke, set},
        builtin_function::BuiltinFunction,
        error::type_error_,
        function::get_argument,
        get,
        intrinsics::array_iterator::{ArrayIterator, ArrayIteratorKind},
        object_value::{Object, ObjectValue},
        ordinary_object::OrdinaryObject,
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
    typed_array::{ContentType, TypedArray},
};

pub struct TypedArrayPrototype;

impl TypedArrayPrototype {
    // 23.2.3 Properties of the %TypedArray% Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Constructor property is added once TypedArrayConstructor has been created

        // Create values function as it is referenced by multiple properties
        let values_function = BuiltinFunction::create(
            cx,
            Self::values,
            0,
            &cx.names.values(),
            Some(realm),
            None,
            None,
        )
        .into();

        object.intrinsic_func(cx, &cx.names.at(), Self::at, 1, realm);
        object.intrinsic_getter(cx, &cx.names.buffer(), Self::buffer, realm);
        object.intrinsic_getter(cx, &cx.names.byte_length(), Self::byte_length, realm);
        object.intrinsic_getter(cx, &cx.names.byte_offset(), Self::byte_offset, realm);
        object.intrinsic_func(cx, &cx.names.copy_within(), Self::copy_within, 2, realm);
        object.intrinsic_func(cx, &cx.names.entries(), Self::entries, 0, realm);
        object.intrinsic_func(cx, &cx.names.every(), Self::every, 1, realm);
        object.intrinsic_func(cx, &cx.names.fill(), Self::fill, 1, realm);
        object.intrinsic_func(cx, &cx.names.find(), Self::find, 1, realm);
        object.intrinsic_func(cx, &cx.names.find_index(), Self::find_index, 1, realm);
        object.intrinsic_func(cx, &cx.names.for_each(), Self::for_each, 1, realm);
        object.intrinsic_func(cx, &cx.names.includes(), Self::includes, 1, realm);
        object.intrinsic_func(cx, &cx.names.index_of(), Self::index_of, 1, realm);
        object.intrinsic_func(cx, &cx.names.join(), Self::join, 1, realm);
        object.intrinsic_func(cx, &cx.names.keys(), Self::keys, 0, realm);
        object.intrinsic_func(cx, &cx.names.last_index_of(), Self::last_index_of, 1, realm);
        object.intrinsic_getter(cx, &cx.names.length(), Self::length, realm);
        object.intrinsic_func(cx, &cx.names.reduce(), Self::reduce, 1, realm);
        object.intrinsic_func(cx, &cx.names.reduce_right(), Self::reduce_right, 1, realm);
        object.intrinsic_func(cx, &cx.names.reverse(), Self::reverse, 0, realm);
        object.intrinsic_func(cx, &cx.names.some(), Self::some, 1, realm);
        object.intrinsic_func(cx, &cx.names.to_locale_string(), Self::to_locale_string, 0, realm);
        // Use Array.prototype.toString directly
        object.intrinsic_data_prop(
            &cx.names.to_string(),
            realm
                .get_intrinsic(Intrinsic::ArrayPrototypeToString)
                .into(),
        );
        object.intrinsic_data_prop(&cx.names.values(), values_function);

        // 23.2.3.32 %TypedArray%.prototype [ @@iterator ]
        let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
        object.set_property(&iterator_key, Property::data(values_function, true, false, true));

        // 23.2.3.33 get %TypedArray%.prototype [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        object.intrinsic_getter(cx, &to_string_tag_key, Self::get_to_string_tag, realm);

        cx.heap.alloc(object).into()
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

        get(cx, object, &key)
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
        let from_index = if relative_target < 0.0 {
            if relative_target == f64::NEG_INFINITY {
                0
            } else {
                u64::max(length + relative_target as u64, 0)
            }
        } else {
            u64::min(relative_target as u64, length)
        };

        let relative_start = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 1)));
        let to_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                u64::max(length + relative_start as u64, 0)
            }
        } else {
            u64::min(relative_start as u64, length)
        };

        let final_index = if arguments.len() >= 3 {
            let relative_end = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 2)));

            if relative_end < 0.0 {
                if relative_end == f64::NEG_INFINITY {
                    0
                } else {
                    u64::max(length + relative_end as u64, 0)
                }
            } else {
                u64::min(relative_end as u64, length)
            }
        } else {
            length
        };

        let count = u64::min(final_index - from_index, length - to_index);
        if count == 0 {
            return object.into();
        }

        let byte_offset = typed_array.byte_offset() as u64;
        let element_size = typed_array.element_size() as u64;

        let to_byte_index = to_index * element_size + byte_offset;
        let from_byte_index = from_index * element_size + byte_offset;
        let mut count_bytes = count * element_size;

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
            let value = must!(get(cx, object, &index_key));

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
                u64::max(length + relative_start as u64, 0)
            }
        } else {
            u64::min(relative_start as u64, length)
        };

        let end_index = if arguments.len() >= 3 {
            let relative_end = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 2)));

            if relative_end < 0.0 {
                if relative_end == f64::NEG_INFINITY {
                    0
                } else {
                    u64::max(length + relative_end as u64, 0)
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
            must!(set(cx, object, &key, value, true));
        }

        object.into()
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
            let value = must!(get(cx, object, &index_key));

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
            let value = must!(get(cx, object, &index_key));

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
            let value = must!(get(cx, object, &index_key));

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
            u64::max(length + n as u64, 0)
        };

        for i in start_index..length {
            let key = PropertyKey::from_u64(cx, i);
            let element = must!(get(cx, object, &key));

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
            u64::max(length + n as u64, 0)
        };

        for i in start_index..length {
            let key = PropertyKey::from_u64(cx, i);
            if must!(has_property(cx, object, &key)) {
                let element = must!(get(cx, object, &key));
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
            cx.get_interned_string(",")
        } else {
            maybe!(to_string(cx, separator))
        };

        let mut joined = cx.names.empty_string().as_string();

        for i in 0..length {
            if i > 0 {
                joined = StringValue::concat(cx, joined, separator);
            }

            let key = PropertyKey::from_u64(cx, i as u64);
            let element = must!(get(cx, object, &key));

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
                f64::min(n, (length - 1) as f64) as u64
            } else {
                length + n as u64
            }
        } else {
            length - 1
        };

        for i in (0..=start_index).rev() {
            let key = PropertyKey::from_u64(cx, i);
            if must!(has_property(cx, object, &key)) {
                let element = must!(get(cx, object, &key));
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
        } else {
            initial_index = 1;
            let first_index_key = PropertyKey::array_index(cx, 0);
            must!(get(cx, object, &first_index_key))
        };

        for i in initial_index..length {
            let index_key = PropertyKey::from_u64(cx, i);
            let value = must!(get(cx, object, &index_key));

            let index_value = Value::from(i);
            let arguments = [accumulator, value, index_value, object.into()];

            accumulator =
                maybe!(call_object(cx, callback_function, Value::undefined(), &arguments));
        }

        accumulator.into()
    }

    // 23.2.3.21 %TypedArray%.prototype.reduce
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
        } else {
            let last_index_key = PropertyKey::from_u64(cx, initial_index as u64);
            initial_index -= 1;
            must!(get(cx, object, &last_index_key))
        };

        for i in (0..=initial_index).rev() {
            let index_key = PropertyKey::from_u64(cx, i as u64);
            let value = must!(get(cx, object, &index_key));

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
        let mut upper = length - 1;

        while lower != middle {
            let lower_key = PropertyKey::from_u64(cx, lower);
            let upper_key = PropertyKey::from_u64(cx, upper);

            let lower_value = must!(get(cx, object, &lower_key));
            let upper_value = must!(get(cx, object, &upper_key));

            must!(set(cx, object, &lower_key, upper_value, true));
            must!(set(cx, object, &upper_key, lower_value, true));

            lower += 1;
            upper -= 1;
        }

        object.into()
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
            let value = must!(get(cx, object, &index_key));

            let index_value = Value::from(i);
            let arguments = [value, index_value, object.into()];

            let test_result = maybe!(call_object(cx, callback_function, this_arg, &arguments));
            if to_boolean(test_result) {
                return true.into();
            }
        }

        false.into()
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
        let separator = cx.get_interned_string(", ");

        for i in 0..length {
            if i > 0 {
                result = StringValue::concat(cx, result, separator);
            }

            let key = PropertyKey::from_u64(cx, i as u64);
            let next_element = must!(get(cx, object, &key));

            if !next_element.is_nullish() {
                let string_result =
                    maybe!(invoke(cx, next_element, &cx.names.to_locale_string(), &[]));
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
                let mut object = OrdinaryObject::new(
                    Some(realm.get_intrinsic(Intrinsic::TypedArrayPrototype)),
                    true,
                );

                // Constructor property is added once TypedArrayConstructor has been created
                object.set_property(
                    &cx.names.bytes_per_element(),
                    Property::data(
                        Value::smi(std::mem::size_of::<$element_type>() as i32),
                        false,
                        false,
                        false,
                    ),
                );

                cx.heap.alloc(object).into()
            }
        }
    };
}

#[inline]
fn require_typed_array(cx: &mut Context, value: Value) -> EvalResult<Gc<dyn TypedArray>> {
    if !value.is_object() {
        return type_error_(cx, "expected typed array");
    }

    let object = value.as_object();
    if !object.is_typed_array() {
        return type_error_(cx, "expected typed array");
    }

    object.as_typed_array().into()
}

// 23.2.4.3 ValidateTypedArray
#[inline]
fn validate_typed_array(cx: &mut Context, value: Value) -> EvalResult<Gc<dyn TypedArray>> {
    let typed_array = maybe!(require_typed_array(cx, value));

    if typed_array.viewed_array_buffer().is_detached() {
        return type_error_(cx, "array buffer is detached");
    }

    typed_array.into()
}
