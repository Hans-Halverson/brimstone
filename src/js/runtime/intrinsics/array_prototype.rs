use crate::{
    js::runtime::{
        abstract_operations::{
            call_object, create_data_property_or_throw, delete_property_or_throw, has_property,
            length_of_array_like, set,
        },
        array_object::{array_species_create, ArrayObject},
        builtin_function::BuiltinFunction,
        error::type_error_,
        function::get_argument,
        gc::Gc,
        get,
        numeric_constants::MAX_SAFE_INTEGER_U64,
        object_value::{Object, ObjectValue},
        ordinary_object::ordinary_object_create,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        to_string,
        type_utilities::{
            is_array, is_callable, is_strictly_equal, same_value_zero, to_boolean,
            to_integer_or_infinity, to_object,
        },
        Context, EvalResult, Value,
    },
    maybe,
};

use super::{
    array_iterator::{ArrayIterator, ArrayIteratorKind},
    intrinsics::Intrinsic,
};

pub struct ArrayPrototype;

impl ArrayPrototype {
    // 23.1.3 Properties of the Array Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let mut object = ordinary_object_create(object_proto);

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

        // Constructor property is added once ArrayConstructor has been created
        object.intrinsic_func(cx, &cx.names.at(), Self::at, 1, realm);
        object.intrinsic_func(cx, &cx.names.concat(), Self::concat, 1, realm);
        object.intrinsic_func(cx, &cx.names.copy_within(), Self::copy_within, 2, realm);
        object.intrinsic_func(cx, &cx.names.entries(), Self::entries, 0, realm);
        object.intrinsic_func(cx, &cx.names.every(), Self::every, 1, realm);
        object.intrinsic_func(cx, &cx.names.fill(), Self::fill, 1, realm);
        object.intrinsic_func(cx, &cx.names.filter(), Self::filter, 1, realm);
        object.intrinsic_func(cx, &cx.names.find(), Self::find, 1, realm);
        object.intrinsic_func(cx, &cx.names.find_index(), Self::find_index, 1, realm);
        object.intrinsic_func(cx, &cx.names.for_each(), Self::for_each, 1, realm);
        object.intrinsic_func(cx, &cx.names.includes(), Self::includes, 1, realm);
        object.intrinsic_func(cx, &cx.names.index_of(), Self::index_of, 1, realm);
        object.intrinsic_func(cx, &cx.names.join(), Self::join, 1, realm);
        object.intrinsic_func(cx, &cx.names.keys(), Self::keys, 0, realm);
        object.intrinsic_func(cx, &cx.names.last_index_of(), Self::last_index_of, 1, realm);
        object.intrinsic_func(cx, &cx.names.map(), Self::map, 1, realm);
        object.intrinsic_func(cx, &cx.names.pop(), Self::pop, 0, realm);
        object.intrinsic_func(cx, &cx.names.push(), Self::push, 0, realm);
        object.intrinsic_func(cx, &cx.names.reduce(), Self::reduce, 1, realm);
        object.intrinsic_func(cx, &cx.names.reduce_right(), Self::reduce_right, 1, realm);
        object.intrinsic_func(cx, &cx.names.reverse(), Self::reverse, 0, realm);
        object.intrinsic_func(cx, &cx.names.shift(), Self::shift, 0, realm);
        object.intrinsic_func(cx, &cx.names.some(), Self::some, 1, realm);
        object.intrinsic_func(cx, &cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_func(cx, &cx.names.unshift(), Self::unshift, 1, realm);
        object.intrinsic_data_prop(&cx.names.values(), values_function);

        // 23.1.3.34 Array.prototype [ @@iterator ]
        let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
        object.set_property(&iterator_key, Property::data(values_function, true, false, true));

        cx.heap.alloc(ArrayObject::new(object)).into()
    }

    // 23.1.3.1 Array.prototype.at
    fn at(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let relative_index = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 0)));

        let key = if relative_index >= 0.0 {
            if relative_index >= length as f64 {
                return Value::undefined().into();
            }

            PropertyKey::from_u64(relative_index as u64)
        } else {
            if -relative_index > length as f64 {
                return Value::undefined().into();
            }

            PropertyKey::from_u64((length as i64 + relative_index as i64) as u64)
        };

        get(cx, object, &key)
    }

    // 23.1.3.2 Array.prototype.concat
    fn concat(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let array = maybe!(array_species_create(cx, object, 0));

        let mut n = 0;

        Self::apply_concat_to_element(cx, object.into(), array, &mut n);

        for element in arguments {
            Self::apply_concat_to_element(cx, *element, array, &mut n);
        }

        array.into()
    }

    // 23.1.3.2.1 IsConcatSpreadable
    fn is_concat_spreadable(cx: &mut Context, object: Value) -> EvalResult<bool> {
        if !object.is_object() {
            return false.into();
        }

        let is_concat_spreadable_key =
            PropertyKey::symbol(cx.well_known_symbols.is_concat_spreadable);
        let is_spreadable = maybe!(get(cx, object.as_object(), &is_concat_spreadable_key));

        if !is_spreadable.is_undefined() {
            return to_boolean(is_spreadable).into();
        }

        is_array(cx, object)
    }

    #[inline]
    fn apply_concat_to_element(
        cx: &mut Context,
        element: Value,
        array: Gc<ObjectValue>,
        n: &mut u64,
    ) -> EvalResult<()> {
        if maybe!(Self::is_concat_spreadable(cx, element)) {
            let element = element.as_object();
            let length = maybe!(length_of_array_like(cx, element));

            if *n + length > MAX_SAFE_INTEGER_U64 {
                return type_error_(cx, "array is too large");
            }

            for i in 0..length {
                let element_index_key = PropertyKey::from_u64(i);

                if maybe!(has_property(cx, element, &element_index_key)) {
                    let sub_element = maybe!(get(cx, element, &element_index_key));
                    let array_index_key = PropertyKey::from_u64(*n);
                    maybe!(create_data_property_or_throw(cx, array, &array_index_key, sub_element))
                }

                *n += 1;
            }
        } else {
            if *n >= MAX_SAFE_INTEGER_U64 {
                return type_error_(cx, "array is too large");
            }

            let index_key = PropertyKey::from_u64(*n);
            maybe!(create_data_property_or_throw(cx, array, &index_key, element));

            *n += 1;
        }

        ().into()
    }

    // 23.1.3.4 Array.prototype.copyWithin
    fn copy_within(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let relative_target = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 0)));
        let mut from_index = if relative_target < 0.0 {
            if relative_target == f64::NEG_INFINITY {
                0
            } else {
                u64::max(length + relative_target as u64, 0)
            }
        } else {
            u64::min(relative_target as u64, length)
        };

        let relative_start = maybe!(to_integer_or_infinity(cx, get_argument(arguments, 1)));
        let mut to_index = if relative_start < 0.0 {
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

        let mut count = u64::min(final_index - from_index, length - to_index);

        if from_index < to_index && to_index < from_index + count {
            from_index = from_index + count - 1;
            to_index = to_index + count - 1;

            while count > 0 {
                let from_key = PropertyKey::from_u64(from_index);
                let to_key = PropertyKey::from_u64(to_index);

                if maybe!(has_property(cx, object, &from_key)) {
                    let from_value = maybe!(get(cx, object, &from_key));
                    maybe!(set(cx, object, &to_key, from_value, true));
                } else {
                    maybe!(delete_property_or_throw(cx, object, &to_key));
                }

                from_index -= 1;
                to_index -= 1;
                count -= 1;
            }
        } else {
            while count > 0 {
                let from_key = PropertyKey::from_u64(from_index);
                let to_key = PropertyKey::from_u64(to_index);

                if maybe!(has_property(cx, object, &from_key)) {
                    let from_value = maybe!(get(cx, object, &from_key));
                    maybe!(set(cx, object, &to_key, from_value, true));
                } else {
                    maybe!(delete_property_or_throw(cx, object, &to_key));
                }

                from_index += 1;
                to_index += 1;
                count -= 1;
            }
        }

        object.into()
    }

    // 23.1.3.5 Array.prototype.entries
    fn entries(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        ArrayIterator::new(cx, object, ArrayIteratorKind::KeyAndValue).into()
    }

    // 23.1.3.6 Array.prototype.every
    fn every(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(arguments, 1);

        for i in 0..length {
            let index_key = PropertyKey::from_u64(i);
            if maybe!(has_property(cx, object, &index_key)) {
                let value = maybe!(get(cx, object, &index_key));

                let index_value = Value::from_u64(i);
                let arguments = [value, index_value, object.into()];

                let test_result = maybe!(call_object(cx, callback_function, this_arg, &arguments));
                if !to_boolean(test_result) {
                    return false.into();
                }
            }
        }

        true.into()
    }

    // 23.1.3.7 Array.prototype.fill
    fn fill(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let value = get_argument(arguments, 0);

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

        for i in start_index..end_index {
            maybe!(set(cx, object, &PropertyKey::from_u64(i), value, true));
        }

        object.into()
    }

    // 23.1.3.8 Array.prototype.filter
    fn filter(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(arguments, 1);

        let array = maybe!(array_species_create(cx, object, 0));

        let mut result_index = 0;

        for i in 0..length {
            let index_key = PropertyKey::from_u64(i);
            if maybe!(has_property(cx, object, &index_key)) {
                let value = maybe!(get(cx, object, &index_key));

                let index_value = Value::from_u64(i);
                let arguments = [value, index_value, object.into()];

                let is_selected = maybe!(call_object(cx, callback_function, this_arg, &arguments));

                if to_boolean(is_selected) {
                    let result_index_key = PropertyKey::from_u64(result_index);
                    maybe!(create_data_property_or_throw(cx, array, &result_index_key, value));

                    result_index += 1;
                }
            }
        }

        array.into()
    }

    // 23.1.3.9 Array.prototype.find
    fn find(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let predicate_function = get_argument(arguments, 0);
        if !is_callable(predicate_function) {
            return type_error_(cx, "expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(arguments, 1);

        for i in 0..length {
            let index_key = PropertyKey::from_u64(i);
            if maybe!(has_property(cx, object, &index_key)) {
                let value = maybe!(get(cx, object, &index_key));

                let index_value = Value::from_u64(i);
                let arguments = [value, index_value, object.into()];

                let test_result = maybe!(call_object(cx, predicate_function, this_arg, &arguments));
                if to_boolean(test_result) {
                    return value.into();
                }
            }
        }

        Value::undefined().into()
    }

    // 23.1.3.10 Array.prototype.findIndex
    fn find_index(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let predicate_function = get_argument(arguments, 0);
        if !is_callable(predicate_function) {
            return type_error_(cx, "expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(arguments, 1);

        for i in 0..length {
            let index_key = PropertyKey::from_u64(i);
            if maybe!(has_property(cx, object, &index_key)) {
                let value = maybe!(get(cx, object, &index_key));

                let index_value = Value::from_u64(i);
                let arguments = [value, index_value, object.into()];

                let test_result = maybe!(call_object(cx, predicate_function, this_arg, &arguments));
                if to_boolean(test_result) {
                    return index_value.into();
                }
            }
        }

        Value::number(-1.0).into()
    }

    // 23.1.3.13 Array.prototype.forEach
    fn for_each(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(arguments, 1);

        for i in 0..length {
            let index_key = PropertyKey::from_u64(i);
            if maybe!(has_property(cx, object, &index_key)) {
                let value = maybe!(get(cx, object, &index_key));

                let index_value = Value::from_u64(i);
                let arguments = [value, index_value, object.into()];

                maybe!(call_object(cx, callback_function, this_arg, &arguments));
            }
        }

        Value::undefined().into()
    }

    // 23.1.3.14 Array.prototype.includes
    fn includes(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

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
            let key = PropertyKey::from_u64(i);
            let element = maybe!(get(cx, object, &key));

            if same_value_zero(search_element, element) {
                return true.into();
            }
        }

        false.into()
    }

    // 23.1.3.15 Array.prototype.indexOf
    fn index_of(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

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
            let key = PropertyKey::from_u64(i);
            if maybe!(has_property(cx, object, &key)) {
                let element = maybe!(get(cx, object, &key));
                if is_strictly_equal(search_element, element) {
                    return Value::from_u64(i).into();
                }
            }
        }

        Value::smi(-1).into()
    }

    // 23.1.3.16 Array.prototype.join
    fn join(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let separator = get_argument(arguments, 0);
        let separator = if separator.is_undefined() {
            cx.get_interned_string(",")
        } else {
            maybe!(to_string(cx, separator))
        };

        let mut joined = String::new();

        for i in 0..length {
            if i > 0 {
                joined.push_str(separator.str());
            }

            let element = maybe!(get(cx, object, &PropertyKey::from_u64(i)));

            if !element.is_nullish() {
                let next = maybe!(to_string(cx, element));
                joined.push_str(next.str());
            }
        }

        cx.heap.alloc_string(joined).into()
    }

    // 23.1.3.17 Array.prototype.keys
    fn keys(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        ArrayIterator::new(cx, object, ArrayIteratorKind::Key).into()
    }

    // 23.1.3.18 Array.prototype.lastIndexOf
    fn last_index_of(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

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
            let key = PropertyKey::from_u64(i);
            if maybe!(has_property(cx, object, &key)) {
                let element = maybe!(get(cx, object, &key));
                if is_strictly_equal(search_element, element) {
                    return Value::from_u64(i).into();
                }
            }
        }

        Value::smi(-1).into()
    }

    // 23.1.3.19 Array.prototype.map
    fn map(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(arguments, 1);

        let array = maybe!(array_species_create(cx, object, length));

        for i in 0..length {
            let index_key = PropertyKey::from_u64(i);
            if maybe!(has_property(cx, object, &index_key)) {
                let value = maybe!(get(cx, object, &index_key));

                let index_value = Value::from_u64(i);
                let arguments = [value, index_value, object.into()];

                let mapped_value = maybe!(call_object(cx, callback_function, this_arg, &arguments));
                maybe!(create_data_property_or_throw(cx, array, &index_key, mapped_value));
            }
        }

        array.into()
    }

    // 23.1.3.20 Array.prototype.pop
    fn pop(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        if length == 0 {
            maybe!(set(cx, object, &cx.names.length(), Value::smi(0), true));
            return Value::undefined().into();
        }

        let new_length = length - 1;
        let index_key = PropertyKey::from_u64(new_length);

        let element = maybe!(get(cx, object, &index_key));
        maybe!(delete_property_or_throw(cx, object, &index_key));

        let new_length_value = Value::from_u64(new_length);
        maybe!(set(cx, object, &cx.names.length(), new_length_value, true));

        element.into()
    }

    // 23.1.3.21 Array.prototype.push
    fn push(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let new_length = length + arguments.len() as u64;
        if new_length > MAX_SAFE_INTEGER_U64 {
            return type_error_(cx, "index is too large");
        }

        for (i, argument) in arguments.iter().enumerate() {
            let key = PropertyKey::from_u64(length + i as u64);
            maybe!(set(cx, object, &key, *argument, true));
        }

        let new_length_value = Value::from_u64(new_length);
        maybe!(set(cx, object, &cx.names.length(), new_length_value, true));

        new_length_value.into()
    }

    // 23.1.3.22 Array.prototype.reduce
    fn reduce(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let mut initial_index = 0;

        let mut accumulator = if arguments.len() >= 2 {
            get_argument(arguments, 1)
        } else {
            // Find the first value in the array if an initial value was not specified
            loop {
                if initial_index >= length {
                    return type_error_(cx, "reduce of empty array with no initial value");
                }

                let index_key = PropertyKey::from_u64(initial_index);
                initial_index += 1;

                if maybe!(has_property(cx, object, &index_key)) {
                    break maybe!(get(cx, object, &index_key));
                }
            }
        };

        for i in initial_index..length {
            let index_key = PropertyKey::from_u64(i);
            if maybe!(has_property(cx, object, &index_key)) {
                let value = maybe!(get(cx, object, &index_key));

                let index_value = Value::from_u64(i);
                let arguments = [accumulator, value, index_value, object.into()];

                accumulator =
                    maybe!(call_object(cx, callback_function, Value::undefined(), &arguments));
            }
        }

        accumulator.into()
    }

    // 23.1.3.23 Array.prototype.reduceRight
    fn reduce_right(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let mut initial_index = length as i64 - 1;

        let mut accumulator = if arguments.len() >= 2 {
            get_argument(arguments, 1)
        } else {
            // Find the first value in the array if an initial value was not specified
            loop {
                if initial_index < 0 {
                    return type_error_(cx, "reduce of empty array with no initial value");
                }

                let index_key = PropertyKey::from_u64(initial_index as u64);
                initial_index -= 1;

                if maybe!(has_property(cx, object, &index_key)) {
                    break maybe!(get(cx, object, &index_key));
                }
            }
        };

        for i in (0..=initial_index).rev() {
            let index_key = PropertyKey::from_u64(i as u64);
            if maybe!(has_property(cx, object, &index_key)) {
                let value = maybe!(get(cx, object, &index_key));

                let index_value = Value::from_u64(i as u64);
                let arguments = [accumulator, value, index_value, object.into()];

                accumulator =
                    maybe!(call_object(cx, callback_function, Value::undefined(), &arguments));
            }
        }

        accumulator.into()
    }

    // 23.1.3.24 Array.prototype.reverse
    fn reverse(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let middle = length / 2;
        let mut lower = 0;
        let mut upper = length - 1;

        while lower != middle {
            let lower_key = PropertyKey::from_u64(lower);
            let upper_key = PropertyKey::from_u64(upper);

            let lower_exists = maybe!(has_property(cx, object, &lower_key));
            let upper_exists = maybe!(has_property(cx, object, &upper_key));

            match (lower_exists, upper_exists) {
                (true, true) => {
                    let lower_value = maybe!(get(cx, object, &lower_key));
                    let upper_value = maybe!(get(cx, object, &upper_key));

                    maybe!(set(cx, object, &lower_key, upper_value, true));
                    maybe!(set(cx, object, &upper_key, lower_value, true));
                }
                (true, false) => {
                    let lower_value = maybe!(get(cx, object, &lower_key));

                    maybe!(delete_property_or_throw(cx, object, &lower_key));
                    maybe!(set(cx, object, &upper_key, lower_value, true));
                }
                (false, true) => {
                    let upper_value = maybe!(get(cx, object, &upper_key));

                    maybe!(set(cx, object, &lower_key, upper_value, true));
                    maybe!(delete_property_or_throw(cx, object, &upper_key));
                }
                (false, false) => {}
            }

            lower += 1;
            upper -= 1;
        }

        object.into()
    }

    // 23.1.3.25 Array.prototype.shift
    fn shift(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        if length == 0 {
            maybe!(set(cx, object, &cx.names.length(), Value::smi(0), true));
            return Value::undefined().into();
        }

        let first = maybe!(get(cx, object, &PropertyKey::array_index(0)));

        for i in 1..length {
            let from_key = PropertyKey::from_u64(i);
            let to_key = PropertyKey::from_u64(i - 1);

            if maybe!(has_property(cx, object, &from_key)) {
                let from_value = maybe!(get(cx, object, &from_key));
                maybe!(set(cx, object, &to_key, from_value, true));
            } else {
                maybe!(delete_property_or_throw(cx, object, &to_key));
            }
        }

        maybe!(delete_property_or_throw(cx, object, &PropertyKey::from_u64(length - 1)));
        maybe!(set(cx, object, &cx.names.length(), Value::from_u64(length - 1), true));

        first.into()
    }

    // 23.1.3.27 Array.prototype.some
    fn some(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(arguments, 1);

        for i in 0..length {
            let index_key = PropertyKey::from_u64(i);
            if maybe!(has_property(cx, object, &index_key)) {
                let value = maybe!(get(cx, object, &index_key));

                let index_value = Value::from_u64(i);
                let arguments = [value, index_value, object.into()];

                let test_result = maybe!(call_object(cx, callback_function, this_arg, &arguments));
                if to_boolean(test_result) {
                    return true.into();
                }
            }
        }

        false.into()
    }

    // 23.1.3.31 Array.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let array = maybe!(to_object(cx, this_value));
        let func = maybe!(get(cx, array, &cx.names.join()));

        let func = if is_callable(func) {
            func.as_object()
        } else {
            cx.current_realm()
                .get_intrinsic(Intrinsic::ObjectPrototypeToString)
        };

        call_object(cx, func, array.into(), &[])
    }

    // 23.1.3.32 Array.prototype.unshift
    fn unshift(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let num_arguments = arguments.len() as u64;
        if num_arguments > 0 {
            if length + num_arguments > MAX_SAFE_INTEGER_U64 {
                return type_error_(cx, "array is too large");
            }

            for i in (0..length).rev() {
                let from_key = PropertyKey::from_u64(i);
                let to_key = PropertyKey::from_u64(i + num_arguments);

                if maybe!(has_property(cx, object, &from_key)) {
                    let from_value = maybe!(get(cx, object, &from_key));
                    maybe!(set(cx, object, &to_key, from_value, true));
                } else {
                    maybe!(delete_property_or_throw(cx, object, &to_key));
                }
            }

            for (i, argument) in arguments.iter().enumerate() {
                let key = PropertyKey::from_u64(i as u64);
                maybe!(set(cx, object, &key, *argument, true));
            }
        }

        let new_length = Value::from_u64(length + num_arguments);
        maybe!(set(cx, object, &cx.names.length(), new_length, true));

        new_length.into()
    }

    // 23.1.3.33 Array.prototype.values
    fn values(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        ArrayIterator::new(cx, object, ArrayIteratorKind::Value).into()
    }
}
