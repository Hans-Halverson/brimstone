use crate::{
    js::runtime::{
        abstract_operations::{
            call, call_object, create_data_property_or_throw, delete_property_or_throw,
            has_property, invoke, length_of_array_like, set,
        },
        array_object::{array_species_create, ArrayObject},
        builtin_function::BuiltinFunction,
        error::type_error_,
        function::get_argument,
        gc::HandleValue,
        get,
        interned_strings::InternedStrings,
        numeric_constants::MAX_SAFE_INTEGER_U64,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_with_optional_proto,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        string_value::StringValue,
        to_string,
        type_utilities::{
            is_array, is_callable, is_strictly_equal, same_value_zero, to_boolean,
            to_integer_or_infinity, to_object,
        },
        Context, EvalResult, Handle, Value,
    },
    maybe, must,
};

use super::{
    array_iterator::{ArrayIterator, ArrayIteratorKind},
    intrinsics::Intrinsic,
};

pub struct ArrayPrototype;

impl ArrayPrototype {
    // 23.1.3 Properties of the Array Prototype Object
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let array = ArrayObject::new(cx, object_proto);

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

        // Constructor property is added once ArrayConstructor has been created
        array
            .object()
            .intrinsic_func(cx, cx.names.at(), Self::at, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.concat(), Self::concat, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.copy_within(), Self::copy_within, 2, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.entries(), Self::entries, 0, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.every(), Self::every, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.fill(), Self::fill, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.filter(), Self::filter, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.find(), Self::find, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.find_index(), Self::find_index, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.flat(), Self::flat, 0, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.flat_map(), Self::flat_map, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.for_each(), Self::for_each, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.includes(), Self::includes, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.index_of(), Self::index_of, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.join(), Self::join, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.keys(), Self::keys, 0, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.last_index_of(), Self::last_index_of, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.map_(), Self::map, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.pop(), Self::pop, 0, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.push(), Self::push, 0, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.reduce(), Self::reduce, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.reduce_right(), Self::reduce_right, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.reverse(), Self::reverse, 0, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.shift(), Self::shift, 0, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.slice(), Self::slice, 2, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.some(), Self::some, 1, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.splice(), Self::splice, 2, realm);
        array.object().intrinsic_func(
            cx,
            cx.names.to_locale_string(),
            Self::to_locale_string,
            0,
            realm,
        );
        array
            .object()
            .intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);
        array
            .object()
            .intrinsic_func(cx, cx.names.unshift(), Self::unshift, 1, realm);
        array
            .object()
            .intrinsic_data_prop(cx, cx.names.values(), values_function);

        // 23.1.3.34 Array.prototype [ @@iterator ]
        let iterator_key = cx.well_known_symbols.iterator();
        array.object().set_property(
            cx,
            iterator_key,
            Property::data(values_function, true, false, true),
        );

        // 23.1.3.35 Array.prototype [ @@unscopables ]
        let unscopables_key = cx.well_known_symbols.unscopables();
        let unscopables = Property::data(Self::create_unscopables(cx).into(), false, false, true);
        array
            .object()
            .set_property(cx, unscopables_key, unscopables);

        array.into()
    }

    // 23.1.3.1 Array.prototype.at
    fn at(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

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

    // 23.1.3.2 Array.prototype.concat
    fn concat(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
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
    fn is_concat_spreadable(cx: &mut Context, object: HandleValue) -> EvalResult<bool> {
        if !object.is_object() {
            return false.into();
        }

        let is_concat_spreadable_key = cx.well_known_symbols.is_concat_spreadable();
        let is_spreadable = maybe!(get(cx, object.as_object(), is_concat_spreadable_key));

        if !is_spreadable.is_undefined() {
            return to_boolean(is_spreadable).into();
        }

        is_array(cx, object)
    }

    #[inline]
    fn apply_concat_to_element(
        cx: &mut Context,
        element: HandleValue,
        array: Handle<ObjectValue>,
        n: &mut u64,
    ) -> EvalResult<()> {
        if maybe!(Self::is_concat_spreadable(cx, element)) {
            let element = element.as_object();
            let length = maybe!(length_of_array_like(cx, element));

            if *n + length > MAX_SAFE_INTEGER_U64 {
                return type_error_(cx, "array is too large");
            }

            // Property key is shared between iterations
            let mut element_index_key = PropertyKey::uninit().to_handle(cx);

            for i in 0..length {
                element_index_key.replace(PropertyKey::from_u64(cx, i));

                if maybe!(has_property(cx, element, element_index_key)) {
                    let sub_element = maybe!(get(cx, element, element_index_key));

                    // Share property key, since element_index_key is no longer used
                    let mut array_index_key = element_index_key;
                    array_index_key.replace(PropertyKey::from_u64(cx, *n));

                    maybe!(create_data_property_or_throw(cx, array, array_index_key, sub_element))
                }

                *n += 1;
            }
        } else {
            if *n >= MAX_SAFE_INTEGER_U64 {
                return type_error_(cx, "array is too large");
            }

            let index_key = PropertyKey::from_u64(cx, *n).to_handle(cx).to_handle(cx);
            maybe!(create_data_property_or_throw(cx, array, index_key, element));

            *n += 1;
        }

        ().into()
    }

    // 23.1.3.4 Array.prototype.copyWithin
    fn copy_within(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let target_arg = get_argument(cx, arguments, 0);
        let relative_target = maybe!(to_integer_or_infinity(cx, target_arg));
        let mut to_index = if relative_target < 0.0 {
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
        let mut from_index = if relative_start < 0.0 {
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

        let count =
            i64::min(from_end_index as i64 - from_index as i64, length as i64 - to_index as i64);

        if count <= 0 {
            return object.into();
        }

        let mut count = count as u64;

        // Property keys are shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);
        let mut to_key = PropertyKey::uninit().to_handle(cx);

        if from_index < to_index && to_index < from_index + count {
            from_index = from_index + count - 1;
            to_index = to_index + count - 1;

            while count > 0 {
                from_key.replace(PropertyKey::from_u64(cx, from_index));
                to_key.replace(PropertyKey::from_u64(cx, to_index));

                if maybe!(has_property(cx, object, from_key)) {
                    let from_value = maybe!(get(cx, object, from_key));
                    maybe!(set(cx, object, to_key, from_value, true));
                } else {
                    maybe!(delete_property_or_throw(cx, object, to_key));
                }

                from_index -= 1;
                to_index -= 1;
                count -= 1;
            }
        } else {
            while count > 0 {
                from_key.replace(PropertyKey::from_u64(cx, from_index));
                to_key.replace(PropertyKey::from_u64(cx, to_index));

                if maybe!(has_property(cx, object, from_key)) {
                    let from_value = maybe!(get(cx, object, from_key));
                    maybe!(set(cx, object, to_key, from_value, true));
                } else {
                    maybe!(delete_property_or_throw(cx, object, to_key));
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
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        ArrayIterator::new(cx, object, ArrayIteratorKind::KeyAndValue).into()
    }

    // 23.1.3.6 Array.prototype.every
    fn every(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if maybe!(has_property(cx, object, index_key)) {
                let value = maybe!(get(cx, object, index_key));

                index_value.replace(Value::from(i));
                let arguments = [value, index_value, object.into()];

                let test_result = maybe!(call_object(cx, callback_function, this_arg, &arguments));
                if !to_boolean(test_result) {
                    return cx.bool(false).into();
                }
            }
        }

        cx.bool(true).into()
    }

    // 23.1.3.7 Array.prototype.fill
    fn fill(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let value = get_argument(cx, arguments, 0);

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

        // Property key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..end_index {
            key.replace(PropertyKey::from_u64(cx, i));
            maybe!(set(cx, object, key, value, true));
        }

        object.into()
    }

    // 23.1.3.8 Array.prototype.filter
    fn filter(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let array = maybe!(array_species_create(cx, object, 0));

        let mut result_index = 0;

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if maybe!(has_property(cx, object, index_key)) {
                let value = maybe!(get(cx, object, index_key));

                index_value.replace(Value::from(i));
                let arguments = [value, index_value, object.into()];

                let is_selected = maybe!(call_object(cx, callback_function, this_arg, &arguments));

                if to_boolean(is_selected) {
                    // Reuse index_key handle as it is never referenced again
                    let mut result_index_key = index_key;

                    result_index_key.replace(PropertyKey::from_u64(cx, result_index));
                    maybe!(create_data_property_or_throw(cx, array, result_index_key, value));

                    result_index += 1;
                }
            }
        }

        array.into()
    }

    // 23.1.3.9 Array.prototype.find
    fn find(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let predicate_function = get_argument(cx, arguments, 0);
        if !is_callable(predicate_function) {
            return type_error_(cx, "expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if maybe!(has_property(cx, object, index_key)) {
                let value = maybe!(get(cx, object, index_key));

                index_value.replace(Value::from(i));
                let arguments = [value, index_value, object.into()];

                let test_result = maybe!(call_object(cx, predicate_function, this_arg, &arguments));
                if to_boolean(test_result) {
                    return value.into();
                }
            }
        }

        cx.undefined().into()
    }

    // 23.1.3.10 Array.prototype.findIndex
    fn find_index(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let predicate_function = get_argument(cx, arguments, 0);
        if !is_callable(predicate_function) {
            return type_error_(cx, "expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if maybe!(has_property(cx, object, index_key)) {
                let value = maybe!(get(cx, object, index_key));

                index_value.replace(Value::from(i));
                let arguments = [value, index_value, object.into()];

                let test_result = maybe!(call_object(cx, predicate_function, this_arg, &arguments));
                if to_boolean(test_result) {
                    return index_value.to_handle(cx).into();
                }
            }
        }

        Value::smi(-1).to_handle(cx).into()
    }

    // 23.1.3.11 Array.prototype.flat
    fn flat(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let depth = get_argument(cx, arguments, 0);
        let depth = if depth.is_undefined() {
            1.0
        } else {
            let depth = maybe!(to_integer_or_infinity(cx, depth));
            f64::max(depth, 0.0)
        };

        let array = maybe!(array_species_create(cx, object, 0));

        maybe!(Self::flatten_into_array(
            cx,
            array,
            object,
            length,
            0,
            depth,
            None,
            cx.undefined()
        ));

        array.into()
    }

    // 23.1.3.11.1 FlattenIntoArray
    fn flatten_into_array(
        cx: &mut Context,
        target: Handle<ObjectValue>,
        source: Handle<ObjectValue>,
        source_length: u64,
        start: u64,
        depth: f64,
        mapper_function: Option<HandleValue>,
        this_arg: HandleValue,
    ) -> EvalResult<u64> {
        let mut target_index = start;

        // Property key is shared between iterations
        let mut source_key = PropertyKey::uninit().to_handle(cx);
        let mut target_key = PropertyKey::uninit().to_handle(cx);

        for i in 0..source_length {
            source_key.replace(PropertyKey::from_u64(cx, i));
            if maybe!(has_property(cx, source, source_key)) {
                let mut element = maybe!(get(cx, source, source_key));

                if let Some(mapper_function) = mapper_function {
                    let index_value = Value::from(i).to_handle(cx);
                    let arguments = [element, index_value, source.into()];
                    element = maybe!(call(cx, mapper_function, this_arg, &arguments));
                }

                let should_flatten = if depth > 0.0 {
                    maybe!(is_array(cx, element))
                } else {
                    false
                };

                if should_flatten {
                    let new_depth = if depth == f64::INFINITY {
                        depth
                    } else {
                        depth - 1.0
                    };

                    let element_object = element.as_object();
                    let element_length = maybe!(length_of_array_like(cx, element_object));

                    target_index = maybe!(Self::flatten_into_array(
                        cx,
                        target,
                        element_object,
                        element_length,
                        target_index,
                        new_depth,
                        mapper_function,
                        this_arg
                    ));
                } else {
                    if target_index >= MAX_SAFE_INTEGER_U64 {
                        return type_error_(cx, "array is too large");
                    }

                    target_key.replace(PropertyKey::from_u64(cx, target_index));
                    maybe!(create_data_property_or_throw(cx, target, target_key, element));

                    target_index += 1;
                }
            }
        }

        target_index.into()
    }

    // 23.1.3.12 Array.prototype.flatMap
    fn flat_map(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let mapper_function = get_argument(cx, arguments, 0);
        let this_arg = get_argument(cx, arguments, 1);

        if !is_callable(mapper_function) {
            return type_error_(cx, "expected function");
        }

        let array = maybe!(array_species_create(cx, object, 0));

        maybe!(Self::flatten_into_array(
            cx,
            array,
            object,
            length,
            0,
            1.0,
            Some(mapper_function),
            this_arg
        ));

        array.into()
    }

    // 23.1.3.13 Array.prototype.forEach
    fn for_each(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if maybe!(has_property(cx, object, index_key)) {
                let value = maybe!(get(cx, object, index_key));

                index_value.replace(Value::from(i));
                let arguments = [value, index_value, object.into()];

                maybe!(call_object(cx, callback_function, this_arg, &arguments));
            }
        }

        cx.undefined().into()
    }

    // 23.1.3.14 Array.prototype.includes
    fn includes(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

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

        // Property key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..length {
            key.replace(PropertyKey::from_u64(cx, i));
            let element = maybe!(get(cx, object, key));

            if same_value_zero(search_element, element) {
                return cx.bool(true).into();
            }
        }

        cx.bool(false).into()
    }

    // 23.1.3.15 Array.prototype.indexOf
    fn index_of(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

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

        // Property key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..length {
            key.replace(PropertyKey::from_u64(cx, i));
            if maybe!(has_property(cx, object, key)) {
                let element = maybe!(get(cx, object, key));
                if is_strictly_equal(search_element, element) {
                    return Value::from(i).to_handle(cx).into();
                }
            }
        }

        Value::smi(-1).to_handle(cx).into()
    }

    // 23.1.3.16 Array.prototype.join
    fn join(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let separator = get_argument(cx, arguments, 0);
        let separator = if separator.is_undefined() {
            InternedStrings::get_str(cx, ",")
        } else {
            maybe!(to_string(cx, separator))
        };

        let mut joined = cx.names.empty_string().as_string();

        // Property key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in 0..length {
            if i > 0 {
                joined = StringValue::concat(cx, joined, separator);
            }

            key.replace(PropertyKey::from_u64(cx, i));
            let element = maybe!(get(cx, object, key));

            if !element.is_nullish() {
                let next = maybe!(to_string(cx, element));
                joined = StringValue::concat(cx, joined, next);
            }
        }

        joined.into()
    }

    // 23.1.3.17 Array.prototype.keys
    fn keys(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        ArrayIterator::new(cx, object, ArrayIteratorKind::Key).into()
    }

    // 23.1.3.18 Array.prototype.lastIndexOf
    fn last_index_of(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

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

        // Property key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in (0..=start_index).rev() {
            key.replace(PropertyKey::from_u64(cx, i));
            if maybe!(has_property(cx, object, key)) {
                let element = maybe!(get(cx, object, key));
                if is_strictly_equal(search_element, element) {
                    return Value::from(i).to_handle(cx).into();
                }
            }
        }

        Value::smi(-1).to_handle(cx).into()
    }

    // 23.1.3.19 Array.prototype.map
    fn map(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let array = maybe!(array_species_create(cx, object, length));

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if maybe!(has_property(cx, object, index_key)) {
                let value = maybe!(get(cx, object, index_key));

                index_value.replace(Value::from(i));
                let arguments = [value, index_value, object.into()];

                let mapped_value = maybe!(call_object(cx, callback_function, this_arg, &arguments));
                maybe!(create_data_property_or_throw(cx, array, index_key, mapped_value));
            }
        }

        array.into()
    }

    // 23.1.3.20 Array.prototype.pop
    fn pop(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        if length == 0 {
            let length_zero = Value::smi(0).to_handle(cx);
            maybe!(set(cx, object, cx.names.length(), length_zero, true));
            return cx.undefined().into();
        }

        let new_length = length - 1;
        let index_key = PropertyKey::from_u64(cx, new_length).to_handle(cx);

        let element = maybe!(get(cx, object, index_key));
        maybe!(delete_property_or_throw(cx, object, index_key));

        let new_length_value = Value::from(new_length).to_handle(cx);
        maybe!(set(cx, object, cx.names.length(), new_length_value, true));

        element.into()
    }

    // 23.1.3.21 Array.prototype.push
    fn push(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let new_length = length + arguments.len() as u64;
        if new_length > MAX_SAFE_INTEGER_U64 {
            return type_error_(cx, "index is too large");
        }

        // Property key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for (i, argument) in arguments.iter().enumerate() {
            key.replace(PropertyKey::from_u64(cx, length + i as u64));
            maybe!(set(cx, object, key, *argument, true));
        }

        let new_length_value = Value::from(new_length).to_handle(cx);
        maybe!(set(cx, object, cx.names.length(), new_length_value, true));

        new_length_value.into()
    }

    // 23.1.3.22 Array.prototype.reduce
    fn reduce(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let mut initial_index = 0;

        let mut accumulator = if arguments.len() >= 2 {
            get_argument(cx, arguments, 1)
        } else if length == 0 {
            return type_error_(cx, "reduce does not have initial value");
        } else {
            // Property key is shared between iterations
            let mut index_key = PropertyKey::uninit().to_handle(cx);

            // Find the first value in the array if an initial value was not specified
            loop {
                if initial_index >= length {
                    return type_error_(cx, "reduce of empty array with no initial value");
                }

                index_key.replace(PropertyKey::from_u64(cx, initial_index));
                initial_index += 1;

                if maybe!(has_property(cx, object, index_key)) {
                    break maybe!(get(cx, object, index_key));
                }
            }
        };

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in initial_index..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if maybe!(has_property(cx, object, index_key)) {
                let value = maybe!(get(cx, object, index_key));

                index_value.replace(Value::from(i));
                let arguments = [accumulator, value, index_value, object.into()];

                accumulator =
                    maybe!(call_object(cx, callback_function, cx.undefined(), &arguments));
            }
        }

        accumulator.into()
    }

    // 23.1.3.23 Array.prototype.reduceRight
    fn reduce_right(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let mut initial_index = length as i64 - 1;

        let mut accumulator = if arguments.len() >= 2 {
            get_argument(cx, arguments, 1)
        } else if length == 0 {
            return type_error_(cx, "reduceRight does not have initial value");
        } else {
            let mut index_key = PropertyKey::uninit().to_handle(cx);

            // Find the first value in the array if an initial value was not specified
            loop {
                if initial_index < 0 {
                    return type_error_(cx, "reduce of empty array with no initial value");
                }

                index_key.replace(PropertyKey::from_u64(cx, initial_index as u64));
                initial_index -= 1;

                if maybe!(has_property(cx, object, index_key)) {
                    break maybe!(get(cx, object, index_key));
                }
            }
        };

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in (0..=initial_index).rev() {
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            if maybe!(has_property(cx, object, index_key)) {
                let value = maybe!(get(cx, object, index_key));

                index_value.replace(Value::from(i));
                let arguments = [accumulator, value, index_value, object.into()];

                accumulator =
                    maybe!(call_object(cx, callback_function, cx.undefined(), &arguments));
            }
        }

        accumulator.into()
    }

    // 23.1.3.24 Array.prototype.reverse
    fn reverse(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

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

            let lower_exists = maybe!(has_property(cx, object, lower_key));
            let upper_exists = maybe!(has_property(cx, object, upper_key));

            match (lower_exists, upper_exists) {
                (true, true) => {
                    let lower_value = maybe!(get(cx, object, lower_key));
                    let upper_value = maybe!(get(cx, object, upper_key));

                    maybe!(set(cx, object, lower_key, upper_value, true));
                    maybe!(set(cx, object, upper_key, lower_value, true));
                }
                (true, false) => {
                    let lower_value = maybe!(get(cx, object, lower_key));

                    maybe!(delete_property_or_throw(cx, object, lower_key));
                    maybe!(set(cx, object, upper_key, lower_value, true));
                }
                (false, true) => {
                    let upper_value = maybe!(get(cx, object, upper_key));

                    maybe!(set(cx, object, lower_key, upper_value, true));
                    maybe!(delete_property_or_throw(cx, object, upper_key));
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
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        if length == 0 {
            let zero_value = Value::smi(0).to_handle(cx);
            maybe!(set(cx, object, cx.names.length(), zero_value, true));
            return cx.undefined().into();
        }

        let first_key = PropertyKey::array_index(cx, 0).to_handle(cx);
        let first = maybe!(get(cx, object, first_key));

        // Shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);
        let mut to_key = PropertyKey::uninit().to_handle(cx);

        for i in 1..length {
            from_key.replace(PropertyKey::from_u64(cx, i));
            to_key.replace(PropertyKey::from_u64(cx, i - 1));

            if maybe!(has_property(cx, object, from_key)) {
                let from_value = maybe!(get(cx, object, from_key));
                maybe!(set(cx, object, to_key, from_value, true));
            } else {
                maybe!(delete_property_or_throw(cx, object, to_key));
            }
        }

        let last_key = PropertyKey::from_u64(cx, length - 1).to_handle(cx);
        maybe!(delete_property_or_throw(cx, object, last_key));

        let new_length_value = Value::from(length - 1).to_handle(cx);
        maybe!(set(cx, object, cx.names.length(), new_length_value, true));

        first.into()
    }

    // 23.1.3.26 Array.prototype.slice
    fn slice(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

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
        let array = maybe!(array_species_create(cx, object, count));

        let mut to_index = 0;

        // Shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..end_index {
            from_key.replace(PropertyKey::from_u64(cx, i));
            if maybe!(has_property(cx, object, from_key)) {
                let value = maybe!(get(cx, object, from_key));

                // Reuse from_key handle since it is no longer referenced
                let mut to_key = from_key;
                to_key.replace(PropertyKey::from_u64(cx, to_index));

                maybe!(create_data_property_or_throw(cx, object, to_key, value));
            }

            to_index += 1;
        }

        let to_index_value = Value::from(to_index).to_handle(cx);
        maybe!(set(cx, array, cx.names.length(), to_index_value, true));

        array.into()
    }

    // 23.1.3.27 Array.prototype.some
    fn some(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if maybe!(has_property(cx, object, index_key)) {
                let value = maybe!(get(cx, object, index_key));

                index_value.replace(Value::from(i));
                let arguments = [value, index_value, object.into()];

                let test_result = maybe!(call_object(cx, callback_function, this_arg, &arguments));
                if to_boolean(test_result) {
                    return cx.bool(true).into();
                }
            }
        }

        cx.bool(false).into()
    }

    // 23.1.3.29 Array.prototype.splice
    fn splice(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

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

        let insert_count = u64::max(arguments.len() as u64 - 2, 0);

        let actual_delete_count = if arguments.len() == 0 {
            0
        } else if arguments.len() == 1 {
            length - start_index
        } else {
            let delete_count_arg = get_argument(cx, arguments, 1);
            let delete_count = maybe!(to_integer_or_infinity(cx, delete_count_arg));
            f64::min(f64::max(delete_count, 0.0), (length - start_index) as f64) as u64
        };

        let new_length = length + insert_count - actual_delete_count;
        if new_length > MAX_SAFE_INTEGER_U64 {
            return type_error_(cx, "array is too large");
        }

        // Create array containing deleted elements, which will be return value
        let array = maybe!(array_species_create(cx, object, actual_delete_count));

        // Shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);
        let mut to_key = PropertyKey::uninit().to_handle(cx);

        for i in 0..actual_delete_count {
            from_key.replace(PropertyKey::from_u64(cx, start_index + i));
            if maybe!(has_property(cx, object, from_key)) {
                let from_value = maybe!(get(cx, object, from_key));
                to_key.replace(PropertyKey::from_u64(cx, i));
                maybe!(create_data_property_or_throw(cx, array, to_key, from_value));
            }
        }

        let actual_delete_count_value = Value::from(actual_delete_count).to_handle(cx);
        maybe!(set(cx, array, cx.names.length(), actual_delete_count_value, true));

        // Move existing items in array to make space for inserted items
        if insert_count < actual_delete_count {
            for i in start_index..(length - actual_delete_count) {
                from_key.replace(PropertyKey::from_u64(cx, i + actual_delete_count));
                to_key.replace(PropertyKey::from_u64(cx, i + insert_count));

                if maybe!(has_property(cx, object, from_key)) {
                    let from_value = maybe!(get(cx, object, from_key));
                    maybe!(set(cx, object, to_key, from_value, true));
                } else {
                    maybe!(delete_property_or_throw(cx, object, to_key));
                }
            }

            for i in (new_length..length).rev() {
                from_key.replace(PropertyKey::from_u64(cx, i));
                maybe!(delete_property_or_throw(cx, object, from_key));
            }
        } else {
            for i in (start_index..(length - actual_delete_count)).rev() {
                from_key.replace(PropertyKey::from_u64(cx, i + actual_delete_count));
                to_key.replace(PropertyKey::from_u64(cx, i + insert_count));

                if maybe!(has_property(cx, object, from_key)) {
                    let from_value = maybe!(get(cx, object, from_key));
                    maybe!(set(cx, object, to_key, from_value, true));
                } else {
                    maybe!(delete_property_or_throw(cx, object, to_key));
                }
            }
        }

        // Insert items into array
        if arguments.len() > 2 {
            for (i, item) in (&arguments[2..]).iter().enumerate() {
                to_key.replace(PropertyKey::from_u64(cx, start_index + i as u64));
                maybe!(set(cx, object, to_key, *item, true));
            }
        }

        let new_length_value = Value::from(new_length).to_handle(cx);
        maybe!(set(cx, object, cx.names.length(), new_length_value, true));

        array.into()
    }

    // 23.1.3.30 Array.prototype.toLocaleString
    fn to_locale_string(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let mut result = cx.names.empty_string().as_string();
        let separator = InternedStrings::get_str(cx, ", ");

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in 0..length {
            if i > 0 {
                result = StringValue::concat(cx, result, separator);
            }

            key.replace(PropertyKey::from_u64(cx, i));
            let next_element = maybe!(get(cx, object, key));

            if !next_element.is_nullish() {
                let string_result =
                    maybe!(invoke(cx, next_element, cx.names.to_locale_string(), &[]));
                let string_result = maybe!(to_string(cx, string_result));

                result = StringValue::concat(cx, result, string_result);
            }
        }

        result.into()
    }

    // 23.1.3.31 Array.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let array = maybe!(to_object(cx, this_value));
        let func = maybe!(get(cx, array, cx.names.join()));

        let func = if is_callable(func) {
            func.as_object()
        } else {
            cx.get_intrinsic(Intrinsic::ObjectPrototypeToString)
        };

        call_object(cx, func, array.into(), &[])
    }

    // 23.1.3.32 Array.prototype.unshift
    fn unshift(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let length = maybe!(length_of_array_like(cx, object));

        let num_arguments = arguments.len() as u64;
        if num_arguments > 0 {
            if length + num_arguments > MAX_SAFE_INTEGER_U64 {
                return type_error_(cx, "array is too large");
            }

            // Shared between iterations
            let mut from_key = PropertyKey::uninit().to_handle(cx);
            let mut to_key = PropertyKey::uninit().to_handle(cx);

            for i in (0..length).rev() {
                from_key.replace(PropertyKey::from_u64(cx, i));
                to_key.replace(PropertyKey::from_u64(cx, i + num_arguments));

                if maybe!(has_property(cx, object, from_key)) {
                    let from_value = maybe!(get(cx, object, from_key));
                    maybe!(set(cx, object, to_key, from_value, true));
                } else {
                    maybe!(delete_property_or_throw(cx, object, to_key));
                }
            }

            for (i, argument) in arguments.iter().enumerate() {
                to_key.replace(PropertyKey::from_u64(cx, i as u64));
                maybe!(set(cx, object, to_key, *argument, true));
            }
        }

        let new_length = Value::from(length + num_arguments).to_handle(cx);
        maybe!(set(cx, object, cx.names.length(), new_length, true));

        new_length.into()
    }

    // 23.1.3.33 Array.prototype.values
    fn values(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        ArrayIterator::new(cx, object, ArrayIteratorKind::Value).into()
    }

    // 23.1.3.35 Array.prototype [ @@unscopables ]
    fn create_unscopables(cx: &mut Context) -> Handle<ObjectValue> {
        let list =
            object_create_with_optional_proto::<ObjectValue>(cx, ObjectKind::OrdinaryObject, None)
                .to_handle();

        let true_value = cx.bool(true);

        must!(create_data_property_or_throw(cx, list, cx.names.at(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.copy_within(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.entries(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.fill(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.find(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.find_index(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.flat(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.flat_map(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.includes(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.keys(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.values(), true_value));

        list
    }
}
