use std::cmp::Ordering;

use crate::{
    must,
    runtime::{
        abstract_operations::{
            call, call_object, create_data_property_or_throw, delete_property_or_throw,
            has_property, invoke, length_of_array_like, set,
        },
        array_object::{array_create, array_species_create, ArrayObject},
        builtin_function::BuiltinFunction,
        error::{range_error, type_error},
        function::get_argument,
        get,
        heap_item_descriptor::HeapItemKind,
        numeric_constants::MAX_SAFE_INTEGER_U64,
        object_value::ObjectValue,
        ordinary_object::object_create_with_optional_proto,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        string_value::StringValue,
        to_string,
        type_utilities::{
            is_array, is_callable, is_less_than, is_strictly_equal, same_value_zero, to_boolean,
            to_integer_or_infinity, to_number, to_object,
        },
        Context, EvalResult, Handle, Value,
    },
};

use super::{
    array_iterator::{ArrayIterator, ArrayIteratorKind},
    intrinsics::Intrinsic,
    typed_array_prototype::compare_typed_array_elements,
};

pub struct ArrayPrototype;

impl ArrayPrototype {
    /// Properties of the Array Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-array-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let object_proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
        let mut array = ArrayObject::new(cx, object_proto).as_object();

        // Create values function as it is referenced by multiple properties
        let values_function =
            BuiltinFunction::create(cx, Self::values, 0, cx.names.values(), realm, None).into();

        // Constructor property is added once ArrayConstructor has been created
        array.intrinsic_func(cx, cx.names.at(), Self::at, 1, realm);
        array.intrinsic_func(cx, cx.names.concat(), Self::concat, 1, realm);
        array.intrinsic_func(cx, cx.names.copy_within(), Self::copy_within, 2, realm);
        array.intrinsic_func(cx, cx.names.entries(), Self::entries, 0, realm);
        array.intrinsic_func(cx, cx.names.every(), Self::every, 1, realm);
        array.intrinsic_func(cx, cx.names.fill(), Self::fill, 1, realm);
        array.intrinsic_func(cx, cx.names.filter(), Self::filter, 1, realm);
        array.intrinsic_func(cx, cx.names.find(), Self::find, 1, realm);
        array.intrinsic_func(cx, cx.names.find_index(), Self::find_index, 1, realm);
        array.intrinsic_func(cx, cx.names.find_last(), Self::find_last, 1, realm);
        array.intrinsic_func(cx, cx.names.find_last_index(), Self::find_last_index, 1, realm);
        array.intrinsic_func(cx, cx.names.flat(), Self::flat, 0, realm);
        array.intrinsic_func(cx, cx.names.flat_map(), Self::flat_map, 1, realm);
        array.intrinsic_func(cx, cx.names.for_each(), Self::for_each, 1, realm);
        array.intrinsic_func(cx, cx.names.includes(), Self::includes, 1, realm);
        array.intrinsic_func(cx, cx.names.index_of(), Self::index_of, 1, realm);
        array.intrinsic_func(cx, cx.names.join(), Self::join, 1, realm);
        array.intrinsic_func(cx, cx.names.keys(), Self::keys, 0, realm);
        array.intrinsic_func(cx, cx.names.last_index_of(), Self::last_index_of, 1, realm);
        array.intrinsic_func(cx, cx.names.map_(), Self::map, 1, realm);
        array.intrinsic_func(cx, cx.names.pop(), Self::pop, 0, realm);
        array.intrinsic_func(cx, cx.names.push(), Self::push, 1, realm);
        array.intrinsic_func(cx, cx.names.reduce(), Self::reduce, 1, realm);
        array.intrinsic_func(cx, cx.names.reduce_right(), Self::reduce_right, 1, realm);
        array.intrinsic_func(cx, cx.names.reverse(), Self::reverse, 0, realm);
        array.intrinsic_func(cx, cx.names.shift(), Self::shift, 0, realm);
        array.intrinsic_func(cx, cx.names.slice(), Self::slice, 2, realm);
        array.intrinsic_func(cx, cx.names.some(), Self::some, 1, realm);
        array.intrinsic_func(cx, cx.names.sort(), Self::sort, 1, realm);
        array.intrinsic_func(cx, cx.names.splice(), Self::splice, 2, realm);
        array.intrinsic_func(cx, cx.names.to_locale_string(), Self::to_locale_string, 0, realm);
        array.intrinsic_func(cx, cx.names.to_reversed(), Self::to_reversed, 0, realm);
        array.intrinsic_func(cx, cx.names.to_sorted(), Self::to_sorted, 1, realm);
        array.intrinsic_func(cx, cx.names.to_spliced(), Self::to_spliced, 2, realm);
        array.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);
        array.intrinsic_func(cx, cx.names.unshift(), Self::unshift, 1, realm);
        array.intrinsic_data_prop(cx, cx.names.values(), values_function);
        array.intrinsic_func(cx, cx.names.with(), Self::with, 2, realm);

        // Array.prototype [ @@iterator ] (https://tc39.es/ecma262/#sec-array.prototype-%symbol.iterator%)
        let iterator_key = cx.well_known_symbols.iterator();
        array.set_property(cx, iterator_key, Property::data(values_function, true, false, true));

        // Array.prototype [ @@unscopables ] (https://tc39.es/ecma262/#sec-array.prototype-%symbol.unscopables%)
        let unscopables_key = cx.well_known_symbols.unscopables();
        let unscopables = Property::data(Self::create_unscopables(cx).into(), false, false, true);
        array.set_property(cx, unscopables_key, unscopables);

        array
    }

    /// Array.prototype.at (https://tc39.es/ecma262/#sec-array.prototype.at)
    pub fn at(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

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

    /// Array.prototype.concat (https://tc39.es/ecma262/#sec-array.prototype.concat)
    pub fn concat(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let array = array_species_create(cx, object, 0)?;

        let mut n = 0;

        Self::apply_concat_to_element(cx, object.into(), array, &mut n)?;

        for element in arguments {
            Self::apply_concat_to_element(cx, *element, array, &mut n)?;
        }

        let new_length_value = Value::from(n).to_handle(cx);
        set(cx, array, cx.names.length(), new_length_value, true)?;

        Ok(array.as_value())
    }

    /// IsConcatSpreadable (https://tc39.es/ecma262/#sec-isconcatspreadable)
    pub fn is_concat_spreadable(cx: Context, object: Handle<Value>) -> EvalResult<bool> {
        if !object.is_object() {
            return Ok(false);
        }

        let is_spreadable =
            get(cx, object.as_object(), cx.well_known_symbols.is_concat_spreadable())?;

        if !is_spreadable.is_undefined() {
            return Ok(to_boolean(*is_spreadable));
        }

        is_array(cx, object)
    }

    #[inline]
    fn apply_concat_to_element(
        cx: Context,
        element: Handle<Value>,
        array: Handle<ObjectValue>,
        n: &mut u64,
    ) -> EvalResult<()> {
        if Self::is_concat_spreadable(cx, element)? {
            let element = element.as_object();
            let length = length_of_array_like(cx, element)?;

            if *n + length > MAX_SAFE_INTEGER_U64 {
                return type_error(cx, "array is too large");
            }

            // Property key is shared between iterations
            let mut element_index_key = PropertyKey::uninit().to_handle(cx);

            for i in 0..length {
                element_index_key.replace(PropertyKey::from_u64(cx, i));

                if has_property(cx, element, element_index_key)? {
                    let sub_element = get(cx, element, element_index_key)?;

                    // Share property key, since element_index_key is no longer used
                    let mut array_index_key = element_index_key;
                    array_index_key.replace(PropertyKey::from_u64(cx, *n));

                    create_data_property_or_throw(cx, array, array_index_key, sub_element)?
                }

                *n += 1;
            }
        } else {
            if *n >= MAX_SAFE_INTEGER_U64 {
                return type_error(cx, "array is too large");
            }

            let index_key = PropertyKey::from_u64(cx, *n).to_handle(cx).to_handle(cx);
            create_data_property_or_throw(cx, array, index_key, element)?;

            *n += 1;
        }

        Ok(())
    }

    /// Array.prototype.copyWithin (https://tc39.es/ecma262/#sec-array.prototype.copywithin)
    pub fn copy_within(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let target_arg = get_argument(cx, arguments, 0);
        let relative_target = to_integer_or_infinity(cx, target_arg)?;
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
        let relative_start = to_integer_or_infinity(cx, start_arg)?;
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

        let count =
            i64::min(from_end_index as i64 - from_index as i64, length as i64 - to_index as i64);

        if count <= 0 {
            return Ok(object.as_value());
        }

        let mut count = count as u64;

        // Property keys are shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);
        let mut to_key = PropertyKey::uninit().to_handle(cx);

        if from_index < to_index && to_index < from_index + count {
            // Treat as i64 due to potential subtraction below 0. Guaranteed to not need number
            // out of i64 range since these are array indices.
            let mut from_index = (from_index + count - 1) as i64;
            let mut to_index = (to_index + count - 1) as i64;

            while count > 0 {
                from_key.replace(PropertyKey::from_u64(cx, from_index as u64));
                to_key.replace(PropertyKey::from_u64(cx, to_index as u64));

                if has_property(cx, object, from_key)? {
                    let from_value = get(cx, object, from_key)?;
                    set(cx, object, to_key, from_value, true)?;
                } else {
                    delete_property_or_throw(cx, object, to_key)?;
                }

                from_index -= 1;
                to_index -= 1;
                count -= 1;
            }
        } else {
            while count > 0 {
                from_key.replace(PropertyKey::from_u64(cx, from_index));
                to_key.replace(PropertyKey::from_u64(cx, to_index));

                if has_property(cx, object, from_key)? {
                    let from_value = get(cx, object, from_key)?;
                    set(cx, object, to_key, from_value, true)?;
                } else {
                    delete_property_or_throw(cx, object, to_key)?;
                }

                from_index += 1;
                to_index += 1;
                count -= 1;
            }
        }

        Ok(object.as_value())
    }

    /// Array.prototype.entries (https://tc39.es/ecma262/#sec-array.prototype.entries)
    pub fn entries(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        Ok(ArrayIterator::new(cx, object, ArrayIteratorKind::KeyAndValue).as_value())
    }

    /// Array.prototype.every (https://tc39.es/ecma262/#sec-array.prototype.every)
    pub fn every(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "Array.prototype.every expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if has_property(cx, object, index_key)? {
                let value = get(cx, object, index_key)?;

                index_value.replace(Value::from(i));
                let arguments = [value, index_value, object.into()];

                let test_result = call_object(cx, callback_function, this_arg, &arguments)?;
                if !to_boolean(*test_result) {
                    return Ok(cx.bool(false));
                }
            }
        }

        Ok(cx.bool(true))
    }

    /// Array.prototype.fill (https://tc39.es/ecma262/#sec-array.prototype.fill)
    pub fn fill(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let value = get_argument(cx, arguments, 0);

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

        // Property key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..end_index {
            key.replace(PropertyKey::from_u64(cx, i));
            set(cx, object, key, value, true)?;
        }

        Ok(object.as_value())
    }

    /// Array.prototype.filter (https://tc39.es/ecma262/#sec-array.prototype.filter)
    pub fn filter(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "Array.prototype.filter expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let array = array_species_create(cx, object, 0)?;

        let mut result_index = 0;

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if has_property(cx, object, index_key)? {
                let value = get(cx, object, index_key)?;

                index_value.replace(Value::from(i));
                let arguments = [value, index_value, object.into()];

                let is_selected = call_object(cx, callback_function, this_arg, &arguments)?;

                if to_boolean(*is_selected) {
                    // Reuse index_key handle as it is never referenced again
                    let mut result_index_key = index_key;

                    result_index_key.replace(PropertyKey::from_u64(cx, result_index));
                    create_data_property_or_throw(cx, array, result_index_key, value)?;

                    result_index += 1;
                }
            }
        }

        Ok(array.as_value())
    }

    /// Array.prototype.find (https://tc39.es/ecma262/#sec-array.prototype.find)
    pub fn find(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let predicate_function = get_argument(cx, arguments, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "Array.prototype.find expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let find_result = find_via_predicate(cx, object, 0..length, predicate_function, this_arg)?;

        match find_result {
            Some((value, _)) => Ok(value),
            None => Ok(cx.undefined()),
        }
    }

    /// Array.prototype.findIndex (https://tc39.es/ecma262/#sec-array.prototype.findindex)
    pub fn find_index(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let predicate_function = get_argument(cx, arguments, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "Array.prototype.findIndex expected function");
        }

        let predicate_function = predicate_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let find_result = find_via_predicate(cx, object, 0..length, predicate_function, this_arg)?;

        match find_result {
            Some((_, index_value)) => Ok(index_value),
            None => Ok(cx.negative_one()),
        }
    }

    /// Array.prototype.findLast (https://tc39.es/ecma262/#sec-array.prototype.findlast)
    pub fn find_last(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let predicate_function = get_argument(cx, arguments, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "Array.prototype.findLast expected function");
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

    /// Array.prototype.findLastIndex (https://tc39.es/ecma262/#sec-array.prototype.findlastindex)
    pub fn find_last_index(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let predicate_function = get_argument(cx, arguments, 0);
        if !is_callable(predicate_function) {
            return type_error(cx, "Array.prototype.findLastIndex expected function");
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

    /// Array.prototype.flat (https://tc39.es/ecma262/#sec-array.prototype.flat)
    pub fn flat(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let depth = get_argument(cx, arguments, 0);
        let depth = if depth.is_undefined() {
            1.0
        } else {
            let depth = to_integer_or_infinity(cx, depth)?;
            f64::max(depth, 0.0)
        };

        let array = array_species_create(cx, object, 0)?;

        Self::flatten_into_array(cx, array, object, length, 0, depth, None, cx.undefined())?;

        Ok(array.as_value())
    }

    /// FlattenIntoArray (https://tc39.es/ecma262/#sec-flattenintoarray)
    pub fn flatten_into_array(
        cx: Context,
        target: Handle<ObjectValue>,
        source: Handle<ObjectValue>,
        source_length: u64,
        start: u64,
        depth: f64,
        mapper_function: Option<Handle<Value>>,
        this_arg: Handle<Value>,
    ) -> EvalResult<u64> {
        let mut target_index = start;

        // Property key is shared between iterations
        let mut source_key = PropertyKey::uninit().to_handle(cx);
        let mut target_key = PropertyKey::uninit().to_handle(cx);

        for i in 0..source_length {
            source_key.replace(PropertyKey::from_u64(cx, i));
            if has_property(cx, source, source_key)? {
                let mut element = get(cx, source, source_key)?;

                if let Some(mapper_function) = mapper_function {
                    let index_value = Value::from(i).to_handle(cx);
                    let arguments = [element, index_value, source.into()];
                    element = call(cx, mapper_function, this_arg, &arguments)?;
                }

                let should_flatten = if depth > 0.0 {
                    is_array(cx, element)?
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
                    let element_length = length_of_array_like(cx, element_object)?;

                    target_index = Self::flatten_into_array(
                        cx,
                        target,
                        element_object,
                        element_length,
                        target_index,
                        new_depth,
                        None,
                        this_arg,
                    )?;
                } else {
                    if target_index >= MAX_SAFE_INTEGER_U64 {
                        return type_error(cx, "array is too large");
                    }

                    target_key.replace(PropertyKey::from_u64(cx, target_index));
                    create_data_property_or_throw(cx, target, target_key, element)?;

                    target_index += 1;
                }
            }
        }

        Ok(target_index)
    }

    /// Array.prototype.flatMap (https://tc39.es/ecma262/#sec-array.prototype.flatmap)
    pub fn flat_map(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let mapper_function = get_argument(cx, arguments, 0);
        let this_arg = get_argument(cx, arguments, 1);

        if !is_callable(mapper_function) {
            return type_error(cx, "Array.prototype.flatMap expected function");
        }

        let array = array_species_create(cx, object, 0)?;

        Self::flatten_into_array(
            cx,
            array,
            object,
            length,
            0,
            1.0,
            Some(mapper_function),
            this_arg,
        )?;

        Ok(array.as_value())
    }

    /// Array.prototype.forEach (https://tc39.es/ecma262/#sec-array.prototype.foreach)
    pub fn for_each(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "Array.prototype.forEach expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if has_property(cx, object, index_key)? {
                let value = get(cx, object, index_key)?;

                index_value.replace(Value::from(i));
                let arguments = [value, index_value, object.into()];

                call_object(cx, callback_function, this_arg, &arguments)?;
            }
        }

        Ok(cx.undefined())
    }

    /// Array.prototype.includes (https://tc39.es/ecma262/#sec-array.prototype.includes)
    pub fn includes(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

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

        // Property key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..length {
            key.replace(PropertyKey::from_u64(cx, i));
            let element = get(cx, object, key)?;

            if same_value_zero(search_element, element) {
                return Ok(cx.bool(true));
            }
        }

        Ok(cx.bool(false))
    }

    /// Array.prototype.indexOf (https://tc39.es/ecma262/#sec-array.prototype.indexof)
    pub fn index_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

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

        // Property key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..length {
            key.replace(PropertyKey::from_u64(cx, i));
            if has_property(cx, object, key)? {
                let element = get(cx, object, key)?;
                if is_strictly_equal(search_element, element) {
                    return Ok(Value::from(i).to_handle(cx));
                }
            }
        }

        Ok(cx.negative_one())
    }

    /// Array.prototype.join (https://tc39.es/ecma262/#sec-array.prototype.join)
    pub fn join(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let separator = get_argument(cx, arguments, 0);
        let separator = if separator.is_undefined() {
            cx.names.comma().as_string()
        } else {
            to_string(cx, separator)?
        };

        let mut joined = cx.names.empty_string().as_string();

        // Property key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in 0..length {
            if i > 0 {
                joined = StringValue::concat(cx, joined, separator);
            }

            key.replace(PropertyKey::from_u64(cx, i));
            let element = get(cx, object, key)?;

            if !element.is_nullish() {
                let next = to_string(cx, element)?;
                joined = StringValue::concat(cx, joined, next);
            }
        }

        Ok(joined.as_value())
    }

    /// Array.prototype.keys (https://tc39.es/ecma262/#sec-array.prototype.keys)
    pub fn keys(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        Ok(ArrayIterator::new(cx, object, ArrayIteratorKind::Key).as_value())
    }

    /// Array.prototype.lastIndexOf (https://tc39.es/ecma262/#sec-array.prototype.lastindexof)
    pub fn last_index_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

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

        // Property key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in (0..=start_index).rev() {
            key.replace(PropertyKey::from_u64(cx, i));
            if has_property(cx, object, key)? {
                let element = get(cx, object, key)?;
                if is_strictly_equal(search_element, element) {
                    return Ok(Value::from(i).to_handle(cx));
                }
            }
        }

        Ok(cx.negative_one())
    }

    /// Array.prototype.map (https://tc39.es/ecma262/#sec-array.prototype.map)
    pub fn map(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "Array.prototype.map expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        let array = array_species_create(cx, object, length)?;

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if has_property(cx, object, index_key)? {
                let value = get(cx, object, index_key)?;

                index_value.replace(Value::from(i));
                let arguments = [value, index_value, object.into()];

                let mapped_value = call_object(cx, callback_function, this_arg, &arguments)?;
                create_data_property_or_throw(cx, array, index_key, mapped_value)?;
            }
        }

        Ok(array.as_value())
    }

    /// Array.prototype.pop (https://tc39.es/ecma262/#sec-array.prototype.pop)
    pub fn pop(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        if length == 0 {
            let length_zero = cx.zero();
            set(cx, object, cx.names.length(), length_zero, true)?;
            return Ok(cx.undefined());
        }

        let new_length = length - 1;
        let index_key = PropertyKey::from_u64(cx, new_length).to_handle(cx);

        let element = get(cx, object, index_key)?;
        delete_property_or_throw(cx, object, index_key)?;

        let new_length_value = Value::from(new_length).to_handle(cx);
        set(cx, object, cx.names.length(), new_length_value, true)?;

        Ok(element)
    }

    /// Array.prototype.push (https://tc39.es/ecma262/#sec-array.prototype.push)
    pub fn push(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let new_length = length + arguments.len() as u64;
        if new_length > MAX_SAFE_INTEGER_U64 {
            return type_error(cx, "index is too large");
        }

        // Property key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for (i, argument) in arguments.iter().enumerate() {
            key.replace(PropertyKey::from_u64(cx, length + i as u64));
            set(cx, object, key, *argument, true)?;
        }

        let new_length_value = Value::from(new_length).to_handle(cx);
        set(cx, object, cx.names.length(), new_length_value, true)?;

        Ok(new_length_value)
    }

    /// Array.prototype.reduce (https://tc39.es/ecma262/#sec-array.prototype.reduce)
    pub fn reduce(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "Array.prototype.reduce expected function");
        }

        let callback_function = callback_function.as_object();
        let mut initial_index = 0;

        let mut accumulator = if arguments.len() >= 2 {
            get_argument(cx, arguments, 1)
        } else if length == 0 {
            return type_error(cx, "reduce does not have initial value");
        } else {
            // Property key is shared between iterations
            let mut index_key = PropertyKey::uninit().to_handle(cx);

            // Find the first value in the array if an initial value was not specified
            loop {
                if initial_index >= length {
                    return type_error(cx, "reduce of empty array with no initial value");
                }

                index_key.replace(PropertyKey::from_u64(cx, initial_index));
                initial_index += 1;

                if has_property(cx, object, index_key)? {
                    break get(cx, object, index_key)?;
                }
            }
        };

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in initial_index..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if has_property(cx, object, index_key)? {
                let value = get(cx, object, index_key)?;

                index_value.replace(Value::from(i));
                let arguments = [accumulator, value, index_value, object.into()];

                accumulator = call_object(cx, callback_function, cx.undefined(), &arguments)?;
            }
        }

        Ok(accumulator)
    }

    /// Array.prototype.reduceRight (https://tc39.es/ecma262/#sec-array.prototype.reduceright)
    pub fn reduce_right(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "Array.prototype.reduceRight expected function");
        }

        let callback_function = callback_function.as_object();
        let mut initial_index = length as i64 - 1;

        let mut accumulator = if arguments.len() >= 2 {
            get_argument(cx, arguments, 1)
        } else if length == 0 {
            return type_error(cx, "reduceRight does not have initial value");
        } else {
            let mut index_key = PropertyKey::uninit().to_handle(cx);

            // Find the first value in the array if an initial value was not specified
            loop {
                if initial_index < 0 {
                    return type_error(cx, "reduce of empty array with no initial value");
                }

                index_key.replace(PropertyKey::from_u64(cx, initial_index as u64));
                initial_index -= 1;

                if has_property(cx, object, index_key)? {
                    break get(cx, object, index_key)?;
                }
            }
        };

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in (0..=initial_index).rev() {
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            if has_property(cx, object, index_key)? {
                let value = get(cx, object, index_key)?;

                index_value.replace(Value::from(i));
                let arguments = [accumulator, value, index_value, object.into()];

                accumulator = call_object(cx, callback_function, cx.undefined(), &arguments)?;
            }
        }

        Ok(accumulator)
    }

    /// Array.prototype.reverse (https://tc39.es/ecma262/#sec-array.prototype.reverse)
    pub fn reverse(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

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

            let lower_value = if has_property(cx, object, lower_key)? {
                Some(get(cx, object, lower_key)?)
            } else {
                None
            };

            let upper_value = if has_property(cx, object, upper_key)? {
                Some(get(cx, object, upper_key)?)
            } else {
                None
            };

            match (lower_value, upper_value) {
                (Some(lower_value), Some(upper_value)) => {
                    set(cx, object, lower_key, upper_value, true)?;
                    set(cx, object, upper_key, lower_value, true)?;
                }
                (Some(lower_value), None) => {
                    delete_property_or_throw(cx, object, lower_key)?;
                    set(cx, object, upper_key, lower_value, true)?;
                }
                (None, Some(upper_value)) => {
                    set(cx, object, lower_key, upper_value, true)?;
                    delete_property_or_throw(cx, object, upper_key)?;
                }
                (None, None) => {}
            }

            lower += 1;
            upper -= 1;
        }

        Ok(object.as_value())
    }

    /// Array.prototype.shift (https://tc39.es/ecma262/#sec-array.prototype.shift)
    pub fn shift(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        if length == 0 {
            let zero_value = cx.zero();
            set(cx, object, cx.names.length(), zero_value, true)?;
            return Ok(cx.undefined());
        }

        let first_key = PropertyKey::array_index(cx, 0).to_handle(cx);
        let first = get(cx, object, first_key)?;

        // Shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);
        let mut to_key = PropertyKey::uninit().to_handle(cx);

        for i in 1..length {
            from_key.replace(PropertyKey::from_u64(cx, i));
            to_key.replace(PropertyKey::from_u64(cx, i - 1));

            if has_property(cx, object, from_key)? {
                let from_value = get(cx, object, from_key)?;
                set(cx, object, to_key, from_value, true)?;
            } else {
                delete_property_or_throw(cx, object, to_key)?;
            }
        }

        let last_key = PropertyKey::from_u64(cx, length - 1).to_handle(cx);
        delete_property_or_throw(cx, object, last_key)?;

        let new_length_value = Value::from(length - 1).to_handle(cx);
        set(cx, object, cx.names.length(), new_length_value, true)?;

        Ok(first)
    }

    /// Array.prototype.slice (https://tc39.es/ecma262/#sec-array.prototype.slice)
    pub fn slice(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

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
        let array = array_species_create(cx, object, count)?;

        let mut to_index = 0;

        // Shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);

        for i in start_index..end_index {
            from_key.replace(PropertyKey::from_u64(cx, i));
            if has_property(cx, object, from_key)? {
                let value = get(cx, object, from_key)?;

                // Reuse from_key handle since it is no longer referenced
                let mut to_key = from_key;
                to_key.replace(PropertyKey::from_u64(cx, to_index));

                create_data_property_or_throw(cx, array, to_key, value)?;
            }

            to_index += 1;
        }

        let to_index_value = Value::from(to_index).to_handle(cx);
        set(cx, array, cx.names.length(), to_index_value, true)?;

        Ok(array.as_value())
    }

    /// Array.prototype.some (https://tc39.es/ecma262/#sec-array.prototype.some)
    pub fn some(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "Array.prototype.some expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Shared between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);
        let mut index_value = Value::uninit().to_handle(cx);

        for i in 0..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            if has_property(cx, object, index_key)? {
                let value = get(cx, object, index_key)?;

                index_value.replace(Value::from(i));
                let arguments = [value, index_value, object.into()];

                let test_result = call_object(cx, callback_function, this_arg, &arguments)?;
                if to_boolean(*test_result) {
                    return Ok(cx.bool(true));
                }
            }
        }

        Ok(cx.bool(false))
    }

    /// Array.prototype.sort (https://tc39.es/ecma262/#sec-array.prototype.sort)
    pub fn sort(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let compare_function_arg = get_argument(cx, arguments, 0);
        if !compare_function_arg.is_undefined() && !is_callable(compare_function_arg) {
            return type_error(cx, "Array.prototype.sort expects a function");
        };

        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let sorted_values = sort_indexed_properties::<IGNORE_HOLES, REGULAR_ARRAY>(
            cx,
            object,
            length,
            compare_function_arg,
        )?;

        // Reuse handle between iterations
        let mut index_key = PropertyKey::uninit().to_handle(cx);

        // Copy sorted values into start of array
        for (i, value) in sorted_values.iter().enumerate() {
            index_key.replace(PropertyKey::from_u64(cx, i as u64));
            set(cx, object, index_key, *value, true)?;
        }

        // If there were holes then delete that number of holes from the end of the array
        for i in (sorted_values.len() as u64)..length {
            index_key.replace(PropertyKey::from_u64(cx, i));
            delete_property_or_throw(cx, object, index_key)?;
        }

        Ok(object.as_value())
    }

    /// Array.prototype.splice (https://tc39.es/ecma262/#sec-array.prototype.splice)
    pub fn splice(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

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

        let insert_count = (arguments.len() as u64).saturating_sub(2);

        let actual_delete_count = if arguments.is_empty() {
            0
        } else if arguments.len() == 1 {
            length - start_index
        } else {
            let delete_count_arg = get_argument(cx, arguments, 1);
            let delete_count = to_integer_or_infinity(cx, delete_count_arg)?;
            f64::min(f64::max(delete_count, 0.0), (length - start_index) as f64) as u64
        };

        let new_length = length + insert_count - actual_delete_count;
        if new_length > MAX_SAFE_INTEGER_U64 {
            return type_error(cx, "array is too large");
        }

        // Create array containing deleted elements, which will be return value
        let array = array_species_create(cx, object, actual_delete_count)?;

        // Shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);
        let mut to_key = PropertyKey::uninit().to_handle(cx);

        for i in 0..actual_delete_count {
            from_key.replace(PropertyKey::from_u64(cx, start_index + i));
            if has_property(cx, object, from_key)? {
                let from_value = get(cx, object, from_key)?;
                to_key.replace(PropertyKey::from_u64(cx, i));
                create_data_property_or_throw(cx, array, to_key, from_value)?;
            }
        }

        let actual_delete_count_value = Value::from(actual_delete_count).to_handle(cx);
        set(cx, array, cx.names.length(), actual_delete_count_value, true)?;

        // Move existing items in array to make space for inserted items
        if insert_count < actual_delete_count {
            for i in start_index..(length - actual_delete_count) {
                from_key.replace(PropertyKey::from_u64(cx, i + actual_delete_count));
                to_key.replace(PropertyKey::from_u64(cx, i + insert_count));

                if has_property(cx, object, from_key)? {
                    let from_value = get(cx, object, from_key)?;
                    set(cx, object, to_key, from_value, true)?;
                } else {
                    delete_property_or_throw(cx, object, to_key)?;
                }
            }

            for i in (new_length..length).rev() {
                from_key.replace(PropertyKey::from_u64(cx, i));
                delete_property_or_throw(cx, object, from_key)?;
            }
        } else if insert_count > actual_delete_count {
            for i in (start_index..(length - actual_delete_count)).rev() {
                from_key.replace(PropertyKey::from_u64(cx, i + actual_delete_count));
                to_key.replace(PropertyKey::from_u64(cx, i + insert_count));

                if has_property(cx, object, from_key)? {
                    let from_value = get(cx, object, from_key)?;
                    set(cx, object, to_key, from_value, true)?;
                } else {
                    delete_property_or_throw(cx, object, to_key)?;
                }
            }
        }

        // Insert items into array
        for (i, item) in arguments.iter().skip(2).enumerate() {
            to_key.replace(PropertyKey::from_u64(cx, start_index + i as u64));
            set(cx, object, to_key, *item, true)?;
        }

        let new_length_value = Value::from(new_length).to_handle(cx);
        set(cx, object, cx.names.length(), new_length_value, true)?;

        Ok(array.as_value())
    }

    /// Array.prototype.toLocaleString (https://tc39.es/ecma262/#sec-array.prototype.tolocalestring)
    pub fn to_locale_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let mut result = cx.names.empty_string().as_string();
        let separator = cx.names.comma().as_string();

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in 0..length {
            if i > 0 {
                result = StringValue::concat(cx, result, separator);
            }

            key.replace(PropertyKey::from_u64(cx, i));
            let next_element = get(cx, object, key)?;

            if !next_element.is_nullish() {
                let string_result = invoke(cx, next_element, cx.names.to_locale_string(), &[])?;
                let string_result = to_string(cx, string_result)?;

                result = StringValue::concat(cx, result, string_result);
            }
        }

        Ok(result.as_value())
    }

    /// Array.prototype.toReversed (https://tc39.es/ecma262/#sec-array.prototype.toreversed)
    pub fn to_reversed(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let array = array_create(cx, length, None)?;

        // Keys are shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);
        let mut to_key = PropertyKey::uninit().to_handle(cx);

        for i in 0..length {
            from_key.replace(PropertyKey::from_u64(cx, length - i - 1));
            to_key.replace(PropertyKey::from_u64(cx, i));

            let value = get(cx, object, from_key)?;
            must!(create_data_property_or_throw(cx, array.into(), to_key, value));
        }

        Ok(array.as_value())
    }

    /// Array.prototype.toSorted (https://tc39.es/ecma262/#sec-array.prototype.tosorted)
    pub fn to_sorted(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let compare_function_arg = get_argument(cx, arguments, 0);
        if !compare_function_arg.is_undefined() && !is_callable(compare_function_arg) {
            return type_error(cx, "Array.prototype.toSorted expects a function");
        };

        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let sorted_array = array_create(cx, length, None)?;

        let sorted_values = sort_indexed_properties::<INCLUDE_HOLES, REGULAR_ARRAY>(
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
            create_data_property_or_throw(cx, sorted_array.into(), index_key, *value)?;
        }

        Ok(sorted_array.as_value())
    }

    /// Array.prototype.toSpliced (https://tc39.es/ecma262/#sec-array.prototype.tospliced)
    pub fn to_spliced(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        // Determine absolute start index from the relative index argument
        let start_arg = get_argument(cx, arguments, 0);
        let relative_start = to_integer_or_infinity(cx, start_arg)?;
        let actual_start_index = if relative_start < 0.0 {
            if relative_start == f64::NEG_INFINITY {
                0
            } else {
                i64::max(length as i64 + relative_start as i64, 0) as u64
            }
        } else {
            u64::min(relative_start as u64, length)
        };

        let insert_count = (arguments.len() as u64).saturating_sub(2);

        // Determine the skip count from the optional argument
        let actual_skip_count = if arguments.is_empty() {
            0
        } else if arguments.len() == 1 {
            length - actual_start_index
        } else {
            let skip_count_arg = get_argument(cx, arguments, 1);
            let skip_count = to_integer_or_infinity(cx, skip_count_arg)?;
            f64::min(f64::max(skip_count, 0.0), (length - actual_start_index) as f64) as u64
        };

        // Determine length of new array and make sure it is in range
        let new_length = length + insert_count - actual_skip_count;
        if new_length > MAX_SAFE_INTEGER_U64 {
            return type_error(cx, "TypedArray.prototype.toSpliced result array is too large");
        }

        let array = array_create(cx, new_length, None)?;

        // Keys are shared between iterations
        let mut from_key = PropertyKey::uninit().to_handle(cx);
        let mut to_key = PropertyKey::uninit().to_handle(cx);

        // Elements before the start index are unchanged and can be copied
        for i in 0..actual_start_index {
            from_key.replace(PropertyKey::from_u64(cx, i));
            let value = get(cx, object, from_key)?;
            must!(create_data_property_or_throw(cx, array.into(), from_key, value));
        }

        // Insert every element of provided items
        for (i, item) in arguments.iter().skip(2).enumerate() {
            to_key.replace(PropertyKey::from_u64(cx, actual_start_index + i as u64));
            create_data_property_or_throw(cx, array.into(), to_key, *item)?;
        }

        // All remaining elements after the skip count are copied
        for i in (actual_start_index + insert_count)..new_length {
            to_key.replace(PropertyKey::from_u64(cx, i));
            from_key.replace(PropertyKey::from_u64(cx, i - insert_count + actual_skip_count));

            let value = get(cx, object, from_key)?;
            must!(create_data_property_or_throw(cx, array.into(), to_key, value));
        }

        Ok(array.as_value())
    }

    /// Array.prototype.toString (https://tc39.es/ecma262/#sec-array.prototype.tostring)
    pub fn to_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let array = to_object(cx, this_value)?;
        let func = get(cx, array, cx.names.join())?;

        let func = if is_callable(func) {
            func.as_object()
        } else {
            cx.get_intrinsic(Intrinsic::ObjectPrototypeToString)
        };

        call_object(cx, func, array.into(), &[])
    }

    /// Array.prototype.unshift (https://tc39.es/ecma262/#sec-array.prototype.unshift)
    pub fn unshift(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let num_arguments = arguments.len() as u64;
        if num_arguments > 0 {
            if length + num_arguments > MAX_SAFE_INTEGER_U64 {
                return type_error(cx, "array is too large");
            }

            // Shared between iterations
            let mut from_key = PropertyKey::uninit().to_handle(cx);
            let mut to_key = PropertyKey::uninit().to_handle(cx);

            for i in (0..length).rev() {
                from_key.replace(PropertyKey::from_u64(cx, i));
                to_key.replace(PropertyKey::from_u64(cx, i + num_arguments));

                if has_property(cx, object, from_key)? {
                    let from_value = get(cx, object, from_key)?;
                    set(cx, object, to_key, from_value, true)?;
                } else {
                    delete_property_or_throw(cx, object, to_key)?;
                }
            }

            for (i, argument) in arguments.iter().enumerate() {
                to_key.replace(PropertyKey::from_u64(cx, i as u64));
                set(cx, object, to_key, *argument, true)?;
            }
        }

        let new_length = Value::from(length + num_arguments).to_handle(cx);
        set(cx, object, cx.names.length(), new_length, true)?;

        Ok(new_length)
    }

    /// Array.prototype.values (https://tc39.es/ecma262/#sec-array.prototype.values)
    pub fn values(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        Ok(ArrayIterator::new(cx, object, ArrayIteratorKind::Value).as_value())
    }

    /// Array.prototype.with (https://tc39.es/ecma262/#sec-array.prototype.with)
    pub fn with(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let length = length_of_array_like(cx, object)?;

        let index_arg = get_argument(cx, arguments, 0);
        let relative_index = to_integer_or_infinity(cx, index_arg)?;

        // Convert from relative to actual index, making sure index is in range
        let actual_index = if relative_index >= 0.0 {
            if relative_index >= length as f64 {
                return range_error(cx, "Array.prototype.with index is out of range");
            }

            relative_index as u64
        } else {
            let actual_index = relative_index + length as f64;
            if actual_index < 0.0 {
                return range_error(cx, "Array.prototype.with index is out of range");
            }

            actual_index as u64
        };

        let array = array_create(cx, length, None)?;
        let new_value = get_argument(cx, arguments, 1);

        // Key is shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for i in 0..length {
            key.replace(PropertyKey::from_u64(cx, i));

            // Replace the i'th value with the new value
            let value = if i == actual_index {
                new_value
            } else {
                get(cx, object, key)?
            };

            must!(create_data_property_or_throw(cx, array.into(), key, value));
        }

        Ok(array.as_value())
    }

    /// Array.prototype [ @@unscopables ] (https://tc39.es/ecma262/#sec-array.prototype-%symbol.unscopables%)
    fn create_unscopables(cx: Context) -> Handle<ObjectValue> {
        let list = object_create_with_optional_proto::<ObjectValue>(
            cx,
            HeapItemKind::OrdinaryObject,
            None,
        )
        .to_handle();

        let true_value = cx.bool(true);

        must!(create_data_property_or_throw(cx, list, cx.names.at(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.copy_within(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.entries(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.fill(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.find(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.find_index(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.find_last(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.find_last_index(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.flat(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.flat_map(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.includes(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.keys(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.to_reversed(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.to_sorted(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.to_spliced(), true_value));
        must!(create_data_property_or_throw(cx, list, cx.names.values(), true_value));

        list
    }
}

/// FindViaPredicate (https://tc39.es/ecma262/#sec-findviapredicate)
#[inline]
pub fn find_via_predicate(
    cx: Context,
    object: Handle<ObjectValue>,
    indices_iter: impl Iterator<Item = u64>,
    predicate: Handle<ObjectValue>,
    this_arg: Handle<Value>,
) -> EvalResult<Option<(Handle<Value>, Handle<Value>)>> {
    // Shared between iterations
    let mut index_key = PropertyKey::uninit().to_handle(cx);
    let mut index_value = Value::uninit().to_handle(cx);

    for i in indices_iter {
        index_key.replace(PropertyKey::from_u64(cx, i));
        let value = get(cx, object, index_key)?;

        index_value.replace(Value::from(i));
        let arguments = [value, index_value, object.into()];

        let test_result = call_object(cx, predicate, this_arg, &arguments)?;
        if to_boolean(*test_result) {
            return Ok(Some((value, index_value)));
        }
    }

    Ok(None)
}

// Whether to exclude holes from the sorted output or not
pub const IGNORE_HOLES: bool = true;
pub const INCLUDE_HOLES: bool = false;

// Whether the object is a typed array or not
pub const TYPED_ARRAY: bool = true;
pub const REGULAR_ARRAY: bool = false;

/// SortIndexedProperties (https://tc39.es/ecma262/#sec-sortindexedproperties)
pub fn sort_indexed_properties<const IGNORE_HOLES: bool, const IS_TYPED_ARRAY: bool>(
    cx: Context,
    object: Handle<ObjectValue>,
    length: u64,
    compare_function: Handle<Value>,
) -> EvalResult<Vec<Handle<Value>>> {
    // Reuse handle between iterations
    let mut index_key = PropertyKey::uninit().to_handle(cx);

    // Gather all non-empty values
    let mut values = vec![];

    for i in 0..length {
        index_key.replace(PropertyKey::from_u64(cx, i));
        if !IGNORE_HOLES || has_property(cx, object, index_key)? {
            let value = get(cx, object, index_key)?;
            values.push(value);
        }
    }

    merge_sort(cx, &values, &mut |cx, v1, v2| {
        if IS_TYPED_ARRAY {
            compare_typed_array_elements(cx, v1, v2, compare_function)
        } else {
            compare_array_elements(cx, v1, v2, compare_function)
        }
    })
}

/// CompareArrayElements (https://tc39.es/ecma262/#sec-comparearrayelements)
fn compare_array_elements(
    cx: Context,
    v1: Handle<Value>,
    v2: Handle<Value>,
    compare_function: Handle<Value>,
) -> EvalResult<Ordering> {
    let v1_is_undefined = v1.is_undefined();
    let v2_is_undefined = v2.is_undefined();
    if v1_is_undefined && v2_is_undefined {
        return Ok(Ordering::Equal);
    } else if v1_is_undefined {
        return Ok(Ordering::Greater);
    } else if v2_is_undefined {
        return Ok(Ordering::Less);
    }

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

    // Otherwise convert to strings and compare
    let v1_string = to_string(cx, v1)?;
    let v2_string = to_string(cx, v2)?;

    if must!(is_less_than(cx, v1_string.into(), v2_string.into())).is_true() {
        Ok(Ordering::Less)
    } else if must!(is_less_than(cx, v2_string.into(), v1_string.into())).is_true() {
        Ok(Ordering::Greater)
    } else {
        Ok(Ordering::Equal)
    }
}

/// Naive merge sort where comparator function may have an abrupt completion.
///
/// Much room for optimization.
fn merge_sort<F>(cx: Context, items: &[Handle<Value>], f: &mut F) -> EvalResult<Vec<Handle<Value>>>
where
    F: FnMut(Context, Handle<Value>, Handle<Value>) -> EvalResult<Ordering>,
{
    if items.len() <= 1 {
        return Ok(items.to_vec());
    }

    let (first_half, second_half) = items.split_at(items.len() / 2);
    let merged_first_half = merge_sort(cx, first_half, f)?;
    let merged_second_half = merge_sort(cx, second_half, f)?;

    let mut result = vec![];
    result.reserve_exact(merged_first_half.len() + merged_second_half.len());

    let mut i = 0;
    let mut j = 0;

    while i < merged_first_half.len() && j < merged_second_half.len() {
        let v1 = merged_first_half[i];
        let v2 = merged_second_half[j];

        let ordering = f(cx, v1, v2)?;

        if ordering.is_gt() {
            result.push(v2);
            j += 1;
        } else {
            result.push(v1);
            i += 1;
        }
    }

    while i < merged_first_half.len() {
        result.push(merged_first_half[i]);
        i += 1;
    }

    while j < merged_second_half.len() {
        result.push(merged_second_half[j]);
        j += 1;
    }

    Ok(result)
}
