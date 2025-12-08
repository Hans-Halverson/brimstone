use crate::{
    must,
    runtime::{
        abstract_operations::{
            call_object, construct, create_data_property_or_throw, get_method,
            length_of_array_like, set,
        },
        alloc_error::AllocResult,
        array_object::array_create,
        builtin_function::BuiltinFunction,
        error::{range_error, type_error},
        function::get_argument,
        get,
        iterator::iter_iterator_method_values,
        numeric_constants::MAX_SAFE_INTEGER_U64,
        object_value::ObjectValue,
        ordinary_object::get_prototype_from_constructor,
        property_key::PropertyKey,
        type_utilities::{is_array, is_callable, is_constructor_value, to_object, to_uint32},
        Context, EvalResult, Handle, Realm, Value,
    },
};

use super::{intrinsics::Intrinsic, rust_runtime::return_this};

pub struct ArrayConstructor;

impl ArrayConstructor {
    /// Properties of the Array Constructor (https://tc39.es/ecma262/#sec-properties-of-the-array-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.array(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::ArrayPrototype).into(),
        )?;

        func.intrinsic_func(cx, cx.names.from(), Self::from, 1, realm)?;
        func.intrinsic_func(cx, cx.names.is_array(), Self::is_array, 1, realm)?;
        func.intrinsic_func(cx, cx.names.of(), Self::of, 0, realm)?;

        // get Array [ @@species ] (https://tc39.es/ecma262/#sec-get-array-%symbol.species%)
        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, return_this, realm)?;

        Ok(func)
    }

    /// Array (https://tc39.es/ecma262/#sec-array)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let new_target = cx
            .current_new_target()
            .unwrap_or_else(|| cx.current_function());
        let proto = get_prototype_from_constructor(cx, new_target, Intrinsic::ArrayPrototype)?;

        if arguments.is_empty() {
            Ok(must!(array_create(cx, 0, Some(proto))).as_value())
        } else if arguments.len() == 1 {
            let length = get_argument(cx, arguments, 0);
            let array = must!(array_create(cx, 0, Some(proto)));

            let int_len = if length.is_number() {
                let int_len = must!(to_uint32(cx, length));

                if int_len as f64 != length.as_number() {
                    return range_error(cx, "invalid array size");
                }

                int_len
            } else {
                let first_key = PropertyKey::array_index_handle(cx, 0)?;
                must!(create_data_property_or_throw(cx, array.into(), first_key, length));
                1
            };

            let int_len_value = Value::from(int_len).to_handle(cx);
            must!(set(cx, array.into(), cx.names.length(), int_len_value, true));

            Ok(array.as_value())
        } else {
            let array = array_create(cx, arguments.len() as u64, Some(proto))?;

            // Property key is shared between iterations
            let mut key = PropertyKey::uninit().to_handle(cx);

            for index in 0..arguments.len() {
                key.replace(PropertyKey::array_index(cx, index as u32)?);
                let value = get_argument(cx, arguments, index);

                must!(create_data_property_or_throw(cx, array.into(), key, value));
            }

            Ok(array.as_value())
        }
    }

    /// Array.from (https://tc39.es/ecma262/#sec-array.from)
    pub fn from(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        // Determine if map function was provided and is callable
        let map_function_arg = get_argument(cx, arguments, 1);
        let map_function = if map_function_arg.is_undefined() {
            None
        } else {
            if !is_callable(map_function_arg) {
                return type_error(cx, "Array.from map function is not callable");
            }

            Some(map_function_arg.as_object())
        };

        let items_arg = get_argument(cx, arguments, 0);
        let this_arg = get_argument(cx, arguments, 2);

        // If an iterator was supplied use it to create array
        let iterator = get_method(cx, items_arg, cx.well_known_symbols.iterator())?;
        if let Some(iterator) = iterator {
            let array = if is_constructor_value(this_value) {
                construct(cx, this_value.as_object(), &[], None)?
            } else {
                must!(array_create(cx, 0, None)).into()
            };

            // Handle is shared between iterations
            let mut key = PropertyKey::uninit().to_handle(cx);
            let mut index_value: Handle<Value> = Handle::empty(cx);
            let mut i = 0;

            iter_iterator_method_values(cx, items_arg, iterator, &mut |cx, value| {
                if i >= MAX_SAFE_INTEGER_U64 {
                    return Some(type_error(cx, "array is too large"));
                }

                let value = if let Some(map_function) = map_function {
                    index_value.replace(Value::from(i));

                    // Apply map function if present, returning if abnormal completion
                    let result = call_object(cx, map_function, this_arg, &[value, index_value]);
                    match result {
                        Ok(mapped_value) => mapped_value,
                        Err(_) => return Some(result),
                    }
                } else {
                    value
                };

                // May allocate. Propagate allocation error upwards.
                match PropertyKey::from_u64(cx, i) {
                    Ok(k) => key.replace(k),
                    Err(err) => return Some(Err(err.into())),
                }

                // Append value to array, returning if abnormal completion
                let result = create_data_property_or_throw(cx, array, key, value);
                if let Err(error) = result {
                    return Some(Err(error));
                }

                i += 1;

                None
            })?;

            let length_value = Value::from(i).to_handle(cx);
            set(cx, array, cx.names.length(), length_value, true)?;

            return Ok(array.as_value());
        }

        // Otherwise assume items arg is array like and copy elements from it
        let array_like = must!(to_object(cx, items_arg));
        let length = length_of_array_like(cx, array_like)?;
        let length_value = Value::from(length).to_handle(cx);

        let array = if is_constructor_value(this_value) {
            construct(cx, this_value.as_object(), &[length_value], None)?
        } else {
            array_create(cx, length, None)?.into()
        };

        // Handles are shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);
        let mut index_value: Handle<Value> = Handle::empty(cx);

        // Copy elements from items array
        for i in 0..length {
            key.replace(PropertyKey::from_u64(cx, i)?);

            let mut value = get(cx, array_like, key)?;

            // Apply map function if present
            if let Some(map_function) = map_function {
                index_value.replace(Value::from(i));
                value = call_object(cx, map_function, this_arg, &[value, index_value])?;
            }

            create_data_property_or_throw(cx, array, key, value)?;
        }

        set(cx, array, cx.names.length(), length_value, true)?;

        Ok(array.as_value())
    }

    /// Array.isArray (https://tc39.es/ecma262/#sec-array.isarray)
    pub fn is_array(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let is_array = is_array(cx, argument)?;
        Ok(cx.bool(is_array))
    }

    /// Array.of (https://tc39.es/ecma262/#sec-array.of)
    pub fn of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let length = arguments.len();
        let length_value = Value::from(length).to_handle(cx);

        let array = if is_constructor_value(this_value) {
            construct(cx, this_value.as_object(), &[length_value], None)?
        } else {
            array_create(cx, length as u64, None)?.into()
        };

        let mut key = PropertyKey::uninit().to_handle(cx);

        for index in 0..length {
            key.replace(PropertyKey::array_index(cx, index as u32)?);
            let value = get_argument(cx, arguments, index);

            create_data_property_or_throw(cx, array, key, value)?;
        }

        set(cx, array, cx.names.length(), length_value, true)?;

        Ok(array.as_value())
    }
}
