use crate::{
    js::runtime::{
        abstract_operations::{
            call_object, construct, create_data_property_or_throw, get_method,
            length_of_array_like, set,
        },
        array_object::array_create,
        builtin_function::BuiltinFunction,
        error::{range_error_, type_error, type_error_},
        function::get_argument,
        get,
        iterator::iter_iterator_method_values,
        numeric_constants::MAX_SAFE_INTEGER_U64,
        object_value::ObjectValue,
        ordinary_object::get_prototype_from_constructor,
        property_key::PropertyKey,
        type_utilities::{is_array, is_callable, is_constructor_value, to_object, to_uint32},
        Completion, Context, EvalResult, Handle, Realm, Value,
    },
    maybe, must,
};

use super::intrinsics::Intrinsic;

pub struct ArrayConstructor;

impl ArrayConstructor {
    // 23.1.2 Properties of the Array Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.array(),
            Some(realm),
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::ArrayPrototype).into(),
        );

        func.intrinsic_func(cx, cx.names.from(), Self::from, 1, realm);
        func.intrinsic_func(cx, cx.names.is_array(), Self::is_array, 1, realm);
        func.intrinsic_func(cx, cx.names.of(), Self::of, 0, realm);

        // 23.1.2.5 get Array [ @@species ]
        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, Self::get_species, realm);

        func
    }

    // 23.1.1.1 Array
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = new_target.unwrap_or_else(|| cx.current_function());
        let proto =
            maybe!(get_prototype_from_constructor(cx, new_target, Intrinsic::ArrayPrototype));

        if arguments.is_empty() {
            return must!(array_create(cx, 0, Some(proto))).into();
        } else if arguments.len() == 1 {
            let length = get_argument(cx, arguments, 0);
            let array = must!(array_create(cx, 0, Some(proto)));

            let int_len = if length.is_number() {
                let int_len = must!(to_uint32(cx, length));

                if int_len as f64 != length.as_number() {
                    return range_error_(cx, "invalid array size");
                }

                int_len
            } else {
                let first_key = PropertyKey::array_index(cx, 0).to_handle(cx);
                must!(create_data_property_or_throw(cx, array.into(), first_key, length));
                1
            };

            let int_len_value = Value::from(int_len).to_handle(cx);
            must!(set(cx, array.into(), cx.names.length(), int_len_value, true));

            return array.into();
        } else {
            let array = maybe!(array_create(cx, arguments.len() as u64, Some(proto)));

            // Property key is shared between iterations
            let mut key = PropertyKey::uninit().to_handle(cx);

            for index in 0..arguments.len() {
                key.replace(PropertyKey::array_index(cx, index as u32));
                let value = get_argument(cx, arguments, index);

                must!(create_data_property_or_throw(cx, array.into(), key, value));
            }

            return array.into();
        }
    }

    // 23.1.2.1 Array.from
    pub fn from(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        // Determine if map function was provided and is callable
        let map_function_arg = get_argument(cx, arguments, 1);
        let map_function = if map_function_arg.is_undefined() {
            None
        } else {
            if !is_callable(map_function_arg) {
                return type_error_(cx, "Array.from map function is not callable");
            }

            Some(map_function_arg.as_object())
        };

        let items_arg = get_argument(cx, arguments, 0);
        let this_arg = get_argument(cx, arguments, 2);

        // If an iterator was supplied use it to create array
        let iterator = maybe!(get_method(cx, items_arg, cx.well_known_symbols.iterator()));
        if let Some(iterator) = iterator {
            let array = if is_constructor_value(cx, this_value) {
                maybe!(construct(cx, this_value.as_object(), &[], None))
            } else {
                must!(array_create(cx, 0, None)).into()
            };

            // Handle is shared between iterations
            let mut key = PropertyKey::uninit().to_handle(cx);
            let mut index_value: Handle<Value> = Handle::empty(cx);
            let mut i = 0;

            let completion =
                iter_iterator_method_values(cx, items_arg, iterator, &mut |cx, value| {
                    if i >= MAX_SAFE_INTEGER_U64 {
                        return Some(type_error(cx, "array is too large"));
                    }

                    let value = if let Some(map_function) = map_function {
                        index_value.replace(Value::from(i));

                        // Apply map function if present, returning if abnormal completion
                        let result = call_object(cx, map_function, this_arg, &[value, index_value]);
                        match result {
                            EvalResult::Ok(mapped_value) => mapped_value,
                            EvalResult::Throw(_) => return Some(result.into()),
                        }
                    } else {
                        value
                    };

                    key.replace(PropertyKey::from_u64(cx, i));

                    // Append value to array, returning if abnormal completion
                    let result = create_data_property_or_throw(cx, array, key, value);
                    if let EvalResult::Throw(thrown_value) = result {
                        return Some(Completion::throw(thrown_value));
                    }

                    i += 1;

                    None
                });

            maybe!(completion.into_eval_result());

            let length_value = Value::from(i).to_handle(cx);
            maybe!(set(cx, array, cx.names.length(), length_value, true));

            return array.into();
        }

        // Otherwise assume items arg is array like and copy elements from it
        let array_like = must!(to_object(cx, items_arg));
        let length = maybe!(length_of_array_like(cx, array_like));
        let length_value = Value::from(length).to_handle(cx);

        let array = if is_constructor_value(cx, this_value) {
            maybe!(construct(cx, this_value.as_object(), &[length_value], None))
        } else {
            maybe!(array_create(cx, length, None)).into()
        };

        // Handles are shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);
        let mut index_value: Handle<Value> = Handle::empty(cx);

        // Copy elements from items array
        for i in 0..length {
            key.replace(PropertyKey::from_u64(cx, i));

            let mut value = maybe!(get(cx, array_like, key));

            // Apply map function if present
            if let Some(map_function) = map_function {
                index_value.replace(Value::from(i));
                value = maybe!(call_object(cx, map_function, this_arg, &[value, index_value]));
            }

            maybe!(create_data_property_or_throw(cx, array, key, value));
        }

        maybe!(set(cx, array, cx.names.length(), length_value, true));

        array.into()
    }

    // 23.1.2.2 Array.isArray
    pub fn is_array(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let argument = get_argument(cx, arguments, 0);
        let is_array = maybe!(is_array(cx, argument));
        cx.bool(is_array).into()
    }

    // 23.1.2.3 Array.of
    pub fn of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let length = arguments.len();
        let length_value = Value::from(length).to_handle(cx);

        let array = if is_constructor_value(cx, this_value) {
            maybe!(construct(cx, this_value.as_object(), &[length_value], None))
        } else {
            maybe!(array_create(cx, length as u64, None)).into()
        };

        let mut key = PropertyKey::uninit().to_handle(cx);

        for index in 0..length {
            key.replace(PropertyKey::array_index(cx, index as u32));
            let value = get_argument(cx, arguments, index);

            maybe!(create_data_property_or_throw(cx, array.into(), key, value));
        }

        maybe!(set(cx, array.into(), cx.names.length(), length_value, true));

        array.into()
    }

    // 23.1.2.5 get Array [ @@species ]
    pub fn get_species(
        _: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_value.into()
    }
}
