use crate::{
    js::runtime::{
        abstract_operations::{construct, create_data_property_or_throw, set},
        array_object::array_create,
        builtin_function::BuiltinFunction,
        error::range_error_,
        function::get_argument,
        object_value::ObjectValue,
        ordinary_object::get_prototype_from_constructor,
        property::Property,
        property_key::PropertyKey,
        type_utilities::{is_array, is_constructor, to_uint32},
        Context, EvalResult, Handle, Realm, Value,
    },
    maybe, must,
};

use super::intrinsics::Intrinsic;

pub struct ArrayConstructor;

impl ArrayConstructor {
    // 23.1.2 Properties of the Array Constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            cx.names.array(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            cx,
            cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::ArrayPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func.intrinsic_func(cx, cx.names.is_array(), Self::is_array, 1, realm);
        func.intrinsic_func(cx, cx.names.of(), Self::of, 0, realm);

        // 23.1.2.5 get Array [ @@species ]
        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, Self::get_species, realm);

        func
    }

    // 23.1.1.1 Array
    fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target =
            new_target.unwrap_or_else(|| cx.current_execution_context_ptr().function());
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

    // 23.1.2.2 Array.isArray
    fn is_array(
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
    fn of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let length = arguments.len();
        let length_value = Value::from(length).to_handle(cx);

        let array = if is_constructor(this_value) {
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
    fn get_species(
        _: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        this_value.into()
    }
}
