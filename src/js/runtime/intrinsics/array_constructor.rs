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
        Context, EvalResult, Gc, Realm, Value,
    },
    maybe, must,
};

use super::intrinsics::Intrinsic;

pub struct ArrayConstructor;

impl ArrayConstructor {
    // 23.1.2 Properties of the Array Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            0,
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
        let species_key = PropertyKey::symbol(cx.well_known_symbols.species);
        func.intrinsic_getter(cx, species_key, Self::get_species, realm);

        func
    }

    // 23.1.1.1 Array
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let new_target =
            new_target.unwrap_or_else(|| cx.current_execution_context().function().unwrap());
        let proto =
            maybe!(get_prototype_from_constructor(cx, new_target, Intrinsic::ArrayPrototype));

        if arguments.is_empty() {
            return must!(array_create(cx, 0, Some(proto))).into();
        } else if arguments.len() == 1 {
            let length = get_argument(arguments, 0);
            let array = must!(array_create(cx, 0, Some(proto)));

            let int_len = if length.is_number() {
                let int_len = must!(to_uint32(cx, length));

                if int_len as f64 != length.as_number() {
                    return range_error_(cx, "invalid array size");
                }

                int_len
            } else {
                let first_key = PropertyKey::array_index(cx, 0);
                must!(create_data_property_or_throw(cx, array.into(), first_key, length));
                1
            };

            must!(set(cx, array.into(), cx.names.length(), Value::from(int_len), true));

            return array.into();
        } else {
            let array = maybe!(array_create(cx, arguments.len() as u64, Some(proto)));

            for index in 0..arguments.len() {
                let key = PropertyKey::array_index(cx, index as u32);
                let value = get_argument(arguments, index);

                must!(create_data_property_or_throw(cx, array.into(), key, value));
            }

            return array.into();
        }
    }

    // 23.1.2.2 Array.isArray
    fn is_array(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        maybe!(is_array(cx, get_argument(arguments, 0))).into()
    }

    // 23.1.2.3 Array.of
    fn of(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let length = arguments.len();
        let length_value = Value::from(length);

        let array = if is_constructor(this_value) {
            maybe!(construct(cx, this_value.as_object(), &[length_value], None))
        } else {
            maybe!(array_create(cx, length as u64, None)).into()
        };

        for index in 0..length {
            let key = PropertyKey::array_index(cx, index as u32);
            let value = get_argument(arguments, index);

            maybe!(create_data_property_or_throw(cx, array.into(), key, value));
        }

        maybe!(set(cx, array.into(), cx.names.length(), length_value, true));

        array.into()
    }

    // 23.1.2.5 get Array [ @@species ]
    fn get_species(
        _: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        this_value.into()
    }
}
