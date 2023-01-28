use crate::{
    js::runtime::{
        abstract_operations::{call_object, length_of_array_like, set},
        array_object::ArrayObject,
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
        type_utilities::{is_callable, to_object},
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
        object.intrinsic_func(cx, &cx.names.entries(), Self::entries, 0, realm);
        object.intrinsic_func(cx, &cx.names.join(), Self::join, 1, realm);
        object.intrinsic_func(cx, &cx.names.keys(), Self::keys, 0, realm);
        object.intrinsic_func(cx, &cx.names.push(), Self::push, 0, realm);
        object.intrinsic_func(cx, &cx.names.to_string(), Self::to_string, 0, realm);
        object.intrinsic_data_prop(&cx.names.values(), values_function);

        // 23.1.3.34 Array.prototype [ @@iterator ]
        let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
        object.set_property(&iterator_key, Property::data(values_function, true, false, true));

        cx.heap.alloc(ArrayObject::new(object)).into()
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
