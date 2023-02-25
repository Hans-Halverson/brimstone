use crate::{
    js::runtime::{
        abstract_operations::call_object,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        gc::Gc,
        object_value::{Object, ObjectValue},
        ordinary_object::OrdinaryObject,
        property::Property,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::is_callable,
        value::Value,
        Context,
    },
    maybe,
};

use super::{
    intrinsics::Intrinsic,
    set_constructor::SetObject,
    set_iterator::{SetIterator, SetIteratorKind},
};

pub struct SetPrototype;

impl SetPrototype {
    // 24.2.3 Properties of the Set Prototype Object
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

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

        // Constructor property is added once SetConstructor has been created
        object.intrinsic_func(cx, &cx.names.add(), Self::add, 1, realm);
        object.intrinsic_func(cx, &cx.names.clear(), Self::clear, 0, realm);
        object.intrinsic_func(cx, &cx.names.delete(), Self::delete, 1, realm);
        object.intrinsic_func(cx, &cx.names.entries(), Self::entries, 0, realm);
        object.intrinsic_func(cx, &cx.names.for_each(), Self::for_each, 1, realm);
        object.intrinsic_func(cx, &cx.names.has(), Self::has, 1, realm);
        object.intrinsic_data_prop(&cx.names.keys(), values_function);
        object.intrinsic_getter(cx, &cx.names.size(), Self::size, realm);
        object.intrinsic_data_prop(&cx.names.values(), values_function);

        // 24.2.3.11 Set.prototype [ @@iterator ]
        let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
        object.set_property(&iterator_key, Property::data(values_function, true, false, true));

        // 24.2.3.12 Set.prototype [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        object.set_property(
            &to_string_tag_key,
            Property::data(cx.names.set().as_string().into(), false, false, true),
        );

        cx.heap.alloc(object).into()
    }

    // 24.2.3.1 Set.prototype.add
    fn add(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "add method must be called on set");
        };

        // Convert negative zero to positive zero in set
        let mut value = get_argument(arguments, 0);
        if value.is_negative_zero() {
            value = Value::number(0.0);
        }

        set.set_data().insert(value);

        this_value.into()
    }

    // 24.2.3.2 Set.prototype.clear
    fn clear(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "clear method must be called on set");
        };

        set.set_data().clear();

        Value::undefined().into()
    }

    // 24.2.3.4 Set.prototype.delete
    fn delete(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "delete method must be called on set");
        };

        let key = get_argument(arguments, 0);
        let existed = set.set_data().remove(key);

        existed.into()
    }

    // 24.2.3.5 Set.prototype.entries
    fn entries(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "entries method must be called on set");
        };

        SetIterator::new(cx, set, SetIteratorKind::KeyAndValue).into()
    }

    // 24.2.3.6 Set.prototype.forEach
    fn for_each(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "forEach method must be called on set");
        };

        let callback_function = get_argument(arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(arguments, 1);

        for value in set.set_data().iter() {
            let value = (*value).into();
            let arguments = [value, value, this_value];

            maybe!(call_object(cx, callback_function, this_arg, &arguments));
        }

        SetIterator::new(cx, set, SetIteratorKind::KeyAndValue).into()
    }

    // 24.2.3.7 Set.prototype.has
    fn has(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "has method must be called on set");
        };

        let value = get_argument(arguments, 0);

        set.set_data().contains(value).into()
    }

    // 24.2.3.9 get Set.prototype.size
    fn size(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let mut set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "size accessor must be called on set");
        };

        Value::from_u64(set.set_data().len() as u64).into()
    }

    // 24.2.3.10 Set.prototype.values
    fn values(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "values method must be called on set");
        };

        SetIterator::new(cx, set, SetIteratorKind::Value).into()
    }
}

fn this_set_value(value: Value) -> Option<Gc<SetObject>> {
    if !value.is_object() {
        return None;
    }

    let object = value.as_object();
    if !object.is_set_object() {
        return None;
    }

    Some(object.cast::<SetObject>())
}
