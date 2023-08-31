use crate::{
    js::runtime::{
        abstract_operations::call_object,
        builtin_function::BuiltinFunction,
        collections::BsIndexSetField,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        object_value::ObjectValue,
        property::Property,
        realm::Realm,
        type_utilities::is_callable,
        value::{Value, ValueCollectionKey},
        Context, Handle,
    },
    maybe,
};

use super::{
    intrinsics::Intrinsic,
    set_iterator::{SetIterator, SetIteratorKind},
    set_object::SetObject,
};

pub struct SetPrototype;

impl SetPrototype {
    // 24.2.3 Properties of the Set Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

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

        // Constructor property is added once SetConstructor has been created
        object.intrinsic_func(cx, cx.names.add(), Self::add, 1, realm);
        object.intrinsic_func(cx, cx.names.clear(), Self::clear, 0, realm);
        object.intrinsic_func(cx, cx.names.delete(), Self::delete, 1, realm);
        object.intrinsic_func(cx, cx.names.entries(), Self::entries, 0, realm);
        object.intrinsic_func(cx, cx.names.for_each(), Self::for_each, 1, realm);
        object.intrinsic_func(cx, cx.names.has(), Self::has, 1, realm);
        object.intrinsic_data_prop(cx, cx.names.keys(), values_function);
        object.intrinsic_getter(cx, cx.names.size(), Self::size, realm);
        object.intrinsic_data_prop(cx, cx.names.values(), values_function);

        // 24.2.3.11 Set.prototype [ @@iterator ]
        let iterator_key = cx.well_known_symbols.iterator();
        object.set_property(cx, iterator_key, Property::data(values_function, true, false, true));

        // 24.2.3.12 Set.prototype [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.set().as_string().into(), false, false, true),
        );

        object
    }

    // 24.2.3.1 Set.prototype.add
    fn add(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "add method must be called on set");
        };

        // Convert negative zero to positive zero in set
        let mut value = get_argument(cx, arguments, 0);
        if value.is_negative_zero() {
            value = Value::smi(0).to_handle(cx);
        }

        set.set_data_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(ValueCollectionKey::from(value));

        this_value.into()
    }

    // 24.2.3.2 Set.prototype.clear
    fn clear(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "clear method must be called on set");
        };

        set.set_data().clear();

        cx.undefined().into()
    }

    // 24.2.3.4 Set.prototype.delete
    fn delete(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "delete method must be called on set");
        };

        let key = get_argument(cx, arguments, 0);
        let existed = set.set_data().remove(&ValueCollectionKey::from(key));

        cx.bool(existed).into()
    }

    // 24.2.3.5 Set.prototype.entries
    fn entries(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "entries method must be called on set");
        };

        SetIterator::new(cx, set, SetIteratorKind::KeyAndValue).into()
    }

    // 24.2.3.6 Set.prototype.forEach
    fn for_each(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "forEach method must be called on set");
        };

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error_(cx, "expected function");
        }

        let callback_function = callback_function.as_object();
        let this_arg = get_argument(cx, arguments, 1);

        // Share handle across iterations
        let mut value_handle = Handle::<Value>::empty(cx);

        // Must use gc and invalidation safe iteration since arbitrary code can be executed between
        // iterations.
        for (value, _) in set.set_data().to_handle().iter_gc_safe() {
            value_handle.replace(value.into());

            let arguments = [value_handle, value_handle, this_value];
            maybe!(call_object(cx, callback_function, this_arg, &arguments));
        }

        cx.undefined().into()
    }

    // 24.2.3.7 Set.prototype.has
    fn has(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "has method must be called on set");
        };

        let value = get_argument(cx, arguments, 0);

        cx.bool(set.set_data().contains(&ValueCollectionKey::from(value)))
            .into()
    }

    // 24.2.3.9 get Set.prototype.size
    fn size(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "size accessor must be called on set");
        };

        Value::from(set.set_data().num_entries_occupied())
            .to_handle(cx)
            .into()
    }

    // 24.2.3.10 Set.prototype.values
    fn values(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error_(cx, "values method must be called on set");
        };

        SetIterator::new(cx, set, SetIteratorKind::Value).into()
    }
}

fn this_set_value(value: Handle<Value>) -> Option<Handle<SetObject>> {
    if !value.is_object() {
        return None;
    }

    let object = value.as_object();
    if !object.is_set_object() {
        return None;
    }

    Some(object.cast::<SetObject>())
}
