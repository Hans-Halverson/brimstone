use crate::{
    js::runtime::{
        abstract_operations::{call_object, canonicalize_keyed_collection_key},
        builtin_function::BuiltinFunction,
        collections::BsIndexSetField,
        completion::EvalResult,
        error::type_error,
        function::get_argument,
        get,
        intrinsics::set_object::ValueSet,
        iterator::{
            get_iterator, iter_iterator_method_values, iterator_step, iterator_value, IteratorHint,
        },
        object_value::ObjectValue,
        property::Property,
        realm::Realm,
        type_utilities::{is_callable, to_integer_or_infinity, to_number},
        value::{Value, ValueCollectionKey},
        Context, Handle,
    },
    maybe, must,
};

use super::{
    intrinsics::Intrinsic,
    set_iterator::{SetIterator, SetIteratorKind},
    set_object::{SetObject, SetObjectSetField},
};

pub struct SetPrototype;

impl SetPrototype {
    // 24.2.3 Properties of the Set Prototype Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Create values function as it is referenced by multiple properties
        let values_function =
            BuiltinFunction::create(cx, Self::values, 0, cx.names.values(), realm, None, None)
                .into();

        // Constructor property is added once SetConstructor has been created
        object.intrinsic_func(cx, cx.names.add(), Self::add, 1, realm);
        object.intrinsic_func(cx, cx.names.clear(), Self::clear, 0, realm);
        object.intrinsic_func(cx, cx.names.delete(), Self::delete, 1, realm);
        object.intrinsic_func(cx, cx.names.difference(), Self::difference, 1, realm);
        object.intrinsic_func(cx, cx.names.entries(), Self::entries, 0, realm);
        object.intrinsic_func(cx, cx.names.for_each(), Self::for_each, 1, realm);
        object.intrinsic_func(cx, cx.names.has(), Self::has, 1, realm);
        object.intrinsic_func(cx, cx.names.intersection(), Self::intersection, 1, realm);
        object.intrinsic_func(cx, cx.names.is_disjoint_from(), Self::is_disjoint_from, 1, realm);
        object.intrinsic_func(cx, cx.names.is_subset_of(), Self::is_subset_of, 1, realm);
        object.intrinsic_func(cx, cx.names.is_superset_of(), Self::is_superset_of, 1, realm);
        object.intrinsic_data_prop(cx, cx.names.keys(), values_function);
        object.intrinsic_getter(cx, cx.names.size(), Self::size, realm);
        object.intrinsic_func(
            cx,
            cx.names.symmetric_difference(),
            Self::symmetric_difference,
            1,
            realm,
        );
        object.intrinsic_func(cx, cx.names.union(), Self::union, 1, realm);
        object.intrinsic_data_prop(cx, cx.names.values(), values_function);

        // 24.2.3.18 Set.prototype [ @@iterator ]
        let iterator_key = cx.well_known_symbols.iterator();
        object.set_property(cx, iterator_key, Property::data(values_function, true, false, true));

        // 24.2.3.19 Set.prototype [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.set().as_string().into(), false, false, true),
        );

        object
    }

    // 24.2.3.1 Set.prototype.add
    pub fn add(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "add method must be called on set");
        };

        // Convert negative zero to positive zero in set
        let value = get_argument(cx, arguments, 0);
        let value = canonicalize_keyed_collection_key(cx, value);

        set.insert(cx, ValueCollectionKey::from(value));

        this_value.into()
    }

    // 24.2.3.2 Set.prototype.clear
    pub fn clear(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "clear method must be called on set");
        };

        set.set_data().clear();

        cx.undefined().into()
    }

    // 24.2.3.4 Set.prototype.delete
    pub fn delete(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "delete method must be called on set");
        };

        let key = get_argument(cx, arguments, 0);
        let existed = set.set_data().remove(&ValueCollectionKey::from(key));

        cx.bool(existed).into()
    }

    // 24.2.4.5 Set.prototype.difference
    pub fn difference(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "difference method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = maybe!(get_set_record(cx, other));

        // Create a copy of this set
        let new_set_data = ValueSet::new_from_set(cx, this_set.set_data()).to_handle();
        let new_set = SetObject::new_from_set(cx, new_set_data).to_handle();

        if this_set.set_data().num_entries_occupied() as f64 <= other_set_record.size {
            // If this set is smaller or equal to the other set, iterate through this set's keys and
            // determine if they are in the other set by calling the other set's `has` method. Then
            // remove the key from the new set if it is in the other set.

            // Handle is shared between iterations
            let mut item_handle = Handle::<Value>::empty(cx);

            for (item, _) in new_set.set_data().to_handle().iter_gc_safe() {
                item_handle.replace(item.get());

                let in_other = maybe!(call_object(
                    cx,
                    other_set_record.has_method,
                    other_set_record.set_object.into(),
                    &[item_handle]
                ));

                if in_other.is_true() {
                    new_set.set_data().remove(&item);
                }
            }
        } else {
            // Otherwise iterate through other set's keys and remove them from the new set
            maybe!(iter_iterator_method_values(
                cx,
                other_set_record.set_object.into(),
                other_set_record.keys_method,
                &mut |cx, key| {
                    let key = ValueCollectionKey::from(canonicalize_keyed_collection_key(cx, key));
                    new_set.set_data().remove(&key);

                    None
                }
            ));
        }

        new_set.into()
    }

    // 24.2.3.6 Set.prototype.entries
    pub fn entries(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "entries method must be called on set");
        };

        SetIterator::new(cx, set, SetIteratorKind::KeyAndValue).into()
    }

    // 24.2.3.7 Set.prototype.forEach
    pub fn for_each(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "forEach method must be called on set");
        };

        let callback_function = get_argument(cx, arguments, 0);
        if !is_callable(callback_function) {
            return type_error(cx, "expected function");
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

    // 24.2.3.8 Set.prototype.has
    pub fn has(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "has method must be called on set");
        };

        let value = get_argument(cx, arguments, 0);

        cx.bool(set.set_data().contains(&ValueCollectionKey::from(value)))
            .into()
    }

    // 24.2.4.9 Set.prototype.intersection
    pub fn intersection(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "intersection method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = maybe!(get_set_record(cx, other));

        // Create an empty set
        let new_set_data = SetObjectSetField::new(cx, ValueSet::MIN_CAPACITY).to_handle();
        let new_set = SetObject::new_from_set(cx, new_set_data).to_handle();

        if this_set.set_data().num_entries_occupied() as f64 <= other_set_record.size {
            // If this set is smaller or equal to the other set, iterate through this set's keys and
            // determine if they are in the other set by calling the other set's `has` method. Then
            // add the key to the new set if it is in the other set.

            // Handle is shared between iterations
            let mut item_handle = Handle::<Value>::empty(cx);

            for (item, _) in this_set.set_data().to_handle().iter_gc_safe() {
                item_handle.replace(item.get());

                let in_other = maybe!(call_object(
                    cx,
                    other_set_record.has_method,
                    other_set_record.set_object.into(),
                    &[item_handle]
                ));

                if in_other.is_true() {
                    new_set.insert(cx, item);
                }
            }
        } else {
            // Otherwise iterate through other set's keys and add them to the new set if they are
            // also in this set.
            maybe!(iter_iterator_method_values(
                cx,
                other_set_record.set_object.into(),
                other_set_record.keys_method,
                &mut |cx, key| {
                    let key = ValueCollectionKey::from(canonicalize_keyed_collection_key(cx, key));

                    if this_set.set_data().contains(&key) {
                        new_set.insert(cx, key);
                    }

                    None
                }
            ));
        }

        new_set.into()
    }

    // 24.2.4.10 Set.prototype.isDisjointFrom
    pub fn is_disjoint_from(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "isDisjointFrom method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = maybe!(get_set_record(cx, other));

        if this_set.set_data().num_entries_occupied() as f64 <= other_set_record.size {
            // If this set is smaller or equal to the other set, iterate through this set's keys and
            // determine if they are in the other set by calling the other set's `has` method.

            // Handle is shared between iterations
            let mut item_handle = Handle::<Value>::empty(cx);

            for (item, _) in this_set.set_data().to_handle().iter_gc_safe() {
                item_handle.replace(item.get());

                let in_other = maybe!(call_object(
                    cx,
                    other_set_record.has_method,
                    other_set_record.set_object.into(),
                    &[item_handle]
                ));

                // Return as soon as we find an element of this set that is in the other set
                if in_other.is_true() {
                    return cx.bool(false).into();
                }
            }
        } else {
            // Otherwise iterate through other set's keys and check if they are in this set
            let iterator = maybe!(get_iterator(
                cx,
                other_set_record.set_object.into(),
                IteratorHint::Sync,
                Some(other_set_record.keys_method)
            ));

            while let Some(iter_result) = maybe!(iterator_step(cx, &iterator)) {
                let item = maybe!(iterator_value(cx, iter_result));

                // Return as soon as we find an element of the other set that is in this set
                if this_set
                    .set_data()
                    .contains(&ValueCollectionKey::from(item))
                {
                    return cx.bool(false).into();
                }
            }
        }

        cx.bool(true).into()
    }

    // 24.2.4.11 Set.prototype.isSubsetOf
    pub fn is_subset_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "isSubsetOf method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = maybe!(get_set_record(cx, other));

        // We can return early if this set is larger than the other set
        if (this_set.set_data().num_entries_occupied() as f64) > other_set_record.size {
            return cx.bool(false).into();
        }

        // If this set is smaller or equal to the other set, iterate through this set's keys and
        // determine if they are in the other set by calling the other set's `has` method.

        // Handle is shared between iterations
        let mut item_handle = Handle::<Value>::empty(cx);

        for (item, _) in this_set.set_data().to_handle().iter_gc_safe() {
            item_handle.replace(item.get());

            let in_other = maybe!(call_object(
                cx,
                other_set_record.has_method,
                other_set_record.set_object.into(),
                &[item_handle]
            ));

            if !in_other.is_true() {
                return cx.bool(false).into();
            }
        }

        cx.bool(true).into()
    }

    // 24.2.4.12 Set.prototype.isSupersetOf
    pub fn is_superset_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "isSupersetOf method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = maybe!(get_set_record(cx, other));

        // We can return early if this set is smaller than the other set
        if (this_set.set_data().num_entries_occupied() as f64) < other_set_record.size {
            return cx.bool(false).into();
        }

        // Otherwise iterate through other set's keys and check if they are in this set
        let iterator = maybe!(get_iterator(
            cx,
            other_set_record.set_object.into(),
            IteratorHint::Sync,
            Some(other_set_record.keys_method)
        ));

        while let Some(iter_result) = maybe!(iterator_step(cx, &iterator)) {
            let item = maybe!(iterator_value(cx, iter_result));

            // Return as soon as we find an element of the other set that is not in this set
            if !this_set
                .set_data()
                .contains(&ValueCollectionKey::from(item))
            {
                return cx.bool(false).into();
            }
        }

        cx.bool(true).into()
    }

    // 24.2.3.14 get Set.prototype.size
    pub fn size(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "size accessor must be called on set");
        };

        Value::from(set.set_data().num_entries_occupied())
            .to_handle(cx)
            .into()
    }

    // 24.2.4.15 Set.prototype.symmetricDifference
    pub fn symmetric_difference(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "symmetricDifference method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = maybe!(get_set_record(cx, other));

        // Create a copy of this set
        let new_set_data = ValueSet::new_from_set(cx, this_set.set_data()).to_handle();
        let new_set = SetObject::new_from_set(cx, new_set_data).to_handle();

        // Iterate through keys of other set and add or remove them from the new set to ensure that
        // the new set contains only the keys that are in one set but not both.
        maybe!(iter_iterator_method_values(
            cx,
            other_set_record.set_object.into(),
            other_set_record.keys_method,
            &mut |cx, key| {
                let key = ValueCollectionKey::from(canonicalize_keyed_collection_key(cx, key));

                if this_set.set_data().contains(&key) {
                    // Both sets contain the key so remove it from the new set
                    new_set.set_data().remove(&key);
                } else {
                    // Key is in the other set but not in this set so add it to the new set
                    new_set.insert(cx, key);
                }

                None
            }
        ));

        new_set.into()
    }

    // 24.2.4.16 Set.prototype.union
    pub fn union(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "union method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = maybe!(get_set_record(cx, other));

        // Create a copy of this set
        let new_set_data = ValueSet::new_from_set(cx, this_set.set_data()).to_handle();
        let new_set = SetObject::new_from_set(cx, new_set_data).to_handle();

        // Iterate through keys of other set and add them to the new set
        maybe!(iter_iterator_method_values(
            cx,
            other_set_record.set_object.into(),
            other_set_record.keys_method,
            &mut |cx, key| {
                let key = canonicalize_keyed_collection_key(cx, key);
                new_set.insert(cx, ValueCollectionKey::from(key));

                None
            }
        ));

        new_set.into()
    }

    // 24.2.3.17 Set.prototype.values
    pub fn values(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "values method must be called on set");
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

struct SetRecord {
    set_object: Handle<ObjectValue>,
    size: f64,
    has_method: Handle<ObjectValue>,
    keys_method: Handle<ObjectValue>,
}

// 24.2.1.2 GetSetRecord
fn get_set_record(cx: Context, value: Handle<Value>) -> EvalResult<SetRecord> {
    if !value.is_object() {
        return type_error(cx, "value is not an object");
    }

    let object = value.as_object();

    let raw_size = maybe!(get(cx, object, cx.names.size()));
    let num_size = maybe!(to_number(cx, raw_size));
    if num_size.is_nan() {
        return type_error(cx, "size is not a number");
    }

    let int_size = must!(to_integer_or_infinity(cx, num_size));
    if int_size < 0.0 {
        return type_error(cx, "size is negative");
    }

    let has_method = maybe!(get(cx, object, cx.names.has()));
    if !is_callable(has_method) {
        return type_error(cx, "has method is not callable");
    }

    let keys_method = maybe!(get(cx, object, cx.names.keys()));
    if !is_callable(keys_method) {
        return type_error(cx, "keys method is not callable");
    }

    SetRecord {
        set_object: object,
        size: int_size,
        has_method: has_method.as_object(),
        keys_method: keys_method.as_object(),
    }
    .into()
}
