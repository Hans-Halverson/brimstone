use crate::{
    must,
    runtime::{
        abstract_operations::{call_object, canonicalize_keyed_collection_key},
        alloc_error::AllocResult,
        builtin_function::BuiltinFunction,
        collections::BsIndexSetField,
        error::type_error,
        eval_result::EvalResult,
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
};

use super::{
    intrinsics::Intrinsic,
    set_iterator::{SetIterator, SetIteratorKind},
    set_object::{SetObject, SetObjectSetField},
};

pub struct SetPrototype;

impl SetPrototype {
    /// Properties of the Set Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-set-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        // Create values function as it is referenced by multiple properties
        let values_function =
            BuiltinFunction::create(cx, Self::values, 0, cx.names.values(), realm, None)?.into();

        // Constructor property is added once SetConstructor has been created
        object.intrinsic_func(cx, cx.names.add(), Self::add, 1, realm)?;
        object.intrinsic_func(cx, cx.names.clear(), Self::clear, 0, realm)?;
        object.intrinsic_func(cx, cx.names.delete(), Self::delete, 1, realm)?;
        object.intrinsic_func(cx, cx.names.difference(), Self::difference, 1, realm)?;
        object.intrinsic_func(cx, cx.names.entries(), Self::entries, 0, realm)?;
        object.intrinsic_func(cx, cx.names.for_each(), Self::for_each, 1, realm)?;
        object.intrinsic_func(cx, cx.names.has(), Self::has, 1, realm)?;
        object.intrinsic_func(cx, cx.names.intersection(), Self::intersection, 1, realm)?;
        object.intrinsic_func(cx, cx.names.is_disjoint_from(), Self::is_disjoint_from, 1, realm)?;
        object.intrinsic_func(cx, cx.names.is_subset_of(), Self::is_subset_of, 1, realm)?;
        object.intrinsic_func(cx, cx.names.is_superset_of(), Self::is_superset_of, 1, realm)?;
        object.intrinsic_data_prop(cx, cx.names.keys(), values_function)?;
        object.intrinsic_getter(cx, cx.names.size(), Self::size, realm)?;
        object.intrinsic_func(
            cx,
            cx.names.symmetric_difference(),
            Self::symmetric_difference,
            1,
            realm,
        )?;
        object.intrinsic_func(cx, cx.names.union(), Self::union, 1, realm)?;
        object.intrinsic_data_prop(cx, cx.names.values(), values_function)?;

        // Set.prototype [ @@iterator ] (https://tc39.es/ecma262/#sec-set.prototype-%symbol.iterator%)
        let iterator_key = cx.well_known_symbols.iterator();
        object.set_property(
            cx,
            iterator_key,
            Property::data(values_function, true, false, true),
        )?;

        // Set.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-set.prototype-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.set().as_string().into(), false, false, true),
        )?;

        Ok(object)
    }

    /// Set.prototype.add (https://tc39.es/ecma262/#sec-set.prototype.add)
    pub fn add(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "add method must be called on set");
        };

        // Convert negative zero to positive zero in set
        let value = get_argument(cx, arguments, 0);
        let value = canonicalize_keyed_collection_key(cx, value);

        set.insert(cx, value)?;

        Ok(this_value)
    }

    /// Set.prototype.clear (https://tc39.es/ecma262/#sec-set.prototype.clear)
    pub fn clear(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "clear method must be called on set");
        };

        set.set_data_ptr().clear();

        Ok(cx.undefined())
    }

    /// Set.prototype.delete (https://tc39.es/ecma262/#sec-set.prototype.delete)
    pub fn delete(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "delete method must be called on set");
        };

        let key = get_argument(cx, arguments, 0);

        // May allocate
        let set_key = ValueCollectionKey::from(key)?;

        let existed = set.set_data_ptr().remove(&set_key);

        Ok(cx.bool(existed))
    }

    /// Set.prototype.difference (https://tc39.es/ecma262/#sec-set.prototype.difference)
    pub fn difference(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "difference method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = get_set_record(cx, other)?;

        // Create a copy of this set
        let new_set_data = ValueSet::new_from_set(cx, this_set.set_data())?.to_handle();
        let new_set = SetObject::new_from_set(cx, new_set_data)?;

        if this_set.set_data_ptr().num_entries_occupied() as f64 <= other_set_record.size {
            // If this set is smaller or equal to the other set, iterate through this set's keys and
            // determine if they are in the other set by calling the other set's `has` method. Then
            // remove the key from the new set if it is in the other set.

            // Handle is shared between iterations
            let mut item_handle = Handle::<Value>::empty(cx);

            for (item, _) in new_set.set_data().iter_gc_safe() {
                item_handle.replace(item.get());

                let in_other = call_object(
                    cx,
                    other_set_record.has_method,
                    other_set_record.set_object.into(),
                    &[item_handle],
                )?;

                if in_other.is_true() {
                    // May allocate
                    let set_key = ValueCollectionKey::from(item_handle)?;
                    new_set.set_data_ptr().remove(&set_key);
                }
            }
        } else {
            // Otherwise iterate through other set's keys and remove them from the new set
            iter_iterator_method_values(
                cx,
                other_set_record.set_object.into(),
                other_set_record.keys_method,
                &mut |cx, key| {
                    // May allocate
                    let key = match ValueCollectionKey::from(canonicalize_keyed_collection_key(
                        cx, key,
                    )) {
                        Ok(key) => key,
                        // Propagate allocation errors upwards
                        Err(err) => return Some(Err(err.into())),
                    };

                    new_set.set_data_ptr().remove(&key);

                    None
                },
            )?;
        }

        Ok(new_set.as_value())
    }

    /// Set.prototype.entries (https://tc39.es/ecma262/#sec-set.prototype.entries)
    pub fn entries(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "entries method must be called on set");
        };

        Ok(SetIterator::new(cx, set, SetIteratorKind::KeyAndValue)?.as_value())
    }

    /// Set.prototype.forEach (https://tc39.es/ecma262/#sec-set.prototype.foreach)
    pub fn for_each(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
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
        for (value, _) in set.set_data().iter_gc_safe() {
            value_handle.replace(value.into());

            let arguments = [value_handle, value_handle, this_value];
            call_object(cx, callback_function, this_arg, &arguments)?;
        }

        Ok(cx.undefined())
    }

    /// Set.prototype.has (https://tc39.es/ecma262/#sec-set.prototype.has)
    pub fn has(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "has method must be called on set");
        };

        let value = get_argument(cx, arguments, 0);

        // May allocate
        let set_value = ValueCollectionKey::from(value)?;

        Ok(cx.bool(set.set_data_ptr().contains(&set_value)))
    }

    /// Set.prototype.intersection (https://tc39.es/ecma262/#sec-set.prototype.intersection)
    pub fn intersection(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "intersection method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = get_set_record(cx, other)?;

        // Create an empty set
        let new_set_data = SetObjectSetField::new(cx, ValueSet::MIN_CAPACITY)?.to_handle();
        let new_set = SetObject::new_from_set(cx, new_set_data)?;

        if this_set.set_data_ptr().num_entries_occupied() as f64 <= other_set_record.size {
            // If this set is smaller or equal to the other set, iterate through this set's keys and
            // determine if they are in the other set by calling the other set's `has` method. Then
            // add the key to the new set if it is in the other set.

            // Handle is shared between iterations
            let mut item_handle = Handle::<Value>::empty(cx);

            for (item, _) in this_set.set_data().iter_gc_safe() {
                item_handle.replace(item.get());

                let in_other = call_object(
                    cx,
                    other_set_record.has_method,
                    other_set_record.set_object.into(),
                    &[item_handle],
                )?;

                if in_other.is_true() {
                    new_set.insert(cx, item_handle)?;
                }
            }
        } else {
            // Otherwise iterate through other set's keys and add them to the new set if they are
            // also in this set.
            iter_iterator_method_values(
                cx,
                other_set_record.set_object.into(),
                other_set_record.keys_method,
                &mut |cx, key| {
                    let key = canonicalize_keyed_collection_key(cx, key);

                    // May allocate
                    let set_key = match ValueCollectionKey::from(key) {
                        Ok(key) => key,
                        // Propagate allocation errors upwards
                        Err(err) => return Some(Err(err.into())),
                    };

                    if this_set.set_data_ptr().contains(&set_key) {
                        if let Err(err) = new_set.insert(cx, key) {
                            return Some(Err(err.into()));
                        }
                    }

                    None
                },
            )?;
        }

        Ok(new_set.as_value())
    }

    /// Set.prototype.isDisjointFrom (https://tc39.es/ecma262/#sec-set.prototype.isdisjointfrom)
    pub fn is_disjoint_from(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "isDisjointFrom method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = get_set_record(cx, other)?;

        if this_set.set_data_ptr().num_entries_occupied() as f64 <= other_set_record.size {
            // If this set is smaller or equal to the other set, iterate through this set's keys and
            // determine if they are in the other set by calling the other set's `has` method.

            // Handle is shared between iterations
            let mut item_handle = Handle::<Value>::empty(cx);

            for (item, _) in this_set.set_data().iter_gc_safe() {
                item_handle.replace(item.get());

                let in_other = call_object(
                    cx,
                    other_set_record.has_method,
                    other_set_record.set_object.into(),
                    &[item_handle],
                )?;

                // Return as soon as we find an element of this set that is in the other set
                if in_other.is_true() {
                    return Ok(cx.bool(false));
                }
            }
        } else {
            // Otherwise iterate through other set's keys and check if they are in this set
            let iterator = get_iterator(
                cx,
                other_set_record.set_object.into(),
                IteratorHint::Sync,
                Some(other_set_record.keys_method),
            )?;

            while let Some(iter_result) = iterator_step(cx, &iterator)? {
                let item = iterator_value(cx, iter_result)?;

                // May allocate
                let set_key = ValueCollectionKey::from(item)?;

                // Return as soon as we find an element of the other set that is in this set
                if this_set.set_data_ptr().contains(&set_key) {
                    return Ok(cx.bool(false));
                }
            }
        }

        Ok(cx.bool(true))
    }

    /// Set.prototype.isSubsetOf (https://tc39.es/ecma262/#sec-set.prototype.issubsetof)
    pub fn is_subset_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "isSubsetOf method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = get_set_record(cx, other)?;

        // We can return early if this set is larger than the other set
        if (this_set.set_data_ptr().num_entries_occupied() as f64) > other_set_record.size {
            return Ok(cx.bool(false));
        }

        // If this set is smaller or equal to the other set, iterate through this set's keys and
        // determine if they are in the other set by calling the other set's `has` method.

        // Handle is shared between iterations
        let mut item_handle = Handle::<Value>::empty(cx);

        for (item, _) in this_set.set_data().iter_gc_safe() {
            item_handle.replace(item.get());

            let in_other = call_object(
                cx,
                other_set_record.has_method,
                other_set_record.set_object.into(),
                &[item_handle],
            )?;

            if !in_other.is_true() {
                return Ok(cx.bool(false));
            }
        }

        Ok(cx.bool(true))
    }

    /// Set.prototype.isSupersetOf (https://tc39.es/ecma262/#sec-set.prototype.issupersetof)
    pub fn is_superset_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "isSupersetOf method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = get_set_record(cx, other)?;

        // We can return early if this set is smaller than the other set
        if (this_set.set_data_ptr().num_entries_occupied() as f64) < other_set_record.size {
            return Ok(cx.bool(false));
        }

        // Otherwise iterate through other set's keys and check if they are in this set
        let iterator = get_iterator(
            cx,
            other_set_record.set_object.into(),
            IteratorHint::Sync,
            Some(other_set_record.keys_method),
        )?;

        while let Some(iter_result) = iterator_step(cx, &iterator)? {
            let item = iterator_value(cx, iter_result)?;

            // May allocate
            let set_key = ValueCollectionKey::from(item)?;

            // Return as soon as we find an element of the other set that is not in this set
            if !this_set.set_data_ptr().contains(&set_key) {
                return Ok(cx.bool(false));
            }
        }

        Ok(cx.bool(true))
    }

    /// get Set.prototype.size (https://tc39.es/ecma262/#sec-get-set.prototype.size)
    pub fn size(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "size accessor must be called on set");
        };

        Ok(Value::from(set.set_data_ptr().num_entries_occupied()).to_handle(cx))
    }

    /// Set.prototype.symmetricDifference (https://tc39.es/ecma262/#sec-set.prototype.symmetricdifference)
    pub fn symmetric_difference(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "symmetricDifference method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = get_set_record(cx, other)?;

        // Create a copy of this set
        let new_set_data = ValueSet::new_from_set(cx, this_set.set_data())?.to_handle();
        let new_set = SetObject::new_from_set(cx, new_set_data)?;

        // Iterate through keys of other set and add or remove them from the new set to ensure that
        // the new set contains only the keys that are in one set but not both.
        iter_iterator_method_values(
            cx,
            other_set_record.set_object.into(),
            other_set_record.keys_method,
            &mut |cx, key| {
                let key = canonicalize_keyed_collection_key(cx, key);

                // May allocate
                let collection_key = match ValueCollectionKey::from(key) {
                    Ok(key) => key,
                    // Propagate allocation errors upwards
                    Err(err) => return Some(Err(err.into())),
                };

                if this_set.set_data_ptr().contains(&collection_key) {
                    // Both sets contain the key so remove it from the new set
                    new_set.set_data_ptr().remove(&collection_key);
                } else {
                    // Key is in the other set but not in this set so add it to the new set
                    if let Err(err) = new_set.insert(cx, key) {
                        return Some(Err(err.into()));
                    }
                }

                None
            },
        )?;

        Ok(new_set.as_value())
    }

    /// Set.prototype.union (https://tc39.es/ecma262/#sec-set.prototype.union)
    pub fn union(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let this_set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "union method must be called on set");
        };

        let other = get_argument(cx, arguments, 0);
        let other_set_record = get_set_record(cx, other)?;

        // Create a copy of this set
        let new_set_data = ValueSet::new_from_set(cx, this_set.set_data())?.to_handle();
        let new_set = SetObject::new_from_set(cx, new_set_data)?;

        // Iterate through keys of other set and add them to the new set
        iter_iterator_method_values(
            cx,
            other_set_record.set_object.into(),
            other_set_record.keys_method,
            &mut |cx, key| {
                let key = canonicalize_keyed_collection_key(cx, key);

                if let Err(err) = new_set.insert(cx, key) {
                    return Some(Err(err.into()));
                }

                None
            },
        )?;

        Ok(new_set.as_value())
    }

    /// Set.prototype.values (https://tc39.es/ecma262/#sec-set.prototype.values)
    pub fn values(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let set = if let Some(set) = this_set_value(this_value) {
            set
        } else {
            return type_error(cx, "values method must be called on set");
        };

        Ok(SetIterator::new(cx, set, SetIteratorKind::Value)?.as_value())
    }
}

fn this_set_value(value: Handle<Value>) -> Option<Handle<SetObject>> {
    if !value.is_object() {
        return None;
    }

    value.as_object().as_set_object()
}

struct SetRecord {
    set_object: Handle<ObjectValue>,
    size: f64,
    has_method: Handle<ObjectValue>,
    keys_method: Handle<ObjectValue>,
}

/// GetSetRecord (https://tc39.es/ecma262/#sec-getsetrecord)
fn get_set_record(cx: Context, value: Handle<Value>) -> EvalResult<SetRecord> {
    if !value.is_object() {
        return type_error(cx, "value is not an object");
    }

    let object = value.as_object();

    let raw_size = get(cx, object, cx.names.size())?;
    let num_size = to_number(cx, raw_size)?;
    if num_size.is_nan() {
        return type_error(cx, "size is not a number");
    }

    let int_size = must!(to_integer_or_infinity(cx, num_size));
    if int_size < 0.0 {
        return type_error(cx, "size is negative");
    }

    let has_method = get(cx, object, cx.names.has())?;
    if !is_callable(has_method) {
        return type_error(cx, "has method is not callable");
    }

    let keys_method = get(cx, object, cx.names.keys())?;
    if !is_callable(keys_method) {
        return type_error(cx, "keys method is not callable");
    }

    Ok(SetRecord {
        set_object: object,
        size: int_size,
        has_method: has_method.as_object(),
        keys_method: keys_method.as_object(),
    })
}
