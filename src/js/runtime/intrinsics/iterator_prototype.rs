use crate::{
    js::runtime::{
        abstract_operations::{call_object, setter_that_ignores_prototype_properties},
        array_object::create_array_from_list,
        error::{range_error_value, type_error, type_error_value},
        function::get_argument,
        iterator::{get_iterator_direct, iterator_close, iterator_step_value},
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::{is_callable, to_boolean, to_integer_or_infinity, to_number},
        Context, EvalResult, Handle, Value,
    },
    must,
};

use super::{
    intrinsics::Intrinsic, iterator_helper_object::IteratorHelperObject, rust_runtime::return_this,
};

/// The %IteratorPrototype% Object (https://tc39.es/ecma262/#sec-%iteratorprototype%-object)
pub struct IteratorPrototype;

impl IteratorPrototype {
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        // Iterator.prototype.constructor (https://tc39.es/ecma262/#sec-iterator.prototype.constructor)
        object.intrinsic_getter_and_setter(
            cx,
            cx.names.constructor(),
            Self::get_constructor,
            Self::set_constructor,
            realm,
        );

        object.intrinsic_func(cx, cx.names.drop(), Self::drop, 1, realm);
        object.intrinsic_func(cx, cx.names.every(), Self::every, 1, realm);
        object.intrinsic_func(cx, cx.names.find(), Self::find, 1, realm);
        object.intrinsic_func(cx, cx.names.for_each(), Self::for_each, 1, realm);
        object.intrinsic_func(cx, cx.names.reduce(), Self::reduce, 1, realm);
        object.intrinsic_func(cx, cx.names.some(), Self::some, 1, realm);
        object.intrinsic_func(cx, cx.names.take(), Self::take, 1, realm);
        object.intrinsic_func(cx, cx.names.to_array(), Self::to_array, 0, realm);

        // Iterator.prototype [ @@iterator ] (https://tc39.es/ecma262/#sec-iterator.prototype-%symbol.iterator%)
        let iterator_key = cx.well_known_symbols.iterator();
        object.intrinsic_func(cx, iterator_key, return_this, 0, realm);

        // Iterator.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-iterator.prototype-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.intrinsic_getter_and_setter(
            cx,
            to_string_tag_key,
            Self::iterator_prototype_get_to_string_tag,
            Self::set_to_string_tag,
            realm,
        );

        object
    }

    /// get Iterator.prototype.constructor (https://tc39.es/ecma262/#sec-get-iterator.prototype.constructor)
    pub fn get_constructor(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        Ok(cx.get_intrinsic(Intrinsic::IteratorConstructor).as_value())
    }

    /// set Iterator.prototype.constructor (https://tc39.es/ecma262/#sec-set-iterator.prototype.constructor)
    pub fn set_constructor(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);

        setter_that_ignores_prototype_properties(
            cx,
            this_value,
            cx.get_intrinsic(Intrinsic::IteratorPrototype),
            cx.names.constructor(),
            value,
        )?;

        Ok(cx.undefined())
    }

    /// Iterator.prototype.drop (https://tc39.es/ecma262/#sec-iterator.prototype.drop)
    pub fn drop(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Iterator.prototype.drop called on non-object");
        }

        let iterator_object = this_value.as_object();

        // Verify the limit is a number, closing the underlying iterator if not
        let num_limit_arg = get_argument(cx, arguments, 0);
        let num_limit = match to_number(cx, num_limit_arg) {
            Err(error) => {
                return iterator_close(cx, iterator_object, Err(error));
            }
            Ok(num_limit) => num_limit,
        };

        // Verify that the limit is not NaN or negative, closing the underlying iterator if it is
        if num_limit.is_nan() {
            let error = range_error_value(cx, "Iterator.prototype.drop limit is NaN");
            return iterator_close(cx, iterator_object, Err(error));
        }

        let integer_limit = must!(to_integer_or_infinity(cx, num_limit));
        if integer_limit < 0.0 {
            let error = range_error_value(cx, "Iterator.prototype.drop limit is negative");
            return iterator_close(cx, iterator_object, Err(error));
        }

        // Get the underlying iterator and create a new iterator helper drop object
        let iterated = get_iterator_direct(cx, iterator_object)?;
        Ok(IteratorHelperObject::new_drop(cx, &iterated, integer_limit).as_value())
    }

    /// Iterator.prototype.every (https://tc39.es/ecma262/#sec-iterator.prototype.every)
    pub fn every(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Iterator.prototype.every called on non-object");
        }

        // Verify the predicate argument is a function, closing the underlying iterator if not
        let predicate_arg = get_argument(cx, arguments, 0);
        if !is_callable(predicate_arg) {
            let error =
                type_error_value(cx, "Iterator.prototype.every predicate is not a function");
            return iterator_close(cx, this_value.as_object(), Err(error));
        }
        let predicate = predicate_arg.as_object();

        let mut iterated = get_iterator_direct(cx, this_value.as_object())?;
        let mut counter: u64 = 0;
        let mut counter_handle: Handle<Value> = Handle::empty(cx);

        loop {
            // Get the next value from the iterator, returning true if done
            let value = match iterator_step_value(cx, &mut iterated)? {
                None => return Ok(cx.bool(true)),
                Some(value) => value,
            };

            // Pass value and counter to the predicate function
            counter_handle.replace(Value::from(counter));
            let result = call_object(cx, predicate, cx.undefined(), &[value, counter_handle]);

            // Finish iterating if predicate threw or returned false
            match result {
                Err(_) => return iterator_close(cx, iterated.iterator, result),
                Ok(result) if !to_boolean(*result) => {
                    return iterator_close(cx, iterated.iterator, Ok(cx.bool(false)))
                }
                Ok(_) => counter += 1,
            }
        }
    }

    /// Iterator.prototype.find (https://tc39.es/ecma262/#sec-iterator.prototype.find)
    pub fn find(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Iterator.prototype.find called on non-object");
        }

        // Verify the predicate argument is a function, closing the underlying iterator if not
        let predicate_arg = get_argument(cx, arguments, 0);
        if !is_callable(predicate_arg) {
            let error = type_error_value(cx, "Iterator.prototype.find predicate is not a function");
            return iterator_close(cx, this_value.as_object(), Err(error));
        }
        let predicate = predicate_arg.as_object();

        let mut iterated = get_iterator_direct(cx, this_value.as_object())?;
        let mut counter: u64 = 0;
        let mut counter_handle: Handle<Value> = Handle::empty(cx);

        loop {
            // Get the next value from the iterator, returning undefined if done
            let value = match iterator_step_value(cx, &mut iterated)? {
                None => return Ok(cx.undefined()),
                Some(value) => value,
            };

            // Pass value and counter to the predicate function
            counter_handle.replace(Value::from(counter));
            let result = call_object(cx, predicate, cx.undefined(), &[value, counter_handle]);

            // Finish iterating if predicate threw or returned true
            match result {
                Err(_) => return iterator_close(cx, iterated.iterator, result),
                Ok(result) if to_boolean(*result) => {
                    return iterator_close(cx, iterated.iterator, Ok(value))
                }
                Ok(_) => counter += 1,
            }
        }
    }

    /// Iterator.prototype.forEach (https://tc39.es/ecma262/#sec-iterator.prototype.foreach)
    pub fn for_each(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Iterator.prototype.forEach called on non-object");
        }

        // Verify the callback argument is a function, closing the underlying iterator if not
        let callback_arg = get_argument(cx, arguments, 0);
        if !is_callable(callback_arg) {
            let error =
                type_error_value(cx, "Iterator.prototype.forEach callback is not a function");
            return iterator_close(cx, this_value.as_object(), Err(error));
        }
        let callback = callback_arg.as_object();

        let mut iterated = get_iterator_direct(cx, this_value.as_object())?;
        let mut counter: u64 = 0;
        let mut counter_handle: Handle<Value> = Handle::empty(cx);

        loop {
            // Get the next value from the iterator, returning true if done
            let value = match iterator_step_value(cx, &mut iterated)? {
                None => return Ok(cx.undefined()),
                Some(value) => value,
            };

            // Pass value and counter to the callback function
            counter_handle.replace(Value::from(counter));
            let result = call_object(cx, callback, cx.undefined(), &[value, counter_handle]);

            // Finish iterating if callback threw
            match result {
                Err(_) => return iterator_close(cx, iterated.iterator, result),
                Ok(_) => counter += 1,
            }
        }
    }

    /// Iterator.prototype.reduce (https://tc39.es/ecma262/#sec-iterator.prototype.reduce)
    pub fn reduce(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Iterator.prototype.reduce called on non-object");
        }

        // Verify the callback argument is a function, closing the underlying iterator if not
        let callback_arg = get_argument(cx, arguments, 0);
        if !is_callable(callback_arg) {
            let error =
                type_error_value(cx, "Iterator.prototype.reduce callback is not a function");
            return iterator_close(cx, this_value.as_object(), Err(error));
        }
        let callback = callback_arg.as_object();

        let mut iterated = get_iterator_direct(cx, this_value.as_object())?;

        let mut accumulator;
        let mut counter: u64;

        if arguments.len() < 2 {
            match iterator_step_value(cx, &mut iterated)? {
                None => {
                    return type_error(cx, "Iterator.prototype.reduce called on empty iterator")
                }
                Some(value) => {
                    accumulator = value;
                    counter = 1;
                }
            }
        } else {
            accumulator = get_argument(cx, arguments, 1);
            counter = 0;
        };

        let mut counter_handle: Handle<Value> = Handle::empty(cx);

        loop {
            // Get the next value from the iterator, returning the accumulator if done
            let value = match iterator_step_value(cx, &mut iterated)? {
                None => return Ok(accumulator),
                Some(value) => value,
            };

            // Pass accumulator, value, and counter to the predicate function
            counter_handle.replace(Value::from(counter));
            let result =
                call_object(cx, callback, cx.undefined(), &[accumulator, value, counter_handle]);

            // Finish iterating if callback threw, otherwise update the accumulator
            match result {
                Err(_) => return iterator_close(cx, iterated.iterator, result),
                Ok(value) => {
                    accumulator = value;
                    counter += 1;
                }
            }
        }
    }

    /// Iterator.prototype.some (https://tc39.es/ecma262/#sec-iterator.prototype.some)
    pub fn some(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Iterator.prototype.some called on non-object");
        }

        // Verify the predicate argument is a function, closing the underlying iterator if not
        let predicate_arg = get_argument(cx, arguments, 0);
        if !is_callable(predicate_arg) {
            let error = type_error_value(cx, "Iterator.prototype.some predicate is not a function");
            return iterator_close(cx, this_value.as_object(), Err(error));
        }
        let predicate = predicate_arg.as_object();

        let mut iterated = get_iterator_direct(cx, this_value.as_object())?;
        let mut counter: u64 = 0;
        let mut counter_handle: Handle<Value> = Handle::empty(cx);

        loop {
            // Get the next value from the iterator, returning false if done
            let value = match iterator_step_value(cx, &mut iterated)? {
                None => return Ok(cx.bool(false)),
                Some(value) => value,
            };

            // Pass value and counter to the predicate function
            counter_handle.replace(Value::from(counter));
            let result = call_object(cx, predicate, cx.undefined(), &[value, counter_handle]);

            // Finish iterating if predicate threw or returned true
            match result {
                Err(_) => return iterator_close(cx, iterated.iterator, result),
                Ok(result) if to_boolean(*result) => {
                    return iterator_close(cx, iterated.iterator, Ok(cx.bool(true)))
                }
                Ok(_) => counter += 1,
            }
        }
    }

    /// Iterator.prototype.take (https://tc39.es/ecma262/#sec-iterator.prototype.take)
    pub fn take(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Iterator.prototype.take called on non-object");
        }

        let iterator_object = this_value.as_object();

        // Verify the limit is a number, closing the underlying iterator if not
        let num_limit_arg = get_argument(cx, arguments, 0);
        let num_limit = match to_number(cx, num_limit_arg) {
            Err(error) => {
                return iterator_close(cx, iterator_object, Err(error));
            }
            Ok(num_limit) => num_limit,
        };

        // Verify that the limit is not NaN or negative, closing the underlying iterator if it is
        if num_limit.is_nan() {
            let error = range_error_value(cx, "Iterator.prototype.take limit is NaN");
            return iterator_close(cx, iterator_object, Err(error));
        }

        let integer_limit = must!(to_integer_or_infinity(cx, num_limit));
        if integer_limit < 0.0 {
            let error = range_error_value(cx, "Iterator.prototype.take limit is negative");
            return iterator_close(cx, iterator_object, Err(error));
        }

        // Get the underlying iterator and create a new iterator helper take object
        let iterated = get_iterator_direct(cx, iterator_object)?;
        Ok(IteratorHelperObject::new_take(cx, &iterated, integer_limit).as_value())
    }

    /// Iterator.prototype.toArray (https://tc39.es/ecma262/#sec-iterator.prototype.toarray)
    pub fn to_array(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Iterator.prototype.toArray called on non-object");
        }

        let mut iterated = get_iterator_direct(cx, this_value.as_object())?;
        let mut items = vec![];

        // Collect all all items from the iterator until the iterator is done
        loop {
            match iterator_step_value(cx, &mut iterated)? {
                None => return Ok(create_array_from_list(cx, &items).as_value()),
                Some(value) => items.push(value),
            }
        }
    }

    /// get Iterator.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-get-iterator.prototype-%symbol.tostringtag%)
    #[no_mangle]
    pub fn iterator_prototype_get_to_string_tag(
        cx: Context,
        _: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        Ok(cx.names.iterator().as_string().as_value())
    }

    /// set Iterator.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-set-iterator.prototype-%symbol.tostringtag%)
    pub fn set_to_string_tag(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);

        setter_that_ignores_prototype_properties(
            cx,
            this_value,
            cx.get_intrinsic(Intrinsic::IteratorPrototype),
            cx.well_known_symbols.to_string_tag(),
            value,
        )?;

        Ok(cx.undefined())
    }
}
