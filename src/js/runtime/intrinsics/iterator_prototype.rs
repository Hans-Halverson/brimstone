use crate::js::runtime::{
    abstract_operations::call_object,
    error::{type_error, type_error_value},
    function::get_argument,
    get,
    iterator::{iterator_close, iterator_step_value, Iterator},
    object_value::ObjectValue,
    realm::Realm,
    type_utilities::{is_callable, to_boolean},
    Context, EvalResult, Handle, Value,
};

use super::{intrinsics::Intrinsic, rust_runtime::return_this};

/// The %IteratorPrototype% Object (https://tc39.es/ecma262/#sec-%iteratorprototype%-object)
pub struct IteratorPrototype;

impl IteratorPrototype {
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        object.intrinsic_func(cx, cx.names.every(), Self::every, 1, realm);
        object.intrinsic_func(cx, cx.names.find(), Self::find, 1, realm);
        object.intrinsic_func(cx, cx.names.for_each(), Self::for_each, 1, realm);
        object.intrinsic_func(cx, cx.names.some(), Self::some, 1, realm);

        let iterator_key = cx.well_known_symbols.iterator();
        object.intrinsic_func(cx, iterator_key, return_this, 0, realm);

        object
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
}

/// GetIteratorDirect (https://tc39.es/ecma262/#sec-getiteratordirect)
fn get_iterator_direct(cx: Context, object: Handle<ObjectValue>) -> EvalResult<Iterator> {
    let next_method = get(cx, object, cx.names.next())?;
    Ok(Iterator { iterator: object, next_method, is_done: false })
}
