use crate::{
    js::runtime::{
        abstract_operations::{call_object, get_method},
        error::type_error,
        get,
        intrinsics::async_from_sync_iterator_prototype::AsyncFromSyncIterator,
    },
    must,
};

use super::{
    abstract_operations::{call, create_data_property_or_throw},
    object_value::ObjectValue,
    ordinary_object::ordinary_object_create,
    type_utilities::to_boolean,
    Context, EvalResult, Handle, Value,
};

/// Iterator Records (https://tc39.es/ecma262/#sec-iterator-records)
pub struct Iterator {
    pub iterator: Handle<ObjectValue>,
    pub next_method: Handle<Value>,
    pub is_done: bool,
}

#[derive(PartialEq)]
pub enum IteratorHint {
    Sync,
    Async,
}

/// GetIterator (https://tc39.es/ecma262/#sec-getiterator)
pub fn get_iterator(
    cx: Context,
    object: Handle<Value>,
    hint: IteratorHint,
    method: Option<Handle<ObjectValue>>,
) -> EvalResult<Iterator> {
    let method = if let Some(method) = method {
        method
    } else {
        if hint == IteratorHint::Async {
            let async_iterator_key = cx.well_known_symbols.async_iterator();
            let method = get_method(cx, object, async_iterator_key)?;

            if let Some(method) = method {
                method
            } else {
                let sync_iterator_key = cx.well_known_symbols.iterator();
                let sync_method = get_method(cx, object, sync_iterator_key)?;
                let sync_iterator_record =
                    get_iterator(cx, object, IteratorHint::Sync, sync_method)?;

                return Ok(create_async_from_sync_iterator(cx, sync_iterator_record));
            }
        } else {
            let iterator_key = cx.well_known_symbols.iterator();
            let method = get_method(cx, object, iterator_key)?;

            if let Some(method) = method {
                method
            } else {
                // Inlined from eventual call to IsCallable(undefined) which would fail
                return type_error(cx, "value is not a function");
            }
        }
    };

    let iterator = call_object(cx, method, object, &[])?;

    if !iterator.is_object() {
        return type_error(cx, "iterator must be an object");
    }
    let iterator = iterator.as_object();

    let next_method = get(cx, iterator, cx.names.next())?;

    let iterator_record = Iterator { iterator, next_method, is_done: false };

    Ok(iterator_record)
}

/// GetIteratorDirect (https://tc39.es/ecma262/#sec-getiteratordirect)
pub fn get_iterator_direct(cx: Context, object: Handle<ObjectValue>) -> EvalResult<Iterator> {
    let next_method = get(cx, object, cx.names.next())?;
    Ok(Iterator { iterator: object, next_method, is_done: false })
}

/// GetIteratorFlattenable (https://tc39.es/ecma262/#sec-getiteratorflattenable)
pub fn get_iterator_flattenable(
    cx: Context,
    value: Handle<Value>,
    reject_primitives: bool,
) -> EvalResult<Iterator> {
    if !value.is_object() {
        if reject_primitives || !value.is_string() {
            return type_error(cx, "value is not iterable");
        }
    }

    let iterator = match get_method(cx, value, cx.well_known_symbols.iterator())? {
        None => value,
        Some(method) => call_object(cx, method, value, &[])?,
    };

    if !iterator.is_object() {
        return type_error(cx, "iterator must be an object");
    }

    get_iterator_direct(cx, iterator.as_object())
}

/// IteratorNext (https://tc39.es/ecma262/#sec-iteratornext)
pub fn iterator_next(
    cx: Context,
    iterator: Handle<ObjectValue>,
    next_method: Handle<Value>,
    value: Option<Handle<Value>>,
) -> EvalResult<Handle<ObjectValue>> {
    let result = if let Some(value) = value {
        call(cx, next_method, iterator.as_value(), &[value])?
    } else {
        call(cx, next_method, iterator.as_value(), &[])?
    };

    if !result.is_object() {
        return type_error(cx, "iterator's next method must return an object");
    }

    Ok(result.as_object())
}

/// IteratorComplete (https://tc39.es/ecma262/#sec-iteratorcomplete)
pub fn iterator_complete(cx: Context, iter_result: Handle<ObjectValue>) -> EvalResult<bool> {
    let is_done = get(cx, iter_result, cx.names.done())?;
    Ok(to_boolean(*is_done))
}

/// IteratorValue (https://tc39.es/ecma262/#sec-iteratorvalue)
pub fn iterator_value(cx: Context, iter_result: Handle<ObjectValue>) -> EvalResult<Handle<Value>> {
    get(cx, iter_result, cx.names.value())
}

/// IteratorStep (https://tc39.es/ecma262/#sec-iteratorstep)
pub fn iterator_step(cx: Context, iterator: &Iterator) -> EvalResult<Option<Handle<ObjectValue>>> {
    let iter_result = iterator_next(cx, iterator.iterator, iterator.next_method, None)?;
    let is_done = iterator_complete(cx, iter_result)?;

    if is_done {
        Ok(None)
    } else {
        Ok(Some(iter_result))
    }
}

/// IteratorClose (https://tc39.es/ecma262/#sec-iteratorclose)
pub fn iterator_close(
    cx: Context,
    iterator: Handle<ObjectValue>,
    completion: EvalResult<Handle<Value>>,
) -> EvalResult<Handle<Value>> {
    let inner_result = get_method(cx, iterator.into(), cx.names.return_());
    let inner_result = match inner_result {
        Ok(None) => return completion,
        Ok(Some(return_)) => call_object(cx, return_, iterator.into(), &[]),
        Err(thrown_value) => Err(thrown_value),
    };

    // Return completion if it is an error
    completion?;

    let inner_value = inner_result?;
    if !inner_value.is_object() {
        return type_error(cx, "iterator's return method must return an object");
    }

    completion
}

/// IteratorStepValue (https://tc39.es/ecma262/#sec-iteratorstepvalue)
///
/// Returns the value from the next iteration of the iterator, unless the iterator is done, in which
/// case it returns None.
///
/// Also mutates the iterator, marking done if the iterator is done or an abrupt completion occurs.
pub fn iterator_step_value(
    cx: Context,
    iterator: &mut Iterator,
) -> EvalResult<Option<Handle<Value>>> {
    let iter_result_completion = iterator_next(cx, iterator.iterator, iterator.next_method, None);
    let iter_result = match iter_result_completion {
        Ok(iter_result) => iter_result,
        Err(error) => {
            iterator.is_done = true;
            return Err(error);
        }
    };

    let done_completion = iterator_complete(cx, iter_result);
    let done = match done_completion {
        Ok(done) => done,
        Err(error) => {
            iterator.is_done = true;
            return Err(error);
        }
    };

    if done {
        iterator.is_done = true;
        return Ok(None);
    }

    let value_completion = get(cx, iter_result, cx.names.value());
    match value_completion {
        Ok(value) => Ok(Some(value)),
        Err(error) => {
            iterator.is_done = true;
            Err(error)
        }
    }
}

/// CreateIteratorResultObject (https://tc39.es/ecma262/#sec-createiterresultobject)
pub fn create_iter_result_object(
    cx: Context,
    value: Handle<Value>,
    is_done: bool,
) -> Handle<Value> {
    let object = ordinary_object_create(cx);

    must!(create_data_property_or_throw(cx, object, cx.names.value(), value));

    let is_done_value = cx.bool(is_done);
    must!(create_data_property_or_throw(cx, object, cx.names.done(), is_done_value));

    object.as_value()
}

// Iterate over an object, executing a callback function against every value returned by the
// iterator. Return a completion from the callback function to stop and close the iterator.
pub fn iter_iterator_values<
    F: FnMut(Context, Handle<Value>) -> Option<EvalResult<Handle<Value>>>,
>(
    cx: Context,
    object: Handle<Value>,
    f: &mut F,
) -> EvalResult<Handle<Value>> {
    let iterator = get_iterator(cx, object, IteratorHint::Sync, None)?;

    loop {
        let iter_result = iterator_step(cx, &iterator)?;
        match iter_result {
            None => return Ok(cx.empty()),
            Some(iter_result) => {
                let value = iterator_value(cx, iter_result)?;

                let completion = f(cx, value);

                if let Some(completion) = completion {
                    return iterator_close(cx, iterator.iterator, completion);
                }
            }
        }
    }
}

pub fn iter_iterator_method_values<
    F: FnMut(Context, Handle<Value>) -> Option<EvalResult<Handle<Value>>>,
>(
    cx: Context,
    object: Handle<Value>,
    method: Handle<ObjectValue>,
    f: &mut F,
) -> EvalResult<Handle<Value>> {
    let iterator = get_iterator(cx, object, IteratorHint::Sync, Some(method))?;

    loop {
        let iter_result = iterator_step(cx, &iterator)?;
        match iter_result {
            None => return Ok(cx.empty()),
            Some(iter_result) => {
                let value = iterator_value(cx, iter_result)?;

                let completion = f(cx, value);

                if let Some(completion) = completion {
                    return iterator_close(cx, iterator.iterator, completion);
                }
            }
        }
    }
}

/// CreateAsyncFromSyncIterator (https://tc39.es/ecma262/#sec-createasyncfromsynciterator)
fn create_async_from_sync_iterator(cx: Context, sync_iterator: Iterator) -> Iterator {
    let async_iterator = AsyncFromSyncIterator::new(cx, sync_iterator).into();
    let next_method = must!(get(cx, async_iterator, cx.names.next()));

    Iterator { iterator: async_iterator, next_method, is_done: false }
}
