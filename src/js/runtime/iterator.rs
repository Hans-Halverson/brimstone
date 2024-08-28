use crate::{
    js::runtime::{
        abstract_operations::{call_object, get_method},
        error::type_error,
        get,
        intrinsics::async_from_sync_iterator_prototype::AsyncFromSyncIterator,
    },
    maybe, must,
};

use super::{
    abstract_operations::{call, create_data_property_or_throw},
    object_value::ObjectValue,
    ordinary_object::ordinary_object_create,
    type_utilities::to_boolean,
    Context, EvalResult, Handle, Value,
};

// 7.4.1 Iterator Records
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

// 7.4.2 GetIterator
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
            let method = maybe!(get_method(cx, object, async_iterator_key));

            if let Some(method) = method {
                method
            } else {
                let sync_iterator_key = cx.well_known_symbols.iterator();
                let sync_method = maybe!(get_method(cx, object, sync_iterator_key));
                let sync_iterator_record =
                    maybe!(get_iterator(cx, object, IteratorHint::Sync, sync_method));

                return create_async_from_sync_iterator(cx, sync_iterator_record).into();
            }
        } else {
            let iterator_key = cx.well_known_symbols.iterator();
            let method = maybe!(get_method(cx, object, iterator_key));

            if let Some(method) = method {
                method
            } else {
                // Inlined from eventual call to IsCallable(undefined) which would fail
                return type_error(cx, "value is not a function");
            }
        }
    };

    let iterator = maybe!(call_object(cx, method, object, &[]));

    if !iterator.is_object() {
        return type_error(cx, "iterator must be an object");
    }
    let iterator = iterator.as_object();

    let next_method = maybe!(get(cx, iterator, cx.names.next()));

    let iterator_record = Iterator { iterator, next_method, is_done: false };

    iterator_record.into()
}

// 7.4.3 IteratorNext
pub fn iterator_next(
    cx: Context,
    iterator: Handle<ObjectValue>,
    next_method: Handle<Value>,
    value: Option<Handle<Value>>,
) -> EvalResult<Handle<ObjectValue>> {
    let result = if let Some(value) = value {
        maybe!(call(cx, next_method, iterator.into(), &[value]))
    } else {
        maybe!(call(cx, next_method, iterator.into(), &[]))
    };

    if !result.is_object() {
        return type_error(cx, "iterator's next method must return an object");
    }

    result.as_object().into()
}

// 7.4.4 IteratorComplete
pub fn iterator_complete(cx: Context, iter_result: Handle<ObjectValue>) -> EvalResult<bool> {
    let is_done = maybe!(get(cx, iter_result, cx.names.done()));
    to_boolean(is_done.get()).into()
}

// 7.4.5 IteratorValue
pub fn iterator_value(cx: Context, iter_result: Handle<ObjectValue>) -> EvalResult<Handle<Value>> {
    get(cx, iter_result, cx.names.value())
}

// 7.4.6 IteratorStep
pub fn iterator_step(cx: Context, iterator: &Iterator) -> EvalResult<Option<Handle<ObjectValue>>> {
    let iter_result = maybe!(iterator_next(cx, iterator.iterator, iterator.next_method, None));
    let is_done = maybe!(iterator_complete(cx, iter_result));

    if is_done {
        None.into()
    } else {
        Some(iter_result).into()
    }
}

// 7.4.7 IteratorClose
pub fn iterator_close(
    cx: Context,
    iterator: &Iterator,
    completion: EvalResult<Handle<Value>>,
) -> EvalResult<Handle<Value>> {
    let inner_result = get_method(cx, iterator.iterator.into(), cx.names.return_());
    let inner_result = match inner_result {
        EvalResult::Ok(None) => return completion,
        EvalResult::Ok(Some(return_)) => call_object(cx, return_, iterator.iterator.into(), &[]),
        EvalResult::Throw(thrown_value) => EvalResult::Throw(thrown_value),
    };

    if let EvalResult::Throw(_) = completion {
        return completion;
    }

    let inner_value = maybe!(inner_result);
    if !inner_value.is_object() {
        return type_error(cx, "iterator's return method must return an object");
    }

    completion
}

/// 7.4.8 IteratorStepValue
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
        EvalResult::Ok(iter_result) => iter_result,
        EvalResult::Throw(error) => {
            iterator.is_done = true;
            return EvalResult::Throw(error);
        }
    };

    let done_completion = iterator_complete(cx, iter_result);
    let done = match done_completion {
        EvalResult::Ok(done) => done,
        EvalResult::Throw(error) => {
            iterator.is_done = true;
            return EvalResult::Throw(error);
        }
    };

    if done {
        iterator.is_done = true;
        return None.into();
    }

    let value_completion = get(cx, iter_result, cx.names.value());
    match value_completion {
        EvalResult::Ok(value) => Some(value).into(),
        EvalResult::Throw(error) => {
            iterator.is_done = true;
            EvalResult::Throw(error)
        }
    }
}

// 7.4.10 CreateIterResultObject
pub fn create_iter_result_object(
    cx: Context,
    value: Handle<Value>,
    is_done: bool,
) -> Handle<ObjectValue> {
    let object = ordinary_object_create(cx);

    must!(create_data_property_or_throw(cx, object, cx.names.value(), value));

    let is_done_value = cx.bool(is_done);
    must!(create_data_property_or_throw(cx, object, cx.names.done(), is_done_value));

    object
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
    let iterator = maybe!(get_iterator(cx, object, IteratorHint::Sync, None));

    loop {
        let iter_result = maybe!(iterator_step(cx, &iterator));
        match iter_result {
            None => return cx.empty().into(),
            Some(iter_result) => {
                let value = maybe!(iterator_value(cx, iter_result));

                let completion = f(cx, value);

                if let Some(completion) = completion {
                    return maybe!(iterator_close(cx, &iterator, completion)).into();
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
    let iterator = maybe!(get_iterator(cx, object, IteratorHint::Sync, Some(method)));

    loop {
        let iter_result = maybe!(iterator_step(cx, &iterator));
        match iter_result {
            None => return cx.empty().into(),
            Some(iter_result) => {
                let value = maybe!(iterator_value(cx, iter_result));

                let completion = f(cx, value);

                if let Some(completion) = completion {
                    return maybe!(iterator_close(cx, &iterator, completion)).into();
                }
            }
        }
    }
}

// 27.1.4.1 CreateAsyncFromSyncIterator
fn create_async_from_sync_iterator(cx: Context, sync_iterator: Iterator) -> Iterator {
    let async_iterator = AsyncFromSyncIterator::new(cx, sync_iterator).into();
    let next_method = must!(get(cx, async_iterator, cx.names.next()));

    Iterator { iterator: async_iterator, next_method, is_done: false }
}
