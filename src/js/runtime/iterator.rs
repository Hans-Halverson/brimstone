use crate::{
    js::runtime::{
        abstract_operations::{call_object, get_method},
        error::type_error_,
        get,
    },
    maybe, maybe_, maybe__, must,
};

use super::{
    abstract_operations::{call, create_data_property_or_throw},
    error::type_error,
    object_value::ObjectValue,
    ordinary_object::ordinary_object_create,
    type_utilities::to_boolean,
    Completion, CompletionKind, Context, EvalResult, Handle, Value,
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
    cx: &mut Context,
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

                return create_async_from_sync_iterator(sync_iterator_record).into();
            }
        } else {
            let iterator_key = cx.well_known_symbols.iterator();
            let method = maybe!(get_method(cx, object, iterator_key));

            if let Some(method) = method {
                method
            } else {
                // Inlined from eventual call to IsCallable(undefined) which would fail
                return type_error_(cx, "value is not a function");
            }
        }
    };

    let iterator = maybe!(call_object(cx, method, object, &[]));

    if !iterator.is_object() {
        return type_error_(cx, "iterator must be an object");
    }
    let iterator = iterator.as_object();

    let next_method = maybe!(get(cx, iterator, cx.names.next()));

    let iterator_record = Iterator { iterator, next_method, is_done: false };

    iterator_record.into()
}

// 7.4.3 IteratorNext
pub fn iterator_next(
    cx: &mut Context,
    iterator: &Iterator,
    value: Option<Handle<Value>>,
) -> EvalResult<Handle<ObjectValue>> {
    let result = if let Some(value) = value {
        maybe!(call(cx, iterator.next_method, iterator.iterator.into(), &[value]))
    } else {
        maybe!(call(cx, iterator.next_method, iterator.iterator.into(), &[]))
    };

    if !result.is_object() {
        return type_error_(cx, "iterator's next method must return an object");
    }

    return result.as_object().into();
}

// 7.4.4 IteratorComplete
pub fn iterator_complete(cx: &mut Context, iter_result: Handle<ObjectValue>) -> EvalResult<bool> {
    let is_done = maybe!(get(cx, iter_result, cx.names.done()));
    to_boolean(is_done.get()).into()
}

// 7.4.5 IteratorValue
pub fn iterator_value(
    cx: &mut Context,
    iter_result: Handle<ObjectValue>,
) -> EvalResult<Handle<Value>> {
    get(cx, iter_result, cx.names.value())
}

// 7.4.6 IteratorStep
pub fn iterator_step(
    cx: &mut Context,
    iterator: &Iterator,
) -> EvalResult<Option<Handle<ObjectValue>>> {
    let iter_result = maybe!(iterator_next(cx, iterator, None));
    let is_done = maybe!(iterator_complete(cx, iter_result));

    if is_done {
        None.into()
    } else {
        Some(iter_result).into()
    }
}

// 7.4.7 IteratorClose
pub fn iterator_close(cx: &mut Context, iterator: &Iterator, completion: Completion) -> Completion {
    let inner_result = get_method(cx, iterator.iterator.into(), cx.names.return_());
    let inner_result = match inner_result {
        EvalResult::Ok(None) => return completion,
        EvalResult::Ok(Some(return_)) => call_object(cx, return_, iterator.iterator.into(), &[]),
        EvalResult::Throw(thrown_value) => EvalResult::Throw(thrown_value),
    };

    if completion.kind() == CompletionKind::Throw {
        return completion;
    }

    match inner_result {
        EvalResult::Throw(thrown_value) => return Completion::throw(thrown_value),
        EvalResult::Ok(inner_value) => {
            if !inner_value.is_object() {
                return type_error(cx, "iterator's return method must return an object");
            }

            return completion;
        }
    }
}

// 7.4.10 CreateIterResultObject
pub fn create_iter_result_object(
    cx: &mut Context,
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
pub fn iter_iterator_values<F: FnMut(&mut Context, Handle<Value>) -> Option<Completion>>(
    cx: &mut Context,
    object: Handle<Value>,
    f: &mut F,
) -> Completion {
    let iterator = maybe__!(get_iterator(cx, object, IteratorHint::Sync, None));

    loop {
        let iter_result = maybe__!(iterator_step(cx, &iterator));
        match iter_result {
            None => return Completion::empty(cx),
            Some(iter_result) => {
                let value = maybe__!(iterator_value(cx, iter_result));

                let completion = f(cx, value);

                if let Some(completion) = completion {
                    return maybe_!(iterator_close(cx, &iterator, completion)).into();
                }
            }
        }
    }
}

pub fn iter_iterator_method_values<F: FnMut(&mut Context, Handle<Value>) -> Option<Completion>>(
    cx: &mut Context,
    object: Handle<Value>,
    method: Handle<ObjectValue>,
    f: &mut F,
) -> Completion {
    let iterator = maybe__!(get_iterator(cx, object, IteratorHint::Sync, Some(method)));

    loop {
        let iter_result = maybe__!(iterator_step(cx, &iterator));
        match iter_result {
            None => return Completion::empty(cx),
            Some(iter_result) => {
                let value = maybe__!(iterator_value(cx, iter_result));

                let completion = f(cx, value);

                if let Some(completion) = completion {
                    return maybe_!(iterator_close(cx, &iterator, completion)).into();
                }
            }
        }
    }
}

// 27.1.4.1 CreateAsyncFromSyncIterator
fn create_async_from_sync_iterator(_sync_iterator_record: Iterator) -> Iterator {
    unimplemented!("CreateAsyncFromSyncIterator")
}
