use crate::{
    js::runtime::{
        abstract_operations::{call_object, get_method},
        error::type_error_,
        get,
        property_key::PropertyKey,
    },
    maybe, maybe_, maybe__, must,
};

use super::{
    abstract_operations::{call, create_data_property_or_throw},
    error::type_error,
    intrinsics::intrinsics::Intrinsic,
    object_value::ObjectValue,
    ordinary_object::ordinary_object_create,
    type_utilities::to_boolean,
    Completion, CompletionKind, Context, EvalResult, Gc, Value,
};

// 7.4.1 Iterator Records
pub struct Iterator {
    pub iterator: Gc<ObjectValue>,
    pub next_method: Value,
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
    object: Value,
    hint: IteratorHint,
    method: Option<Gc<ObjectValue>>,
) -> EvalResult<Iterator> {
    let method = if let Some(method) = method {
        method
    } else {
        if hint == IteratorHint::Async {
            let async_iterator_key = PropertyKey::symbol(cx.well_known_symbols.async_iterator);
            let method = maybe!(get_method(cx, object, &async_iterator_key));

            if let Some(method) = method {
                method
            } else {
                let sync_iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
                let sync_method = maybe!(get_method(cx, object, &sync_iterator_key));
                let sync_iterator_record =
                    maybe!(get_iterator(cx, object, IteratorHint::Sync, sync_method));

                return create_async_from_sync_iterator(sync_iterator_record).into();
            }
        } else {
            let iterator_key = PropertyKey::symbol(cx.well_known_symbols.iterator);
            let method = maybe!(get_method(cx, object, &iterator_key));

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

    let next_method = maybe!(get(cx, iterator, &cx.names.next()));

    let iterator_record = Iterator { iterator, next_method, is_done: false };

    iterator_record.into()
}

// 7.4.3 IteratorNext
pub fn iterator_next(
    cx: &mut Context,
    iterator: &Iterator,
    value: Option<Value>,
) -> EvalResult<Gc<ObjectValue>> {
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
pub fn iterator_complete(cx: &mut Context, iter_result: Gc<ObjectValue>) -> EvalResult<bool> {
    let is_done = maybe!(get(cx, iter_result, &cx.names.done()));
    to_boolean(is_done).into()
}

// 7.4.5 IteratorValue
pub fn iterator_value(cx: &mut Context, iter_result: Gc<ObjectValue>) -> EvalResult<Value> {
    get(cx, iter_result, &cx.names.value())
}

// 7.4.6 IteratorStep
pub fn iterator_step(cx: &mut Context, iterator: &Iterator) -> EvalResult<Option<Gc<ObjectValue>>> {
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
    let inner_result = get_method(cx, iterator.iterator.into(), &cx.names.return_());
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
pub fn create_iter_result_object(cx: &mut Context, value: Value, is_done: bool) -> Gc<ObjectValue> {
    let object_proto = cx.current_realm().get_intrinsic(Intrinsic::ObjectPrototype);
    let ordinary_object = ordinary_object_create(object_proto);
    let object: Gc<ObjectValue> = cx.heap.alloc(ordinary_object).into();

    must!(create_data_property_or_throw(cx, object, &cx.names.value(), value));
    must!(create_data_property_or_throw(cx, object, &cx.names.done(), is_done.into()));

    object
}

// Iterate over an object, executing a callback function against every value returned by the
// iterator. Return a completion from the callback function to stop and close the iterator.
pub fn iter_iterator_values<F: FnMut(&mut Context, Value) -> Option<Completion>>(
    cx: &mut Context,
    object: Value,
    f: &mut F,
) -> Completion {
    let iterator = maybe__!(get_iterator(cx, object, IteratorHint::Sync, None));

    loop {
        let iter_result = maybe__!(iterator_step(cx, &iterator));
        match iter_result {
            None => return Completion::empty(),
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
fn create_async_from_sync_iterator(sync_iterator_record: Iterator) -> Iterator {
    unimplemented!("CreateAsyncFromSyncIterator")
}
