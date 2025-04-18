use crate::{
    extend_object, if_abrupt_reject_promise, must,
    runtime::{
        abstract_operations::{call_object, get_method},
        builtin_function::BuiltinFunction,
        error::type_error_value,
        eval_result::EvalResult,
        function::get_argument,
        gc::{HeapObject, HeapVisitor},
        intrinsics::promise_prototype::perform_promise_then,
        iterator::{
            create_iter_result_object, iterator_close, iterator_complete, iterator_next,
            iterator_value, Iterator,
        },
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        promise_object::{coerce_to_ordinary_promise, PromiseCapability},
        realm::Realm,
        Context, Handle, HeapPtr, Value,
    },
    set_uninit,
};

use super::intrinsics::Intrinsic;

// Async-from-Sync Iterator Objects (https://tc39.es/ecma262/#sec-async-from-sync-iterator-objects)
extend_object! {
    pub struct AsyncFromSyncIterator {
        iterator: HeapPtr<ObjectValue>,
        next_method: Value,
    }
}

impl AsyncFromSyncIterator {
    pub fn new(cx: Context, iterator: Iterator) -> Handle<AsyncFromSyncIterator> {
        let mut object = object_create::<AsyncFromSyncIterator>(
            cx,
            ObjectKind::AsyncFromSyncIterator,
            Intrinsic::AsyncFromSyncIteratorPrototype,
        );

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::AsyncFromSyncIterator));
        set_uninit!(object.iterator, *iterator.iterator);
        set_uninit!(object.next_method, *iterator.next_method);

        object.to_handle()
    }

    fn iterator(&self) -> Handle<ObjectValue> {
        self.iterator.to_handle()
    }

    fn next_method(&self, cx: Context) -> Handle<Value> {
        self.next_method.to_handle(cx)
    }
}

impl HeapObject for HeapPtr<AsyncFromSyncIterator> {
    fn byte_size(&self) -> usize {
        std::mem::size_of::<AsyncFromSyncIterator>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.iterator);
        visitor.visit_value(&mut self.next_method);
    }
}

pub struct AsyncFromSyncIteratorPrototype;

impl AsyncFromSyncIteratorPrototype {
    /// The %AsyncFromSyncIteratorPrototype% Object (https://tc39.es/ecma262/#sec-%asyncfromsynciteratorprototype%-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object = ObjectValue::new(
            cx,
            Some(realm.get_intrinsic(Intrinsic::AsyncIteratorPrototype)),
            true,
        );

        object.intrinsic_func(cx, cx.names.next(), Self::next, 0, realm);
        object.intrinsic_func(cx, cx.names.return_(), Self::return_, 0, realm);
        object.intrinsic_func(cx, cx.names.throw(), Self::throw, 0, realm);

        object
    }

    /// %AsyncFromSyncIteratorPrototype%.next (https://tc39.es/ecma262/#sec-%asyncfromsynciteratorprototype%.next)
    pub fn next(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
        let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

        let async_iterator = this_value.as_object().cast::<AsyncFromSyncIterator>();
        let sync_iterator = async_iterator.iterator();

        let value = if arguments.is_empty() {
            None
        } else {
            Some(get_argument(cx, arguments, 0))
        };

        let iter_result_completion =
            iterator_next(cx, async_iterator.iterator(), async_iterator.next_method(cx), value);

        let iter_result = if_abrupt_reject_promise!(cx, iter_result_completion, capability);

        async_from_sync_iterator_continuation(
            cx,
            iter_result,
            capability,
            sync_iterator,
            /* close_on_rejection */ true,
        )
    }

    /// %AsyncFromSyncIteratorPrototype%.return (https://tc39.es/ecma262/#sec-%asyncfromsynciteratorprototype%.return)
    pub fn return_(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
        let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

        let async_iterator = this_value.as_object().cast::<AsyncFromSyncIterator>();
        let sync_iterator = async_iterator.iterator();

        let return_method_completion = get_method(cx, sync_iterator.into(), cx.names.return_());
        let return_method = if_abrupt_reject_promise!(cx, return_method_completion, capability);

        // If there is no return method the promise can immediately be resolved
        if return_method.is_none() {
            let value = get_argument(cx, arguments, 0);
            let iter_result = create_iter_result_object(cx, value, true);
            must!(call_object(cx, capability.resolve(), cx.undefined(), &[iter_result]));

            return Ok(capability.promise().as_value());
        }

        // If return method is present then call it, passing in value if necessary
        let return_method = return_method.unwrap();
        let return_result_completion = if arguments.is_empty() {
            call_object(cx, return_method, sync_iterator.into(), &[])
        } else {
            let value = get_argument(cx, arguments, 0);
            call_object(cx, return_method, sync_iterator.into(), &[value])
        };

        // Return result must be an object
        let return_result = if_abrupt_reject_promise!(cx, return_result_completion, capability);
        if !return_result.is_object() {
            let error = type_error_value(cx, "return method must return an object");
            must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));

            return Ok(capability.promise().as_value());
        }

        async_from_sync_iterator_continuation(
            cx,
            return_result.as_object(),
            capability,
            sync_iterator,
            /* close_on_rejection */ false,
        )
    }

    /// %AsyncFromSyncIteratorPrototype%.throw (https://tc39.es/ecma262/#sec-%asyncfromsynciteratorprototype%.throw)
    pub fn throw(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
        let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

        let async_iterator = this_value.as_object().cast::<AsyncFromSyncIterator>();
        let sync_iterator = async_iterator.iterator();

        let throw_method_completion = get_method(cx, sync_iterator.into(), cx.names.throw());
        let throw_method = if_abrupt_reject_promise!(cx, throw_method_completion, capability);

        // If there is no throw method the promise can immediately be rejected
        if throw_method.is_none() {
            // First close the iterator to give it a chance to clean up before we reject the promise
            let close_result = iterator_close(cx, sync_iterator, Ok(cx.empty()));
            if_abrupt_reject_promise!(cx, close_result, capability);

            // Reject the promise with a new TypeError
            let error = type_error_value(cx, "throw method is not present");
            must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));

            return Ok(capability.promise().as_value());
        }

        // If throw method is present then call it, passing in value if necessary
        let throw_method = throw_method.unwrap();
        let throw_result_completion = if arguments.is_empty() {
            call_object(cx, throw_method, sync_iterator.into(), &[])
        } else {
            let value = get_argument(cx, arguments, 0);
            call_object(cx, throw_method, sync_iterator.into(), &[value])
        };

        // Throw result must be an object
        let throw_result = if_abrupt_reject_promise!(cx, throw_result_completion, capability);
        if !throw_result.is_object() {
            let error = type_error_value(cx, "throw method must return an object");
            must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));

            return Ok(capability.promise().as_value());
        }

        async_from_sync_iterator_continuation(
            cx,
            throw_result.as_object(),
            capability,
            sync_iterator,
            /* close_on_rejection */ true,
        )
    }
}

/// AsyncFromSyncIteratorContinuation (https://tc39.es/ecma262/#sec-asyncfromsynciteratorcontinuation)
fn async_from_sync_iterator_continuation(
    cx: Context,
    iter_result: Handle<ObjectValue>,
    capability: Handle<PromiseCapability>,
    sync_iterator: Handle<ObjectValue>,
    close_on_rejection: bool,
) -> EvalResult<Handle<Value>> {
    let is_done_completion = iterator_complete(cx, iter_result);
    let is_done = if_abrupt_reject_promise!(cx, is_done_completion, capability);

    let value_completion = iterator_value(cx, iter_result);
    let value = if_abrupt_reject_promise!(cx, value_completion, capability);

    let value_promise_completion = coerce_to_ordinary_promise(cx, value);

    if value_promise_completion.is_err() && !is_done && close_on_rejection {
        if let Err(error) = value_promise_completion {
            // Can ignore result since passing in an Err completion guarantees that same completion
            // will be returned, which is a no-op when reassigned as the `value_promise_completion`.
            let _ = iterator_close(cx, sync_iterator, Err(error));
        }
    }

    let value_promise = if_abrupt_reject_promise!(cx, value_promise_completion, capability);

    // Create a function that turns a value into an iter result object
    let create_iter_result = if is_done {
        create_done_iter_result_object
    } else {
        create_continuing_iter_result_object
    };

    let on_fulfilled = BuiltinFunction::create(
        cx,
        create_iter_result,
        1,
        cx.names.empty_string(),
        cx.current_realm(),
        None,
        None,
    );

    let on_reject = if is_done || !close_on_rejection {
        cx.undefined()
    } else {
        // Create the reject function with the sync iterator attached
        let on_reject = BuiltinFunction::create(
            cx,
            async_from_sync_iterator_continuation_on_reject,
            1,
            cx.names.empty_string(),
            cx.current_realm(),
            None,
            None,
        );
        set_sync_iterator(cx, on_reject, sync_iterator);

        on_reject.as_value()
    };

    perform_promise_then(cx, value_promise, on_fulfilled.into(), on_reject, Some(capability));

    Ok(capability.promise().as_value())
}

pub fn create_continuing_iter_result_object(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    let value = get_argument(cx, arguments, 0);
    Ok(create_iter_result_object(cx, value, /* is_done */ false))
}

pub fn create_done_iter_result_object(
    cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    let value = get_argument(cx, arguments, 0);
    Ok(create_iter_result_object(cx, value, /* is_done */ true))
}

pub fn async_from_sync_iterator_continuation_on_reject(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    // Fetch the iterator passed from the caller
    let current_function = cx.current_function();
    let sync_iterator = get_sync_iterator(cx, current_function);

    let error = get_argument(cx, arguments, 0);

    iterator_close(cx, sync_iterator, Err(error))
}

fn get_sync_iterator(cx: Context, function: Handle<ObjectValue>) -> Handle<ObjectValue> {
    function
        .private_element_find(cx, cx.well_known_symbols.index().as_symbol())
        .unwrap()
        .value()
        .as_object()
}

fn set_sync_iterator(cx: Context, mut function: Handle<ObjectValue>, value: Handle<ObjectValue>) {
    function.private_element_set(cx, cx.well_known_symbols.index().as_symbol(), value.into());
}
