use crate::{
    js::runtime::{
        abstract_operations::{call, call_object, create_data_property_or_throw, invoke},
        array_object::{array_create, ArrayObject},
        builtin_function::{BuiltinFunction, BuiltinFunctionPtr},
        error::type_error,
        eval_result::EvalResult,
        function::get_argument,
        get,
        intrinsics::{
            aggregate_error_constructor::AggregateErrorObject, boolean_constructor::BooleanObject,
        },
        iterator::{get_iterator, iterator_close, iterator_step_value, Iterator, IteratorHint},
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create,
        promise_object::{promise_resolve, resolve, PromiseCapability, PromiseObject},
        realm::Realm,
        type_utilities::is_callable,
        Context, Handle, PropertyKey, Value,
    },
    must,
};

use super::{intrinsics::Intrinsic, number_constructor::NumberObject, rust_runtime::return_this};

/// IfAbruptRejectPromise (https://tc39.es/ecma262/#sec-ifabruptrejectpromise)
#[macro_export]
macro_rules! if_abrupt_reject_promise {
    ($cx:expr, $completion:expr, $capability:expr) => {{
        let completion = $completion;
        let capability = $capability;

        match completion {
            Ok(value) => value,
            Err(error) => {
                call_object($cx, capability.reject(), $cx.undefined(), &[error])?;
                return Ok(capability.promise().into());
            }
        }
    }};
}

pub struct PromiseConstructor;

impl PromiseConstructor {
    /// Properties of the Promise Constructor (https://tc39.es/ecma262/#sec-properties-of-the-promise-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.promise(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::PromisePrototype).into(),
        );

        func.intrinsic_func(cx, cx.names.all(), Self::all, 1, realm);
        func.intrinsic_func(cx, cx.names.all_settled(), Self::all_settled, 1, realm);
        func.intrinsic_func(cx, cx.names.any(), Self::any, 1, realm);
        func.intrinsic_func(cx, cx.names.race(), Self::race, 1, realm);
        func.intrinsic_func(cx, cx.names.reject(), Self::reject, 1, realm);
        func.intrinsic_func(cx, cx.names.resolve(), Self::resolve, 1, realm);
        func.intrinsic_func(cx, cx.names.try_(), Self::try_, 1, realm);
        func.intrinsic_func(cx, cx.names.with_resolvers(), Self::with_resolvers, 0, realm);

        // get Promise [ @@species ] (https://tc39.es/ecma262/#sec-get-promise-%symbol.species%)
        let species_key = cx.well_known_symbols.species();
        func.intrinsic_getter(cx, species_key, return_this, realm);

        func
    }

    //// Promise (https://tc39.es/ecma262/#sec-promise-executor)
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let new_target = if let Some(target) = new_target {
            target
        } else {
            return type_error(cx, "Promise constructor must be called with new");
        };

        // Extract and check type of executor
        let executor = get_argument(cx, arguments, 0);
        if !is_callable(executor) {
            return type_error(cx, "Promise executor must be a function");
        }
        let executor = executor.as_object();

        let promise = PromiseObject::new_from_constructor(cx, new_target)?;

        execute_then(cx, executor, cx.undefined(), promise)
    }

    fn collect_iterable_promises(
        cx: Context,
        constructor: Handle<Value>,
        iterable: Handle<Value>,
        mut f: impl FnMut(
            Context,
            &mut Iterator,
            Handle<ObjectValue>,
            Handle<PromiseCapability>,
            Handle<ObjectValue>,
        ) -> EvalResult<Handle<Value>>,
    ) -> EvalResult<Handle<Value>> {
        let capability = PromiseCapability::new(cx, constructor)?;
        let constructor = constructor.as_object();

        let resolve_completion = get_promise_resolve(cx, constructor);
        let resolve = if_abrupt_reject_promise!(cx, resolve_completion, capability);

        let iterator_completion = get_iterator(cx, iterable, IteratorHint::Sync, None);
        let mut iterator = if_abrupt_reject_promise!(cx, iterator_completion, capability);

        let mut completion = f(cx, &mut iterator, constructor, capability, resolve);

        if completion.is_err() {
            if !iterator.is_done {
                completion = iterator_close(cx, iterator.iterator, completion);
            }

            if_abrupt_reject_promise!(cx, completion, capability);
        }

        completion
    }

    fn get_already_called_or_false(cx: Context, function: Handle<ObjectValue>) -> bool {
        if let Some(property) =
            function.private_element_find(cx, cx.well_known_symbols.already_called().cast())
        {
            property.value().as_bool()
        } else {
            false
        }
    }

    fn get_already_called_object(
        cx: Context,
        function: Handle<ObjectValue>,
    ) -> Handle<BooleanObject> {
        function
            .private_element_find(cx, cx.well_known_symbols.already_called().cast())
            .unwrap()
            .value()
            .cast::<BooleanObject>()
    }

    fn set_already_called(cx: Context, mut function: Handle<ObjectValue>, value: Handle<Value>) {
        function.private_element_set(cx, cx.well_known_symbols.already_called().cast(), value);
    }

    fn get_index(cx: Context, function: Handle<ObjectValue>) -> Handle<Value> {
        function
            .private_element_find(cx, cx.well_known_symbols.index().cast())
            .unwrap()
            .value()
    }

    fn set_index(cx: Context, mut function: Handle<ObjectValue>, value: Value) {
        function.private_element_set(cx, cx.well_known_symbols.index().cast(), value.to_handle(cx));
    }

    fn get_values(cx: Context, function: Handle<ObjectValue>) -> Handle<ArrayObject> {
        function
            .private_element_find(cx, cx.well_known_symbols.values().cast())
            .unwrap()
            .value()
            .as_object()
            .cast::<ArrayObject>()
    }

    fn set_values(cx: Context, mut function: Handle<ObjectValue>, value: Handle<ArrayObject>) {
        function.private_element_set(cx, cx.well_known_symbols.values().cast(), value.into());
    }

    fn get_capability(cx: Context, function: Handle<ObjectValue>) -> Handle<PromiseCapability> {
        function
            .private_element_find(cx, cx.well_known_symbols.capability().cast())
            .unwrap()
            .value()
            .as_object()
            .cast::<PromiseCapability>()
    }

    fn set_capability(
        cx: Context,
        mut function: Handle<ObjectValue>,
        value: Handle<PromiseCapability>,
    ) {
        function.private_element_set(cx, cx.well_known_symbols.capability().cast(), value.into());
    }

    fn get_remaining_elements(cx: Context, function: Handle<ObjectValue>) -> Handle<NumberObject> {
        function
            .private_element_find(cx, cx.well_known_symbols.remaining_elements().cast())
            .unwrap()
            .value()
            .as_object()
            .cast::<NumberObject>()
    }

    fn set_remaining_elements(
        cx: Context,
        mut function: Handle<ObjectValue>,
        value: Handle<NumberObject>,
    ) {
        function.private_element_set(
            cx,
            cx.well_known_symbols.remaining_elements().cast(),
            value.into(),
        );
    }

    /// Promise.all (https://tc39.es/ecma262/#sec-promise.all)
    pub fn all(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let iterable = get_argument(cx, arguments, 0);
        Self::collect_iterable_promises(cx, this_value, iterable, Self::perform_promise_all)
    }

    /// PerformPromiseAll (https://tc39.es/ecma262/#sec-performpromiseall)
    fn perform_promise_all(
        cx: Context,
        iterator: &mut Iterator,
        constructor: Handle<ObjectValue>,
        capability: Handle<PromiseCapability>,
        resolve: Handle<ObjectValue>,
    ) -> EvalResult<Handle<Value>> {
        let values = must!(array_create(cx, 0, None));
        let mut remaining_elements = NumberObject::new(cx, 1.0);
        let mut index = 0;

        loop {
            let next_value = iterator_step_value(cx, iterator)?;
            let next_value = match next_value {
                None => {
                    // Increment number of remaining elements
                    let num_remaining = remaining_elements.number_data();
                    remaining_elements.set_number_data(num_remaining - 1.0);

                    // Resolve the outer promise if all promises have resolved
                    if remaining_elements.number_data() == 0.0 {
                        call_object(cx, capability.resolve(), cx.undefined(), &[values.into()])?;
                    }

                    return Ok(capability.promise().as_value());
                }
                Some(next_value) => next_value,
            };

            // Create a resolve function for each of the promises
            let promise_all_resolve = BuiltinFunction::create(
                cx,
                Self::promise_all_resolve,
                1,
                cx.names.empty_string(),
                cx.current_realm(),
                None,
                None,
            );

            // Attach various private properties to the resolve function
            Self::set_index(cx, promise_all_resolve, Value::from(index));
            Self::set_values(cx, promise_all_resolve, values);
            Self::set_capability(cx, promise_all_resolve, capability);
            Self::set_remaining_elements(cx, promise_all_resolve, remaining_elements);

            // Increment number of remaining elements
            let num_remaining = remaining_elements.number_data();
            remaining_elements.set_number_data(num_remaining + 1.0);

            // Call the promise's `then` with the custom resolve and default reject functions
            let next_promise = call_object(cx, resolve, constructor.into(), &[next_value])?;

            let arguments = &[promise_all_resolve.into(), capability.reject().into()];
            invoke(cx, next_promise, cx.names.then(), arguments)?;

            index += 1;
        }
    }

    /// Promise.all Resolve (https://tc39.es/ecma262/#sec-promise.all-resolve-element-functions)
    pub fn promise_all_resolve(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let function = cx.current_function();

        // Check if already called and mark as called
        if Self::get_already_called_or_false(cx, function) {
            return Ok(cx.undefined());
        }

        Self::set_already_called(cx, function, cx.bool(true));

        // Set the value at the index in the values array
        let resolved_value = get_argument(cx, arguments, 0);
        let index = Self::get_index(cx, function);
        let values = Self::get_values(cx, function);

        let key = PropertyKey::from_value(cx, index)?.to_handle(cx);
        must!(create_data_property_or_throw(cx, values.into(), key, resolved_value));

        // Decrement the number of remaining elements
        let mut remaining_elements = Self::get_remaining_elements(cx, function);
        let num_remaining = remaining_elements.number_data();
        remaining_elements.set_number_data(num_remaining - 1.0);

        // If all promises have been resolved then resolve the outer promise
        if remaining_elements.number_data() == 0.0 {
            let capability = Self::get_capability(cx, function);
            return call_object(cx, capability.resolve(), cx.undefined(), &[values.into()]);
        }

        Ok(cx.undefined())
    }

    /// Promise.allSettled (https://tc39.es/ecma262/#sec-promise.allsettled)
    pub fn all_settled(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let iterable = get_argument(cx, arguments, 0);
        Self::collect_iterable_promises(cx, this_value, iterable, Self::perform_promise_all_settled)
    }

    /// Promise.allSettled (https://tc39.es/ecma262/#sec-performpromiseallsettled)
    fn perform_promise_all_settled(
        cx: Context,
        iterator: &mut Iterator,
        constructor: Handle<ObjectValue>,
        capability: Handle<PromiseCapability>,
        resolve: Handle<ObjectValue>,
    ) -> EvalResult<Handle<Value>> {
        let values = must!(array_create(cx, 0, None));
        let mut remaining_elements = NumberObject::new(cx, 1.0);
        let mut index = 0;

        loop {
            let next_value = iterator_step_value(cx, iterator)?;
            let next_value = match next_value {
                None => {
                    // Increment number of remaining elements
                    let num_remaining = remaining_elements.number_data();
                    remaining_elements.set_number_data(num_remaining - 1.0);

                    // Resolve the outer promise if all promises have resolved
                    if remaining_elements.number_data() == 0.0 {
                        call_object(cx, capability.resolve(), cx.undefined(), &[values.into()])?;
                    }

                    return Ok(capability.promise().as_value());
                }
                Some(next_value) => next_value,
            };

            // AlreadyCalled is a boolean object so it can be shared between resolve/reject
            let already_called = BooleanObject::new(cx, false);

            // Create a resolve function for each of the promises
            let promise_all_settled_resolve = BuiltinFunction::create(
                cx,
                Self::promise_all_settled_resolve,
                1,
                cx.names.empty_string(),
                cx.current_realm(),
                None,
                None,
            );

            // Attach various private properties to the resolve function
            Self::set_already_called(cx, promise_all_settled_resolve, already_called.into());
            Self::set_index(cx, promise_all_settled_resolve, Value::from(index));
            Self::set_values(cx, promise_all_settled_resolve, values);
            Self::set_capability(cx, promise_all_settled_resolve, capability);
            Self::set_remaining_elements(cx, promise_all_settled_resolve, remaining_elements);

            // Create a reject function for each of the promises
            let promise_all_settled_reject = BuiltinFunction::create(
                cx,
                Self::promise_all_settled_reject,
                1,
                cx.names.empty_string(),
                cx.current_realm(),
                None,
                None,
            );

            // Attach various private properties to the reject function
            Self::set_already_called(cx, promise_all_settled_reject, already_called.into());
            Self::set_index(cx, promise_all_settled_reject, Value::from(index));
            Self::set_values(cx, promise_all_settled_reject, values);
            Self::set_capability(cx, promise_all_settled_reject, capability);
            Self::set_remaining_elements(cx, promise_all_settled_reject, remaining_elements);

            // Increment number of remaining elements
            let num_remaining = remaining_elements.number_data();
            remaining_elements.set_number_data(num_remaining + 1.0);

            // Call the promise's `then` with the custom resolve and reject functions
            let next_promise = call_object(cx, resolve, constructor.into(), &[next_value])?;

            let arguments = &[
                promise_all_settled_resolve.into(),
                promise_all_settled_reject.into(),
            ];
            invoke(cx, next_promise, cx.names.then(), arguments)?;

            index += 1;
        }
    }

    /// Promise.allSettled Resolve (https://tc39.es/ecma262/#sec-promise.allsettled-resolve-element-functions)
    pub fn promise_all_settled_resolve(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let function = cx.current_function();

        // Check if already called and mark as called
        let mut already_called = Self::get_already_called_object(cx, function);
        if already_called.boolean_data() {
            return Ok(cx.undefined());
        }

        already_called.set_boolean_data(true);

        // Create the result object
        let resolved_value = get_argument(cx, arguments, 0);
        let result_object = ordinary_object_create(cx);
        must!(create_data_property_or_throw(
            cx,
            result_object,
            cx.names.status(),
            cx.names.fulfilled().as_string().into(),
        ));
        must!(create_data_property_or_throw(
            cx,
            result_object,
            cx.names.value(),
            resolved_value
        ));

        // Set the value at the index in the values array
        let index = Self::get_index(cx, function);
        let values = Self::get_values(cx, function);

        let key = PropertyKey::from_value(cx, index)?.to_handle(cx);
        must!(create_data_property_or_throw(cx, values.into(), key, result_object.into()));

        // Decrement the number of remaining elements
        let mut remaining_elements = Self::get_remaining_elements(cx, function);
        let num_remaining = remaining_elements.number_data();
        remaining_elements.set_number_data(num_remaining - 1.0);

        // If all promises have been settled then resolve the outer promise
        if remaining_elements.number_data() == 0.0 {
            let capability = Self::get_capability(cx, function);
            return call_object(cx, capability.resolve(), cx.undefined(), &[values.into()]);
        }

        Ok(cx.undefined())
    }

    /// Promise.allSettled Reject (https://tc39.es/ecma262/#sec-promise.allsettled-reject-element-functions)
    pub fn promise_all_settled_reject(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let function = cx.current_function();

        // Check if already called and mark as called
        let mut already_called = Self::get_already_called_object(cx, function);
        if already_called.boolean_data() {
            return Ok(cx.undefined());
        }

        already_called.set_boolean_data(true);

        // Create the result object
        let rejected_value = get_argument(cx, arguments, 0);
        let result_object = ordinary_object_create(cx);
        must!(create_data_property_or_throw(
            cx,
            result_object,
            cx.names.status(),
            cx.names.rejected().as_string().into(),
        ));
        must!(create_data_property_or_throw(
            cx,
            result_object,
            cx.names.reason(),
            rejected_value
        ));

        // Set the value at the index in the values array
        let index = Self::get_index(cx, function);
        let values = Self::get_values(cx, function);

        let key = PropertyKey::from_value(cx, index)?.to_handle(cx);
        must!(create_data_property_or_throw(cx, values.into(), key, result_object.into()));

        // Decrement the number of remaining elements
        let mut remaining_elements = Self::get_remaining_elements(cx, function);
        let num_remaining = remaining_elements.number_data();
        remaining_elements.set_number_data(num_remaining - 1.0);

        // If all promises have been settled then resolve the outer promise
        if remaining_elements.number_data() == 0.0 {
            let capability = Self::get_capability(cx, function);
            return call_object(cx, capability.resolve(), cx.undefined(), &[values.into()]);
        }

        Ok(cx.undefined())
    }

    /// Promise.any (https://tc39.es/ecma262/#sec-promise.any)
    pub fn any(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let iterable = get_argument(cx, arguments, 0);
        Self::collect_iterable_promises(cx, this_value, iterable, Self::perform_promise_any)
    }

    /// PerformPromiseAny (https://tc39.es/ecma262/#sec-performpromiseany)
    fn perform_promise_any(
        cx: Context,
        iterator: &mut Iterator,
        constructor: Handle<ObjectValue>,
        capability: Handle<PromiseCapability>,
        resolve: Handle<ObjectValue>,
    ) -> EvalResult<Handle<Value>> {
        let errors = must!(array_create(cx, 0, None));
        let mut remaining_elements = NumberObject::new(cx, 1.0);
        let mut index = 0;

        loop {
            let next_value = iterator_step_value(cx, iterator)?;
            let next_value = match next_value {
                None => {
                    // Increment number of remaining elements
                    let num_remaining = remaining_elements.number_data();
                    remaining_elements.set_number_data(num_remaining - 1.0);

                    // Throw an aggregate error to reject the outer promise if all promises have
                    // rejected.
                    if remaining_elements.number_data() == 0.0 {
                        let error = AggregateErrorObject::new(cx, errors.into());
                        call_object(cx, capability.reject(), cx.undefined(), &[error.into()])?;
                    }

                    return Ok(capability.promise().as_value());
                }
                Some(next_value) => next_value,
            };

            // Create a reject function for each of the promises
            let promise_any_reject = BuiltinFunction::create(
                cx,
                Self::promise_any_reject,
                1,
                cx.names.empty_string(),
                cx.current_realm(),
                None,
                None,
            );

            // Attach various private properties to the resolve function
            Self::set_index(cx, promise_any_reject, Value::from(index));
            Self::set_values(cx, promise_any_reject, errors);
            Self::set_capability(cx, promise_any_reject, capability);
            Self::set_remaining_elements(cx, promise_any_reject, remaining_elements);

            // Increment number of remaining elements
            let num_remaining = remaining_elements.number_data();
            remaining_elements.set_number_data(num_remaining + 1.0);

            // Call the promise's `then` with the default resolve and custom reject functions
            let next_promise = call_object(cx, resolve, constructor.into(), &[next_value])?;

            let arguments = &[capability.resolve().into(), promise_any_reject.into()];
            invoke(cx, next_promise, cx.names.then(), arguments)?;

            index += 1;
        }
    }

    /// Promise.any Reject (https://tc39.es/ecma262/#sec-promise.any-reject-element-functions)
    pub fn promise_any_reject(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let function = cx.current_function();

        // Check if already called and mark as called
        if Self::get_already_called_or_false(cx, function) {
            return Ok(cx.undefined());
        }

        Self::set_already_called(cx, function, cx.bool(true));

        // Set the rejected value at the index in the errors array
        let rejected_value = get_argument(cx, arguments, 0);
        let index = Self::get_index(cx, function);
        let errors = Self::get_values(cx, function);

        let key = PropertyKey::from_value(cx, index)?.to_handle(cx);
        must!(create_data_property_or_throw(cx, errors.into(), key, rejected_value));

        // Decrement the number of remaining elements
        let mut remaining_elements = Self::get_remaining_elements(cx, function);
        let num_remaining = remaining_elements.number_data();
        remaining_elements.set_number_data(num_remaining - 1.0);

        // If all promises have been rejected then reject the outer promise with an aggregate error
        if remaining_elements.number_data() == 0.0 {
            let error = AggregateErrorObject::new(cx, errors.into());
            let capability = Self::get_capability(cx, function);
            return call_object(cx, capability.reject(), cx.undefined(), &[error.into()]);
        }

        Ok(cx.undefined())
    }

    /// Promise.race (https://tc39.es/ecma262/#sec-promise.race)
    pub fn race(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let iterable = get_argument(cx, arguments, 0);
        Self::collect_iterable_promises(cx, this_value, iterable, Self::perform_promise_race)
    }

    /// PerformPromiseRace (https://tc39.es/ecma262/#sec-performpromiserace)
    fn perform_promise_race(
        cx: Context,
        iterator: &mut Iterator,
        constructor: Handle<ObjectValue>,
        capability: Handle<PromiseCapability>,
        resolve: Handle<ObjectValue>,
    ) -> EvalResult<Handle<Value>> {
        loop {
            let next_value = iterator_step_value(cx, iterator)?;
            match next_value {
                None => return Ok(capability.promise().as_value()),
                Some(next_value) => {
                    let next_promise = call_object(cx, resolve, constructor.into(), &[next_value])?;

                    let arguments = &[capability.resolve().into(), capability.reject().into()];
                    invoke(cx, next_promise, cx.names.then(), arguments)?;
                }
            }
        }
    }

    /// Promise.reject (https://tc39.es/ecma262/#sec-promise.reject)
    pub fn reject(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let result = get_argument(cx, arguments, 0);

        // Create a new promise and immediately reject it
        let capability = PromiseCapability::new(cx, this_value)?;
        call_object(cx, capability.reject(), cx.undefined(), &[result])?;

        Ok(capability.promise().as_value())
    }

    /// Promise.resolve (https://tc39.es/ecma262/#sec-promise.resolve)
    pub fn resolve(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Promise.resolve called on non-object");
        }

        let result = get_argument(cx, arguments, 0);
        Ok(promise_resolve(cx, this_value, result)?.as_value())
    }

    /// Promise.try (https://tc39.es/ecma262/#sec-promise.try)
    pub fn try_(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if !this_value.is_object() {
            return type_error(cx, "Promise.try called on non-object");
        }

        let capability = PromiseCapability::new(cx, this_value)?;

        let callback_arg = get_argument(cx, arguments, 0);
        let completion = call(cx, callback_arg, cx.undefined(), &arguments[1..]);

        match completion {
            Ok(value) => call_object(cx, capability.resolve(), cx.undefined(), &[value])?,
            Err(error) => call_object(cx, capability.reject(), cx.undefined(), &[error])?,
        };

        Ok(capability.promise().as_value())
    }

    /// Promise.withResolvers (https://tc39.es/ecma262/#sec-promise.withResolvers)
    pub fn with_resolvers(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let capability = PromiseCapability::new(cx, this_value)?;

        let object = ordinary_object_create(cx);

        must!(create_data_property_or_throw(
            cx,
            object,
            cx.names.promise_(),
            capability.promise().into()
        ));
        must!(create_data_property_or_throw(
            cx,
            object,
            cx.names.resolve(),
            capability.resolve().into()
        ));
        must!(create_data_property_or_throw(
            cx,
            object,
            cx.names.reject(),
            capability.reject().into()
        ));

        Ok(object.as_value())
    }
}

pub fn execute_then(
    cx: Context,
    executor: Handle<ObjectValue>,
    this_value: Handle<Value>,
    mut promise: Handle<PromiseObject>,
) -> EvalResult<Handle<Value>> {
    // Create resolve and reject functions, passing into the executor
    let resolve_function = create_resolve_function(cx, promise);
    let reject_function = create_reject_function(cx, promise);

    promise.set_already_resolved(false);

    let completion =
        call_object(cx, executor, this_value, &[resolve_function.into(), reject_function.into()]);

    // Reject if the executor function throws
    if let Err(error) = completion {
        call_object(cx, reject_function, cx.undefined(), &[error])?;
    }

    Ok(promise.as_value())
}

fn create_resolve_function(cx: Context, promise: Handle<PromiseObject>) -> Handle<ObjectValue> {
    create_settle_function(cx, promise, resolve_builtin_function)
}

fn create_reject_function(cx: Context, promise: Handle<PromiseObject>) -> Handle<ObjectValue> {
    create_settle_function(cx, promise, reject_builtin_function)
}

fn create_settle_function(
    cx: Context,
    promise: Handle<PromiseObject>,
    func: BuiltinFunctionPtr,
) -> Handle<ObjectValue> {
    let mut function = BuiltinFunction::create(
        cx,
        func,
        1,
        cx.names.empty_string(),
        cx.current_realm(),
        None,
        None,
    );

    function.private_element_set(cx, cx.well_known_symbols.promise().cast(), promise.into());

    function
}

fn get_promise(cx: Context, settle_function: Handle<ObjectValue>) -> Handle<PromiseObject> {
    settle_function
        .private_element_find(cx, cx.well_known_symbols.promise().cast())
        .unwrap()
        .value()
        .as_object()
        .cast::<PromiseObject>()
}

/// Promise Resolve Functions (https://tc39.es/ecma262/#sec-promise-resolve-functions)
pub fn resolve_builtin_function(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let resolution = get_argument(cx, arguments, 0);

    let function = cx.current_function();
    let promise = get_promise(cx, function);

    resolve(cx, promise, resolution);

    Ok(cx.undefined())
}

/// Promise Reject Functions (https://tc39.es/ecma262/#sec-promise-reject-functions)
pub fn reject_builtin_function(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let resolution = get_argument(cx, arguments, 0);

    let function = cx.current_function();
    let mut promise = get_promise(cx, function);

    // Rejecting an already settled promise has no effect
    if promise.is_pending() {
        promise.reject(cx, *resolution);
    }

    Ok(cx.undefined())
}

/// GetPromiseResolve (https://tc39.es/ecma262/#sec-getpromiseresolve)
fn get_promise_resolve(
    cx: Context,
    constructor: Handle<ObjectValue>,
) -> EvalResult<Handle<ObjectValue>> {
    let resolve = get(cx, constructor, cx.names.resolve())?;
    if !is_callable(resolve) {
        return type_error(cx, "resolve property must be a function");
    }

    Ok(resolve.as_object())
}
