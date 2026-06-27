use crate::{
    common::numeric::MAX_SAFE_INTEGER_U64,
    completion_value, eval_err, must,
    runtime::{
        Context, EvalResult, Handle, HeapPtr, PropertyKey, Value,
        abstract_operations::{
            call, call_object, construct, create_data_property_or_throw, get_method,
            length_of_array_like, set,
        },
        array_object::array_create,
        builtin_generator::{BuiltinGenerator, BuiltinGeneratorCompletion, BuiltinGeneratorState},
        error::{type_error, type_error_value},
        function::get_argument,
        gc::{AnyHeapItem, HeapVisitor},
        get,
        intrinsics::intrinsics::Intrinsic,
        iterator::{
            IteratorHint, create_async_from_sync_iterator, get_iterator, iterator_complete,
            iterator_value,
        },
        object_value::ObjectValue,
        promise_object::{PromiseCapability, coerce_to_ordinary_promise},
        type_utilities::{is_callable, is_constructor_value, to_object},
    },
};

pub enum ArrayFromAsyncState {
    /// Suspended at an await in the iterator path
    IteratorAwaitValue {
        /// Result array that will be returned from Array.fromAsync
        array: HeapPtr<ObjectValue>,
        /// Source iterator that is providing values for the result array
        iterator: HeapPtr<ObjectValue>,
        /// `next` method on the source iterator
        next_method: Value,
        /// Current index in the result array
        index: u64,
        /// Optional mapper function
        mapper: Option<HeapPtr<ObjectValue>>,
        /// Value of `this` to use when calling the mapper function
        mapper_this_value: Value,
        /// Whether we are awaiting the mapped value vs the original value
        is_mapped_await: bool,
    },
    /// Suspended at an await in the array like path
    ArrayLikeAwaitValue {
        /// Result array that will be returned from Array.fromAsync
        array: HeapPtr<ObjectValue>,
        /// Source array that is providing values for the result array
        source_array_like: HeapPtr<ObjectValue>,
        /// Current index in the source and result arrays
        index: u64,
        /// Length of the source array
        length: u64,
        /// Optional mapper function
        mapper: Option<HeapPtr<ObjectValue>>,
        /// Value of `this` to use when calling the mapper function
        mapper_this_value: Value,
        /// Whether we are awaiting the mapped value vs the original value
        is_mapped_await: bool,
    },
    /// Reject Array.fromAsync's promise with the given value
    Reject { error_value: Value },
}

/// The builtin generator for the Array.fromAsync method.
pub struct ArrayFromAsyncGenerator;

impl ArrayFromAsyncGenerator {
    /// Start a new call to the Array.fromAsync generator function.
    pub fn start(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
        let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

        let from_async_completion = Self::start_impl(cx, capability, this_value, arguments);
        BuiltinGenerator::handle_async_function_completion(cx, capability, from_async_completion)
    }

    /// Resume a paused Array.fromAsync generator function at a particular await point.
    ///
    /// Resumed with a completion that is the result of the await.
    pub fn resume(
        cx: Context,
        capability: Handle<PromiseCapability>,
        await_completion: EvalResult<Handle<Value>>,
        state: &ArrayFromAsyncState,
    ) -> EvalResult<Handle<Value>> {
        let from_async_completion = Self::resume_impl(cx, capability, await_completion, state);
        BuiltinGenerator::handle_async_function_completion(cx, capability, from_async_completion)
    }

    fn start_impl(
        cx: Context,
        capability: Handle<PromiseCapability>,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<BuiltinGeneratorCompletion> {
        let items_arg = get_argument(cx, arguments, 0);
        let mapper_arg = get_argument(cx, arguments, 1);
        let mapper_this_arg = get_argument(cx, arguments, 2);

        let has_mapper = !mapper_arg.is_undefined();
        let mapper = if has_mapper {
            if !is_callable(mapper_arg) {
                return type_error(cx, "Array.fromAsync mapper must be a function");
            }

            Some(mapper_arg.as_object())
        } else {
            None
        };

        let async_iterator = get_method(cx, items_arg, cx.well_known_symbols.async_iterator())?;

        // Async iterator has priority if present
        let iterator = if let Some(async_iterator) = async_iterator {
            let async_iterator_record =
                get_iterator(cx, items_arg, IteratorHint::Sync, Some(async_iterator))?;
            Some(async_iterator_record)
        } else {
            // Otherwise if sync iterator is present convert it to an async iterator
            let sync_iterator = get_method(cx, items_arg, cx.well_known_symbols.iterator())?;
            if let Some(sync_iterator) = sync_iterator {
                let sync_iterator_record =
                    get_iterator(cx, items_arg, IteratorHint::Sync, Some(sync_iterator))?;
                Some(create_async_from_sync_iterator(cx, sync_iterator_record)?)
            } else {
                None
            }
        };

        if let Some(iterator) = iterator {
            // Argument is an iterator
            let array = if is_constructor_value(this_value) {
                construct(cx, this_value.as_object(), &[], None)?
            } else {
                must!(array_create(cx, 0, None)).into()
            };

            // Call the iterator's `next` method the first time and await the result
            let next_result = call(cx, iterator.next_method, iterator.iterator.as_value(), &[])?;

            Self::await_(cx, capability, next_result, || ArrayFromAsyncState::IteratorAwaitValue {
                array: *array,
                iterator: *iterator.iterator,
                next_method: *iterator.next_method,
                index: 0,
                mapper: mapper.map(|f| *f),
                mapper_this_value: *mapper_this_arg,
                is_mapped_await: false,
            })
        } else {
            // Otherwise assume argument is an array-like object
            let array_like = must!(to_object(cx, items_arg));

            let length = length_of_array_like(cx, array_like)?;
            let length_value = cx.number(length);

            let array = if is_constructor_value(this_value) {
                construct(cx, this_value.as_object(), &[length_value], None)?
            } else {
                array_create(cx, length, None)?.into()
            };

            // Array is empty, immediately return and resolve promise
            if length == 0 {
                set(cx, array, cx.names.length(), length_value, true)?;
                return Ok(BuiltinGeneratorCompletion::Returned(array.as_value()));
            }

            // Otherwise get the first value then await it
            let key = PropertyKey::from_u8(0).to_handle(cx);
            let value = get(cx, array_like, key)?;

            Self::await_(cx, capability, value, || ArrayFromAsyncState::ArrayLikeAwaitValue {
                array: *array,
                source_array_like: *array_like,
                index: 0,
                length,
                mapper: mapper.map(|f| *f),
                mapper_this_value: *mapper_this_arg,
                is_mapped_await: false,
            })
        }
    }

    pub fn resume_impl(
        cx: Context,
        capability: Handle<PromiseCapability>,
        await_completion: EvalResult<Handle<Value>>,
        state: &ArrayFromAsyncState,
    ) -> EvalResult<BuiltinGeneratorCompletion> {
        match state {
            ArrayFromAsyncState::IteratorAwaitValue {
                array,
                iterator,
                next_method,
                index,
                mapper,
                mapper_this_value,
                is_mapped_await,
            } => {
                let array = array.to_handle();
                let iterator = iterator.to_handle();
                let next_method = next_method.to_handle(cx);
                let mapper = mapper.map(|f| f.to_handle());
                let mapper_this_value = mapper_this_value.to_handle(cx);

                Self::resume_after_iterator_await(
                    cx,
                    capability,
                    await_completion,
                    array,
                    iterator,
                    next_method,
                    *index,
                    mapper,
                    mapper_this_value,
                    *is_mapped_await,
                )
            }
            ArrayFromAsyncState::ArrayLikeAwaitValue {
                array,
                source_array_like,
                index,
                length,
                mapper,
                mapper_this_value,
                is_mapped_await,
            } => {
                let array = array.to_handle();
                let source_array_like = source_array_like.to_handle();
                let mapper = mapper.map(|f| f.to_handle());
                let mapper_this_value = mapper_this_value.to_handle(cx);

                Self::resume_after_array_like_await(
                    cx,
                    capability,
                    await_completion,
                    array,
                    source_array_like,
                    *index,
                    *length,
                    mapper,
                    mapper_this_value,
                    *is_mapped_await,
                )
            }
            ArrayFromAsyncState::Reject { error_value } => {
                eval_err!(error_value.to_handle(cx))
            }
        }
    }

    fn resume_after_iterator_await(
        cx: Context,
        capability: Handle<PromiseCapability>,
        await_completion: EvalResult<Handle<Value>>,
        array: Handle<ObjectValue>,
        iterator: Handle<ObjectValue>,
        next_method: Handle<Value>,
        index: u64,
        mapper: Option<Handle<ObjectValue>>,
        mapper_this_value: Handle<Value>,
        is_mapped_await: bool,
    ) -> EvalResult<BuiltinGeneratorCompletion> {
        // Resumed after the iterator's `next` method was called
        let value = if !is_mapped_await {
            let awaited_value = await_completion?;
            if !awaited_value.is_object() {
                return type_error(cx, "Array.fromAsync iterator must return an object");
            }
            let iterator_result = awaited_value.as_object();

            // Reached the end of the iterator, we are done building the array
            if iterator_complete(cx, iterator_result)? {
                let length_value = cx.number(index);
                set(cx, array, cx.names.length(), length_value, true)?;

                return Ok(BuiltinGeneratorCompletion::Returned(array.as_value()));
            }

            let next_value = iterator_value(cx, iterator_result)?;

            // If a mapper is present then call it and await the return value
            if let Some(mapper) = mapper {
                let key_value = cx.number(index);
                let mapped_completion =
                    call_object(cx, mapper, mapper_this_value, &[next_value, key_value]);

                let mapped_value = match completion_value!(mapped_completion) {
                    Ok(value) => value,
                    Err(error_value) => {
                        return Self::async_iterator_close(cx, capability, iterator, error_value);
                    }
                };

                let await_completion = Self::await_(cx, capability, mapped_value, || {
                    ArrayFromAsyncState::IteratorAwaitValue {
                        array: *array,
                        iterator: *iterator,
                        next_method: *next_method,
                        index,
                        mapper: Some(*mapper),
                        mapper_this_value: *mapper_this_value,
                        is_mapped_await: true,
                    }
                });

                return match completion_value!(await_completion) {
                    Ok(generator_completion) => Ok(generator_completion),
                    Err(error_value) => {
                        Self::async_iterator_close(cx, capability, iterator, error_value)
                    }
                };
            }

            next_value
        } else {
            // Resumed after the mapper function was called
            match completion_value!(await_completion) {
                Ok(awaited_mapped_value) => awaited_mapped_value,
                Err(error_value) => {
                    return Self::async_iterator_close(cx, capability, iterator, error_value);
                }
            }
        };

        // Append the (possibly mapped) value to the target array
        let key = PropertyKey::from_u64(cx, index)?.to_handle(cx);
        let create_property_completion = create_data_property_or_throw(cx, array, key, value);

        if let Err(error_value) = completion_value!(create_property_completion) {
            return Self::async_iterator_close(cx, capability, iterator, error_value);
        }

        // Check that next array size is still within safe integer bounds
        let next_index = index + 1;

        if next_index >= MAX_SAFE_INTEGER_U64 {
            let error_value = type_error_value(cx, "Array.fromAsync array is too large")?;
            return Self::async_iterator_close(cx, capability, iterator, error_value);
        }

        // Call the iterator's `next` method again to get the next value, and await the result
        let next_result = call(cx, next_method, iterator.as_value(), &[])?;

        Self::await_(cx, capability, next_result, || ArrayFromAsyncState::IteratorAwaitValue {
            array: *array,
            iterator: *iterator,
            next_method: *next_method,
            index: next_index,
            mapper: mapper.map(|f| *f),
            mapper_this_value: *mapper_this_value,
            is_mapped_await: false,
        })
    }

    fn resume_after_array_like_await(
        cx: Context,
        capability: Handle<PromiseCapability>,
        await_completion: EvalResult<Handle<Value>>,
        array: Handle<ObjectValue>,
        source_array_like: Handle<ObjectValue>,
        index: u64,
        length: u64,
        mapper: Option<Handle<ObjectValue>>,
        mapper_this_value: Handle<Value>,
        is_mapped_await: bool,
    ) -> EvalResult<BuiltinGeneratorCompletion> {
        let awaited_value = await_completion?;

        // If a mapper is present then call it and await the return value
        if !is_mapped_await {
            if let Some(mapper) = mapper {
                let key_value = cx.number(index);
                let mapped_value =
                    call_object(cx, mapper, mapper_this_value, &[awaited_value, key_value])?;

                return Self::await_(cx, capability, mapped_value, || {
                    ArrayFromAsyncState::ArrayLikeAwaitValue {
                        array: *array,
                        source_array_like: *source_array_like,
                        index,
                        length,
                        mapper: Some(*mapper),
                        mapper_this_value: *mapper_this_value,
                        is_mapped_await: true,
                    }
                });
            }
        }

        // Append the (possibly mapped) value from the source array to the target array
        let key = PropertyKey::from_u64(cx, index)?.to_handle(cx);
        create_data_property_or_throw(cx, array, key, awaited_value)?;

        let next_index = index + 1;

        // Reached the end of the source array, we are done building the result array
        if next_index >= length {
            let length_value = cx.number(length);
            set(cx, array, cx.names.length(), length_value, true)?;

            return Ok(BuiltinGeneratorCompletion::Returned(array.as_value()));
        }

        // Otherwise proceed to the next value and await it
        let next_index_key = PropertyKey::from_u64(cx, next_index)?.to_handle(cx);
        let next_value = get(cx, source_array_like, next_index_key)?;

        Self::await_(cx, capability, next_value, || ArrayFromAsyncState::ArrayLikeAwaitValue {
            array: *array,
            source_array_like: *source_array_like,
            index: next_index,
            length,
            mapper: mapper.map(|f| *f),
            mapper_this_value: *mapper_this_value,
            is_mapped_await: false,
        })
    }

    /// Perform an await on the provided value within Array.fromAsync.
    ///
    /// The paused generator state is created via a callback.
    #[inline]
    fn await_(
        cx: Context,
        capability: Handle<PromiseCapability>,
        value: Handle<Value>,
        create_state_fn: impl FnOnce() -> ArrayFromAsyncState,
    ) -> EvalResult<BuiltinGeneratorCompletion> {
        let mut value_promise = coerce_to_ordinary_promise(cx, value)?;

        let generator = BuiltinGenerator::new(cx, || BuiltinGeneratorState::ArrayFromAsync {
            capability: *capability,
            state: create_state_fn(),
        })?;

        let any_generator = generator.cast::<AnyHeapItem>();
        value_promise.add_await_reaction(cx, any_generator)?;

        Ok(BuiltinGeneratorCompletion::Suspended)
    }

    /// Specialized AsyncIteratorClose when called in Array.fromAsync on a thrown error.
    fn async_iterator_close(
        cx: Context,
        capability: Handle<PromiseCapability>,
        iterator: Handle<ObjectValue>,
        error_value: Handle<Value>,
    ) -> EvalResult<BuiltinGeneratorCompletion> {
        let iterator_value = iterator.as_value();
        let get_return_completion = get_method(cx, iterator_value, cx.names.return_());
        if let Ok(Some(return_method)) = get_return_completion {
            let call_completion = call_object(cx, return_method, iterator_value, &[]);
            if let Ok(call_result) = call_completion {
                Self::await_(cx, capability, call_result, || ArrayFromAsyncState::Reject {
                    error_value: *error_value,
                })
            } else {
                eval_err!(error_value)
            }
        } else {
            eval_err!(error_value)
        }
    }
}

impl ArrayFromAsyncState {
    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        match self {
            ArrayFromAsyncState::IteratorAwaitValue {
                array,
                iterator,
                next_method,
                index: _,
                mapper,
                mapper_this_value,
                is_mapped_await: _,
            } => {
                visitor.visit_pointer(array);
                visitor.visit_pointer(iterator);
                visitor.visit_value(next_method);
                visitor.visit_pointer_opt(mapper);
                visitor.visit_value(mapper_this_value);
            }
            ArrayFromAsyncState::ArrayLikeAwaitValue {
                array,
                source_array_like,
                index: _,
                length: _,
                mapper,
                mapper_this_value,
                is_mapped_await: _,
            } => {
                visitor.visit_pointer(array);
                visitor.visit_pointer(source_array_like);
                visitor.visit_pointer_opt(mapper);
                visitor.visit_value(mapper_this_value);
            }
            ArrayFromAsyncState::Reject { error_value } => {
                visitor.visit_value(error_value);
            }
        }
    }
}
