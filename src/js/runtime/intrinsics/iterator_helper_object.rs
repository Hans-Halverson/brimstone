use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        Context, EvalResult, Handle, HeapPtr, Value,
        abstract_operations::{call, call_object},
        alloc_error::AllocResult,
        collections::array::ValueArray,
        error::type_error,
        gc::{HeapItem, HeapVisitor},
        generator_object::GeneratorState,
        intrinsics::intrinsics::Intrinsic,
        iterator::{
            HeapIterator, Iterator, create_iter_result_object, get_iterator_direct,
            get_iterator_flattenable, iterator_close, iterator_step, iterator_step_value,
        },
        object_value::ObjectValue,
        ordinary_object::ObjectBuilder,
        type_utilities::to_boolean,
    },
    set_uninit,
};

extend_object! {
    /// Iterator Helper Objects (https://tc39.es/ecma262/#sec-iterator-helper-objects)
    pub struct IteratorHelperObject {
        /// The [[UnderlyingIterator]] internal slot. This is always non-empty except for in
        /// `concat` helpers before `next` has been called the first time.
        iterator: Option<HeapIterator>,
        /// The current state of the iterator, when treated as a generator.
        generator_state: GeneratorState,
        /// The type and captured values of the iterator helper.
        state: IteratorHelperState,
    }
}

enum IteratorHelperState {
    Drop(DropHelper),
    Take(TakeHelper),
    Filter(FilterHelper),
    Map(MapHelper),
    FlatMap(FlatMapHelper),
    Concat(ConcatHelper),
}

/// Iterator helper for the drop method. Contains the number of values to drop, which is either
/// positive infinity or a non-negative integer.
struct DropHelper {
    limit: f64,
}

/// Iterator helper for the take method. Contains the number of remaining values to take, which is
/// either positive infinity or a non-negative integer.
struct TakeHelper {
    remaining: f64,
}

/// Iterator helper for the filter method. Contains the predicate function and a counter.
struct FilterHelper {
    predicate: HeapPtr<ObjectValue>,
    counter: u64,
}

/// Iterator helper for the map method. Contains the mapper function and a counter.
struct MapHelper {
    mapper: HeapPtr<ObjectValue>,
    counter: u64,
}

/// Iterator helper for the flatMap method. Contains the mapper function, a counter, and the
/// (iterator object, next method) pair of the inner iterator. Inner iterator is None before
/// the first iteration, and Some after that.
struct FlatMapHelper {
    mapper: HeapPtr<ObjectValue>,
    inner_iterator: Option<(HeapPtr<ObjectValue>, Value)>,
    counter: u64,
}

/// Iterator helper for the `Iterator.concat` method. Contains an array of iterable objects as well
/// as an array of their corresponding iterator methods. Arrays must be the same size.
struct ConcatHelper {
    iterables: HeapPtr<ValueArray>,
    iterator_methods: HeapPtr<ValueArray>,
    /// Index of the next iterable/iterator to iterate over.
    next_index: u64,
}

impl IteratorHelperObject {
    /// Creates and initializes a new IteratorHelperObject. Does not set the iterator helper's
    /// state, which must be done by the caller.
    fn new(cx: Context, iterator: Option<&Iterator>) -> AllocResult<Handle<IteratorHelperObject>> {
        let prototype = cx.get_intrinsic(Intrinsic::IteratorHelperPrototype);
        let mut object = ObjectBuilder::<IteratorHelperObject>::new(cx)
            .proto(prototype)
            .build()?;

        set_uninit!(object.iterator, iterator.map(Iterator::to_heap));
        set_uninit!(object.generator_state, GeneratorState::SuspendedStart);

        Ok(object.to_handle())
    }

    pub fn new_drop(
        cx: Context,
        iterator: &Iterator,
        limit: f64,
    ) -> AllocResult<Handle<IteratorHelperObject>> {
        let mut object = Self::new(cx, Some(iterator))?;
        set_uninit!(object.state, IteratorHelperState::Drop(DropHelper { limit }));
        Ok(object)
    }

    pub fn new_take(
        cx: Context,
        iterator: &Iterator,
        limit: f64,
    ) -> AllocResult<Handle<IteratorHelperObject>> {
        let mut object = Self::new(cx, Some(iterator))?;
        set_uninit!(object.state, IteratorHelperState::Take(TakeHelper { remaining: limit }));
        Ok(object)
    }

    pub fn new_filter(
        cx: Context,
        iterator: &Iterator,
        predicate: Handle<ObjectValue>,
    ) -> AllocResult<Handle<IteratorHelperObject>> {
        let mut object = Self::new(cx, Some(iterator))?;
        set_uninit!(
            object.state,
            IteratorHelperState::Filter(FilterHelper { predicate: *predicate, counter: 0 })
        );
        Ok(object)
    }

    pub fn new_map(
        cx: Context,
        iterator: &Iterator,
        mapper: Handle<ObjectValue>,
    ) -> AllocResult<Handle<IteratorHelperObject>> {
        let mut object = Self::new(cx, Some(iterator))?;
        set_uninit!(
            object.state,
            IteratorHelperState::Map(MapHelper { mapper: *mapper, counter: 0 })
        );
        Ok(object)
    }

    pub fn new_flat_map(
        cx: Context,
        iterator: &Iterator,
        mapper: Handle<ObjectValue>,
    ) -> AllocResult<Handle<IteratorHelperObject>> {
        let mut object = Self::new(cx, Some(iterator))?;
        set_uninit!(
            object.state,
            IteratorHelperState::FlatMap(FlatMapHelper {
                mapper: *mapper,
                inner_iterator: None,
                counter: 0,
            })
        );
        Ok(object)
    }

    pub fn new_concat(
        cx: Context,
        iterables: Handle<ValueArray>,
        iterator_methods: Handle<ValueArray>,
    ) -> AllocResult<Handle<IteratorHelperObject>> {
        let mut object = Self::new(cx, None)?;
        set_uninit!(
            object.state,
            IteratorHelperState::Concat(ConcatHelper {
                iterables: *iterables,
                iterator_methods: *iterator_methods,
                next_index: 0,
            })
        );
        Ok(object)
    }

    pub fn generator_state(&self) -> GeneratorState {
        self.generator_state
    }

    pub fn set_generator_state(&mut self, state: GeneratorState) {
        self.generator_state = state;
    }

    fn state(&self) -> &IteratorHelperState {
        &self.state
    }

    fn as_drop_helper(&self) -> Option<&DropHelper> {
        if let IteratorHelperState::Drop(helper) = &self.state {
            Some(helper)
        } else {
            None
        }
    }

    fn as_take_helper_mut(&mut self) -> Option<&mut TakeHelper> {
        if let IteratorHelperState::Take(helper) = &mut self.state {
            Some(helper)
        } else {
            None
        }
    }

    fn as_filter_helper(&self) -> Option<&FilterHelper> {
        if let IteratorHelperState::Filter(helper) = &self.state {
            Some(helper)
        } else {
            None
        }
    }

    fn as_filter_helper_mut(&mut self) -> Option<&mut FilterHelper> {
        if let IteratorHelperState::Filter(helper) = &mut self.state {
            Some(helper)
        } else {
            None
        }
    }

    fn as_map_helper_mut(&mut self) -> Option<&mut MapHelper> {
        if let IteratorHelperState::Map(helper) = &mut self.state {
            Some(helper)
        } else {
            None
        }
    }

    fn as_flat_map_helper(&self) -> Option<&FlatMapHelper> {
        if let IteratorHelperState::FlatMap(helper) = &self.state {
            Some(helper)
        } else {
            None
        }
    }

    fn as_flat_map_helper_mut(&mut self) -> Option<&mut FlatMapHelper> {
        if let IteratorHelperState::FlatMap(helper) = &mut self.state {
            Some(helper)
        } else {
            None
        }
    }

    fn as_concat_helper(&self) -> Option<&ConcatHelper> {
        if let IteratorHelperState::Concat(helper) = &self.state {
            Some(helper)
        } else {
            None
        }
    }

    fn as_concat_helper_mut(&mut self) -> Option<&mut ConcatHelper> {
        if let IteratorHelperState::Concat(helper) = &mut self.state {
            Some(helper)
        } else {
            None
        }
    }

    /// The underlying iterator record for this helper.
    ///
    /// Only None for the `concat` helper.
    fn iterator(&self, cx: Context) -> Option<Iterator> {
        self.iterator.as_ref().map(|it| Iterator::from_heap(cx, it))
    }

    /// The underlying iterator object for this helper.
    ///
    /// Only None for the `concat` helper.
    pub fn iterator_object(&self) -> Option<Handle<ObjectValue>> {
        self.iterator.as_ref().map(|it| it.iterator.to_handle())
    }

    fn set_iterator(&mut self, iterator: Iterator) {
        self.iterator = Some(iterator.to_heap());
    }
}

impl Handle<IteratorHelperObject> {
    /// Execute the next() method, either starting from the GeneratedStart state or from the
    /// GeneratedYield state. In other words, starting from either the start of the generator
    /// closure or from after a yield point.
    ///
    /// Return None if the generator is done, or Some of the next iterator result object.
    pub fn next(&mut self, cx: Context, is_start: bool) -> EvalResult<Option<Handle<ObjectValue>>> {
        match self.state() {
            IteratorHelperState::Drop(_) => self.next_drop(cx, is_start),
            IteratorHelperState::Take(_) => self.next_take(cx),
            IteratorHelperState::Filter(_) => self.next_filter(cx, is_start),
            IteratorHelperState::Map(_) => self.next_map(cx, is_start),
            IteratorHelperState::FlatMap(_) => self.next_flat_map(cx),
            IteratorHelperState::Concat(_) => self.next_concat(cx),
        }
    }

    /// Returns either the return value or an error.
    pub fn return_(&mut self, cx: Context) -> EvalResult<Handle<Value>> {
        match self.state() {
            // Handle more complex return logic within flatMap. Implement logic directly following
            // the yield when yielding an abnormal completion.
            //
            // First close the inner iterator, then close the outer iterator.
            IteratorHelperState::FlatMap(helper) => {
                let inner_iterator = helper.inner_iterator.as_ref().unwrap().0.to_handle();
                let backup_completion = iterator_close(cx, inner_iterator, Ok(cx.undefined()));
                let iterator_object = self.iterator_object().unwrap();

                match backup_completion {
                    Err(error) => iterator_close(cx, iterator_object, Err(error)),
                    Ok(_) => iterator_close(cx, iterator_object, Ok(cx.undefined())),
                }
            }
            // Concat may not have an underlying iterator to close
            IteratorHelperState::Concat(_) => {
                if let Some(iterator_object) = self.iterator_object() {
                    iterator_close(cx, iterator_object, Ok(cx.undefined()))
                } else {
                    Ok(cx.undefined())
                }
            }
            // All other iterators have a single underlying iterator to close
            _ => iterator_close(cx, self.iterator_object().unwrap(), Ok(cx.undefined())),
        }
    }

    fn iterator_step(
        &mut self,
        cx: Context,
        iterator: &mut Iterator,
    ) -> EvalResult<Option<Handle<ObjectValue>>> {
        let result = iterator_step(cx, iterator);

        if let Some(heap_iterator) = self.iterator.as_mut() {
            heap_iterator.is_done = iterator.is_done;
        }

        result
    }

    fn iterator_step_value(
        &mut self,
        cx: Context,
        iterator: &mut Iterator,
    ) -> EvalResult<Option<Handle<Value>>> {
        let result = iterator_step_value(cx, iterator);

        if let Some(heap_iterator) = self.iterator.as_mut() {
            heap_iterator.is_done = iterator.is_done;
        }

        result
    }

    fn next_drop(
        &mut self,
        cx: Context,
        is_start: bool,
    ) -> EvalResult<Option<Handle<ObjectValue>>> {
        let limit = self.as_drop_helper().unwrap().limit;

        let mut iterator = self.iterator(cx).unwrap();

        // On first run step over the first `limit` number of values
        if is_start {
            // If infinite then loop forever
            if limit.is_infinite() {
                loop {
                    let step_result = self.iterator_step(cx, &mut iterator)?;
                    if step_result.is_none() {
                        return Ok(None);
                    }
                }
            }

            // Otherwise finite number to drop
            for _ in 0..(limit as u64) {
                let step_result = self.iterator_step(cx, &mut iterator)?;
                if step_result.is_none() {
                    return Ok(None);
                }
            }
        }

        let value_opt = self.iterator_step_value(cx, &mut iterator)?;
        value_opt
            .map(|value| Ok(create_iter_result_object(cx, value, false)?.as_object()))
            .transpose()
    }

    fn next_take(&mut self, cx: Context) -> EvalResult<Option<Handle<ObjectValue>>> {
        let remaining = &mut self.as_take_helper_mut().unwrap().remaining;

        // Close the iterator if we have taken all the values
        if *remaining == 0.0 {
            iterator_close(cx, self.iterator_object().unwrap(), Ok(cx.undefined()))?;
            return Ok(None);
        }

        // Otherwise decrement a non-inifinite remaining value
        if *remaining != f64::INFINITY {
            *remaining -= 1.0;
        }

        let mut iterator = self.iterator(cx).unwrap();
        let value_opt = self.iterator_step_value(cx, &mut iterator)?;
        value_opt
            .map(|value| Ok(create_iter_result_object(cx, value, false)?.as_object()))
            .transpose()
    }

    fn next_filter(
        &mut self,
        cx: Context,
        is_start: bool,
    ) -> EvalResult<Option<Handle<ObjectValue>>> {
        let predicate = {
            let helper = self.as_filter_helper_mut().unwrap();

            // Counter is initialized to 0 on start, increment it after the first iteration
            if !is_start {
                helper.counter += 1;
            }

            helper.predicate.to_handle()
        };

        let mut iterator = self.iterator(cx).unwrap();
        let mut counter_value: Handle<Value> = Handle::empty(cx);

        // Keep looping until we find a value from the underlying iterator which passes the
        // predicate function.
        loop {
            // Get the next value from the underlying iterator
            let value = match self.iterator_step_value(cx, &mut iterator)? {
                None => return Ok(None),
                Some(value) => value,
            };

            // Convert the counter to a value
            let counter_number = self.as_filter_helper().unwrap().counter;
            counter_value.replace(Value::number(counter_number));

            // Run the predicate function on the value, closing the iterator on error
            let is_selected =
                match call_object(cx, predicate, cx.undefined(), &[value, counter_value]) {
                    Err(error) => {
                        iterator_close(cx, self.iterator_object().unwrap(), Err(error))?;
                        return Ok(None);
                    }
                    Ok(is_selected) => to_boolean(*is_selected),
                };

            // Return the value as an iterator result if the predicate returns true
            if is_selected {
                return Ok(Some(create_iter_result_object(cx, value, false)?.as_object()));
            }

            // Increment the counter for the next iteration
            self.as_filter_helper_mut().unwrap().counter += 1;
        }
    }

    fn next_map(&mut self, cx: Context, is_start: bool) -> EvalResult<Option<Handle<ObjectValue>>> {
        let helper = self.as_map_helper_mut().unwrap();
        let mapper = helper.mapper.to_handle();

        // Counter is initialized to 0 on start, increment it after the first iteration
        if !is_start {
            helper.counter += 1;
        }

        let counter_value = cx.number(helper.counter);

        // Get the next value from the underlying iterator
        let value = match self.iterator_step_value(cx, &mut self.iterator(cx).unwrap())? {
            None => return Ok(None),
            Some(value) => value,
        };

        // Run the mapper function, returning the result and closing the iterator on error
        match call_object(cx, mapper, cx.undefined(), &[value, counter_value]) {
            Err(error) => {
                iterator_close(cx, self.iterator_object().unwrap(), Err(error))?;
                Ok(None)
            }
            Ok(value) => Ok(Some(create_iter_result_object(cx, value, false)?.as_object())),
        }
    }

    fn next_flat_map(&mut self, cx: Context) -> EvalResult<Option<Handle<ObjectValue>>> {
        let mut inner_iterator_opt: Option<Iterator> = None;

        let mapper = {
            let helper = self.as_flat_map_helper_mut().unwrap();

            // Set the initial inner iterator, if one exists
            if let Some((iterator, next_method)) = &helper.inner_iterator {
                inner_iterator_opt = Some(Iterator {
                    iterator: iterator.to_handle(),
                    next_method: next_method.to_handle(cx),
                    is_done: false,
                });
            }

            helper.mapper.to_handle()
        };

        let mut iterator = self.iterator(cx).unwrap();
        let mut counter_value: Handle<Value> = Handle::empty(cx);

        // Outer loop iterates through the values of the underlying iterator
        loop {
            // Use the existing inner iterator if one exists (aka has already been started and is
            // not done).
            if inner_iterator_opt.is_none() {
                // Get the next value from the underlying iterator
                let value = match self.iterator_step_value(cx, &mut iterator)? {
                    None => return Ok(None),
                    Some(value) => value,
                };

                // Convert the counter to a value
                counter_value.replace(Value::number(self.as_flat_map_helper().unwrap().counter));

                // Run the mapper function on the value, closing the iterator on error
                let mapped = match call_object(cx, mapper, cx.undefined(), &[value, counter_value])
                {
                    Err(error) => {
                        iterator_close(cx, iterator.iterator, Err(error))?;
                        return Ok(None);
                    }
                    Ok(mapped) => mapped,
                };

                // Get the inner iterator from the mapped value
                match get_iterator_flattenable(cx, mapped, /* reject_primitives */ true) {
                    Err(error) => {
                        iterator_close(cx, iterator.iterator, Err(error))?;
                        return Ok(None);
                    }
                    Ok(inner_iterator_result) => {
                        // Save as the current inner iterator, both locally in this function and in
                        // the iterator helper state.
                        let iterator = inner_iterator_result.iterator;
                        let next_method = inner_iterator_result.next_method;
                        inner_iterator_opt = Some(inner_iterator_result);

                        self.as_flat_map_helper_mut().unwrap().inner_iterator =
                            Some((*iterator, *next_method));
                    }
                }
            }

            // Return next value from the inner iterator.
            match self.iterator_step_value(cx, inner_iterator_opt.as_mut().unwrap()) {
                // Error if the inner iterator throws
                Err(error) => {
                    iterator_close(cx, iterator.iterator, Err(error))?;
                    return Ok(None);
                }
                // If the inner iterator has reached the end then break out of the inner loop
                // and continue to the next iteration of the outer loop.
                Ok(None) => {
                    inner_iterator_opt = None;
                }
                // If the inner iterator returns a value then return it as an iterator result
                Ok(Some(value)) => {
                    return Ok(Some(create_iter_result_object(cx, value, false)?.as_object()));
                }
            }

            // Increment the counter for the next iteration of the outer loop
            self.as_flat_map_helper_mut().unwrap().counter += 1;
        }
    }

    fn next_concat(&mut self, cx: Context) -> EvalResult<Option<Handle<ObjectValue>>> {
        let (iterables, iterator_methods) = {
            let helper = self.as_concat_helper().unwrap();
            (helper.iterables.to_handle(), helper.iterator_methods.to_handle())
        };

        loop {
            // Proceed to the next iterator when we have exhausted the current one, or if this is
            // the first call to `next` which must set up the initial iterator.
            let use_next_iterable = match self.iterator(cx) {
                None => true,
                Some(iterator) => iterator.is_done,
            };

            if use_next_iterable {
                let next_index = self.as_concat_helper().unwrap().next_index as usize;

                // Concat helper is complete since we have exhausted all iterables
                if next_index >= iterables.len() {
                    return Ok(None);
                }

                // Call the next iterable's iterator method to get the next iterator object, which
                // is stored as the underlying iterator for the helper.
                let iterable = iterables.as_slice()[next_index].to_handle(cx);
                let next_method = iterator_methods.as_slice()[next_index].to_handle(cx);

                let iterator_object = call(cx, next_method, iterable, &[])?;
                if !iterator_object.is_object() {
                    return type_error(cx, "iterator method did not return an object");
                }

                let iterator = get_iterator_direct(cx, iterator_object.as_object())?;
                self.set_iterator(iterator);

                self.as_concat_helper_mut().unwrap().next_index += 1;
            }

            // Underlying iterator has been initialized so it is guaranteed to exist now
            let mut iterator = self.iterator(cx).unwrap();

            // Return the next value from the iterator, continuing to the next iterable in next loop
            // when the current one is exhausted.
            if let Some(value) = self.iterator_step_value(cx, &mut iterator)? {
                return Ok(Some(create_iter_result_object(cx, value, false)?.as_object()));
            }
        }
    }
}

impl HeapItem for IteratorHelperObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<IteratorHelperObject>()
    }

    fn visit_pointers(mut iterator_helper_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        iterator_helper_object.visit_object_pointers(visitor);

        if let Some(iterator) = &mut iterator_helper_object.iterator {
            iterator.visit_pointers(visitor);
        }

        match &mut iterator_helper_object.state {
            IteratorHelperState::Filter(helper) => visitor.visit_pointer(&mut helper.predicate),
            IteratorHelperState::Map(helper) => visitor.visit_pointer(&mut helper.mapper),
            IteratorHelperState::FlatMap(helper) => {
                visitor.visit_pointer(&mut helper.mapper);

                if let Some((iterator_object, next_method)) = &mut helper.inner_iterator {
                    visitor.visit_pointer(iterator_object);
                    visitor.visit_value(next_method);
                }
            }
            IteratorHelperState::Concat(helper) => {
                visitor.visit_pointer(&mut helper.iterables);
                visitor.visit_pointer(&mut helper.iterator_methods);
            }
            IteratorHelperState::Drop(_) | IteratorHelperState::Take(_) => {}
        }
    }
}
