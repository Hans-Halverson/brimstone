use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        abstract_operations::call_object,
        gc::{HeapItem, HeapVisitor},
        generator_object::GeneratorState,
        heap_item_descriptor::HeapItemKind,
        iterator::{
            create_iter_result_object, get_iterator_flattenable, iterator_close, iterator_step,
            iterator_step_value, Iterator,
        },
        object_value::ObjectValue,
        ordinary_object::object_create_with_proto,
        type_utilities::to_boolean,
        Context, EvalResult, Handle, HeapPtr, Value,
    },
    set_uninit,
};

use super::intrinsics::Intrinsic;

// Iterator Helper Objects (https://tc39.es/ecma262/#sec-iterator-helper-objects)
extend_object! {
    pub struct IteratorHelperObject {
        // The iterator object part of [[UnderlyingIterator]]
        iterator: HeapPtr<ObjectValue>,
        // The next method part of [[UnderlyingIterator]]
        next_method: Value,
        // The done part of [[UnderlyingIterator]]
        is_done: bool,
        // The current state of the iterator, when treated as a generator.
        generator_state: GeneratorState,
        // The type and captured values of the iterator helpe.
        state: IteratorHelperState,
    }
}

enum IteratorHelperState {
    /// Iterator helper for the drop method. Contains the number of values to drop, which is either
    /// positive infinity or a non-negative integer.
    Drop(f64),
    /// Iterator helper for the take method. Contains the number of remaining values to drop, which
    /// is either positive infinity or a non-negative integer.
    Take(f64),
    /// Iterator helper for the filter method. Contains the predicate function and a counter.
    Filter { predicate: HeapPtr<ObjectValue>, counter: u64 },
    /// Iterator helper for the map method. Contains the mapper function and a counter.
    Map { mapper: HeapPtr<ObjectValue>, counter: u64 },
    /// Iterator helper for the flatMap method. Contains the mapper function, a counter, and the
    /// (iterator object, next method) pair of the inner iterator. Inner iterator is None before
    /// the first iteration, and Some after that.
    FlatMap {
        mapper: HeapPtr<ObjectValue>,
        inner_iterator: Option<(HeapPtr<ObjectValue>, Value)>,
        counter: u64,
    },
}

impl IteratorHelperObject {
    /// Creates and initializes a new IteratorHelperObject. Does not set the iterator helper's
    /// state, which must be done by the caller.
    fn new(cx: Context, iterator: &Iterator) -> Handle<IteratorHelperObject> {
        let prototype = cx.get_intrinsic(Intrinsic::IteratorHelperPrototype);
        let mut object = object_create_with_proto::<IteratorHelperObject>(
            cx,
            HeapItemKind::IteratorHelperObject,
            prototype,
        );

        set_uninit!(object.iterator, *iterator.iterator);
        set_uninit!(object.next_method, *iterator.next_method);
        set_uninit!(object.is_done, iterator.is_done);
        set_uninit!(object.generator_state, GeneratorState::SuspendedStart);

        object.to_handle()
    }

    pub fn new_drop(cx: Context, iterator: &Iterator, limit: f64) -> Handle<IteratorHelperObject> {
        let mut object = Self::new(cx, iterator);
        set_uninit!(object.state, IteratorHelperState::Drop(limit));
        object
    }

    pub fn new_take(cx: Context, iterator: &Iterator, limit: f64) -> Handle<IteratorHelperObject> {
        let mut object = Self::new(cx, iterator);
        set_uninit!(object.state, IteratorHelperState::Take(limit));
        object
    }

    pub fn new_filter(
        cx: Context,
        iterator: &Iterator,
        predicate: Handle<ObjectValue>,
    ) -> Handle<IteratorHelperObject> {
        let mut object = Self::new(cx, iterator);
        set_uninit!(
            object.state,
            IteratorHelperState::Filter { predicate: *predicate, counter: 0 }
        );
        object
    }

    pub fn new_map(
        cx: Context,
        iterator: &Iterator,
        mapper: Handle<ObjectValue>,
    ) -> Handle<IteratorHelperObject> {
        let mut object = Self::new(cx, iterator);
        set_uninit!(object.state, IteratorHelperState::Map { mapper: *mapper, counter: 0 });
        object
    }

    pub fn new_flat_map(
        cx: Context,
        iterator: &Iterator,
        mapper: Handle<ObjectValue>,
    ) -> Handle<IteratorHelperObject> {
        let mut object = Self::new(cx, iterator);
        set_uninit!(
            object.state,
            IteratorHelperState::FlatMap { mapper: *mapper, inner_iterator: None, counter: 0 }
        );
        object
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

    fn state_mut(&mut self) -> &mut IteratorHelperState {
        &mut self.state
    }

    fn iterator(&self, cx: Context) -> Iterator {
        Iterator {
            iterator: self.iterator_object(),
            next_method: self.next_method.to_handle(cx),
            is_done: self.is_done,
        }
    }

    pub fn iterator_object(&self) -> Handle<ObjectValue> {
        self.iterator.to_handle()
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
            IteratorHelperState::Filter { .. } => self.next_filter(cx, is_start),
            IteratorHelperState::Map { .. } => self.next_map(cx, is_start),
            IteratorHelperState::FlatMap { .. } => self.next_flat_map(cx),
        }
    }

    /// Returns either the return value or an error.
    pub fn return_(&mut self, cx: Context) -> EvalResult<Handle<Value>> {
        if let IteratorHelperState::FlatMap { inner_iterator, .. } = self.state() {
            // Handle more complex return logic within flatMap. Implement logic directly following
            // the yield when yielding an abnormal completion.
            //
            // First close the inner iterator, then close the outer iterator.
            let inner_iterator = inner_iterator.as_ref().unwrap().0.to_handle();
            let backup_completion = iterator_close(cx, inner_iterator, Ok(cx.undefined()));
            match backup_completion {
                Err(error) => iterator_close(cx, self.iterator_object(), Err(error)),
                Ok(_) => iterator_close(cx, self.iterator_object(), Ok(cx.undefined())),
            }
        } else {
            iterator_close(cx, self.iterator_object(), Ok(cx.undefined()))
        }
    }

    fn iterator_step(
        &mut self,
        cx: Context,
        iterator: &mut Iterator,
    ) -> EvalResult<Option<Handle<ObjectValue>>> {
        let result = iterator_step(cx, iterator);
        self.is_done = iterator.is_done;
        result
    }

    fn iterator_step_value(
        &mut self,
        cx: Context,
        iterator: &mut Iterator,
    ) -> EvalResult<Option<Handle<Value>>> {
        let result = iterator_step_value(cx, iterator);
        self.is_done = iterator.is_done;
        result
    }

    fn next_drop(
        &mut self,
        cx: Context,
        is_start: bool,
    ) -> EvalResult<Option<Handle<ObjectValue>>> {
        let limit = if let IteratorHelperState::Drop(limit) = self.state() {
            *limit
        } else {
            unreachable!()
        };

        let mut iterator = self.iterator(cx);

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
        Ok(value_opt.map(|value| create_iter_result_object(cx, value, false).as_object()))
    }

    fn next_take(&mut self, cx: Context) -> EvalResult<Option<Handle<ObjectValue>>> {
        let remaining = if let IteratorHelperState::Take(remaining) = self.state_mut() {
            remaining
        } else {
            unreachable!()
        };

        // Close the iterator if we have taken all the values
        if *remaining == 0.0 {
            iterator_close(cx, self.iterator_object(), Ok(cx.undefined()))?;
            return Ok(None);
        }

        // Otherwise decrement a non-inifinite remaining value
        if *remaining != f64::INFINITY {
            *remaining -= 1.0;
        }

        let mut iterator = self.iterator(cx);
        let value_opt = self.iterator_step_value(cx, &mut iterator)?;
        Ok(value_opt.map(|value| create_iter_result_object(cx, value, false).as_object()))
    }

    fn next_filter(
        &mut self,
        cx: Context,
        is_start: bool,
    ) -> EvalResult<Option<Handle<ObjectValue>>> {
        let predicate = if let IteratorHelperState::Filter { predicate, counter } = self.state_mut()
        {
            // Counter is initialized to 0 on start, increment it after the first iteration
            if !is_start {
                *counter += 1;
            }

            predicate.to_handle()
        } else {
            unreachable!()
        };

        let mut iterator = self.iterator(cx);
        let mut counter_value: Handle<Value> = Handle::empty(cx);

        // Keep looping until we find a value fron the underlying iterator which passes the
        // predicate function.
        loop {
            // Get the next value from the underlying iterator
            let value = match self.iterator_step_value(cx, &mut iterator)? {
                None => return Ok(None),
                Some(value) => value,
            };

            // Convert the counter to a value
            if let IteratorHelperState::Filter { counter, .. } = self.state() {
                counter_value.replace(Value::from(*counter));
            } else {
                unreachable!()
            };

            // Run the predicate function on the value, closing the iterator on error
            let is_selected =
                match call_object(cx, predicate, cx.undefined(), &[value, counter_value]) {
                    Err(error) => {
                        iterator_close(cx, self.iterator_object(), Err(error))?;
                        return Ok(None);
                    }
                    Ok(is_selected) => to_boolean(*is_selected),
                };

            // Return the value as an iterator result if the predicate returns true
            if is_selected {
                return Ok(Some(create_iter_result_object(cx, value, false).as_object()));
            }

            // Increment the counter for the next iteration
            if let IteratorHelperState::Filter { counter, .. } = self.state_mut() {
                *counter += 1;
            } else {
                unreachable!()
            };
        }
    }

    fn next_map(&mut self, cx: Context, is_start: bool) -> EvalResult<Option<Handle<ObjectValue>>> {
        let (mapper, counter) =
            if let IteratorHelperState::Map { mapper, counter } = self.state_mut() {
                (mapper.to_handle(), counter)
            } else {
                unreachable!()
            };

        // Counter is initialized to 0 on start, increment it after the first iteration
        if !is_start {
            *counter += 1;
        }

        let counter_value = Value::from(*counter).to_handle(cx);

        // Get the next value from the underlying iterator
        let value = match self.iterator_step_value(cx, &mut self.iterator(cx))? {
            None => return Ok(None),
            Some(value) => value,
        };

        // Run the mapper function, returning the result and closing the iterator on error
        match call_object(cx, mapper, cx.undefined(), &[value, counter_value]) {
            Err(error) => {
                iterator_close(cx, self.iterator_object(), Err(error))?;
                Ok(None)
            }
            Ok(value) => Ok(Some(create_iter_result_object(cx, value, false).as_object())),
        }
    }

    fn next_flat_map(&mut self, cx: Context) -> EvalResult<Option<Handle<ObjectValue>>> {
        let mut inner_iterator_opt: Option<Iterator> = None;

        let mapper =
            if let IteratorHelperState::FlatMap { mapper, inner_iterator, .. } = self.state_mut() {
                // Set the initial inner iterator, if one exists
                if let Some((iterator, next_method)) = inner_iterator {
                    inner_iterator_opt = Some(Iterator {
                        iterator: iterator.to_handle(),
                        next_method: next_method.to_handle(cx),
                        is_done: false,
                    });
                }

                mapper.to_handle()
            } else {
                unreachable!()
            };

        let mut iterator = self.iterator(cx);
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
                if let IteratorHelperState::FlatMap { counter, .. } = self.state() {
                    counter_value.replace(Value::from(*counter));
                } else {
                    unreachable!()
                };

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

                        if let IteratorHelperState::FlatMap { inner_iterator, .. } =
                            self.state_mut()
                        {
                            *inner_iterator = Some((*iterator, *next_method));
                        } else {
                            unreachable!()
                        };
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
                    return Ok(Some(create_iter_result_object(cx, value, false).as_object()))
                }
            }

            // Increment the counter for the next iteration of the outer loop
            if let IteratorHelperState::FlatMap { counter, .. } = self.state_mut() {
                *counter += 1;
            } else {
                unreachable!()
            };
        }
    }
}

impl HeapItem for HeapPtr<IteratorHelperObject> {
    fn byte_size(&self) -> usize {
        size_of::<IteratorHelperObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.iterator);
        visitor.visit_value(&mut self.next_method);

        match &mut self.state {
            IteratorHelperState::Filter { predicate, .. } => visitor.visit_pointer(predicate),
            IteratorHelperState::Map { mapper, .. } => visitor.visit_pointer(mapper),
            IteratorHelperState::FlatMap { mapper, inner_iterator, .. } => {
                visitor.visit_pointer(mapper);

                if let Some((iterator_object, next_method)) = inner_iterator {
                    visitor.visit_pointer(iterator_object);
                    visitor.visit_value(next_method);
                }
            }
            IteratorHelperState::Drop(_) | IteratorHelperState::Take(_) => {}
        }
    }
}
