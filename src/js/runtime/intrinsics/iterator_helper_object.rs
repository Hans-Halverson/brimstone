use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        abstract_operations::call_object,
        gc::{HeapObject, HeapVisitor},
        generator_object::GeneratorState,
        iterator::{
            create_iter_result_object, iterator_close, iterator_step, iterator_step_value, Iterator,
        },
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_with_proto,
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
    /// Iterator helper for the map method. Contains the mapper function and a counter.
    Map { mapper: HeapPtr<ObjectValue>, counter: u64 },
}

impl IteratorHelperObject {
    /// Creates and initializes a new IteratorHelperObject. Does not set the iterator helper's
    /// state, which must be done by the caller.
    fn new(cx: Context, iterator: &Iterator) -> Handle<IteratorHelperObject> {
        let prototype = cx.get_intrinsic(Intrinsic::IteratorHelperPrototype);
        let mut object = object_create_with_proto::<IteratorHelperObject>(
            cx,
            ObjectKind::IteratorHelperObject,
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

    pub fn new_map(
        cx: Context,
        iterator: &Iterator,
        mapper: Handle<ObjectValue>,
    ) -> Handle<IteratorHelperObject> {
        let mut object = Self::new(cx, iterator);
        set_uninit!(object.state, IteratorHelperState::Map { mapper: *mapper, counter: 0 });
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
            IteratorHelperState::Map { .. } => self.next_map(cx, is_start),
        }
    }

    /// Returns either the return value or an error.
    pub fn return_(&mut self, cx: Context) -> EvalResult<Handle<Value>> {
        iterator_close(cx, self.iterator_object(), Ok(cx.undefined()))
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
}

impl HeapObject for HeapPtr<IteratorHelperObject> {
    fn byte_size(&self) -> usize {
        size_of::<IteratorHelperObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.iterator);
        visitor.visit_value(&mut self.next_method);

        match &mut self.state {
            IteratorHelperState::Map { mapper, .. } => visitor.visit_pointer(mapper),
            IteratorHelperState::Drop(_) | IteratorHelperState::Take(_) => {}
        }
    }
}
