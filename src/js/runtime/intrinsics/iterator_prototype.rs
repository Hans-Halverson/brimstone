use crate::{
    eval_err, intrinsic_methods, must,
    runtime::{
        Context, EvalResult, Handle, Value,
        abstract_operations::{call_object, setter_that_ignores_prototype_properties},
        alloc_error::AllocResult,
        array_object::create_array_from_list,
        error::{range_error_value, type_error, type_error_value},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic, iterator_helper_object::IteratorHelperObject,
            rust_runtime::RuntimeFunction,
        },
        iterator::{get_iterator_direct, iterator_close, iterator_step_value},
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::{is_callable, to_boolean, to_integer_or_infinity, to_number},
    },
    runtime_fn,
};

/// The %IteratorPrototype% Object (https://tc39.es/ecma262/#sec-%iteratorprototype%-object)
pub struct IteratorPrototype;

impl IteratorPrototype {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::new_object(cx, realm, Intrinsic::ObjectPrototype)?;

        intrinsic_methods!(cx, builder, {
            drop     IteratorPrototype_drop     (1),
            every    IteratorPrototype_every    (1),
            filter   IteratorPrototype_filter   (1),
            find     IteratorPrototype_find     (1),
            flat_map IteratorPrototype_flat_map (1),
            for_each IteratorPrototype_for_each (1),
            map_     IteratorPrototype_map      (1),
            reduce   IteratorPrototype_reduce   (1),
            some     IteratorPrototype_some     (1),
            take     IteratorPrototype_take     (1),
            to_array IteratorPrototype_to_array (0),
        });

        // Iterator.prototype.constructor (https://tc39.es/ecma262/#sec-iterator.prototype.constructor)
        builder.getter_setter(
            cx.names.constructor(),
            RuntimeFunction::IteratorPrototype_get_constructor,
            RuntimeFunction::IteratorPrototype_set_constructor,
        )?;

        // Iterator.prototype [ @@iterator ] (https://tc39.es/ecma262/#sec-iterator.prototype-%symbol.iterator%)
        builder.method(cx.symbols.iterator(), RuntimeFunction::ReturnThis, 0)?;

        // Iterator.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-iterator.prototype-%symbol.tostringtag%)
        builder.getter_setter(
            cx.symbols.to_string_tag(),
            RuntimeFunction::IteratorPrototype_iterator_prototype_get_to_string_tag,
            RuntimeFunction::IteratorPrototype_set_to_string_tag,
        )?;

        builder.build()
    }

    runtime_fn! {
    /// get Iterator.prototype.constructor (https://tc39.es/ecma262/#sec-get-iterator.prototype.constructor)
    fn get_constructor(cx, _, _) {
        Ok(cx.get_intrinsic(Intrinsic::IteratorConstructor).as_value())
    }}

    runtime_fn! {
    /// set Iterator.prototype.constructor (https://tc39.es/ecma262/#sec-set-iterator.prototype.constructor)
    fn set_constructor(cx, this_value, arguments) {
        let value = arguments.get(cx, 0);

        setter_that_ignores_prototype_properties(
            cx,
            this_value,
            cx.get_intrinsic(Intrinsic::IteratorPrototype),
            cx.names.constructor(),
            value,
        )?;

        Ok(cx.undefined())
    }}

    runtime_fn! {
    /// Iterator.prototype.drop (https://tc39.es/ecma262/#sec-iterator.prototype.drop)
    fn drop(cx, this_value, arguments) {
        let this_object = this_object(cx, this_value, "drop")?;

        let limit_arg = arguments.get(cx, 0);
        let integer_limit = validate_limit_argument(cx, this_object, limit_arg, "drop")?;

        // Get the underlying iterator and create a new iterator helper drop object
        let iterated = get_iterator_direct(cx, this_object)?;
        Ok(IteratorHelperObject::new_drop(cx, &iterated, integer_limit)?.as_value())
    }}

    runtime_fn! {
    /// Iterator.prototype.every (https://tc39.es/ecma262/#sec-iterator.prototype.every)
    fn every(cx, this_value, arguments) {
        let this_object = this_object(cx, this_value, "every")?;

        let predicate_arg = arguments.get(cx, 0);
        let predicate =
            validate_callable_argument(cx, this_object, predicate_arg, "every", "predicate")?;

        let mut iterated = get_iterator_direct(cx, this_object)?;
        let mut counter: u64 = 0;
        let mut counter_handle: Handle<Value> = Handle::empty(cx);

        loop {
            // Get the next value from the iterator, returning true if done
            let value = match iterator_step_value(cx, &mut iterated)? {
                None => return Ok(cx.bool(true)),
                Some(value) => value,
            };

            // Pass value and counter to the predicate function
            counter_handle.replace(Value::number(counter));
            let result = call_object(cx, predicate, cx.undefined(), &[value, counter_handle]);

            // Finish iterating if predicate threw or returned false
            match result {
                Err(_) => return iterator_close(cx, iterated.iterator, result),
                Ok(result) if !to_boolean(*result) => {
                    return iterator_close(cx, iterated.iterator, Ok(cx.bool(false)));
                }
                Ok(_) => counter += 1,
            }
        }
    }}

    runtime_fn! {
    /// Iterator.prototype.filter (https://tc39.es/ecma262/#sec-iterator.prototype.filter)
    fn filter(cx, this_value, arguments) {
        let this_object = this_object(cx, this_value, "filter")?;

        let predicate_arg = arguments.get(cx, 0);
        let predicate =
            validate_callable_argument(cx, this_object, predicate_arg, "filter", "predicate")?;

        // Get the underlying iterator and create a new iterator helper map object
        let iterated = get_iterator_direct(cx, this_object)?;
        Ok(IteratorHelperObject::new_filter(cx, &iterated, predicate)?.as_value())
    }}

    runtime_fn! {
    /// Iterator.prototype.find (https://tc39.es/ecma262/#sec-iterator.prototype.find)
    fn find(cx, this_value, arguments) {
        let this_object = this_object(cx, this_value, "find")?;

        let predicate_arg = arguments.get(cx, 0);
        let predicate =
            validate_callable_argument(cx, this_object, predicate_arg, "find", "predicate")?;

        let mut iterated = get_iterator_direct(cx, this_object)?;
        let mut counter: u64 = 0;
        let mut counter_handle: Handle<Value> = Handle::empty(cx);

        loop {
            // Get the next value from the iterator, returning undefined if done
            let value = match iterator_step_value(cx, &mut iterated)? {
                None => return Ok(cx.undefined()),
                Some(value) => value,
            };

            // Pass value and counter to the predicate function
            counter_handle.replace(Value::number(counter));
            let result = call_object(cx, predicate, cx.undefined(), &[value, counter_handle]);

            // Finish iterating if predicate threw or returned true
            match result {
                Err(_) => return iterator_close(cx, iterated.iterator, result),
                Ok(result) if to_boolean(*result) => {
                    return iterator_close(cx, iterated.iterator, Ok(value));
                }
                Ok(_) => counter += 1,
            }
        }
    }}

    runtime_fn! {
    /// Iterator.prototype.flatMap (https://tc39.es/ecma262/#sec-iterator.prototype.flatmap)
    fn flat_map(cx, this_value, arguments) {
        let this_object = this_object(cx, this_value, "flatMap")?;

        let mapper_arg = arguments.get(cx, 0);
        let mapper = validate_callable_argument(cx, this_object, mapper_arg, "flatMap", "mapper")?;

        // Get the underlying iterator and create a new iterator helper map object
        let iterated = get_iterator_direct(cx, this_object)?;
        Ok(IteratorHelperObject::new_flat_map(cx, &iterated, mapper)?.as_value())
    }}

    runtime_fn! {
    /// Iterator.prototype.forEach (https://tc39.es/ecma262/#sec-iterator.prototype.foreach)
    fn for_each(cx, this_value, arguments) {
        let this_object = this_object(cx, this_value, "forEach")?;

        let callback_arg = arguments.get(cx, 0);
        let callback =
            validate_callable_argument(cx, this_object, callback_arg, "forEach", "callback")?;

        let mut iterated = get_iterator_direct(cx, this_object)?;
        let mut counter: u64 = 0;
        let mut counter_handle: Handle<Value> = Handle::empty(cx);

        loop {
            // Get the next value from the iterator, returning true if done
            let value = match iterator_step_value(cx, &mut iterated)? {
                None => return Ok(cx.undefined()),
                Some(value) => value,
            };

            // Pass value and counter to the callback function
            counter_handle.replace(Value::number(counter));
            let result = call_object(cx, callback, cx.undefined(), &[value, counter_handle]);

            // Finish iterating if callback threw
            match result {
                Err(_) => return iterator_close(cx, iterated.iterator, result),
                Ok(_) => counter += 1,
            }
        }
    }}

    runtime_fn! {
    /// Iterator.prototype.map (https://tc39.es/ecma262/#sec-iterator.prototype.map)
    fn map(cx, this_value, arguments) {
        let this_object = this_object(cx, this_value, "map")?;

        let mapper_arg = arguments.get(cx, 0);
        let mapper = validate_callable_argument(cx, this_object, mapper_arg, "map", "mapper")?;

        // Get the underlying iterator and create a new iterator helper map object
        let iterated = get_iterator_direct(cx, this_object)?;
        Ok(IteratorHelperObject::new_map(cx, &iterated, mapper)?.as_value())
    }}

    runtime_fn! {
    /// Iterator.prototype.reduce (https://tc39.es/ecma262/#sec-iterator.prototype.reduce)
    fn reduce(cx, this_value, arguments) {
        let this_object = this_object(cx, this_value, "reduce")?;

        let callback_arg = arguments.get(cx, 0);
        let callback =
            validate_callable_argument(cx, this_object, callback_arg, "reduce", "callback")?;

        let mut iterated = get_iterator_direct(cx, this_object)?;

        let mut accumulator;
        let mut counter: u64;

        if arguments.len() < 2 {
            match iterator_step_value(cx, &mut iterated)? {
                None => {
                    return type_error(cx, "Iterator.prototype.reduce called on empty iterator");
                }
                Some(value) => {
                    accumulator = value;
                    counter = 1;
                }
            }
        } else {
            accumulator = arguments.get(cx, 1);
            counter = 0;
        };

        let mut counter_handle: Handle<Value> = Handle::empty(cx);

        loop {
            // Get the next value from the iterator, returning the accumulator if done
            let value = match iterator_step_value(cx, &mut iterated)? {
                None => return Ok(accumulator),
                Some(value) => value,
            };

            // Pass accumulator, value, and counter to the predicate function
            counter_handle.replace(Value::number(counter));
            let result =
                call_object(cx, callback, cx.undefined(), &[accumulator, value, counter_handle]);

            // Finish iterating if callback threw, otherwise update the accumulator
            match result {
                Err(_) => return iterator_close(cx, iterated.iterator, result),
                Ok(value) => {
                    accumulator = value;
                    counter += 1;
                }
            }
        }
    }}

    runtime_fn! {
    /// Iterator.prototype.some (https://tc39.es/ecma262/#sec-iterator.prototype.some)
    fn some(cx, this_value, arguments) {
        let this_object = this_object(cx, this_value, "some")?;

        let predicate_arg = arguments.get(cx, 0);
        let predicate =
            validate_callable_argument(cx, this_object, predicate_arg, "some", "predicate")?;

        let mut iterated = get_iterator_direct(cx, this_object)?;
        let mut counter: u64 = 0;
        let mut counter_handle: Handle<Value> = Handle::empty(cx);

        loop {
            // Get the next value from the iterator, returning false if done
            let value = match iterator_step_value(cx, &mut iterated)? {
                None => return Ok(cx.bool(false)),
                Some(value) => value,
            };

            // Pass value and counter to the predicate function
            counter_handle.replace(Value::number(counter));
            let result = call_object(cx, predicate, cx.undefined(), &[value, counter_handle]);

            // Finish iterating if predicate threw or returned true
            match result {
                Err(_) => return iterator_close(cx, iterated.iterator, result),
                Ok(result) if to_boolean(*result) => {
                    return iterator_close(cx, iterated.iterator, Ok(cx.bool(true)));
                }
                Ok(_) => counter += 1,
            }
        }
    }}

    runtime_fn! {
    /// Iterator.prototype.take (https://tc39.es/ecma262/#sec-iterator.prototype.take)
    fn take(cx, this_value, arguments) {
        let this_object = this_object(cx, this_value, "take")?;

        let limit_arg = arguments.get(cx, 0);
        let integer_limit = validate_limit_argument(cx, this_object, limit_arg, "take")?;

        // Get the underlying iterator and create a new iterator helper take object
        let iterated = get_iterator_direct(cx, this_object)?;
        Ok(IteratorHelperObject::new_take(cx, &iterated, integer_limit)?.as_value())
    }}

    runtime_fn! {
    /// Iterator.prototype.toArray (https://tc39.es/ecma262/#sec-iterator.prototype.toarray)
    fn to_array(cx, this_value, _) {
        let this_object = this_object(cx, this_value, "toArray")?;

        let mut iterated = get_iterator_direct(cx, this_object)?;
        let mut items = vec![];

        // Collect all items from the iterator until the iterator is done
        loop {
            match iterator_step_value(cx, &mut iterated)? {
                None => return Ok(create_array_from_list(cx, &items)?.as_value()),
                Some(value) => items.push(value),
            }
        }
    }}

    runtime_fn! {
    /// get Iterator.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-get-iterator.prototype-%symbol.tostringtag%)
    fn iterator_prototype_get_to_string_tag(cx, _, _) {
        Ok(cx.names.iterator().as_string().as_value())
    }}

    runtime_fn! {
    /// set Iterator.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-set-iterator.prototype-%symbol.tostringtag%)
    fn set_to_string_tag(cx, this_value, arguments) {
        let value = arguments.get(cx, 0);

        setter_that_ignores_prototype_properties(
            cx,
            this_value,
            cx.get_intrinsic(Intrinsic::IteratorPrototype),
            cx.symbols.to_string_tag(),
            value,
        )?;

        Ok(cx.undefined())
    }}
}

fn this_object(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<ObjectValue>> {
    if value.is_object() {
        return Ok(value.as_object());
    }

    type_error(cx, &format!("Iterator.prototype.{method_name} must be called on an object"))
}

/// Verify the limit is a number, not NaN, and not negative. Close the underlying iterator if not.
fn validate_limit_argument(
    cx: Context,
    iterator: Handle<ObjectValue>,
    limit_arg: Handle<Value>,
    method_name: &str,
) -> EvalResult<f64> {
    let limit = match to_number(cx, limit_arg) {
        Err(error) => {
            return iterator_close_error(cx, iterator, Err(error));
        }
        Ok(limit) => limit,
    };

    if limit.is_nan() {
        let error =
            range_error_value(cx, &format!("Iterator.prototype.{method_name} limit is NaN"))?;
        return iterator_close_error(cx, iterator, eval_err!(error));
    }

    let integer_limit = must!(to_integer_or_infinity(cx, limit));
    if integer_limit < 0.0 {
        let error =
            range_error_value(cx, &format!("Iterator.prototype.{method_name} limit is negative"))?;
        return iterator_close_error(cx, iterator, eval_err!(error));
    }

    Ok(integer_limit)
}

/// Verify the argument is a function, closing the underlying iterator if not.
#[inline]
fn validate_callable_argument(
    cx: Context,
    iterator: Handle<ObjectValue>,
    argument: Handle<Value>,
    method_name: &str,
    argument_name: &str,
) -> EvalResult<Handle<ObjectValue>> {
    if !is_callable(argument) {
        let error = type_error_value(
            cx,
            &format!("Iterator.prototype.{method_name} {argument_name} must be a function"),
        )?;
        return iterator_close_error(cx, iterator, eval_err!(error));
    }

    Ok(argument.as_object())
}

fn iterator_close_error<T>(
    cx: Context,
    iterator: Handle<ObjectValue>,
    completion: EvalResult<Handle<Value>>,
) -> EvalResult<T> {
    match iterator_close(cx, iterator, completion) {
        Ok(_) => unreachable!("iterator_close with error completion is guaranteed to return Err"),
        Err(error) => Err(error),
    }
}
