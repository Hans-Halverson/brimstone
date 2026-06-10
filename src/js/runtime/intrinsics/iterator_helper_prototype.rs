use crate::runtime::{
    Context, EvalResult, Handle, Realm, Value,
    alloc_error::AllocResult,
    error::type_error,
    generator_object::GeneratorState,
    intrinsics::{
        intrinsics::Intrinsic, iterator_helper_object::IteratorHelperObject,
        rust_runtime::RuntimeFunction,
    },
    iterator::{create_iter_result_object, iterator_close},
    object_value::ObjectValue,
    property::Property,
};

/// The Iterator Helper Prototype Object (https://tc39.es/ecma262/#sec-%iteratorhelperprototype%-object)
pub struct IteratorHelperPrototype;

impl IteratorHelperPrototype {
    pub fn new(mut cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true)?;

        object.intrinsic_func(
            cx,
            cx.names.next(),
            RuntimeFunction::IteratorHelperPrototype_next,
            0,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.return_(),
            RuntimeFunction::IteratorHelperPrototype_return_,
            1,
            realm,
        )?;

        // %IteratorHelperPrototype% [ @@toStringTag ] (https://tc39.es/ecma262/#sec-%iteratorhelperprototype%-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let iterator_helper = cx.alloc_static_string("Iterator Helper")?;
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(iterator_helper.as_value(), false, false, true),
        )?;

        Ok(object)
    }

    /// %IteratorHelperPrototype%.next (https://tc39.es/ecma262/#sec-%iteratorhelperprototype%.next)
    pub fn next(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        // GenerateResume adapted for Iterator Helper Objects
        let mut object = this_iterator_helper_object(cx, this_value, "next")?;

        let is_start = match object.generator_state() {
            // Error if the "generator" is already executing
            GeneratorState::Executing => {
                return type_error(
                    cx,
                    "%IteratorHelperPrototype%.next generator is already executing",
                );
            }
            // Error if the "generator" is already completed
            GeneratorState::Completed => {
                return Ok(create_iter_result_object(cx, cx.undefined(), true)?);
            }
            GeneratorState::SuspendedStart => true,
            GeneratorState::SuspendedYield => false,
        };

        // Execute the next method for the specific iterator helper type
        object.set_generator_state(GeneratorState::Executing);
        match object.next(cx, is_start) {
            // On errors set the "generator" to completed and propagate the error
            Err(error) => {
                object.set_generator_state(GeneratorState::Completed);
                Err(error)
            }
            // On completion set the "generator" to completed and return the done result
            Ok(None) => {
                object.set_generator_state(GeneratorState::Completed);
                Ok(create_iter_result_object(cx, cx.undefined(), true)?)
            }
            // Otherwise must have been a "yield", so set the "generator" to be suspended
            Ok(Some(result)) => {
                object.set_generator_state(GeneratorState::SuspendedYield);
                Ok(result.as_value())
            }
        }
    }

    /// %IteratorHelperPrototype%.return (https://tc39.es/ecma262/#sec-%iteratorhelperprototype%.return)
    pub fn return_(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let mut object = this_iterator_helper_object(cx, this_value, "return")?;

        match object.generator_state() {
            // Error if the "generator" is already executing
            GeneratorState::Executing => {
                return type_error(
                    cx,
                    "%IteratorHelperPrototype%.return generator is already executing",
                );
            }
            // On completion set the "generator" to completed and return the done result
            GeneratorState::Completed => {
                return Ok(create_iter_result_object(cx, cx.undefined(), true)?);
            }
            // If "generator" has not yet started then close the underlying iterator and return done
            GeneratorState::SuspendedStart => {
                object.set_generator_state(GeneratorState::Completed);

                // There may not be an underlying iterator object yet, e.g. in `Iterator.concat`
                // before `next` has been called the first time.
                if let Some(iterator_object) = object.iterator_object() {
                    iterator_close(cx, iterator_object, Ok(cx.undefined()))?;
                }

                return Ok(create_iter_result_object(cx, cx.undefined(), true)?);
            }
            GeneratorState::SuspendedYield => {}
        }

        // Execute the return method for the specific iterator helper type
        object.set_generator_state(GeneratorState::Executing);
        let return_result = object.return_(cx);
        object.set_generator_state(GeneratorState::Completed);

        return_result
    }
}

fn this_iterator_helper_object(
    cx: Context,
    value: Handle<Value>,
    method_name: &str,
) -> EvalResult<Handle<IteratorHelperObject>> {
    if value.is_object() {
        if let Some(iterator_helper) = value.as_object().as_iterator_helper_object() {
            return Ok(iterator_helper);
        }
    }

    type_error(
        cx,
        &format!("%IteratorHelperPrototype%.{method_name} must be called on an Iterator Helper"),
    )
}
