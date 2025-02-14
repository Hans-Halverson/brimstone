use crate::js::runtime::{
    error::type_error,
    generator_object::GeneratorState,
    iterator::{create_iter_result_object, iterator_close},
    object_value::ObjectValue,
    property::Property,
    Context, EvalResult, Handle, Realm, Value,
};

use super::{intrinsics::Intrinsic, iterator_helper_object::IteratorHelperObject};

/// The Iterator Helper Prototype Object (https://tc39.es/ecma262/#sec-%iteratorhelperprototype%-object)
pub struct IteratorHelperPrototype;

impl IteratorHelperPrototype {
    pub fn new(mut cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true);

        object.intrinsic_func(cx, cx.names.next(), Self::next, 0, realm);
        object.intrinsic_func(cx, cx.names.return_(), Self::return_, 1, realm);

        // %IteratorHelperPrototype% [ @@toStringTag ] (https://tc39.es/ecma262/#sec-%iteratorhelperprototype%-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let iterator_helper = cx.alloc_string("Iterator Helper");
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(iterator_helper.as_value(), false, false, true),
        );

        object
    }

    /// %IteratorHelperPrototype%.next (https://tc39.es/ecma262/#sec-%iteratorhelperprototype%.next)
    pub fn next(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        // GenerateResume adapted for Iterator Helper Objects
        let mut object = match validate_iterator_helper_object(this_value) {
            None => {
                return type_error(
                    cx,
                    "%IteratorHelperPrototype%.next called on a non-Iterator Helper object",
                )
            }
            Some(object) => object,
        };

        let is_start = match object.generator_state() {
            // Error if the "generator" is already executing
            GeneratorState::Executing => return type_error(cx, "generator is already executing"),
            // Error if the "generator" is already completed
            GeneratorState::Completed => {
                return Ok(create_iter_result_object(cx, cx.undefined(), true));
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
                Ok(create_iter_result_object(cx, cx.undefined(), true))
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
        let mut object = match validate_iterator_helper_object(this_value) {
            None => {
                return type_error(
                    cx,
                    "%IteratorHelperPrototype%.return called on a non-Iterator Helper object",
                )
            }
            Some(object) => object,
        };

        match object.generator_state() {
            // Error if the "generator" is already executing
            GeneratorState::Executing => return type_error(cx, "generator is already executing"),
            // On completion set the "generator" to completed and return the done result
            GeneratorState::Completed => {
                return Ok(create_iter_result_object(cx, cx.undefined(), true));
            }
            // If "generator" has not yet started then close the underlying iterator and return done
            GeneratorState::SuspendedStart => {
                object.set_generator_state(GeneratorState::Completed);
                iterator_close(cx, object.iterator_object(), Ok(cx.undefined()))?;

                return Ok(create_iter_result_object(cx, cx.undefined(), true));
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

fn validate_iterator_helper_object(value: Handle<Value>) -> Option<Handle<IteratorHelperObject>> {
    if !value.is_object() {
        return None;
    }

    value.as_object().as_iterator_helper_object()
}
