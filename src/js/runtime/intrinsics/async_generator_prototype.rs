use crate::{
    if_abrupt_reject_promise,
    js::runtime::{
        abstract_operations::{call_object, define_property_or_throw},
        async_generator_object::{
            async_generator_await_return, async_generator_resume, async_generator_validate,
            AsyncGeneratorState,
        },
        bytecode::function::Closure,
        completion::EvalResult,
        function::get_argument,
        generator_object::GeneratorCompletionType,
        iterator::create_iter_result_object,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        promise_object::PromiseCapability,
        property::Property,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        Context, Handle, Value,
    },
    maybe, must,
};

use super::intrinsics::Intrinsic;

pub struct AsyncGeneratorPrototype;

impl AsyncGeneratorPrototype {
    // 27.6.1 The %AsyncGeneratorPrototype% Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object = ObjectValue::new(
            cx,
            Some(realm.get_intrinsic(Intrinsic::AsyncIteratorPrototype)),
            true,
        );

        // Constructor property is added once AsyncGeneratorFunctionPrototype is created

        object.intrinsic_func(cx, cx.names.next(), Self::next, 1, realm);
        object.intrinsic_func(cx, cx.names.return_(), Self::return_, 1, realm);
        object.intrinsic_func(cx, cx.names.throw(), Self::throw, 1, realm);

        // 27.6.1.5 %AsyncGeneratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.async_generator().as_string().into(), false, false, true),
        );

        object
    }

    // 27.6.1.2 %AsyncGeneratorPrototype%.next
    pub fn next(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);

        let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
        let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

        let validate_completion = async_generator_validate(cx, this_value);
        let mut async_generator = if_abrupt_reject_promise!(cx, validate_completion, capability);

        let state = async_generator.state();

        // Immediately return if generator was already complete
        if state == AsyncGeneratorState::Completed {
            let iter_result = create_iter_result_object(cx, cx.undefined(), true);
            must!(call_object(cx, capability.resolve(), cx.undefined(), &[iter_result.into()]));
            return capability.promise().into();
        }

        async_generator.enqueue_request(cx, capability, value, GeneratorCompletionType::Normal);

        // Resume with a normal completion if generator is suspended at a yield
        if state == AsyncGeneratorState::SuspendedStart
            || state == AsyncGeneratorState::SuspendedYield
        {
            async_generator_resume(cx, async_generator, value, GeneratorCompletionType::Normal);
        } else {
            debug_assert!(
                state == AsyncGeneratorState::Executing
                    || state == AsyncGeneratorState::AwaitingReturn
            );
        }

        capability.promise().into()
    }

    // 27.6.1.3 %AsyncGeneratorPrototype%.return
    pub fn return_(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);

        let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
        let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

        let validate_completion = async_generator_validate(cx, this_value);
        let mut async_generator = if_abrupt_reject_promise!(cx, validate_completion, capability);

        async_generator.enqueue_request(cx, capability, value, GeneratorCompletionType::Return);

        let state = async_generator.state();
        if state == AsyncGeneratorState::SuspendedStart || state == AsyncGeneratorState::Completed {
            must!(async_generator_await_return(cx, async_generator));
        } else if state == AsyncGeneratorState::SuspendedYield {
            // Resume with a return completion if generator is suspended at a yield
            async_generator_resume(cx, async_generator, value, GeneratorCompletionType::Return);
        } else {
            debug_assert!(
                state == AsyncGeneratorState::Executing
                    || state == AsyncGeneratorState::AwaitingReturn
            );
        }

        capability.promise().into()
    }

    // 27.6.1.4 %AsyncGeneratorPrototype%.throw
    pub fn throw(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let error = get_argument(cx, arguments, 0);

        let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
        let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

        let validate_completion = async_generator_validate(cx, this_value);
        let mut async_generator = if_abrupt_reject_promise!(cx, validate_completion, capability);

        // Throw a generator that has not started executing immediately completes it
        let mut state = async_generator.state();
        if state == AsyncGeneratorState::SuspendedStart {
            async_generator.set_state(AsyncGeneratorState::Completed);
            state = AsyncGeneratorState::Completed;
        }

        // Throw on a completed generator rejects the promise
        if state == AsyncGeneratorState::Completed {
            must!(call_object(cx, capability.reject(), cx.undefined(), &[error.into()]));
            return capability.promise().into();
        }

        async_generator.enqueue_request(cx, capability, error, GeneratorCompletionType::Throw);

        // Resume with a throw completion if generator is suspended at a yield
        if state == AsyncGeneratorState::SuspendedYield {
            async_generator_resume(cx, async_generator, error, GeneratorCompletionType::Throw);
        } else {
            debug_assert!(
                state == AsyncGeneratorState::Executing
                    || state == AsyncGeneratorState::AwaitingReturn
            );
        }

        capability.promise().into()
    }

    /// Every async generator function has a prototype property referencing an instance of the
    /// async generator prototype. Install this property on an async generator function.
    pub fn install_on_async_generator_function(
        cx: Context,
        closure: Handle<Closure>,
    ) -> EvalResult<()> {
        let proto = object_create::<ObjectValue>(
            cx,
            ObjectKind::OrdinaryObject,
            Intrinsic::AsyncGeneratorPrototype,
        )
        .to_handle();

        let proto_desc = PropertyDescriptor::data(proto.to_handle().into(), true, false, false);
        maybe!(define_property_or_throw(cx, closure.into(), cx.names.prototype(), proto_desc));

        ().into()
    }
}
