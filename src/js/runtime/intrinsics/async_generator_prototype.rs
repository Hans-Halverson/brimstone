use crate::{
    if_abrupt_reject_promise, intrinsic_methods, must,
    runtime::{
        Context, Handle,
        abstract_operations::{call_object, define_property_or_throw},
        alloc_error::AllocResult,
        async_generator_object::{
            AsyncGeneratorState, async_generator_await_return, async_generator_resume,
            async_generator_validate,
        },
        bytecode::function::Closure,
        eval_result::EvalResult,
        generator_object::GeneratorCompletionType,
        heap_item_descriptor::HeapItemKind,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::intrinsics::Intrinsic,
        iterator::create_iter_result_object,
        object_value::ObjectValue,
        ordinary_object::object_create,
        promise_object::PromiseCapability,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
    },
    runtime_fn,
};

pub struct AsyncGeneratorPrototype;

impl AsyncGeneratorPrototype {
    /// The %AsyncGeneratorPrototype% Object (https://tc39.es/ecma262/#sec-properties-of-asyncgenerator-prototype)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::AsyncIteratorPrototype)?;

        // Constructor property is added once AsyncGeneratorFunctionPrototype has been created
        intrinsic_methods!(cx, builder, {
            next    AsyncGeneratorPrototype_next    (1),
            return_ AsyncGeneratorPrototype_return_ (1),
            throw   AsyncGeneratorPrototype_throw   (1),
        });

        // %AsyncGeneratorPrototype% [ @@toStringTag ] (https://tc39.es/ecma262/#sec-asyncgenerator-prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.async_generator())?;

        builder.build()
    }

    runtime_fn! {
    /// %AsyncGeneratorPrototype%.next (https://tc39.es/ecma262/#sec-asyncgenerator-prototype-next)
    fn next(cx, this_value, arguments) {
        let value = arguments.get(cx, 0);

        let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
        let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

        let validate_completion = async_generator_validate(cx, this_value);
        let mut async_generator = if_abrupt_reject_promise!(cx, validate_completion, capability);

        let state = async_generator.state();

        // Immediately return if generator was already complete
        if state == AsyncGeneratorState::Completed {
            let iter_result = create_iter_result_object(cx, cx.undefined(), true)?;
            must!(call_object(cx, capability.resolve(), cx.undefined(), &[iter_result]));
            return Ok(capability.promise().as_value());
        }

        async_generator.enqueue_request(cx, capability, value, GeneratorCompletionType::Normal)?;

        // Resume with a normal completion if generator is suspended at a yield
        if state == AsyncGeneratorState::SuspendedStart
            || state == AsyncGeneratorState::SuspendedYield
        {
            async_generator_resume(cx, async_generator, value, GeneratorCompletionType::Normal)?;
        } else {
            debug_assert!(state.is_executing() || state == AsyncGeneratorState::AwaitingReturn);
        }

        Ok(capability.promise().as_value())
    }}

    runtime_fn! {
    /// %AsyncGeneratorPrototype%.return (https://tc39.es/ecma262/#sec-asyncgenerator-prototype-return)
    fn return_(cx, this_value, arguments) {
        let value = arguments.get(cx, 0);

        let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
        let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));

        let validate_completion = async_generator_validate(cx, this_value);
        let mut async_generator = if_abrupt_reject_promise!(cx, validate_completion, capability);

        async_generator.enqueue_request(cx, capability, value, GeneratorCompletionType::Return)?;

        let state = async_generator.state();
        if state == AsyncGeneratorState::SuspendedStart || state == AsyncGeneratorState::Completed {
            async_generator_await_return(cx, async_generator)?;
        } else if state == AsyncGeneratorState::SuspendedYield {
            // Resume with a return completion if generator is suspended at a yield
            async_generator_resume(cx, async_generator, value, GeneratorCompletionType::Return)?;
        } else {
            debug_assert!(state.is_executing() || state == AsyncGeneratorState::AwaitingReturn);
        }

        Ok(capability.promise().as_value())
    }}

    runtime_fn! {
    /// %AsyncGeneratorPrototype%.throw (https://tc39.es/ecma262/#sec-asyncgenerator-prototype-throw)
    fn throw(cx, this_value, arguments) {
        let error = arguments.get(cx, 0);

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
            must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
            return Ok(capability.promise().as_value());
        }

        async_generator.enqueue_request(cx, capability, error, GeneratorCompletionType::Throw)?;

        // Resume with a throw completion if generator is suspended at a yield
        if state == AsyncGeneratorState::SuspendedYield {
            async_generator_resume(cx, async_generator, error, GeneratorCompletionType::Throw)?;
        } else {
            debug_assert!(state.is_executing() || state == AsyncGeneratorState::AwaitingReturn);
        }

        Ok(capability.promise().as_value())
    }}

    /// Every async generator function has a prototype property referencing an instance of the
    /// async generator prototype. Install this property on an async generator function.
    pub fn install_on_async_generator_function(
        cx: Context,
        closure: Handle<Closure>,
    ) -> EvalResult<()> {
        let proto = object_create::<ObjectValue>(
            cx,
            HeapItemKind::OrdinaryObject,
            Intrinsic::AsyncGeneratorPrototype,
        )?
        .to_handle();

        let proto_desc = PropertyDescriptor::data(proto.to_handle().into(), true, false, false);
        define_property_or_throw(cx, closure.into(), cx.names.prototype(), proto_desc)?;

        Ok(())
    }
}
