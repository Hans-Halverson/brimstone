use crate::{
    completion_value, eval_err, extend_object, must_a,
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr, Value,
        abstract_operations::call_object,
        alloc_error::AllocResult,
        builtin_function::BuiltinFunction,
        bytecode::{
            function::ClosureObject,
            stack_frame::{StackFrame, StackFrameArray, StackSlotValue},
        },
        collections::ArrayInstance,
        error::type_error,
        eval_result::EvalResult,
        gc::{HeapItem, HeapVisitor},
        generator_object::{GeneratorCompletionType, GeneratorRegister, TGeneratorObject},
        heap_item_descriptor::HeapItemDescriptor,
        intrinsics::{
            intrinsics::Intrinsic, promise_prototype::perform_promise_then,
            rust_runtime::RuntimeFunction,
        },
        iterator::create_iter_result_object,
        object_value::ObjectValue,
        ordinary_object::{get_prototype_from_constructor, object_ordinary_init},
        promise_object::{PromiseCapability, coerce_to_ordinary_promise},
        realm::Realm,
    },
    runtime_fn, set_uninit,
};

extend_object! {
    /// An async generator object represents the state of an async generator function. It holds the
    /// saved stack frame of the generator function, which is restored when the generator is
    /// resumed.
    ///
    /// Also holds a queue of promises which represent requests to resume the async generator.
    pub struct AsyncGeneratorObject {
        /// The current state of the generator - may be executing, suspended, awaiting return, or
        /// completed.
        state: AsyncGeneratorState,
        /// Address of the next instruction to execute when this generator is resumed.
        /// Stored as a byte offset into the BytecodeFunction.
        pc_to_resume_offset: usize,
        /// Index of the frame pointer in the stack frame.
        fp_index: usize,
        /// Registers for the completion operands to return when the generator is resumed. The value
        /// is written to the first register and the completion type is written to the second
        /// register.
        ///
        /// Value is from AsyncGenerator.prototype.{next, return, throw}.
        completion_registers: Option<(GeneratorRegister, GeneratorRegister)>,
        /// A queue of requests to resume the async generator. Forms a singly linked list, where the
        /// head is the next request that should be consumed.
        request_queue: Option<HeapPtr<AsyncGeneratorRequest>>,
        /// The stack frame of the generator, containing all args, locals, and fixed slots in
        /// between.
        stack_frame: HeapPtr<StackFrameArray>,
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AsyncGeneratorState {
    /// Generator has been created but has not started executing yet.
    SuspendedStart,
    /// Generator is not executing since it has yielded.
    SuspendedYield,
    /// Generator is currently executing.
    Executing,
    /// Generator is suspended at an await expression.
    SuspendedAwait,
    /// Generator has returned and is now waiting for queued requests to be processed.
    AwaitingReturn,
    /// Generator has completed. Once a generator is in the completed state it never leaves.
    Completed,
}

impl AsyncGeneratorState {
    pub fn is_suspended(&self) -> bool {
        matches!(self, Self::SuspendedStart | Self::SuspendedYield | Self::SuspendedAwait)
    }

    pub fn is_executing(&self) -> bool {
        matches!(self, Self::Executing | Self::SuspendedAwait)
    }
}

/// A linked list of requests to resume an async generator.
#[repr(C)]
pub struct AsyncGeneratorRequest {
    descriptor: HeapPtr<HeapItemDescriptor>,
    /// Promise capabilities associated with this request.
    capability: HeapPtr<PromiseCapability>,
    /// The completion value - either the value that the yield evaluates to, the error thrown, or
    /// the value that is returned.
    completion_value: Value,
    /// The type of the completion.
    completion_type: GeneratorCompletionType,
    /// The next request in the queue.
    next: Option<HeapPtr<AsyncGeneratorRequest>>,
}

impl AsyncGeneratorObject {
    pub fn new(
        cx: Context,
        closure: Handle<ClosureObject>,
        pc_to_resume_offset: usize,
        fp_index: usize,
        stack_frame: &[StackSlotValue],
    ) -> EvalResult<HeapPtr<AsyncGeneratorObject>> {
        let prototype =
            get_prototype_from_constructor(cx, closure.into(), Intrinsic::AsyncGeneratorPrototype)?;

        // Stack frame slice is safe to hold across allocations only because it is a view into
        // known roots which will be updated by the GC.
        //
        // For GC safety do not initialize or link the stack frame until after allocations.
        let frame_array = StackFrameArray::new_uninit(cx, stack_frame.len())?.to_handle();

        let mut generator = cx.alloc_uninit::<AsyncGeneratorObject>()?;

        let descriptor = cx.descriptors.get(HeapItemKind::AsyncGeneratorObject);
        object_ordinary_init(cx, generator.into(), descriptor, Some(*prototype));

        set_uninit!(generator.state, AsyncGeneratorState::SuspendedStart);
        set_uninit!(generator.pc_to_resume_offset, pc_to_resume_offset);
        set_uninit!(generator.fp_index, fp_index);
        set_uninit!(generator.completion_registers, None);
        set_uninit!(generator.request_queue, None);

        // No more allocations can occur, now safe to link and initialize the stack frame
        set_uninit!(generator.stack_frame, *frame_array);
        generator
            .stack_frame
            .as_mut_slice()
            .copy_from_slice(stack_frame);

        Ok(generator)
    }

    pub fn state(&self) -> AsyncGeneratorState {
        self.state
    }

    pub fn set_state(&mut self, state: AsyncGeneratorState) {
        self.state = state;
    }

    fn current_fp(&self) -> *const StackSlotValue {
        unsafe { self.stack_frame.as_slice().as_ptr().add(self.fp_index) }
    }

    /// Return the realm for the suspended function in this generator.
    pub fn realm_ptr(&self) -> HeapPtr<Realm> {
        StackFrame::for_fp(self.current_fp().cast_mut())
            .closure()
            .function_ptr()
            .realm_ptr()
    }

    pub fn suspend_yield(
        &mut self,
        pc_to_resume_offset: usize,
        completion_registers: (GeneratorRegister, GeneratorRegister),
        stack_frame: &[StackSlotValue],
    ) {
        self.state = AsyncGeneratorState::SuspendedYield;
        self.save_state(pc_to_resume_offset, completion_registers, stack_frame);
    }

    pub fn suspend_await(
        &mut self,
        pc_to_resume_offset: usize,
        completion_registers: (GeneratorRegister, GeneratorRegister),
        stack_frame: &[StackSlotValue],
    ) {
        self.state = AsyncGeneratorState::SuspendedAwait;
        self.save_state(pc_to_resume_offset, completion_registers, stack_frame);
    }

    fn save_state(
        &mut self,
        pc_to_resume_offset: usize,
        completion_registers: (GeneratorRegister, GeneratorRegister),
        stack_frame: &[StackSlotValue],
    ) {
        self.pc_to_resume_offset = pc_to_resume_offset;
        self.completion_registers = Some(completion_registers);
        self.stack_frame.as_mut_slice().copy_from_slice(stack_frame);
    }

    pub fn peek_request_ptr(&self) -> Option<HeapPtr<AsyncGeneratorRequest>> {
        self.request_queue
    }

    fn peek_request(&self) -> Option<Handle<AsyncGeneratorRequest>> {
        self.request_queue.map(|r| r.to_handle())
    }

    fn pop_request(&mut self) -> Option<HeapPtr<AsyncGeneratorRequest>> {
        if let Some(request) = self.request_queue {
            self.request_queue = request.next;
            Some(request)
        } else {
            None
        }
    }

    /// Set the register at the given index to the given value.
    pub fn set_register(&mut self, index: usize, value: Value) {
        unsafe {
            let fp = self.current_fp();
            let register = fp.sub(index + 1).cast_mut();
            *register = value.as_raw_bits() as StackSlotValue;
        }
    }
}

impl TGeneratorObject for Handle<AsyncGeneratorObject> {
    fn pc_to_resume_offset(&self) -> usize {
        self.pc_to_resume_offset
    }

    fn fp_index(&self) -> usize {
        self.fp_index
    }

    fn completion_registers(&self) -> Option<(GeneratorRegister, GeneratorRegister)> {
        self.completion_registers
    }

    fn stack_frame(&self) -> &[StackSlotValue] {
        self.stack_frame.as_slice()
    }
}

impl Handle<AsyncGeneratorObject> {
    pub fn enqueue_request(
        &mut self,
        cx: Context,
        capability: Handle<PromiseCapability>,
        completion_value: Handle<Value>,
        completion_type: GeneratorCompletionType,
    ) -> AllocResult<()> {
        let new_request =
            AsyncGeneratorRequest::new(cx, capability, completion_value, completion_type)?;

        // Append new request to the end of the queue
        let mut last_request = &mut self.request_queue;
        while let Some(request) = last_request {
            last_request = &mut request.next;
        }

        *last_request = Some(new_request);

        Ok(())
    }
}

impl AsyncGeneratorRequest {
    fn new(
        cx: Context,
        capability: Handle<PromiseCapability>,
        completion_value: Handle<Value>,
        completion_type: GeneratorCompletionType,
    ) -> AllocResult<HeapPtr<AsyncGeneratorRequest>> {
        let mut request = cx.alloc_uninit::<AsyncGeneratorRequest>()?;

        set_uninit!(request.descriptor, cx.descriptors.get(HeapItemKind::AsyncGeneratorRequest));
        set_uninit!(request.capability, *capability);
        set_uninit!(request.completion_value, *completion_value);
        set_uninit!(request.completion_type, completion_type);
        set_uninit!(request.next, None);

        Ok(request)
    }

    pub fn completion_value(&self) -> Value {
        self.completion_value
    }

    pub fn completion_type(&self) -> GeneratorCompletionType {
        self.completion_type
    }
}

/// AsyncGeneratorCompleteStep (https://tc39.es/ecma262/#sec-asyncgeneratorcompletestep)
pub fn async_generator_complete_step(
    cx: Context,
    mut async_generator: Handle<AsyncGeneratorObject>,
    completion: EvalResult<Handle<Value>>,
    is_done: bool,
) -> AllocResult<()> {
    debug_assert!(async_generator.request_queue.is_some());

    let next_request = async_generator.pop_request().unwrap();
    let capability = next_request.capability.to_handle();

    match completion_value!(completion) {
        Ok(value) => {
            let result_object = create_iter_result_object(cx, value, is_done)?;
            must_a!(call_object(cx, capability.resolve(), cx.undefined(), &[result_object]));
        }
        Err(error) => {
            must_a!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
        }
    }

    Ok(())
}

/// AsyncGeneratorValidate (https://tc39.es/ecma262/#sec-asyncgeneratorvalidate)
pub fn async_generator_validate(
    cx: Context,
    generator: Handle<Value>,
) -> EvalResult<Handle<AsyncGeneratorObject>> {
    if let Some(async_generator) = generator.as_opt::<AsyncGeneratorObject>() {
        return Ok(async_generator);
    }

    type_error(cx, "expected an async generator")
}

/// AsyncGeneratorResume (https://tc39.es/ecma262/#sec-asyncgeneratorresume)
pub fn async_generator_resume(
    mut cx: Context,
    mut async_generator: Handle<AsyncGeneratorObject>,
    completion_value: Handle<Value>,
    completion_type: GeneratorCompletionType,
) -> AllocResult<()> {
    async_generator.state = AsyncGeneratorState::Executing;

    let completion = cx
        .vm()
        .resume_generator(async_generator, completion_value, completion_type);

    // An empty normal completion signals that the generator was suspended at an await or yield
    if let Ok(value) = completion {
        if value.is_empty() {
            debug_assert!(async_generator.state.is_suspended());
            return Ok(());
        }
    }

    // Otherwise body returned or throw so the generator has completed
    async_generator.state = AsyncGeneratorState::Completed;

    async_generator_complete_step(cx, async_generator, completion, /* is_done */ true)?;
    async_generator_drain_queue(cx, async_generator)?;

    Ok(())
}

/// AsyncGeneratorAwaitReturn (https://tc39.es/ecma262/#sec-asyncgeneratorawaitreturn)
pub fn async_generator_await_return(
    cx: Context,
    mut async_generator: Handle<AsyncGeneratorObject>,
) -> AllocResult<()> {
    async_generator.state = AsyncGeneratorState::AwaitingReturn;

    let request = async_generator.peek_request_ptr().unwrap();
    let completion_value = request.completion_value().to_handle(cx);

    let promise_completion = coerce_to_ordinary_promise(cx, completion_value);

    let promise = match promise_completion {
        Ok(promise) => promise,
        Err(error) => {
            async_generator_complete_step(
                cx,
                async_generator,
                Err(error),
                /* is_done */ true,
            )?;
            async_generator_drain_queue(cx, async_generator)?;
            return Ok(());
        }
    };

    // Create a resolve function and attach async generator
    let on_resolve = BuiltinFunction::create(
        cx,
        RuntimeFunction::async_generator_object_await_return_resolve,
        1,
        cx.names.empty_string(),
        cx.current_realm(),
        None,
    )?;
    set_async_generator(cx, on_resolve, async_generator)?;

    // Create a reject function and attach async generator
    let on_reject = BuiltinFunction::create(
        cx,
        RuntimeFunction::async_generator_object_await_return_reject,
        1,
        cx.names.empty_string(),
        cx.current_realm(),
        None,
    )?;
    set_async_generator(cx, on_reject, async_generator)?;

    perform_promise_then(cx, promise, on_resolve.into(), on_reject.into(), None)?;

    Ok(())
}

runtime_fn! {
fn await_return_resolve(cx, _, arguments) {
    let value = arguments.get(cx, 0);

    let current_function = cx.current_function();
    let mut async_generator = get_async_generator(cx, current_function);

    async_generator.state = AsyncGeneratorState::Completed;

    async_generator_complete_step(cx, async_generator, Ok(value), /* is_done */ true)?;
    async_generator_drain_queue(cx, async_generator)?;

    Ok(cx.undefined())
}}

runtime_fn! {
fn await_return_reject(cx, _, arguments) {
    let value = arguments.get(cx, 0);

    let current_function = cx.current_function();
    let mut async_generator = get_async_generator(cx, current_function);

    async_generator.state = AsyncGeneratorState::Completed;

    async_generator_complete_step(cx, async_generator, eval_err!(value), /* is_done */ true)?;
    async_generator_drain_queue(cx, async_generator)?;

    Ok(cx.undefined())
}}

fn get_async_generator(cx: Context, function: Handle<ObjectValue>) -> Handle<AsyncGeneratorObject> {
    function
        .private_element_find(cx, cx.symbols.async_generator().cast())
        .unwrap()
        .value()
        .as_object()
        .cast::<AsyncGeneratorObject>()
}

fn set_async_generator(
    cx: Context,
    mut function: Handle<ObjectValue>,
    async_generator: Handle<AsyncGeneratorObject>,
) -> AllocResult<()> {
    function.private_element_set(cx, cx.symbols.async_generator().cast(), async_generator.into())
}

/// AsyncGeneratorDrainQueue (https://tc39.es/ecma262/#sec-asyncgeneratordrainqueue)
pub fn async_generator_drain_queue(
    cx: Context,
    async_generator: Handle<AsyncGeneratorObject>,
) -> AllocResult<()> {
    loop {
        let request = match async_generator.peek_request() {
            None => return Ok(()),
            Some(request) => request,
        };

        match request.completion_type() {
            GeneratorCompletionType::Return => {
                async_generator_await_return(cx, async_generator)?;
                return Ok(());
            }
            GeneratorCompletionType::Normal => {
                let completion = Ok(cx.undefined());
                async_generator_complete_step(
                    cx,
                    async_generator,
                    completion,
                    /* is_done */ true,
                )?;
            }
            GeneratorCompletionType::Throw => {
                let completion = eval_err!(request.completion_value().to_handle(cx));
                async_generator_complete_step(
                    cx,
                    async_generator,
                    completion,
                    /* is_done */ true,
                )?;
            }
        }
    }
}

impl HeapItem for AsyncGeneratorObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        std::mem::size_of::<AsyncGeneratorObject>()
    }

    fn visit_pointers(mut async_generator_object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        async_generator_object.visit_object_pointers(visitor);
        visitor.visit_pointer_opt(&mut async_generator_object.request_queue);
        visitor.visit_pointer(&mut async_generator_object.stack_frame);

        // Visit the separate stack frame array if it is live
        if async_generator_object.state.is_suspended() {
            let mut stack_frame =
                StackFrame::for_fp(async_generator_object.current_fp().cast_mut());
            stack_frame.visit_simple_pointers(visitor);
        }
    }
}

impl HeapItem for AsyncGeneratorRequest {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        std::mem::size_of::<AsyncGeneratorRequest>()
    }

    fn visit_pointers(mut async_generator_request: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut async_generator_request.descriptor);
        visitor.visit_pointer(&mut async_generator_request.capability);
        visitor.visit_value(&mut async_generator_request.completion_value);
        visitor.visit_pointer_opt(&mut async_generator_request.next);
    }
}
