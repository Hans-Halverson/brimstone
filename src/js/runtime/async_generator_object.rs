use crate::{
    extend_object, field_offset, must,
    runtime::{
        abstract_operations::call_object,
        eval_result::EvalResult,
        gc::{HeapObject, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        iterator::create_iter_result_object,
        object_descriptor::ObjectDescriptor,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{get_prototype_from_constructor, object_ordinary_init},
        promise_object::PromiseCapability,
        realm::Realm,
        Context, Handle, HeapPtr,
    },
    set_uninit,
};

use super::{
    builtin_function::BuiltinFunction,
    bytecode::{
        function::Closure,
        stack_frame::{StackFrame, StackSlotValue},
    },
    collections::InlineArray,
    error::type_error,
    function::get_argument,
    generator_object::{GeneratorCompletionType, TGeneratorObject},
    intrinsics::promise_prototype::perform_promise_then,
    promise_object::coerce_to_ordinary_promise,
    Value,
};

// An async generator object represents the state of an async generator function. It holds the
// saved stack frame of the generator function, which is restored when the generator is resumed.
//
// Also holds a queue of promises which represent requests to resume the async generator.
extend_object! {
    pub struct AsyncGeneratorObject {
        // The current state of the generator - may be executing, suspended, awaiting return, or
        // completed.
        state: AsyncGeneratorState,
        // Address of the next instruction to execute when this generator is resumed.
        // Stored as a byte offset into the BytecodeFunction.
        pc_to_resume_offset: usize,
        // Index of the frame pointer in the stack frame.
        fp_index: usize,
        // Indices of registers for the completion operands to return when the generator is resumed.
        // The value is written to the first register and the completion type is written to the
        // second register.
        //
        // Value is from AsyncGenerator.prototype.{next, return, throw}.
        completion_indices: Option<(u32, u32)>,
        // A queue of requests to resume the async generator. Forms a singly linked list, where the
        // head is the next request that should be consumed.
        request_queue: Option<HeapPtr<AsyncGeneratorRequest>>,
        // The stack frame of the generator, containing all args, locals, and fixed slots in
        // between.
        stack_frame: InlineArray<StackSlotValue>,
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
    descriptor: HeapPtr<ObjectDescriptor>,
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
        closure: Handle<Closure>,
        pc_to_resume_offset: usize,
        fp_index: usize,
        stack_frame: &[StackSlotValue],
    ) -> EvalResult<HeapPtr<AsyncGeneratorObject>> {
        let prototype =
            get_prototype_from_constructor(cx, closure.into(), Intrinsic::AsyncGeneratorPrototype)?;

        let size = Self::calculate_size_in_bytes(stack_frame.len());
        let mut generator = cx.alloc_uninit_with_size::<AsyncGeneratorObject>(size);

        let descriptor = cx.base_descriptors.get(ObjectKind::AsyncGenerator);
        object_ordinary_init(cx, generator.into(), descriptor, Some(*prototype));

        set_uninit!(generator.state, AsyncGeneratorState::SuspendedStart);
        set_uninit!(generator.pc_to_resume_offset, pc_to_resume_offset);
        set_uninit!(generator.fp_index, fp_index);
        set_uninit!(generator.completion_indices, None);
        set_uninit!(generator.request_queue, None);
        generator.stack_frame.init_from_slice(stack_frame);

        Ok(generator)
    }

    const STACK_FRAME_OFFSET: usize = field_offset!(AsyncGeneratorObject, stack_frame);

    fn calculate_size_in_bytes(num_stack_slots: usize) -> usize {
        Self::STACK_FRAME_OFFSET
            + InlineArray::<StackSlotValue>::calculate_size_in_bytes(num_stack_slots)
    }

    pub fn state(&self) -> AsyncGeneratorState {
        self.state
    }

    pub fn set_state(&mut self, state: AsyncGeneratorState) {
        self.state = state;
    }

    fn current_fp(&self) -> *const StackSlotValue {
        unsafe { self.stack_frame.data_ptr().add(self.fp_index) }
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
        completion_indices: (u32, u32),
        stack_frame: &[StackSlotValue],
    ) {
        self.state = AsyncGeneratorState::SuspendedYield;
        self.save_state(pc_to_resume_offset, completion_indices, stack_frame);
    }

    pub fn suspend_await(
        &mut self,
        pc_to_resume_offset: usize,
        completion_indices: (u32, u32),
        stack_frame: &[StackSlotValue],
    ) {
        self.state = AsyncGeneratorState::SuspendedAwait;
        self.save_state(pc_to_resume_offset, completion_indices, stack_frame);
    }

    fn save_state(
        &mut self,
        pc_to_resume_offset: usize,
        completion_indices: (u32, u32),
        stack_frame: &[StackSlotValue],
    ) {
        self.pc_to_resume_offset = pc_to_resume_offset;
        self.completion_indices = Some(completion_indices);
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

    fn completion_indices(&self) -> Option<(u32, u32)> {
        self.completion_indices
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
    ) {
        let new_request =
            AsyncGeneratorRequest::new(cx, capability, completion_value, completion_type);

        // Append new request to the end of the queue
        let mut last_request = &mut self.request_queue;
        while let Some(request) = last_request {
            last_request = &mut request.next;
        }

        *last_request = Some(new_request);
    }
}

impl AsyncGeneratorRequest {
    fn new(
        cx: Context,
        capability: Handle<PromiseCapability>,
        completion_value: Handle<Value>,
        completion_type: GeneratorCompletionType,
    ) -> HeapPtr<AsyncGeneratorRequest> {
        let mut request = cx.alloc_uninit::<AsyncGeneratorRequest>();

        set_uninit!(request.descriptor, cx.base_descriptors.get(ObjectKind::AsyncGeneratorRequest));
        set_uninit!(request.capability, *capability);
        set_uninit!(request.completion_value, *completion_value);
        set_uninit!(request.completion_type, completion_type);
        set_uninit!(request.next, None);

        request
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
) {
    debug_assert!(async_generator.request_queue.is_some());

    let next_request = async_generator.pop_request().unwrap();
    let capability = next_request.capability.to_handle();

    match completion {
        Ok(value) => {
            let result_object = create_iter_result_object(cx, value, is_done);
            must!(call_object(cx, capability.resolve(), cx.undefined(), &[result_object]));
        }
        Err(value) => {
            must!(call_object(cx, capability.reject(), cx.undefined(), &[value]));
        }
    }
}

/// AsyncGeneratorValidate (https://tc39.es/ecma262/#sec-asyncgeneratorvalidate)
pub fn async_generator_validate(
    cx: Context,
    generator: Handle<Value>,
) -> EvalResult<Handle<AsyncGeneratorObject>> {
    if generator.is_object() {
        if let Some(async_generator) = generator.as_object().as_async_generator() {
            return Ok(async_generator);
        }
    }

    type_error(cx, "expected async generator")
}

/// AsyncGeneratorResume (https://tc39.es/ecma262/#sec-asyncgeneratorresume)
pub fn async_generator_resume(
    mut cx: Context,
    mut async_generator: Handle<AsyncGeneratorObject>,
    completion_value: Handle<Value>,
    completion_type: GeneratorCompletionType,
) {
    async_generator.state = AsyncGeneratorState::Executing;

    let completion = cx
        .vm()
        .resume_generator(async_generator, completion_value, completion_type);

    // An empty normal completion signals that the generator was suspended at an await or yield
    if let Ok(value) = completion {
        if value.is_empty() {
            debug_assert!(async_generator.state.is_suspended());
            return;
        }
    }

    // Otherwise body returned or throw so the generator has completed
    async_generator.state = AsyncGeneratorState::Completed;

    async_generator_complete_step(cx, async_generator, completion, /* is_done */ true);
    async_generator_drain_queue(cx, async_generator);
}

/// AsyncGeneratorAwaitReturn (https://tc39.es/ecma262/#sec-asyncgeneratorawaitreturn)
pub fn async_generator_await_return(
    cx: Context,
    mut async_generator: Handle<AsyncGeneratorObject>,
) {
    async_generator.state = AsyncGeneratorState::AwaitingReturn;

    let request = async_generator.peek_request_ptr().unwrap();
    let completion_value = request.completion_value().to_handle(cx);

    let promise_completion = coerce_to_ordinary_promise(cx, completion_value);

    let promise = match promise_completion {
        Ok(promise) => promise,
        Err(error) => {
            async_generator_complete_step(cx, async_generator, Err(error), /* is_done */ true);
            async_generator_drain_queue(cx, async_generator);
            return;
        }
    };

    // Create a resolve function and attach async generator
    let on_resolve = BuiltinFunction::create(
        cx,
        await_return_resolve,
        1,
        cx.names.empty_string(),
        cx.current_realm(),
        None,
        None,
    );
    set_async_generator(cx, on_resolve, async_generator);

    // Create a reject function and attach async generator
    let on_reject = BuiltinFunction::create(
        cx,
        await_return_reject,
        1,
        cx.names.empty_string(),
        cx.current_realm(),
        None,
        None,
    );
    set_async_generator(cx, on_resolve, async_generator);

    perform_promise_then(cx, promise, on_resolve.into(), on_reject.into(), None);
}

pub fn await_return_resolve(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    let value = get_argument(cx, arguments, 0);

    let current_function = cx.current_function();
    let mut async_generator = get_async_generator(cx, current_function);

    async_generator.state = AsyncGeneratorState::Completed;

    async_generator_complete_step(cx, async_generator, Ok(value), /* is_done */ true);
    async_generator_drain_queue(cx, async_generator);

    Ok(cx.undefined())
}

pub fn await_return_reject(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
) -> EvalResult<Handle<Value>> {
    let value = get_argument(cx, arguments, 0);

    let current_function = cx.current_function();
    let mut async_generator = get_async_generator(cx, current_function);

    async_generator.state = AsyncGeneratorState::Completed;

    async_generator_complete_step(cx, async_generator, Err(value), /* is_done */ true);
    async_generator_drain_queue(cx, async_generator);

    Ok(cx.undefined())
}

fn get_async_generator(cx: Context, function: Handle<ObjectValue>) -> Handle<AsyncGeneratorObject> {
    function
        .private_element_find(cx, cx.well_known_symbols.async_generator().cast())
        .unwrap()
        .value()
        .as_object()
        .cast::<AsyncGeneratorObject>()
}

fn set_async_generator(
    cx: Context,
    mut function: Handle<ObjectValue>,
    async_generator: Handle<AsyncGeneratorObject>,
) {
    function.private_element_set(
        cx,
        cx.well_known_symbols.async_generator().cast(),
        async_generator.into(),
    );
}

/// AsyncGeneratorDrainQueue (https://tc39.es/ecma262/#sec-asyncgeneratordrainqueue)
pub fn async_generator_drain_queue(cx: Context, async_generator: Handle<AsyncGeneratorObject>) {
    loop {
        let request = match async_generator.peek_request() {
            None => return,
            Some(request) => request,
        };

        match request.completion_type() {
            GeneratorCompletionType::Return => {
                async_generator_await_return(cx, async_generator);
                return;
            }
            GeneratorCompletionType::Normal => {
                let completion = Ok(cx.undefined());
                async_generator_complete_step(
                    cx,
                    async_generator,
                    completion,
                    /* is_done */ true,
                );
            }
            GeneratorCompletionType::Throw => {
                let completion = Err(request.completion_value().to_handle(cx));
                async_generator_complete_step(
                    cx,
                    async_generator,
                    completion,
                    /* is_done */ true,
                );
            }
        }
    }
}

impl HeapObject for HeapPtr<AsyncGeneratorObject> {
    fn byte_size(&self) -> usize {
        AsyncGeneratorObject::calculate_size_in_bytes(self.stack_frame.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer_opt(&mut self.request_queue);

        if self.state.is_suspended() {
            let mut stack_frame = StackFrame::for_fp(self.current_fp().cast_mut());
            stack_frame.visit_simple_pointers(visitor);
        }
    }
}

impl HeapObject for HeapPtr<AsyncGeneratorRequest> {
    fn byte_size(&self) -> usize {
        std::mem::size_of::<AsyncGeneratorRequest>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.capability);
        visitor.visit_value(&mut self.completion_value);
        visitor.visit_pointer_opt(&mut self.next);
    }
}
