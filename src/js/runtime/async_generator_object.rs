use crate::{
    extend_object, field_offset,
    js::runtime::{
        abstract_operations::call_object,
        completion::EvalResult,
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
    maybe, must, set_uninit,
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

#[derive(Clone, Copy, PartialEq)]
pub enum AsyncGeneratorState {
    /// Generator has been created but has not started executing yet.
    SuspendedStart,
    /// Generator is not executing since it has yielded.
    SuspendedYield,
    /// Generator is currently executing.
    Executing,
    /// Generator has returned and is now waiting for queued requests to be processed.
    AwaitingReturn,
    /// Generator has completed. Once a generator is in the completed state it never leaves.
    Completed,
}

impl AsyncGeneratorState {
    pub fn is_suspended(&self) -> bool {
        matches!(self, Self::SuspendedStart | Self::SuspendedYield)
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
        let prototype = maybe!(get_prototype_from_constructor(
            cx,
            closure.into(),
            Intrinsic::AsyncGeneratorPrototype
        ));

        let size = Self::calculate_size_in_bytes(stack_frame.len());
        let mut generator = cx.alloc_uninit_with_size::<AsyncGeneratorObject>(size);

        let descriptor = cx.base_descriptors.get(ObjectKind::AsyncGenerator);
        object_ordinary_init(cx, generator.into(), descriptor, Some(prototype.get_()));

        set_uninit!(generator.state, AsyncGeneratorState::SuspendedStart);
        set_uninit!(generator.pc_to_resume_offset, pc_to_resume_offset);
        set_uninit!(generator.fp_index, fp_index);
        set_uninit!(generator.completion_indices, None);
        set_uninit!(generator.request_queue, None);
        generator.stack_frame.init_from_slice(stack_frame);

        generator.into()
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

    pub fn suspend(
        &mut self,
        pc_to_resume_offset: usize,
        completion_indices: (u32, u32),
        stack_frame: &[StackSlotValue],
    ) {
        self.state = AsyncGeneratorState::SuspendedYield;
        self.pc_to_resume_offset = pc_to_resume_offset;
        self.completion_indices = Some(completion_indices);
        self.stack_frame.as_mut_slice().copy_from_slice(stack_frame);
    }

    pub fn peek_request(&self) -> Option<Handle<AsyncGeneratorRequest>> {
        self.request_queue.map(|r| r.to_handle())
    }

    pub fn pop_request(&mut self) -> Option<HeapPtr<AsyncGeneratorRequest>> {
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
        set_uninit!(request.capability, capability.get_());
        set_uninit!(request.completion_value, completion_value.get());
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

/// 27.6.3.5 AsyncGeneratorCompleteStep
pub fn async_generator_complete_step(
    cx: Context,
    mut async_generator: Handle<AsyncGeneratorObject>,
    completion: EvalResult<Handle<Value>>,
    is_done: bool,
    realm: Option<Handle<Realm>>,
) {
    debug_assert!(async_generator.request_queue.is_some());

    let next_request = async_generator.pop_request().unwrap();
    let capability = next_request.capability.to_handle();

    match completion {
        EvalResult::Ok(value) => {
            // Create iterator object in the given realm if specified
            let result_object = if let Some(_) = realm {
                todo!()
            } else {
                create_iter_result_object(cx, value, is_done).into()
            };

            must!(call_object(cx, capability.resolve(), cx.undefined(), &[result_object]));
        }
        EvalResult::Throw(value) => {
            must!(call_object(cx, capability.reject(), cx.undefined(), &[value],));
        }
    }
}

/// 27.6.3.3 AsyncGeneratorValidate
pub fn async_generator_validate(
    cx: Context,
    generator: Handle<Value>,
) -> EvalResult<Handle<AsyncGeneratorObject>> {
    if !generator.is_object() || !generator.as_object().is_async_generator() {
        return type_error(cx, "expected async generator");
    }

    generator.as_object().cast::<AsyncGeneratorObject>().into()
}

/// 27.6.3.6 AsyncGeneratorResume
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
    if let EvalResult::Ok(value) = completion {
        if value.is_empty() {
            return;
        }
    }

    // Otherwise body returned or throw so the generator has completed
    async_generator.state = AsyncGeneratorState::Completed;

    async_generator_complete_step(cx, async_generator, completion, /* is_done */ true, None);
    async_generator_drain_queue(cx, async_generator);
}

/// 27.6.3.9 AsyncGeneratorAwaitReturn
pub fn async_generator_await_return(
    cx: Context,
    mut async_generator: Handle<AsyncGeneratorObject>,
) -> EvalResult<()> {
    async_generator.state = AsyncGeneratorState::AwaitingReturn;

    let request = async_generator.peek_request().unwrap();
    let completion_value = request.completion_value().to_handle(cx);

    let promise = maybe!(coerce_to_ordinary_promise(cx, completion_value));

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

    ().into()
}

pub fn await_return_resolve(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let value = get_argument(cx, arguments, 0);

    let current_function = cx.current_function();
    let mut async_generator = get_async_generator(cx, current_function);

    async_generator.state = AsyncGeneratorState::Completed;

    async_generator_complete_step(
        cx,
        async_generator,
        EvalResult::Ok(value),
        /* is_done */ true,
        None,
    );

    async_generator_drain_queue(cx, async_generator);

    cx.undefined().into()
}

pub fn await_return_reject(
    mut cx: Context,
    _: Handle<Value>,
    arguments: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    let value = get_argument(cx, arguments, 0);

    let current_function = cx.current_function();
    let mut async_generator = get_async_generator(cx, current_function);

    async_generator.state = AsyncGeneratorState::Completed;

    async_generator_complete_step(
        cx,
        async_generator,
        EvalResult::Throw(value),
        /* is_done */ true,
        None,
    );

    async_generator_drain_queue(cx, async_generator);

    cx.undefined().into()
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

/// 27.6.3.10 AsyncGeneratorDrainQueue
pub fn async_generator_drain_queue(cx: Context, async_generator: Handle<AsyncGeneratorObject>) {
    loop {
        let request = match async_generator.peek_request() {
            None => return,
            Some(request) => request,
        };

        match request.completion_type() {
            GeneratorCompletionType::Return => {
                must!(async_generator_await_return(cx, async_generator));
                return;
            }
            GeneratorCompletionType::Normal => {
                let completion = EvalResult::Ok(cx.undefined());
                async_generator_complete_step(
                    cx,
                    async_generator,
                    completion,
                    /* is_done */ true,
                    None,
                );
            }
            GeneratorCompletionType::Throw => {
                let completion = EvalResult::Throw(request.completion_value().to_handle(cx));
                async_generator_complete_step(
                    cx,
                    async_generator,
                    completion,
                    /* is_done */ true,
                    None,
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
        self.cast::<ObjectValue>().visit_pointers(visitor);

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
