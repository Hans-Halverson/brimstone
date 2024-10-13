use std::collections::VecDeque;

use crate::{
    js::runtime::{
        abstract_operations::{call, call_object},
        intrinsics::promise_constructor::execute_then,
    },
    maybe,
};

use super::{
    async_generator_object::{async_generator_resume, AsyncGeneratorObject},
    gc::{HandleScope, HeapVisitor},
    generator_object::{GeneratorCompletionType, GeneratorObject},
    object_value::ObjectValue,
    promise_object::{PromiseCapability, PromiseObject, PromiseReactionKind},
    Context, EvalResult, HeapPtr, Realm, Value,
};

pub struct TaskQueue {
    tasks: VecDeque<Task>,
}

pub enum Task {
    Callback1(Callback1Task),
    AwaitResume(AwaitResumeTask),
    PromiseThenReaction(PromiseThenReactionTask),
    PromiseThenSettle(PromiseThenSettleTask),
}

impl TaskQueue {
    pub fn new() -> Self {
        Self { tasks: VecDeque::new() }
    }

    pub fn enqueue(&mut self, task: Task) {
        self.tasks.push_back(task);
    }

    pub fn enqueue_callback_1_task(&mut self, func: Value, arg: Value) {
        self.enqueue(Task::Callback1(Callback1Task::new(func, arg)));
    }

    pub fn enqueue_await_resume_task(
        &mut self,
        kind: PromiseReactionKind,
        generator: HeapPtr<ObjectValue>,
        result: Value,
    ) {
        self.enqueue(Task::AwaitResume(AwaitResumeTask::new(kind, generator, result)));
    }

    pub fn enqueue_promise_then_reaction_task(
        &mut self,
        kind: PromiseReactionKind,
        handler: Option<HeapPtr<ObjectValue>>,
        capability: Option<HeapPtr<PromiseCapability>>,
        result: Value,
        realm: Option<HeapPtr<Realm>>,
    ) {
        self.enqueue(Task::PromiseThenReaction(PromiseThenReactionTask::new(
            kind, handler, capability, result, realm,
        )));
    }

    pub fn enqueue_promise_then_settle_task(
        &mut self,
        then_function: HeapPtr<ObjectValue>,
        resolution: HeapPtr<ObjectValue>,
        promise: HeapPtr<PromiseObject>,
        realm: HeapPtr<Realm>,
    ) {
        self.enqueue(Task::PromiseThenSettle(PromiseThenSettleTask::new(
            then_function,
            resolution,
            promise,
            realm,
        )));
    }

    pub fn visit_roots(&mut self, visitor: &mut impl HeapVisitor) {
        for task in &mut self.tasks {
            match task {
                Task::Callback1(Callback1Task { func, arg }) => {
                    visitor.visit_value(func);
                    visitor.visit_value(arg);
                }
                Task::AwaitResume(AwaitResumeTask { generator, result, .. }) => {
                    visitor.visit_pointer(generator);
                    visitor.visit_value(result);
                }
                Task::PromiseThenReaction(PromiseThenReactionTask {
                    kind: _,
                    handler,
                    capability,
                    result,
                    realm,
                }) => {
                    visitor.visit_pointer_opt(handler);
                    visitor.visit_pointer_opt(capability);
                    visitor.visit_value(result);
                    visitor.visit_pointer_opt(realm);
                }
                Task::PromiseThenSettle(PromiseThenSettleTask {
                    then_function,
                    resolution,
                    promise,
                    realm,
                }) => {
                    visitor.visit_pointer(then_function);
                    visitor.visit_pointer(resolution);
                    visitor.visit_pointer(promise);
                    visitor.visit_pointer(realm);
                }
            }
        }
    }
}

impl Context {
    /// Run all tasks until the task queue is empty.
    pub fn run_all_tasks(&mut self) -> EvalResult<()> {
        while let Some(task) = self.task_queue().tasks.pop_front() {
            maybe!(HandleScope::new(*self, |cx| {
                match task {
                    Task::Callback1(task) => task.execute(cx),
                    Task::AwaitResume(task) => task.execute(cx),
                    Task::PromiseThenReaction(task) => task.execute(cx),
                    Task::PromiseThenSettle(task) => task.execute(cx),
                }
            }));
        }

        Ok(())
    }
}

/// Call a function with a single argument.
pub struct Callback1Task {
    func: Value,
    arg: Value,
}

impl Callback1Task {
    fn new(func: Value, arg: Value) -> Self {
        Self { func, arg }
    }

    fn execute(&self, cx: Context) -> EvalResult<()> {
        let func = self.func.to_handle(cx);
        let arg = self.arg.to_handle(cx);

        maybe!(call(cx, func, cx.undefined(), &[arg]));

        Ok(())
    }
}

/// Resume an async function that was paused at an `await` expression.
pub struct AwaitResumeTask {
    /// Whether the awaited promise was resolved or rejected.
    kind: PromiseReactionKind,
    /// The suspended async function that should be resumed with the provided completion. For
    /// regular async functions this is a GeneratorObject, for async generators this is an
    /// AsyncGeneratorObject.
    generator: HeapPtr<ObjectValue>,
    /// The value the await expression completes to, whether a normal value or thrown error.
    result: Value,
}

impl AwaitResumeTask {
    fn new(kind: PromiseReactionKind, generator: HeapPtr<ObjectValue>, result: Value) -> Self {
        Self { kind, generator, result }
    }

    fn execute(&self, mut cx: Context) -> EvalResult<()> {
        let generator = self.generator.to_handle();
        let completion_value = self.result.to_handle(cx);
        let completion_type = match self.kind {
            PromiseReactionKind::Fulfill => GeneratorCompletionType::Normal,
            PromiseReactionKind::Reject => GeneratorCompletionType::Throw,
        };

        if generator.is_generator() {
            let generator = generator.cast::<GeneratorObject>();
            maybe!(cx
                .vm()
                .resume_generator(generator, completion_value, completion_type));
        } else {
            let async_generator = generator.cast::<AsyncGeneratorObject>();

            // Must execute in the realm of the async generator since AsyncGeneratorResume may need
            // to drain the async queue when the VM stack is empty.
            maybe!(cx
                .vm()
                .push_initial_realm_stack_frame(async_generator.realm_ptr()));

            async_generator_resume(cx, async_generator, completion_value, completion_type);

            cx.vm().pop_initial_realm_stack_frame();
        }

        Ok(())
    }
}

pub struct PromiseThenReactionTask {
    /// Whether the promise was resolved or rejected.
    kind: PromiseReactionKind,
    /// A function to call on the result value.
    handler: Option<HeapPtr<ObjectValue>>,
    /// A promise capability to resolve or reject with the result of the handler function.
    capability: Option<HeapPtr<PromiseCapability>>,
    /// The value that the promise was resolved or rejected with.
    result: Value,
    /// The realm to set as the topmost execution context before executing the handler.
    realm: Option<HeapPtr<Realm>>,
}

impl PromiseThenReactionTask {
    fn new(
        kind: PromiseReactionKind,
        handler: Option<HeapPtr<ObjectValue>>,
        capability: Option<HeapPtr<PromiseCapability>>,
        result: Value,
        realm: Option<HeapPtr<Realm>>,
    ) -> Self {
        Self { kind, handler, capability, result, realm }
    }

    fn execute(&self, mut cx: Context) -> EvalResult<()> {
        if let Some(realm) = self.realm {
            maybe!(cx.vm().push_initial_realm_stack_frame(realm));
        }

        let result = self.result.to_handle(cx);
        let capability = self.capability.map(|c| c.to_handle());
        let realm = self.realm.map(|r| r.to_handle());

        // Call the handler if it exists on the result value
        let handler_result = if let Some(handler) = self.handler {
            let handler = handler.to_handle();
            call_object(cx, handler, cx.undefined(), &[result])
        } else {
            // If no handler was provided treat the handler result as a default normal or throw
            match self.kind {
                PromiseReactionKind::Fulfill => Ok(result),
                PromiseReactionKind::Reject => Err(result),
            }
        };

        let completion = if let Some(capability) = capability {
            // Resolve or reject the capability with the result of the handler
            match handler_result {
                Ok(handler_result) => {
                    let resolve = capability.resolve();
                    call_object(cx, resolve, cx.undefined(), &[handler_result])
                }
                Err(handler_result) => {
                    let reject = capability.reject();
                    call_object(cx, reject, cx.undefined(), &[handler_result])
                }
            }
        } else {
            debug_assert!(handler_result.is_ok());
            Ok(cx.undefined())
        };

        // Make sure we clean up the realm's stack frame before returning or throwing
        if realm.is_some() {
            cx.vm().pop_initial_realm_stack_frame();
        }

        maybe!(completion);

        Ok(())
    }
}

/// Call a `then` function with new `resolve` and `reject` functions in order to settle a promise.
pub struct PromiseThenSettleTask {
    /// The `then` function that should be called.
    then_function: HeapPtr<ObjectValue>,
    /// The object that contains the `then` function, used as the `this` value when calling `then`.
    resolution: HeapPtr<ObjectValue>,
    /// The promise that will be settled by calling `then`.
    promise: HeapPtr<PromiseObject>,
    /// The realm to set as the topmost execution context before executing the `then` function.
    realm: HeapPtr<Realm>,
}

impl PromiseThenSettleTask {
    fn new(
        then_function: HeapPtr<ObjectValue>,
        resolution: HeapPtr<ObjectValue>,
        promise: HeapPtr<PromiseObject>,
        realm: HeapPtr<Realm>,
    ) -> Self {
        Self { then_function, resolution, promise, realm }
    }

    fn execute(&self, mut cx: Context) -> EvalResult<()> {
        maybe!(cx.vm().push_initial_realm_stack_frame(self.realm));

        let then_function = self.then_function.to_handle();
        let resolution = self.resolution.to_handle().into();
        let promise = self.promise.to_handle();

        let completion = execute_then(cx, then_function, resolution, promise);

        // Make sure we clean up the realm's stack frame before returning or throwing
        cx.vm().pop_initial_realm_stack_frame();

        maybe!(completion);

        Ok(())
    }
}
