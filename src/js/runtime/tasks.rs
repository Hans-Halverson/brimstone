use std::collections::VecDeque;

use crate::{
    js::runtime::{
        abstract_operations::call_object, intrinsics::promise_constructor::execute_then,
    },
    maybe,
};

use super::{
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

    pub fn enqueue_await_resume_task(
        &mut self,
        kind: PromiseReactionKind,
        suspended_function: HeapPtr<GeneratorObject>,
        result: Value,
    ) {
        self.enqueue(Task::AwaitResume(AwaitResumeTask::new(kind, suspended_function, result)));
    }

    pub fn enqueue_promise_then_reaction_task(
        &mut self,
        kind: PromiseReactionKind,
        handler: Option<HeapPtr<ObjectValue>>,
        capability: HeapPtr<PromiseCapability>,
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
                Task::AwaitResume(AwaitResumeTask { suspended_function, result, .. }) => {
                    visitor.visit_pointer(suspended_function);
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
                    visitor.visit_pointer(capability);
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
                    Task::AwaitResume(task) => task.execute(cx),
                    Task::PromiseThenReaction(task) => task.execute(cx),
                    Task::PromiseThenSettle(task) => task.execute(cx),
                }
            }));
        }

        ().into()
    }
}

/// Resume an async function that was paused at an `await` expression.
pub struct AwaitResumeTask {
    /// Whether the awaited promise was resolved or rejected.
    kind: PromiseReactionKind,
    /// The suspended async function that should be resumed with the provided completion.
    suspended_function: HeapPtr<GeneratorObject>,
    /// The value the await expression completes to, whether a normal value or thrown error.
    result: Value,
}

impl AwaitResumeTask {
    fn new(
        kind: PromiseReactionKind,
        suspended_function: HeapPtr<GeneratorObject>,
        result: Value,
    ) -> Self {
        Self { kind, suspended_function, result }
    }

    fn execute(&self, mut cx: Context) -> EvalResult<()> {
        let generator = self.suspended_function.to_handle();
        let completion_value = self.result.to_handle(cx);
        let completion_type = match self.kind {
            PromiseReactionKind::Fulfill => GeneratorCompletionType::Normal,
            PromiseReactionKind::Reject => GeneratorCompletionType::Throw,
        };

        maybe!(cx
            .vm()
            .resume_generator(generator, completion_value, completion_type));

        ().into()
    }
}

pub struct PromiseThenReactionTask {
    /// Whether the promise was resolved or rejected.
    kind: PromiseReactionKind,
    /// A function to call on the result value.
    handler: Option<HeapPtr<ObjectValue>>,
    /// A promise capability to resolve or reject with the result of the handler function.
    capability: HeapPtr<PromiseCapability>,
    /// The value that the promise was resolved or rejected with.
    result: Value,
    /// The realm to set as the topmost execution context before executing the handler.
    realm: Option<HeapPtr<Realm>>,
}

impl PromiseThenReactionTask {
    fn new(
        kind: PromiseReactionKind,
        handler: Option<HeapPtr<ObjectValue>>,
        capability: HeapPtr<PromiseCapability>,
        result: Value,
        realm: Option<HeapPtr<Realm>>,
    ) -> Self {
        Self { kind, handler, capability, result, realm }
    }

    fn execute(&self, mut cx: Context) -> EvalResult<()> {
        if let Some(realm) = self.realm {
            cx.vm().push_initial_realm_stack_frame(realm);
        }

        let result = self.result.to_handle(cx);
        let capability = self.capability.to_handle();
        let realm = self.realm.map(|r| r.to_handle());

        // Call the handler if it exists on the result value
        let handler_result = if let Some(handler) = self.handler {
            let handler = handler.to_handle();
            call_object(cx, handler, cx.undefined(), &[result])
        } else {
            // If no handler was provided treat the handler result as a default normal or throw
            match self.kind {
                PromiseReactionKind::Fulfill => EvalResult::Ok(result),
                PromiseReactionKind::Reject => EvalResult::Throw(result),
            }
        };

        // Resolve or reject the capability with the result of the handler
        let completion = match handler_result {
            EvalResult::Ok(handler_result) => {
                let resolve = capability.resolve();
                call_object(cx, resolve, cx.undefined(), &[handler_result])
            }
            EvalResult::Throw(handler_result) => {
                let reject = capability.reject();
                call_object(cx, reject, cx.undefined(), &[handler_result])
            }
        };

        // Make sure we clean up the realm's stack frame before returning or throwing
        if realm.is_some() {
            cx.vm().pop_initial_realm_stack_frame();
        }

        maybe!(completion);

        ().into()
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
        cx.vm().push_initial_realm_stack_frame(self.realm);

        let then_function = self.then_function.to_handle();
        let resolution = self.resolution.to_handle().into();
        let promise = self.promise.to_handle();

        let completion = execute_then(cx, then_function, resolution, promise);

        // Make sure we clean up the realm's stack frame before returning or throwing
        cx.vm().pop_initial_realm_stack_frame();

        maybe!(completion);

        ().into()
    }
}
