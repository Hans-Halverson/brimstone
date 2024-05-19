use std::collections::VecDeque;

use crate::{js::runtime::intrinsics::promise_constructor::execute_then, maybe};

use super::{
    gc::HeapVisitor,
    generator_object::{GeneratorCompletionType, GeneratorObject},
    object_value::ObjectValue,
    promise_object::{PromiseObject, PromiseReactionKind},
    Context, EvalResult, HeapPtr, Value,
};

pub struct TaskQueue {
    tasks: VecDeque<Task>,
}

pub enum Task {
    AwaitResume(AwaitResumeTask),
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

    pub fn enqueue_promise_then_settle_task(
        &mut self,
        then_function: HeapPtr<ObjectValue>,
        resolution: HeapPtr<ObjectValue>,
        promise: HeapPtr<PromiseObject>,
    ) {
        self.enqueue(Task::PromiseThenSettle(PromiseThenSettleTask::new(
            then_function,
            resolution,
            promise,
        )));
    }

    pub fn visit_roots(&mut self, visitor: &mut impl HeapVisitor) {
        for task in &mut self.tasks {
            match task {
                Task::AwaitResume(AwaitResumeTask { suspended_function, result, .. }) => {
                    visitor.visit_pointer(suspended_function);
                    visitor.visit_value(result);
                }
                Task::PromiseThenSettle(PromiseThenSettleTask {
                    then_function,
                    resolution,
                    promise,
                }) => {
                    visitor.visit_pointer(then_function);
                    visitor.visit_pointer(resolution);
                    visitor.visit_pointer(promise);
                }
            }
        }
    }
}

impl Context {
    /// Run all tasks until the task queue is empty.
    pub fn run_all_tasks(&mut self) -> EvalResult<()> {
        while let Some(task) = self.task_queue().tasks.pop_front() {
            match task {
                Task::AwaitResume(task) => maybe!(task.execute(*self)),
                Task::PromiseThenSettle(task) => maybe!(task.execute(*self)),
            }
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

/// Call then `then` function with new `resolve` and `reject` functions in order to settle a
/// promise.
pub struct PromiseThenSettleTask {
    /// The `then` function to
    then_function: HeapPtr<ObjectValue>,
    /// The object that contains the `then` function, used as the `this` value when calling `then`.
    resolution: HeapPtr<ObjectValue>,
    /// The promise that will be settled by calling `then`.
    promise: HeapPtr<PromiseObject>,
}

impl PromiseThenSettleTask {
    fn new(
        then_function: HeapPtr<ObjectValue>,
        resolution: HeapPtr<ObjectValue>,
        promise: HeapPtr<PromiseObject>,
    ) -> Self {
        Self { then_function, resolution, promise }
    }

    fn execute(&self, cx: Context) -> EvalResult<()> {
        let then_function = self.then_function.to_handle();
        let resolution = self.resolution.to_handle().into();
        let promise = self.promise.to_handle();

        maybe!(execute_then(cx, then_function, resolution, promise));

        ().into()
    }
}
