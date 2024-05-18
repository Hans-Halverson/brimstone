use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        gc::{HeapObject, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create,
        value::Value,
        Context, HeapPtr,
    },
    maybe, set_uninit,
};

use super::{
    generator_object::GeneratorObject, get, object_descriptor::ObjectDescriptor,
    type_utilities::same_object_value, EvalResult, Handle,
};

// 27.2.6 Properties of Promise Instances
extend_object! {
    pub struct PromiseObject {
        state: PromiseState,
    }
}

enum PromiseState {
    Pending {
        /// A chain of all reactions to be executed when the promise is settled. The first reaction
        /// is the newest and the last is the oldest.
        reactions: Option<HeapPtr<PromiseReaction>>,
    },
    Fulfilled {
        /// The value passed to resolve.
        result: Value,
    },
    Rejected {
        /// The value passed to reject.
        result: Value,
    },
}

/// A function to be called when a promise is settled.
#[repr(C)]
pub struct PromiseReaction {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Whether this is a reaction for when the promise is fulfilled.
    for_fulfill: bool,
    /// Whether this is a reaction for when the promise is rejected.
    for_reject: bool,
    /// The function that should be called when the promise is settled. This may be either a Closure
    /// if a function was registered with Promise.prototype.{catch, finally, then}, otherwise must
    /// be a GeneratorObject representing an async function suspended at an await expression.
    handler: HeapPtr<ObjectValue>,
    /// The promise this reaction references. The promise must be fulfilled or rejected and stores
    /// the value that the promise was fulfilled or rejected with.
    // promise: HeapPtr<PromiseObject>,
    /// The next reaction in the chain of reactions.
    next: Option<HeapPtr<PromiseReaction>>,
}

#[derive(Clone, Copy)]
pub enum PromiseReactionKind {
    Fulfill,
    Reject,
}

impl PromiseObject {
    pub fn new_pending(cx: Context) -> HeapPtr<PromiseObject> {
        let mut object =
            object_create::<PromiseObject>(cx, ObjectKind::Promise, Intrinsic::PromisePrototype);

        set_uninit!(object.state, PromiseState::Pending { reactions: None });

        object
    }

    pub fn new_resolved(cx: Context, result: Handle<Value>) -> HeapPtr<PromiseObject> {
        let mut object =
            object_create::<PromiseObject>(cx, ObjectKind::Promise, Intrinsic::PromisePrototype);

        set_uninit!(object.state, PromiseState::Fulfilled { result: result.get() });

        object
    }

    pub fn resolve(&mut self, cx: Context, value: Value) {
        self.enqueue_tasks_for_reactions(cx, PromiseReactionKind::Fulfill, value);
        self.state = PromiseState::Fulfilled { result: value };
    }

    pub fn reject(&mut self, cx: Context, value: Value) {
        self.enqueue_tasks_for_reactions(cx, PromiseReactionKind::Reject, value);
        self.state = PromiseState::Rejected { result: value };
    }

    fn enqueue_tasks_for_reactions(
        &mut self,
        mut cx: Context,
        kind: PromiseReactionKind,
        value: Value,
    ) {
        let mut next_reaction = if let PromiseState::Pending { reactions } = &mut self.state {
            reactions.clone()
        } else {
            unreachable!("only called when promise is pending");
        };

        let mut matching_reactions = vec![];

        // Traverse linked list of reactions, adding to Vec so they can be processed in reverse
        // order.
        while let Some(reaction) = next_reaction {
            let is_match = match kind {
                PromiseReactionKind::Fulfill => reaction.for_fulfill,
                PromiseReactionKind::Reject => reaction.for_reject,
            };

            if is_match {
                matching_reactions.push(reaction);
            }

            next_reaction = reaction.next;
        }

        // Add a task to the queue for each matching reaction in the order they were added
        for reaction in matching_reactions.into_iter().rev() {
            let handler = reaction.handler;
            if handler.is_generator() {
                let suspended_async_function = handler.cast::<GeneratorObject>();
                cx.task_queue()
                    .enqueue_await_resume_task(kind, suspended_async_function, value);
            } else {
                unimplemented!("tasks for non-generator promise reactions")
            }
        }
    }
}

impl Handle<PromiseObject> {
    /// Prepend reaction onto the current linked list of reactions.
    fn add_reaction(
        &mut self,
        cx: Context,
        is_fullfill: bool,
        is_reject: bool,
        handler: Handle<ObjectValue>,
    ) {
        if let PromiseState::Pending { reactions } = &mut self.state {
            let prev_reactions = reactions.map(|r| r.to_handle());
            *reactions =
                Some(PromiseReaction::new(cx, is_fullfill, is_reject, handler, prev_reactions));
        } else {
            unreachable!("only called when promise is pending");
        }
    }

    pub fn add_await_reaction(
        &mut self,
        mut cx: Context,
        suspended_async_function: Handle<GeneratorObject>,
    ) {
        let handler: Handle<ObjectValue> = suspended_async_function.into();
        match &mut self.state {
            PromiseState::Pending { .. } => self
                .add_reaction(cx, /* is_fulfill */ true, /* is_reject */ true, handler),
            PromiseState::Fulfilled { result } => {
                cx.task_queue().enqueue_await_resume_task(
                    PromiseReactionKind::Fulfill,
                    suspended_async_function.get_(),
                    *result,
                );
            }
            PromiseState::Rejected { result } => {
                cx.task_queue().enqueue_await_resume_task(
                    PromiseReactionKind::Reject,
                    suspended_async_function.get_(),
                    *result,
                );
            }
        }
    }
}

/// 27.2.1.6 IsPromise
fn is_promise(value: Value) -> bool {
    value.is_object() && value.as_object().is_promise()
}

/// Coerce a value to an ordinary promise (aka the Promise constructor). An ordinary promise is
/// returned as-is, otherwise value is wrapped in a resolved promise.
///
/// 27.2.4.7.1 PromiseResolve specialized for the promise constructor.
pub fn coerce_to_ordinary_promise(
    cx: Context,
    value: Handle<Value>,
) -> EvalResult<Handle<PromiseObject>> {
    if is_promise(value.get()) {
        let value = value.cast::<PromiseObject>();

        let value_constructor = maybe!(get(cx, value.into(), cx.names.constructor()));
        let promise_constructor = cx.get_intrinsic_ptr(Intrinsic::PromiseConstructor);
        if value_constructor.is_object()
            && same_object_value(value_constructor.as_object().get_(), promise_constructor)
        {
            return value.into();
        }
    }

    PromiseObject::new_resolved(cx, value).to_handle().into()
}

impl PromiseReaction {
    pub fn new(
        cx: Context,
        for_fulfill: bool,
        for_reject: bool,
        handler: Handle<ObjectValue>,
        next: Option<Handle<PromiseReaction>>,
    ) -> HeapPtr<PromiseReaction> {
        let mut reaction = cx.alloc_uninit::<PromiseReaction>();

        set_uninit!(reaction.descriptor, cx.base_descriptors.get(ObjectKind::PromiseReaction));
        set_uninit!(reaction.for_fulfill, for_fulfill);
        set_uninit!(reaction.for_reject, for_reject);
        set_uninit!(reaction.handler, handler.get_());
        set_uninit!(reaction.next, next.map(|r| r.get_()));

        reaction
    }
}

impl HeapObject for HeapPtr<PromiseObject> {
    fn byte_size(&self) -> usize {
        size_of::<PromiseObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);

        match &mut self.state {
            PromiseState::Pending { reactions } => {
                visitor.visit_pointer_opt(reactions);
            }
            PromiseState::Fulfilled { result } | PromiseState::Rejected { result } => {
                visitor.visit_value(result);
            }
        }
    }
}

impl HeapObject for HeapPtr<PromiseReaction> {
    fn byte_size(&self) -> usize {
        size_of::<PromiseReaction>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.handler);
        visitor.visit_pointer_opt(&mut self.next);
    }
}
