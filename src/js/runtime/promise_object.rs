use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        abstract_operations::{call_object, construct},
        builtin_function::BuiltinFunction,
        error::type_error,
        gc::{HeapObject, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{object_create, object_create_from_constructor},
        type_utilities::{is_callable, is_constructor_value},
        value::Value,
        Context, HeapPtr,
    },
    maybe, set_uninit,
};

use super::{
    function::get_argument,
    generator_object::GeneratorObject,
    get,
    object_descriptor::ObjectDescriptor,
    type_utilities::{same_object_value, same_value},
    EvalResult, Handle,
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

    pub fn new_from_constructor(
        cx: Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<PromiseObject>> {
        let mut object = maybe!(object_create_from_constructor::<PromiseObject>(
            cx,
            constructor,
            ObjectKind::Promise,
            Intrinsic::PromisePrototype
        ));

        set_uninit!(object.state, PromiseState::Pending { reactions: None });

        object.to_handle().into()
    }

    pub fn is_pending(&self) -> bool {
        matches!(self.state, PromiseState::Pending { .. })
    }

    /// 27.2.1.4 FulfillPromise
    pub fn resolve(&mut self, cx: Context, value: Value) {
        self.enqueue_tasks_for_reactions(cx, PromiseReactionKind::Fulfill, value);
        self.state = PromiseState::Fulfilled { result: value };
    }

    /// 27.2.1.7 RejectPromise
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

/// Creates a new promise with the provided constructor and immediately resolves it with a result.
///
/// 27.2.4.7.1 PromiseResolve
pub fn promise_resolve(
    cx: Context,
    constructor: Handle<Value>,
    result: Handle<Value>,
) -> EvalResult<Handle<ObjectValue>> {
    // If result is already a promise, return it if it was constructed with the same constructor.
    if is_promise(result.get()) {
        let result = result.as_object();
        let value_constructor = maybe!(get(cx, result.into(), cx.names.constructor()));
        if same_value(value_constructor, constructor) {
            return result.into();
        }
    }

    // Create a new promise and immediately resolve it
    let capability = maybe!(PromiseCapability::new(cx, constructor));
    maybe!(call_object(cx, capability.resolve(), cx.undefined(), &[result]));

    capability.promise().into()
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

/// A promise along with its resolve and reject functions.
#[repr(C)]
pub struct PromiseCapability {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// The promise object. Guaranteed to be Some after construction.
    promise: Option<HeapPtr<ObjectValue>>,
    /// The resolve function for the promise. Guaranteed to be a callable object after construction.
    resolve: Value,
    /// The reject function for the promise. Guaranteed to be a callable object after construction.
    reject: Value,
}

impl PromiseCapability {
    /// 27.2.1.5 NewPromiseCapability
    pub fn new(cx: Context, constructor: Handle<Value>) -> EvalResult<Handle<PromiseCapability>> {
        if !is_constructor_value(constructor) {
            return type_error(cx, "expected constructor");
        }
        let constructor = constructor.as_object();

        // Create an empty capability object whose fields will be set later
        let mut capability = cx.alloc_uninit::<PromiseCapability>();

        set_uninit!(capability.descriptor, cx.base_descriptors.get(ObjectKind::PromiseCapability));
        set_uninit!(capability.promise, None);
        set_uninit!(capability.resolve, Value::undefined());
        set_uninit!(capability.reject, Value::undefined());

        let mut capability = capability.to_handle();

        // Create the executor function and attach the capability object to it
        let mut executor = BuiltinFunction::create(
            cx,
            Self::executor,
            2,
            cx.names.empty_string(),
            cx.current_realm(),
            None,
            None,
        );

        executor.private_element_set(
            cx,
            cx.well_known_symbols.capability().cast(),
            capability.into(),
        );

        // Construct the promise using the provided constructor. This will fill the resolve and
        // reject fields of the capability.
        let promise = maybe!(construct(cx, constructor, &[executor.into()], None));

        if !is_callable(capability.resolve.to_handle(cx)) {
            return type_error(cx, "resolve must be callable");
        } else if !is_callable(capability.reject.to_handle(cx)) {
            return type_error(cx, "reject must be callable");
        }

        // Finally store the promise in the capability record, completing it
        capability.promise = Some(promise.get_());

        capability.into()
    }

    pub fn promise(&self) -> Handle<ObjectValue> {
        self.promise.unwrap().to_handle()
    }

    pub fn resolve(&self) -> Handle<ObjectValue> {
        self.resolve.as_object().to_handle()
    }

    pub fn reject(&self) -> Handle<ObjectValue> {
        self.reject.as_object().to_handle()
    }

    pub fn executor(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        // Get the capability object that was attached to the executor function
        let current_function = cx.current_function();
        let mut capability = current_function
            .private_element_find(cx, cx.well_known_symbols.capability().cast())
            .unwrap()
            .value()
            .cast::<PromiseCapability>();

        // Check if the resolve and reject fields have already been set
        if !capability.resolve.is_undefined() {
            return type_error(cx, "resolve already set");
        } else if !capability.reject.is_undefined() {
            return type_error(cx, "reject already set");
        }

        // If not, set them from the executor's arguments
        let resolve = get_argument(cx, arguments, 0);
        let reject = get_argument(cx, arguments, 1);

        capability.resolve = resolve.get();
        capability.reject = reject.get();

        cx.undefined().into()
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

impl HeapObject for HeapPtr<PromiseCapability> {
    fn byte_size(&self) -> usize {
        size_of::<PromiseCapability>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer_opt(&mut self.promise);
        visitor.visit_value(&mut self.resolve);
        visitor.visit_value(&mut self.reject);
    }
}
