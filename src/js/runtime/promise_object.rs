use std::mem::size_of;

use crate::{
    extend_object,
    js::runtime::{
        abstract_operations::{call_object, construct},
        builtin_function::BuiltinFunction,
        error::{type_error, type_error_value},
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
    abstract_operations::get_function_realm_no_error,
    function::get_argument,
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
        /// A flag to mark a promise that has already started resolution, even if it has not yet
        /// transitioned to a fulfilled or rejected state with a result value.
        already_resolved: bool,
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
    /// The functions to be called when the promise is settled.
    handler: ReactionHandler,
    /// The next reaction in the chain of reactions.
    next: Option<HeapPtr<PromiseReaction>>,
}

enum ReactionHandler {
    AwaitResume {
        /// An async function suspended at an await expression. Is a regular generator object for
        /// async functions and an async generator object for async generators.
        suspended_generator: HeapPtr<ObjectValue>,
    },
    Then {
        /// A function to be called when the promise is fulfilled, if one exists.
        fulfill_handler: Option<HeapPtr<ObjectValue>>,
        /// A function to be called when the promise is rejected, if one exists.
        reject_handler: Option<HeapPtr<ObjectValue>>,
        /// A capability that will be resolved or rejected depending on the reaction type.
        capability: Option<HeapPtr<PromiseCapability>>,
    },
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

        set_uninit!(
            object.state,
            PromiseState::Pending { reactions: None, already_resolved: false }
        );

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

        set_uninit!(
            object.state,
            PromiseState::Pending { reactions: None, already_resolved: false }
        );

        object.to_handle().into()
    }

    pub fn is_pending(&self) -> bool {
        matches!(self.state, PromiseState::Pending { already_resolved: false, .. })
    }

    pub fn set_already_resolved(&mut self, value: bool) {
        match self.state {
            PromiseState::Pending { ref mut already_resolved, .. } => *already_resolved = value,
            _ => unreachable!("only called when promise is pending"),
        }
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
        let mut next_reaction = if let PromiseState::Pending { reactions, .. } = &mut self.state {
            *reactions
        } else {
            unreachable!("only called when promise is pending");
        };

        let mut matching_reactions = vec![];

        // Traverse linked list of reactions, adding to Vec so they can be processed in reverse
        // order.
        while let Some(reaction) = next_reaction {
            matching_reactions.push(reaction);
            next_reaction = reaction.next;
        }

        // Add a task to the queue for each matching reaction in the order they were added
        for reaction in matching_reactions.into_iter().rev() {
            match reaction.handler {
                ReactionHandler::AwaitResume { suspended_generator } => {
                    cx.task_queue()
                        .enqueue_await_resume_task(kind, suspended_generator, value);
                }
                ReactionHandler::Then { fulfill_handler, reject_handler, capability } => {
                    let handler = match kind {
                        PromiseReactionKind::Fulfill => fulfill_handler,
                        PromiseReactionKind::Reject => reject_handler,
                    };
                    enqueue_promise_then_reaction_task(cx, kind, handler, capability, value);
                }
            }
        }
    }
}

/// 27.2.1.3.2 Promise Resolve Functions
pub fn resolve(mut cx: Context, mut promise: Handle<PromiseObject>, resolution: Handle<Value>) {
    // Resolving an already settled promise has no effect. Immediately mark promise as
    // "already resolved" to prevent further settlement, since fulfill or reject may not be called
    // right away.
    match &mut promise.state {
        PromiseState::Pending { already_resolved, .. } => {
            if *already_resolved {
                return;
            }

            *already_resolved = true;
        }
        _ => return,
    }

    // Check if a promise is trying to resolve itself
    if same_value(resolution, promise.into()) {
        let self_resolution_error = type_error_value(cx, "cannot resolve promise with itself");
        promise.reject(cx, self_resolution_error.get());
        return;
    }

    // Resolving to a non-object immediately fulfills the promise
    if !resolution.is_object() {
        promise.resolve(cx, resolution.get());
        return;
    }

    // Otherwise look for a "then" property on the resolution object
    let then_completion = get(cx, resolution.as_object(), cx.names.then());
    let then_value = match then_completion {
        EvalResult::Ok(value) => value,
        EvalResult::Throw(error) => {
            promise.reject(cx, error.get());
            return;
        }
    };

    // If "then" is not callable, immediately fulfill the promise
    if !is_callable(then_value) {
        promise.resolve(cx, resolution.get());
        return;
    }

    // Get the realm of the "then" function, defaulting to the current realm if getting the
    // realm fails (i.e. the "then" function is a revoked proxy).
    let realm = match get_function_realm_no_error(cx, then_value.as_object()) {
        Some(realm) => realm,
        None => cx.current_realm_ptr(),
    };

    // Otherwise enqueue "then" to be called
    cx.task_queue().enqueue_promise_then_settle_task(
        then_value.as_object().get_(),
        resolution.as_object().get_(),
        promise.get_(),
        realm,
    );
}

impl Handle<PromiseObject> {
    fn set_reactions(&mut self, value: Option<HeapPtr<PromiseReaction>>) {
        match self.state {
            PromiseState::Pending { ref mut reactions, .. } => *reactions = value,
            _ => unreachable!("only called when promise is pending"),
        }
    }

    pub fn add_await_reaction(
        &mut self,
        mut cx: Context,
        suspended_generator: Handle<ObjectValue>,
    ) {
        match &mut self.state {
            // Prepend reaction onto the current linked list of reactions.
            PromiseState::Pending { reactions, .. } => {
                let prev_reactions = reactions.map(|r| r.to_handle());
                let new_reactions = Some(PromiseReaction::new_await_resume(
                    cx,
                    suspended_generator,
                    prev_reactions,
                ));

                self.set_reactions(new_reactions);
            }
            PromiseState::Fulfilled { result } => {
                cx.task_queue().enqueue_await_resume_task(
                    PromiseReactionKind::Fulfill,
                    suspended_generator.get_(),
                    *result,
                );
            }
            PromiseState::Rejected { result } => {
                cx.task_queue().enqueue_await_resume_task(
                    PromiseReactionKind::Reject,
                    suspended_generator.get_(),
                    *result,
                );
            }
        }
    }

    pub fn add_then_reaction(
        &mut self,
        cx: Context,
        fulfill_handler: Option<Handle<ObjectValue>>,
        reject_handler: Option<Handle<ObjectValue>>,
        capability: Option<Handle<PromiseCapability>>,
    ) {
        match &mut self.state {
            // Prepend reaction onto the current linked list of reactions.
            PromiseState::Pending { reactions, .. } => {
                let prev_reactions = reactions.map(|r| r.to_handle());
                let new_reactions = Some(PromiseReaction::new_then(
                    cx,
                    fulfill_handler,
                    reject_handler,
                    capability,
                    prev_reactions,
                ));

                self.set_reactions(new_reactions);
            }
            PromiseState::Fulfilled { result } => {
                enqueue_promise_then_reaction_task(
                    cx,
                    PromiseReactionKind::Fulfill,
                    fulfill_handler.map(|h| h.get_()),
                    capability.map(|c| c.get_()),
                    *result,
                );
            }
            PromiseState::Rejected { result } => {
                enqueue_promise_then_reaction_task(
                    cx,
                    PromiseReactionKind::Reject,
                    reject_handler.map(|h| h.get_()),
                    capability.map(|c| c.get_()),
                    *result,
                );
            }
        }
    }
}

/// 27.2.1.6 IsPromise
pub fn is_promise(value: Value) -> bool {
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

    let promise = PromiseObject::new_pending(cx).to_handle();
    resolve(cx, promise, value);

    promise.into()
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
        let value_constructor = maybe!(get(cx, result, cx.names.constructor()));
        if same_value(value_constructor, constructor) {
            return result.into();
        }
    }

    // Create a new promise and immediately resolve it
    let capability = maybe!(PromiseCapability::new(cx, constructor));
    maybe!(call_object(cx, capability.resolve(), cx.undefined(), &[result]));

    capability.promise().into()
}

fn enqueue_promise_then_reaction_task(
    mut cx: Context,
    kind: PromiseReactionKind,
    handler: Option<HeapPtr<ObjectValue>>,
    capability: Option<HeapPtr<PromiseCapability>>,
    result: Value,
) {
    // Get the realm of the handler function, defaulting to the current realm if getting the
    // realm fails (i.e. the handler function is a revoked proxy).
    let realm = match handler {
        Some(handler) => match get_function_realm_no_error(cx, handler.to_handle()) {
            Some(realm) => Some(realm),
            None => Some(cx.current_realm_ptr()),
        },
        None => None,
    };

    cx.task_queue()
        .enqueue_promise_then_reaction_task(kind, handler, capability, result, realm);
}

impl PromiseReaction {
    fn new_await_resume(
        cx: Context,
        suspended_generator: Handle<ObjectValue>,
        next: Option<Handle<PromiseReaction>>,
    ) -> HeapPtr<PromiseReaction> {
        let mut reaction = cx.alloc_uninit::<PromiseReaction>();

        set_uninit!(reaction.descriptor, cx.base_descriptors.get(ObjectKind::PromiseReaction));
        set_uninit!(
            reaction.handler,
            ReactionHandler::AwaitResume { suspended_generator: suspended_generator.get_() }
        );
        set_uninit!(reaction.next, next.map(|r| r.get_()));

        reaction
    }

    fn new_then(
        cx: Context,
        fulfill_handler: Option<Handle<ObjectValue>>,
        reject_handler: Option<Handle<ObjectValue>>,
        capability: Option<Handle<PromiseCapability>>,
        next: Option<Handle<PromiseReaction>>,
    ) -> HeapPtr<PromiseReaction> {
        let mut reaction = cx.alloc_uninit::<PromiseReaction>();

        set_uninit!(reaction.descriptor, cx.base_descriptors.get(ObjectKind::PromiseReaction));
        set_uninit!(
            reaction.handler,
            ReactionHandler::Then {
                fulfill_handler: fulfill_handler.map(|h| h.get_()),
                reject_handler: reject_handler.map(|h| h.get_()),
                capability: capability.map(|c| c.get_()),
            }
        );
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
            PromiseState::Pending { reactions, .. } => {
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
        match &mut self.handler {
            ReactionHandler::AwaitResume { suspended_generator } => {
                visitor.visit_pointer(suspended_generator);
            }
            ReactionHandler::Then { fulfill_handler, reject_handler, capability } => {
                visitor.visit_pointer_opt(fulfill_handler);
                visitor.visit_pointer_opt(reject_handler);
                visitor.visit_pointer_opt(capability);
            }
        }
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
