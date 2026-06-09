use crate::{
    if_abrupt_reject_promise,
    runtime::{
        Context, EvalResult, Handle, HeapPtr, Realm, Value,
        abstract_operations::call_object,
        alloc_error::AllocResult,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
        intrinsics::array_from_async_generator::{ArrayFromAsyncGenerator, ArrayFromAsyncState},
        promise_object::{PromiseCapability, resolve},
    },
    set_uninit,
};

/// Builtin functions may be generators which can be suspended and resumed at a later point.
///
/// This object stores the function's suspended state and all necessary info needed for the function
/// to continue executing from that point.
#[repr(C)]
pub struct BuiltinGenerator {
    descriptor: HeapPtr<HeapItemDescriptor>,
    /// Realm to use when resuming the function.
    realm: HeapPtr<Realm>,
    /// Resume point and all state needed to resume function.
    state: BuiltinGeneratorState,
}

pub enum BuiltinGeneratorState {
    ArrayFromAsync {
        /// Promise capability for the result of Array.fromAsync
        capability: HeapPtr<PromiseCapability>,
        state: ArrayFromAsyncState,
    },
}

impl BuiltinGenerator {
    #[inline]
    pub fn new(
        cx: Context,
        create_state_fn: impl FnOnce() -> BuiltinGeneratorState,
    ) -> AllocResult<Handle<Self>> {
        let mut generator = cx.alloc_uninit::<BuiltinGenerator>()?.to_handle();

        set_uninit!(generator.descriptor, cx.descriptors.get(HeapItemKind::BuiltinGenerator));
        set_uninit!(generator.realm, cx.current_realm_ptr());
        set_uninit!(generator.state, create_state_fn());

        Ok(generator)
    }

    pub fn realm_ptr(&self) -> HeapPtr<Realm> {
        self.realm
    }

    pub fn resume(
        &self,
        cx: Context,
        completion: EvalResult<Handle<Value>>,
    ) -> EvalResult<Handle<Value>> {
        match &self.state {
            BuiltinGeneratorState::ArrayFromAsync { capability, state } => {
                ArrayFromAsyncGenerator::resume(cx, capability.to_handle(), completion, state)
            }
        }
    }

    /// Handle the completion from running a builtin async function.
    ///
    /// Builtin async function could have returned, thrown, or suspended. If a value was returned
    /// or thrown the provided promise is resolved/rejected.
    pub fn handle_async_function_completion(
        cx: Context,
        capability: Handle<PromiseCapability>,
        generator_result: EvalResult<BuiltinGeneratorCompletion>,
    ) -> EvalResult<Handle<Value>> {
        let promise = capability.promise().as_promise().unwrap();

        let from_async_result = if_abrupt_reject_promise!(cx, generator_result, capability);

        if let BuiltinGeneratorCompletion::Returned(value) = from_async_result {
            let resolve_result = resolve(cx, promise, value);
            if_abrupt_reject_promise!(cx, resolve_result, capability);
        }

        Ok(promise.as_value())
    }
}

/// Non-erroring result of running a generator.
pub enum BuiltinGeneratorCompletion {
    Suspended,
    Returned(Handle<Value>),
}
impl HeapItem for HeapPtr<BuiltinGenerator> {
    fn byte_size(&self) -> usize {
        std::mem::size_of::<BuiltinGenerator>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.realm);

        match &mut self.state {
            BuiltinGeneratorState::ArrayFromAsync { capability, state } => {
                visitor.visit_pointer(capability);
                state.visit_pointers(visitor);
            }
        }
    }
}
