use crate::{
    extend_object, field_offset,
    js::runtime::{
        completion::EvalResult,
        gc::{HeapObject, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        iterator::create_iter_result_object,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{get_prototype_from_constructor, object_ordinary_init},
        Context, Handle, HeapPtr,
    },
    maybe, set_uninit,
};

use super::{
    bytecode::{
        function::Closure,
        stack_frame::{StackFrame, StackSlotValue},
    },
    collections::InlineArray,
    error::type_error,
    Value,
};

// A generator object represents the state of a generator function. It holds the saved stack frame
// of the generator function, which is restored when the generator is resumed.
extend_object! {
    pub struct GeneratorObject {
        // The current state of the generator - may be executing, suspended, or completed.
        state: GeneratorState,
        // Address of the next instruction to execute when this generator is resumed.
        // Stored as a byte offset into the BytecodeFunction.
        pc_to_resume_offset: usize,
        // Index of the frame pointer in the stack frame.
        fp_index: usize,
        // Indices of registers for the completion operands to return when the generator is resumed.
        // The value is written to the first register and the completion type is written to the
        // second register.
        //
        // For a generator the value is from Generator.prototype.{next, return, throw}.
        // For an async function the value is from resolve or reject.
        completion_indices: Option<(u32, u32)>,
        // The stack frame of the generator, containing all args, locals, and fixed slots in
        // between.
        stack_frame: InlineArray<StackSlotValue>,
    }
}

#[derive(PartialEq)]
enum GeneratorState {
    /// Generator has been created but has not started executing yet.
    SuspendedStart,
    /// Generator is not executing since it has yielded.
    SuspendedYield,
    /// Generator is currently executing.
    Executing,
    /// Generator has completed. Once a generator is in the completed state it never leaves.
    Completed,
}

impl GeneratorState {
    fn is_suspended(&self) -> bool {
        matches!(self, Self::SuspendedStart | Self::SuspendedYield)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum GeneratorCompletionType {
    Normal,
    Return,
    Throw,
}

impl GeneratorCompletionType {
    /// Return the representative runtime value for this completion type. This is the value stored
    /// in the second register of a yield instruction, and will be checked at runtime to determine
    /// the completion type.
    ///
    /// The abormal completions are nullish so we can check them with a single JumpNotNullish
    /// instruction.
    pub fn to_value(&self) -> Value {
        match self {
            Self::Normal => Value::bool(true),
            Self::Return => Value::undefined(),
            Self::Throw => Value::null(),
        }
    }
}

/// Trait that generalizes between generator and async generator objects.
pub trait TGeneratorObject {
    fn pc_to_resume_offset(&self) -> usize;

    fn fp_index(&self) -> usize;

    fn completion_indices(&self) -> Option<(u32, u32)>;

    fn stack_frame(&self) -> &[StackSlotValue];
}

impl GeneratorObject {
    fn new(
        cx: Context,
        prototype: Option<Handle<ObjectValue>>,
        pc_to_resume_offset: usize,
        fp_index: usize,
        completion_indices: Option<(u32, u32)>,
        stack_frame: &[StackSlotValue],
    ) -> HeapPtr<GeneratorObject> {
        let size = Self::calculate_size_in_bytes(stack_frame.len());
        let mut generator = cx.alloc_uninit_with_size::<GeneratorObject>(size);

        let descriptor = cx.base_descriptors.get(ObjectKind::Generator);
        object_ordinary_init(cx, generator.into(), descriptor, prototype.map(|p| p.get_()));

        set_uninit!(generator.state, GeneratorState::SuspendedStart);
        set_uninit!(generator.pc_to_resume_offset, pc_to_resume_offset);
        set_uninit!(generator.fp_index, fp_index);
        set_uninit!(generator.completion_indices, completion_indices);
        generator.stack_frame.init_from_slice(stack_frame);

        generator
    }

    pub fn new_for_generator(
        cx: Context,
        closure: Handle<Closure>,
        pc_to_resume_offset: usize,
        fp_index: usize,
        stack_frame: &[StackSlotValue],
    ) -> EvalResult<HeapPtr<GeneratorObject>> {
        let proto = maybe!(get_prototype_from_constructor(
            cx,
            closure.into(),
            Intrinsic::GeneratorPrototype
        ));

        Self::new(cx, Some(proto), pc_to_resume_offset, fp_index, None, stack_frame).into()
    }

    pub fn new_for_async_function(
        cx: Context,
        pc_to_resume_offset: usize,
        fp_index: usize,
        completion_indices: (u32, u32),
        stack_frame: &[StackSlotValue],
    ) -> HeapPtr<GeneratorObject> {
        Self::new(cx, None, pc_to_resume_offset, fp_index, Some(completion_indices), stack_frame)
    }

    const STACK_FRAME_OFFSET: usize = field_offset!(GeneratorObject, stack_frame);

    fn calculate_size_in_bytes(num_stack_slots: usize) -> usize {
        Self::STACK_FRAME_OFFSET
            + InlineArray::<StackSlotValue>::calculate_size_in_bytes(num_stack_slots)
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
        self.state = GeneratorState::SuspendedYield;
        self.pc_to_resume_offset = pc_to_resume_offset;
        self.completion_indices = Some(completion_indices);
        self.stack_frame.as_mut_slice().copy_from_slice(stack_frame);
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

impl TGeneratorObject for Handle<GeneratorObject> {
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

/// 27.5.3.2 GeneratorValidate
fn generator_validate(
    cx: Context,
    generator: Handle<Value>,
) -> EvalResult<Handle<GeneratorObject>> {
    if !generator.is_object() || !generator.as_object().is_generator() {
        return type_error(cx, "expected generator");
    }

    let generator = generator.as_object().cast::<GeneratorObject>();

    if generator.state == GeneratorState::Executing {
        return type_error(cx, "generator is already executing");
    }

    generator.into()
}

/// 27.5.3.3 GeneratorResume
pub fn generator_resume(
    cx: Context,
    generator: Handle<Value>,
    completion_value: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let generator = maybe!(generator_validate(cx, generator));

    // Check if generator has already completed
    if generator.state == GeneratorState::Completed {
        return create_iter_result_object(cx, cx.undefined(), true).into();
    }

    debug_assert!(generator.state.is_suspended());

    generate_resume_impl(cx, generator, completion_value, GeneratorCompletionType::Normal)
}

fn generate_resume_impl(
    mut cx: Context,
    mut generator: Handle<GeneratorObject>,
    completion_value: Handle<Value>,
    completion_type: GeneratorCompletionType,
) -> EvalResult<Handle<Value>> {
    // Mark the generator as executing then resume the generator
    generator.state = GeneratorState::Executing;

    let next_completion = cx
        .vm()
        .resume_generator(generator, completion_value, completion_type);

    // A yielding generator returns the yielded value
    if generator.state == GeneratorState::SuspendedYield {
        return next_completion;
    }

    // If the generator did not yield then it either returned or threw. In either case mark the
    // generator as completed.
    generator.state = GeneratorState::Completed;

    let next_value = maybe!(next_completion);
    let is_done = generator.state == GeneratorState::Completed;

    create_iter_result_object(cx, next_value, is_done).into()
}

/// 27.5.3.4 GeneratorResumeAbrupt
pub fn generator_resume_abrupt(
    cx: Context,
    generator: Handle<Value>,
    completion_value: Handle<Value>,
    completion_type: GeneratorCompletionType,
) -> EvalResult<Handle<Value>> {
    let mut generator = maybe!(generator_validate(cx, generator));

    // An abrupt completion on a generator that has not been started immediately completes it
    if generator.state == GeneratorState::SuspendedStart {
        generator.state = GeneratorState::Completed;
    }

    // Check if generator has already completed
    if generator.state == GeneratorState::Completed {
        if completion_type == GeneratorCompletionType::Return {
            return create_iter_result_object(cx, completion_value, true).into();
        } else {
            return EvalResult::Throw(completion_value);
        }
    }

    debug_assert!(generator.state == GeneratorState::SuspendedYield);

    generate_resume_impl(cx, generator, completion_value, completion_type)
}

impl HeapObject for HeapPtr<GeneratorObject> {
    fn byte_size(&self) -> usize {
        GeneratorObject::calculate_size_in_bytes(self.stack_frame.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);

        if self.state.is_suspended() {
            let mut stack_frame = StackFrame::for_fp(self.current_fp().cast_mut());
            stack_frame.visit_simple_pointers(visitor);
        }
    }
}
