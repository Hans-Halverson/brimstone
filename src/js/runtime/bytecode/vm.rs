use std::{collections::HashSet, ops::Deref};

use crate::{
    js::runtime::{
        abstract_operations::{
            call, call_object, copy_data_properties, create_data_property_or_throw,
            define_property_or_throw, get_method, has_property, private_get, private_set, set,
        },
        arguments_object::{create_unmapped_arguments_object, MappedArgumentsObject},
        array_object::{array_create, ArrayObject},
        class_names::{new_class, ClassNames},
        error::{
            err_assign_constant, err_cannot_set_property, err_not_defined, reference_error,
            type_error, type_error_value,
        },
        eval::{
            eval::perform_eval,
            expression::{
                eval_add, eval_bitwise_and, eval_bitwise_not, eval_bitwise_or, eval_bitwise_xor,
                eval_delete_property, eval_divide, eval_exponentiation, eval_greater_than,
                eval_greater_than_or_equal, eval_in_expression, eval_instanceof_expression,
                eval_less_than, eval_less_than_or_equal, eval_multiply, eval_negate,
                eval_remainder, eval_shift_left, eval_shift_right_arithmetic,
                eval_shift_right_logical, eval_subtract, eval_typeof,
            },
        },
        for_in_iterator::ForInIterator,
        function::build_function_name,
        gc::{HandleScope, HeapVisitor},
        generator_object::{GeneratorCompletionType, GeneratorObject},
        get,
        intrinsics::{
            generator_prototype::GeneratorPrototype, intrinsics::Intrinsic,
            native_error::TypeError, regexp_constructor::RegExpObject,
            rust_runtime::RustRuntimeFunctionId,
        },
        iterator::{get_iterator, iterator_complete, iterator_value, IteratorHint},
        object_descriptor::ObjectKind,
        object_value::{ObjectValue, VirtualObject},
        ordinary_object::{object_create_from_constructor, ordinary_object_create},
        property::Property,
        proxy_object::ProxyObject,
        regexp::compiled_regexp::CompiledRegExpObject,
        scope::Scope,
        scope_names::ScopeNames,
        to_string,
        type_utilities::{
            is_callable_object, is_loosely_equal, is_strictly_equal, same_object_value, to_boolean,
            to_number, to_numeric, to_object, to_property_key,
        },
        value::{AccessorValue, BigIntValue, SymbolValue},
        Context, EvalResult, Handle, HeapPtr, PropertyDescriptor, PropertyKey, Value,
    },
    maybe, must,
};

use super::{
    constant_table::ConstantTable,
    function::{BytecodeFunction, Closure},
    generator::BytecodeProgram,
    instruction::{
        extra_wide_prefix_index_to_opcode_index, wide_prefix_index_to_opcode_index, AddInstruction,
        BitAndInstruction, BitNotInstruction, BitOrInstruction, BitXorInstruction, CallInstruction,
        CallMaybeEvalInstruction, CallMaybeEvalVarargsInstruction, CallVarargsInstruction,
        CallWithReceiverInstruction, CheckSuperAlreadyCalledInstruction, CheckTdzInstruction,
        CheckThisInitializedInstruction, ConstructInstruction, ConstructVarargsInstruction,
        CopyDataPropertiesInstruction, DecInstruction, DefaultSuperCallInstruction,
        DefineNamedPropertyInstruction, DefinePrivatePropertyFlags,
        DefinePrivatePropertyInstruction, DefinePropertyFlags, DefinePropertyInstruction,
        DeleteBindingInstruction, DeletePropertyInstruction, DivInstruction, DupScopeInstruction,
        ErrorConstInstruction, ErrorDeleteSuperPropertyInstruction, EvalFlags, ExpInstruction,
        ForInNextInstruction, GeneratorStartInstruction, GetIteratorInstruction,
        GetNamedPropertyInstruction, GetNamedSuperPropertyInstruction,
        GetPrivatePropertyInstruction, GetPropertyInstruction, GetSuperConstructorInstruction,
        GetSuperPropertyInstruction, GreaterThanInstruction, GreaterThanOrEqualInstruction,
        InInstruction, IncInstruction, InstanceOfInstruction, Instruction,
        IteratorCloseInstruction, IteratorNextInstruction, JumpConstantInstruction,
        JumpFalseConstantInstruction, JumpFalseInstruction, JumpInstruction,
        JumpNotNullishConstantInstruction, JumpNotNullishInstruction,
        JumpNotUndefinedConstantInstruction, JumpNotUndefinedInstruction,
        JumpNullishConstantInstruction, JumpNullishInstruction,
        JumpToBooleanFalseConstantInstruction, JumpToBooleanFalseInstruction,
        JumpToBooleanTrueConstantInstruction, JumpToBooleanTrueInstruction,
        JumpTrueConstantInstruction, JumpTrueInstruction, LessThanInstruction,
        LessThanOrEqualInstruction, LoadConstantInstruction, LoadDynamicInstruction,
        LoadDynamicOrUnresolvedInstruction, LoadEmptyInstruction, LoadFalseInstruction,
        LoadFromScopeInstruction, LoadGlobalInstruction, LoadGlobalOrUnresolvedInstruction,
        LoadImmediateInstruction, LoadNullInstruction, LoadTrueInstruction,
        LoadUndefinedInstruction, LogNotInstruction, LooseEqualInstruction,
        LooseNotEqualInstruction, MovInstruction, MulInstruction, NegInstruction,
        NewAccessorInstruction, NewArrayInstruction, NewClassInstruction, NewClosureInstruction,
        NewForInIteratorInstruction, NewGeneratorInstruction, NewMappedArgumentsInstruction,
        NewObjectInstruction, NewPrivateSymbolInstruction, NewRegExpInstruction,
        NewUnmappedArgumentsInstruction, OpCode, PopScopeInstruction, PushFunctionScopeInstruction,
        PushLexicalScopeInstruction, PushWithScopeInstruction, RemInstruction,
        RestParameterInstruction, RetInstruction, SetArrayPropertyInstruction,
        SetNamedPropertyInstruction, SetPrivatePropertyInstruction, SetPropertyInstruction,
        SetPrototypeOfInstruction, SetSuperPropertyInstruction, ShiftLeftInstruction,
        ShiftRightArithmeticInstruction, ShiftRightLogicalInstruction, StoreDynamicInstruction,
        StoreGlobalInstruction, StoreToScopeInstruction, StrictEqualInstruction,
        StrictNotEqualInstruction, SubInstruction, ThrowInstruction, ToNumberInstruction,
        ToNumericInstruction, ToObjectInstruction, ToPropertyKeyInstruction, ToStringInstruction,
        TypeOfInstruction, YieldInstruction,
    },
    instruction_traits::{
        GenericCallArgs, GenericCallInstruction, GenericConstructInstruction,
        GenericJumpBooleanConstantInstruction, GenericJumpBooleanInstruction,
        GenericJumpNullishConstantInstruction, GenericJumpNullishInstruction,
        GenericJumpToBooleanConstantInstruction, GenericJumpToBooleanInstruction,
        GenericJumpUndefinedConstantInstruction, GenericJumpUndefinedInstruction,
    },
    operand::{ConstantIndex, Register, SInt, UInt},
    stack_frame::{StackFrame, StackSlotValue, FIRST_ARGUMENT_SLOT_INDEX, NUM_STACK_SLOTS},
    width::{ExtraWide, Narrow, SignedWidthRepr, UnsignedWidthRepr, Wide, Width},
};

/// The virtual machine that executes bytecode.
pub struct VM {
    cx: Context,

    /// The program counter (instruction pointer)
    pc: *const u8,

    /// The stack pointer
    sp: *mut StackSlotValue,

    /// The frame pointer
    fp: *mut StackSlotValue,

    stack: Vec<StackSlotValue>,
}

impl VM {
    pub fn new(cx: Context) -> Self {
        // Allocate uninitialized memory for stack
        let mut stack = Vec::<StackSlotValue>::with_capacity(NUM_STACK_SLOTS);
        unsafe { stack.set_len(NUM_STACK_SLOTS) };

        let mut vm = VM {
            cx,
            pc: std::ptr::null(),
            sp: std::ptr::null_mut(),
            fp: std::ptr::null_mut(),

            stack,
        };

        vm.reset_stack();
        vm
    }

    /// Execute a closure with the provided arguments.
    pub fn execute(
        &mut self,
        closure: Handle<Closure>,
        arguments: &[Handle<Value>],
    ) -> Result<Handle<Value>, Handle<Value>> {
        // Evaluate with the global object as the receiver
        let receiver = closure.global_object().into();

        // Evaluate the provided function
        let eval_result = self.call_from_rust(closure.cast(), receiver, arguments);

        eval_result.to_rust_result()
    }

    /// Execute an entire bytecode program, first instantiation the global declarations then
    /// running the script function.
    pub fn execute_program(
        &mut self,
        bytecode_program: BytecodeProgram,
    ) -> Result<Handle<Value>, Handle<Value>> {
        let mut realm = bytecode_program.script_function.realm();
        let global_names = bytecode_program.global_names;

        // Call the GlobalDeclarationInstantiation function in the rust runtime
        let init_closure = realm
            .get_intrinsic_ptr(Intrinsic::GlobalDeclarationInstantiation)
            .cast::<Closure>();
        let init_function_id = init_closure
            .function_ptr()
            .rust_runtime_function_id()
            .unwrap();

        self.call_rust_runtime(
            init_closure,
            init_function_id,
            realm.global_object().into(),
            &[global_names.cast()],
            None,
        )
        .to_rust_result()?;

        // Then create global scope and add lexical names to realm, since GDI would have errored
        // if there were any conflicts.
        let global_scope = realm.new_global_scope(self.cx, global_names.scope_names());

        // Create program closure and execute in VM
        let program_closure =
            Closure::new_in_realm(self.cx, bytecode_program.script_function, global_scope, realm);

        self.execute(program_closure, &[])
    }

    /// Resume a suspended generator, executing it until it yields or completes.
    pub fn resume_generator(
        &mut self,
        mut generator: Handle<GeneratorObject>,
        completion_value: Handle<Value>,
        completion_type: GeneratorCompletionType,
    ) -> EvalResult<Handle<Value>> {
        let saved_stack_frame = generator.stack_frame();
        let stack_frame_size = saved_stack_frame.len();

        // Save the PC that should be used as the return address
        let return_address = self.pc;
        let parent_fp = self.fp;

        // Push the saved stack frame stored in generator onto the stack
        unsafe {
            self.sp = self.sp.sub(stack_frame_size);
            std::ptr::copy_nonoverlapping(saved_stack_frame.as_ptr(), self.sp, stack_frame_size);
        }

        // Restore the frame pointer
        self.fp = unsafe { self.sp.add(generator.fp_index()) };

        let mut stack_frame = StackFrame::for_fp(self.fp);

        // Patch the saved frame pointer in the stack frame
        unsafe { *self.fp = parent_fp as StackSlotValue };

        // Patch the return address in the stack frame
        let encoded_return_address = StackFrame::return_address_from_rust(return_address);
        stack_frame.set_encoded_return_address(encoded_return_address);

        // Patch the address of the return value in the stack frame
        let mut return_value = Value::undefined();
        stack_frame.set_return_value_address((&mut return_value) as *mut Value);

        // Write the resumed value to the yield completion registers if necessary
        if let Some((completion_value_index, completion_type_index)) =
            generator.yield_completion_indices()
        {
            self.write_register(
                Register::<ExtraWide>::local(completion_value_index as usize),
                completion_value.get(),
            );
            self.write_register(
                Register::<ExtraWide>::local(completion_type_index as usize),
                completion_type.to_value(),
            )
        }

        // Restore the PC, which was stored as an offset into the BytecodeFunction
        let func_start = self.closure().function_ptr().as_ptr().cast::<u8>();
        let pc_to_resume = unsafe { func_start.add(generator.pc_to_resume_offset()) };
        self.pc = pc_to_resume;

        // Start executing the dispatch loop from where the generator was suspended, returning out
        // of dispatch loop when the marked return address is encountered.
        let completion = self.dispatch_loop();

        // If the generator did not yeild then it either returned or threw. In either case mark the
        // generator as completed.
        generator.complete_if_not_yielded();

        if let Err(error_value) = completion {
            return EvalResult::Throw(error_value.to_handle(self.cx));
        }

        return_value.to_handle(self.cx).into()
    }

    fn reset_stack(&mut self) {
        // Reset stack
        self.sp = self.stack.as_ptr_range().end as *mut StackSlotValue;
        self.fp = std::ptr::null_mut();
    }

    /// An empty frame pointer indicates that the stack is empty, no bytecode is currently
    /// executing, and the VM stack does not need to be walked for GC roots.
    fn is_executing(&self) -> bool {
        self.fp != std::ptr::null_mut()
    }

    /// Dispatch instructions, one after another, until there are no more instructions to execute.
    ///
    /// Invariants:
    /// - By the time that a throw is possible, PC must be updated to point to the next instruction
    /// - References to the instruction cannot be held over any allocations, since the instruction
    ///   points into the managed heap and may be moved by a GC.
    fn dispatch_loop(&mut self) -> Result<(), Value> {
        'dispatch: loop {
            macro_rules! create_dispatch_macros {
                ($width:ident, $opcode_pc:expr) => {
                    // Get the current $width instruction
                    macro_rules! get_instr {
                        ($instr:ident) => {{
                            // Instruction operands begin after the one byte opcode
                            unsafe { &*$opcode_pc.add(1).cast::<$instr<$width>>() }
                        }};
                    }

                    // Dispatch a $width instruction
                    macro_rules! dispatch {
                        ($instr:ident, $func:ident) => {{
                            let instr = get_instr!($instr);
                            // Set PC before calling function, as function may allocate which would
                            // invalidate the $instr pointer.
                            self.set_pc_after(instr);
                            self.$func::<$width>(instr);
                        }};
                    }

                    macro_rules! dispatch_or_throw {
                        ($instr:ident, $func:ident) => {{
                            let instr = get_instr!($instr);
                            // Set PC before calling function, as function may allocate which would
                            // invalidate the $instr pointer.
                            self.set_pc_after(instr);
                            maybe_throw!(self.$func::<$width>(instr));
                        }};
                    }
                };
            }

            /// Execute a ret instruction
            macro_rules! execute_ret {
                ($get_instr:ident) => {{
                    let instr = $get_instr!(RetInstruction);
                    let return_value = self.read_register(instr.return_value());
                    return_!(return_value)
                }};
            }

            /// Return a value in the VM, writing the return value into the parent frame and popping
            /// the stack frame to continue execution in the caller function.
            macro_rules! return_ {
                ($return_value:expr) => {{
                    let return_value = $return_value;

                    // Store the return value at the return value address
                    let return_value_address = self.get_return_value_address();
                    unsafe { *return_value_address = return_value };

                    // Next instruction to execute is the saved return address
                    let is_rust_caller = StackFrame::for_fp(self.fp).is_rust_caller();

                    // Destroy the stack frame
                    self.pop_stack_frame();

                    // If the caller was rust then return instead of executing next instruction
                    if is_rust_caller {
                        return Ok(());
                    }
                }};
            }

            macro_rules! execute_yield {
                ($get_instr:ident) => {{
                    let instr = $get_instr!(YieldInstruction);

                    let mut generator = self
                        .read_register(instr.generator())
                        .as_object()
                        .cast::<GeneratorObject>();
                    let yield_value = self.read_register(instr.yield_value());
                    let completion_value_index = instr.completion_value_dest().local_index() as u32;
                    let completion_type_index = instr.completion_type_dest().local_index() as u32;

                    // Set the PC to the next instruction to execute
                    self.set_pc_after(instr);
                    let pc_to_resume_offset = self.get_pc_offset();
                    let stack_frame = StackFrame::for_fp(self.fp);

                    // Save the stack frame and PC to resume in the generator object
                    generator.suspend(
                        pc_to_resume_offset,
                        (completion_value_index, completion_type_index),
                        stack_frame.as_slice(),
                    );

                    // Return the yielded value to the caller
                    return_!(yield_value)
                }};
            }

            /// Execute a GeneratorStart instruction, copying the current stack frame and execution
            /// state into the generator object. Returns the generator object to the caller.
            macro_rules! execute_generator_start {
                ($get_instr:ident) => {{
                    let instr = $get_instr!(GeneratorStartInstruction);
                    let generator_reg = instr.generator();

                    // Set the PC to the next instruction to execute
                    self.set_pc_after(instr);
                    let pc_to_resume_offset = self.get_pc_offset();

                    // Find the index of the FP in the stack frame
                    let fp_index = unsafe { self.fp.offset_from(self.sp) as usize };

                    let current_closure = self.closure().to_handle();
                    let stack_frame = StackFrame::for_fp(self.fp);

                    // Create the generator in the started state, copying the current stack frame
                    // and PC to resume.
                    let mut generator = maybe_throw!(GeneratorObject::new(
                        self.cx,
                        current_closure,
                        pc_to_resume_offset,
                        fp_index,
                        stack_frame.as_slice(),
                    ));

                    // Store the generator into the provided register in the stored stack frame
                    let generator_value = generator.cast::<ObjectValue>().into();
                    generator.set_register(generator_reg.local_index(), generator_value);

                    // Return the generator object to the caller
                    return_!(generator_value)
                }};
            }

            macro_rules! throw {
                ($error_value:expr) => {{
                    let error_value = $error_value;

                    // Walk the stack, looking for an exception handler that covers the current
                    // address.
                    let mut stack_frame = StackFrame::for_fp(self.fp);
                    if self.visit_frame_for_exception_unwinding(stack_frame, self.pc, error_value) {
                        continue 'dispatch;
                    }

                    while let Some(caller_stack_frame) = stack_frame.previous_frame() {
                        // If the caller is the Rust runtime then return the thrown error
                        if stack_frame.is_rust_caller() {
                            // Unwind the stack to the caller's frame
                            self.pc = stack_frame.return_address();
                            self.fp = caller_stack_frame.fp();
                            self.sp = caller_stack_frame.sp();

                            return Err(error_value);
                        }

                        if self.visit_frame_for_exception_unwinding(
                            caller_stack_frame,
                            stack_frame.return_address(),
                            error_value,
                        ) {
                            continue 'dispatch;
                        }

                        stack_frame = caller_stack_frame;
                    }

                    // Exception has unwound the entire stack, finish VM execution returning the
                    // thrown error.
                    self.reset_stack();
                    return Err(error_value);
                }};
            }

            macro_rules! execute_throw {
                ($get_instr:ident) => {{
                    let instr = $get_instr!(ThrowInstruction);
                    self.set_pc_after(instr);
                    let error_value = self.read_register(instr.error());
                    throw!(error_value)
                }};
            }

            macro_rules! maybe_throw {
                ($expr:expr) => {
                    match $expr {
                        EvalResult::Ok(result) => result,
                        EvalResult::Throw(error) => throw!(error.get()),
                    }
                };
            }

            macro_rules! create_dispatch_table {
                ($width:ident, $opcode:ident, $opcode_pc:ident) => {
                    create_dispatch_macros!($width, $opcode_pc);

                    match $opcode {
                        // A prefix cannot follow the initial wide prefix
                        OpCode::WidePrefix => panic!("A prefix cannot appear at this position"),
                        OpCode::ExtraWidePrefix => {
                            panic!("A prefix cannot appear at this position")
                        }
                        // Dispatch the instruction
                        OpCode::Mov => dispatch!(MovInstruction, execute_mov),
                        OpCode::LoadImmediate => {
                            dispatch!(LoadImmediateInstruction, execute_load_immediate)
                        }
                        OpCode::LoadUndefined => {
                            dispatch!(LoadUndefinedInstruction, execute_load_undefined)
                        }
                        OpCode::LoadEmpty => dispatch!(LoadEmptyInstruction, execute_load_empty),
                        OpCode::LoadNull => dispatch!(LoadNullInstruction, execute_load_null),
                        OpCode::LoadTrue => dispatch!(LoadTrueInstruction, execute_load_true),
                        OpCode::LoadFalse => dispatch!(LoadFalseInstruction, execute_load_false),
                        OpCode::LoadConstant => {
                            dispatch!(LoadConstantInstruction, execute_load_constant)
                        }
                        OpCode::LoadGlobal => {
                            dispatch_or_throw!(LoadGlobalInstruction, execute_load_global)
                        }
                        OpCode::LoadGlobalOrUnresolved => {
                            dispatch_or_throw!(
                                LoadGlobalOrUnresolvedInstruction,
                                execute_load_global_or_unresolved
                            )
                        }
                        OpCode::StoreGlobal => {
                            dispatch_or_throw!(StoreGlobalInstruction, execute_store_global)
                        }
                        OpCode::LoadDynamic => {
                            dispatch_or_throw!(LoadDynamicInstruction, execute_load_dynamic)
                        }
                        OpCode::LoadDynamicOrUnresolved => {
                            dispatch_or_throw!(
                                LoadDynamicOrUnresolvedInstruction,
                                execute_load_dynamic_or_unresolved
                            )
                        }
                        OpCode::StoreDynamic => {
                            dispatch_or_throw!(StoreDynamicInstruction, execute_store_dynamic)
                        }
                        OpCode::Call => dispatch_or_throw!(CallInstruction, execute_generic_call),
                        OpCode::CallWithReceiver => {
                            dispatch_or_throw!(CallWithReceiverInstruction, execute_generic_call)
                        }
                        OpCode::CallVarargs => {
                            dispatch_or_throw!(CallVarargsInstruction, execute_generic_call)
                        }
                        OpCode::CallMaybeEval => {
                            dispatch_or_throw!(CallMaybeEvalInstruction, execute_call_maybe_eval)
                        }
                        OpCode::CallMaybeEvalVarargs => {
                            dispatch_or_throw!(
                                CallMaybeEvalVarargsInstruction,
                                execute_call_maybe_eval_varargs
                            )
                        }
                        OpCode::Construct => {
                            dispatch_or_throw!(ConstructInstruction, execute_generic_construct)
                        }
                        OpCode::ConstructVarargs => {
                            dispatch_or_throw!(
                                ConstructVarargsInstruction,
                                execute_generic_construct
                            )
                        }
                        OpCode::DefaultSuperCall => {
                            dispatch_or_throw!(
                                DefaultSuperCallInstruction,
                                execute_default_super_call
                            )
                        }
                        OpCode::Ret => execute_ret!(get_instr),
                        OpCode::Add => dispatch_or_throw!(AddInstruction, execute_add),
                        OpCode::Sub => dispatch_or_throw!(SubInstruction, execute_sub),
                        OpCode::Mul => dispatch_or_throw!(MulInstruction, execute_mul),
                        OpCode::Div => dispatch_or_throw!(DivInstruction, execute_div),
                        OpCode::Rem => dispatch_or_throw!(RemInstruction, execute_rem),
                        OpCode::Exp => dispatch_or_throw!(ExpInstruction, execute_exp),
                        OpCode::BitAnd => dispatch_or_throw!(BitAndInstruction, execute_bit_and),
                        OpCode::BitOr => dispatch_or_throw!(BitOrInstruction, execute_bit_or),
                        OpCode::BitXor => dispatch_or_throw!(BitXorInstruction, execute_bit_xor),
                        OpCode::ShiftLeft => {
                            dispatch_or_throw!(ShiftLeftInstruction, execute_shift_left)
                        }
                        OpCode::ShiftRightArithmetic => dispatch_or_throw!(
                            ShiftRightArithmeticInstruction,
                            execute_shift_right_arithmetic
                        ),
                        OpCode::ShiftRightLogical => dispatch_or_throw!(
                            ShiftRightLogicalInstruction,
                            execute_shift_right_logical
                        ),
                        OpCode::LooseEqual => {
                            dispatch_or_throw!(LooseEqualInstruction, execute_loose_equal)
                        }
                        OpCode::LooseNotEqual => {
                            dispatch_or_throw!(LooseNotEqualInstruction, execute_loose_not_equal)
                        }
                        OpCode::StrictEqual => {
                            dispatch!(StrictEqualInstruction, execute_strict_equal)
                        }
                        OpCode::StrictNotEqual => {
                            dispatch!(StrictNotEqualInstruction, execute_strict_not_equal)
                        }
                        OpCode::LessThan => {
                            dispatch_or_throw!(LessThanInstruction, execute_less_than)
                        }
                        OpCode::LessThanOrEqual => {
                            dispatch_or_throw!(
                                LessThanOrEqualInstruction,
                                execute_less_than_or_equal
                            )
                        }
                        OpCode::GreaterThan => {
                            dispatch_or_throw!(GreaterThanInstruction, execute_greater_than)
                        }
                        OpCode::GreaterThanOrEqual => dispatch_or_throw!(
                            GreaterThanOrEqualInstruction,
                            execute_greater_than_or_equal
                        ),
                        OpCode::Neg => dispatch_or_throw!(NegInstruction, execute_neg),
                        OpCode::Inc => dispatch!(IncInstruction, execute_inc),
                        OpCode::Dec => dispatch!(DecInstruction, execute_dec),
                        OpCode::LogNot => dispatch!(LogNotInstruction, execute_log_not),
                        OpCode::BitNot => dispatch_or_throw!(BitNotInstruction, execute_bit_not),
                        OpCode::TypeOf => dispatch!(TypeOfInstruction, execute_typeof),
                        OpCode::In => dispatch_or_throw!(InInstruction, execute_in),
                        OpCode::InstanceOf => {
                            dispatch_or_throw!(InstanceOfInstruction, execute_instance_of)
                        }
                        OpCode::ToNumber => {
                            dispatch_or_throw!(ToNumberInstruction, execute_to_number)
                        }
                        OpCode::ToNumeric => {
                            dispatch_or_throw!(ToNumericInstruction, execute_to_numeric)
                        }
                        OpCode::ToString => {
                            dispatch_or_throw!(ToStringInstruction, execute_to_string)
                        }
                        OpCode::ToPropertyKey => {
                            dispatch_or_throw!(ToPropertyKeyInstruction, execute_to_property_key)
                        }
                        OpCode::ToObject => {
                            dispatch_or_throw!(ToObjectInstruction, execute_to_object)
                        }
                        OpCode::Jump => self.execute_jump(get_instr!(JumpInstruction)),
                        OpCode::JumpConstant => {
                            self.execute_jump_constant(get_instr!(JumpConstantInstruction))
                        }
                        OpCode::JumpTrue => {
                            self.execute_jump_boolean(get_instr!(JumpTrueInstruction))
                        }
                        OpCode::JumpTrueConstant => {
                            let instr = get_instr!(JumpTrueConstantInstruction);
                            self.execute_jump_boolean_constant(instr)
                        }
                        OpCode::JumpToBooleanTrue => {
                            let instr = get_instr!(JumpToBooleanTrueInstruction);
                            self.execute_jump_to_boolean(instr)
                        }
                        OpCode::JumpToBooleanTrueConstant => {
                            let instr = get_instr!(JumpToBooleanTrueConstantInstruction);
                            self.execute_jump_to_boolean_constant(instr)
                        }
                        OpCode::JumpFalse => {
                            self.execute_jump_boolean(get_instr!(JumpFalseInstruction))
                        }
                        OpCode::JumpFalseConstant => {
                            let instr = get_instr!(JumpFalseConstantInstruction);
                            self.execute_jump_boolean_constant(instr)
                        }
                        OpCode::JumpToBooleanFalse => {
                            let instr = get_instr!(JumpToBooleanFalseInstruction);
                            self.execute_jump_to_boolean(instr)
                        }
                        OpCode::JumpToBooleanFalseConstant => {
                            let instr = get_instr!(JumpToBooleanFalseConstantInstruction);
                            self.execute_jump_to_boolean_constant(instr)
                        }
                        OpCode::JumpNotUndefined => {
                            let instr = get_instr!(JumpNotUndefinedInstruction);
                            self.execute_jump_undefined(instr)
                        }
                        OpCode::JumpNotUndefinedConstant => {
                            let instr = get_instr!(JumpNotUndefinedConstantInstruction);
                            self.execute_jump_undefined_constant(instr)
                        }
                        OpCode::JumpNullish => {
                            let instr = get_instr!(JumpNullishInstruction);
                            self.execute_jump_nullish(instr)
                        }
                        OpCode::JumpNullishConstant => {
                            let instr = get_instr!(JumpNullishConstantInstruction);
                            self.execute_jump_nullish_constant(instr)
                        }
                        OpCode::JumpNotNullish => {
                            let instr = get_instr!(JumpNotNullishInstruction);
                            self.execute_jump_nullish(instr)
                        }
                        OpCode::JumpNotNullishConstant => {
                            let instr = get_instr!(JumpNotNullishConstantInstruction);
                            self.execute_jump_nullish_constant(instr)
                        }
                        OpCode::NewClosure => dispatch!(NewClosureInstruction, execute_new_closure),
                        OpCode::NewGenerator => {
                            dispatch_or_throw!(NewGeneratorInstruction, execute_new_generator)
                        }
                        OpCode::NewObject => dispatch!(NewObjectInstruction, execute_new_object),
                        OpCode::NewArray => dispatch!(NewArrayInstruction, execute_new_array),
                        OpCode::NewRegExp => {
                            dispatch_or_throw!(NewRegExpInstruction, execute_new_regexp)
                        }
                        OpCode::NewMappedArguments => {
                            dispatch!(NewMappedArgumentsInstruction, execute_new_mapped_arguments)
                        }
                        OpCode::NewUnmappedArguments => {
                            dispatch!(
                                NewUnmappedArgumentsInstruction,
                                execute_new_unmapped_arguments
                            )
                        }
                        OpCode::NewClass => {
                            dispatch_or_throw!(NewClassInstruction, execute_new_class)
                        }
                        OpCode::NewAccessor => {
                            dispatch!(NewAccessorInstruction, execute_new_accessor)
                        }
                        OpCode::NewPrivateSymbol => {
                            dispatch!(NewPrivateSymbolInstruction, execute_new_private_symbol)
                        }
                        OpCode::GetProperty => {
                            dispatch_or_throw!(GetPropertyInstruction, execute_get_property)
                        }
                        OpCode::SetProperty => {
                            dispatch_or_throw!(SetPropertyInstruction, execute_set_property)
                        }
                        OpCode::DefineProperty => {
                            dispatch_or_throw!(DefinePropertyInstruction, execute_define_property)
                        }
                        OpCode::GetNamedProperty => {
                            dispatch_or_throw!(
                                GetNamedPropertyInstruction,
                                execute_get_named_property
                            )
                        }
                        OpCode::SetNamedProperty => {
                            dispatch_or_throw!(
                                SetNamedPropertyInstruction,
                                execute_set_named_property
                            )
                        }
                        OpCode::DefineNamedProperty => {
                            dispatch_or_throw!(
                                DefineNamedPropertyInstruction,
                                execute_define_named_property
                            )
                        }
                        OpCode::GetSuperProperty => {
                            dispatch_or_throw!(
                                GetSuperPropertyInstruction,
                                execute_get_super_property
                            )
                        }
                        OpCode::GetNamedSuperProperty => {
                            dispatch_or_throw!(
                                GetNamedSuperPropertyInstruction,
                                execute_get_named_super_property
                            )
                        }
                        OpCode::SetSuperProperty => {
                            dispatch_or_throw!(
                                SetSuperPropertyInstruction,
                                execute_set_super_property
                            )
                        }
                        OpCode::DeleteProperty => {
                            dispatch_or_throw!(DeletePropertyInstruction, execute_delete_property)
                        }
                        OpCode::DeleteBinding => {
                            dispatch_or_throw!(DeleteBindingInstruction, execute_delete_binding)
                        }
                        OpCode::GetPrivateProperty => {
                            dispatch_or_throw!(
                                GetPrivatePropertyInstruction,
                                execute_get_private_property
                            )
                        }
                        OpCode::SetPrivateProperty => {
                            dispatch_or_throw!(
                                SetPrivatePropertyInstruction,
                                execute_set_private_property
                            )
                        }
                        OpCode::DefinePrivateProperty => {
                            dispatch_or_throw!(
                                DefinePrivatePropertyInstruction,
                                execute_define_private_property
                            )
                        }
                        OpCode::SetArrayProperty => {
                            dispatch!(SetArrayPropertyInstruction, execute_set_array_property)
                        }
                        OpCode::SetPrototypeOf => {
                            dispatch!(SetPrototypeOfInstruction, execute_set_prototype_of)
                        }
                        OpCode::CopyDataProperties => {
                            dispatch_or_throw!(
                                CopyDataPropertiesInstruction,
                                execute_copy_data_properties
                            )
                        }
                        OpCode::PushLexicalScope => {
                            dispatch!(PushLexicalScopeInstruction, execute_push_lexical_scope)
                        }
                        OpCode::PushFunctionScope => {
                            dispatch!(PushFunctionScopeInstruction, execute_push_function_scope)
                        }
                        OpCode::PushWithScope => {
                            dispatch_or_throw!(PushWithScopeInstruction, execute_push_with_scope)
                        }
                        OpCode::PopScope => dispatch!(PopScopeInstruction, execute_pop_scope),
                        OpCode::DupScope => dispatch!(DupScopeInstruction, execute_dup_scope),
                        OpCode::LoadFromScope => {
                            dispatch!(LoadFromScopeInstruction, execute_load_from_scope)
                        }
                        OpCode::StoreToScope => {
                            dispatch!(StoreToScopeInstruction, execute_store_to_scope)
                        }
                        OpCode::Throw => execute_throw!(get_instr),
                        OpCode::RestParameter => {
                            dispatch!(RestParameterInstruction, execute_rest_parameter)
                        }
                        OpCode::GetSuperConstructor => {
                            dispatch_or_throw!(
                                GetSuperConstructorInstruction,
                                execute_get_super_constructor
                            )
                        }
                        OpCode::CheckTdz => {
                            dispatch_or_throw!(CheckTdzInstruction, execute_check_tdz)
                        }
                        OpCode::CheckThisInitialized => {
                            dispatch_or_throw!(
                                CheckThisInitializedInstruction,
                                execute_check_this_initialized
                            )
                        }
                        OpCode::CheckSuperAlreadyCalled => {
                            dispatch_or_throw!(
                                CheckSuperAlreadyCalledInstruction,
                                execute_check_super_already_called
                            )
                        }
                        OpCode::ErrorConst => {
                            dispatch_or_throw!(ErrorConstInstruction, execute_error_const)
                        }
                        OpCode::ErrorDeleteSuperProperty => {
                            dispatch_or_throw!(
                                ErrorDeleteSuperPropertyInstruction,
                                execute_error_delete_super_property
                            )
                        }
                        OpCode::NewForInIterator => {
                            dispatch_or_throw!(
                                NewForInIteratorInstruction,
                                execute_new_for_in_iterator
                            )
                        }
                        OpCode::ForInNext => {
                            dispatch_or_throw!(ForInNextInstruction, execute_for_in_next)
                        }
                        OpCode::GetIterator => {
                            dispatch_or_throw!(GetIteratorInstruction, execute_get_iterator)
                        }
                        OpCode::IteratorNext => {
                            dispatch_or_throw!(IteratorNextInstruction, execute_iterator_next)
                        }
                        OpCode::IteratorClose => {
                            dispatch_or_throw!(IteratorCloseInstruction, execute_iterator_close)
                        }
                        OpCode::GeneratorStart => execute_generator_start!(get_instr),
                        OpCode::Yield => execute_yield!(get_instr),
                    }
                };
            }

            // PC starts pointing to the next opcode to execute
            let opcode_pc = self.pc;
            let opcode = unsafe { *opcode_pc.cast::<OpCode>() };

            match opcode {
                // Handle wide instructions
                OpCode::WidePrefix => {
                    // PC is pointing to the wide prefix. This is followed by optional padding, the
                    // opcode, then the operands. The operands must be aligned to the next possible
                    // two byte boundary so we can skip the padding and find the opcode location.
                    let opcode_pc =
                        wide_prefix_index_to_opcode_index(self.pc as usize) as *const u8;
                    let opcode = unsafe { *opcode_pc.cast::<OpCode>() };

                    create_dispatch_table!(Wide, opcode, opcode_pc);
                }

                // Handle extra wide instructions
                OpCode::ExtraWidePrefix => {
                    // PC is pointing to the extra wide prefix. This is followed by optional
                    // padding, the opcode, then the operands. The operands must be aligned to the
                    // next possible four byte boundary so we can skip the padding and find the
                    // opcode location.
                    let opcode_pc =
                        extra_wide_prefix_index_to_opcode_index(self.pc as usize) as *const u8;
                    let opcode = unsafe { *opcode_pc.cast::<OpCode>() };

                    create_dispatch_table!(ExtraWide, opcode, opcode_pc);
                }
                // Handle all other instructions, which must be narrow
                _ => {
                    create_dispatch_table!(Narrow, opcode, opcode_pc);
                }
            }
        }
    }

    /// Calculate the PC for the first byte after an instruction
    #[inline]
    fn get_pc_after<I: Instruction>(&self, instr: &I) -> *const u8 {
        let instr_start_ptr = instr as *const _ as *const u8;
        unsafe { instr_start_ptr.add(instr.byte_length()) }
    }

    /// Set the PC to the first byte after an instruction.
    #[inline]
    fn set_pc_after<I: Instruction>(&mut self, instr: &I) {
        self.pc = self.get_pc_after(instr);
    }

    /// Calculate the offset of the current PC in the current BytecodeFunction.
    #[inline]
    fn get_pc_offset(&self) -> usize {
        let func_start_ptr = self.closure().function_ptr().as_ptr().cast::<u8>();
        unsafe { self.pc.offset_from(func_start_ptr) as usize }
    }

    /// Push a value onto the stack. Note that the stack grows downwards.
    #[inline]
    fn push(&mut self, value: StackSlotValue) {
        unsafe {
            self.sp = self.sp.sub(1);
            *self.sp = value;
        }
    }

    /// Push FP onto the stack and set FP to SP.
    #[inline]
    fn push_fp(&mut self) {
        self.push(self.fp as StackSlotValue);
        self.fp = self.sp;
    }

    #[inline]
    fn fp_offset(&self, slot_index: isize) -> *mut StackSlotValue {
        unsafe { self.fp.offset(slot_index as isize) }
    }

    #[inline]
    fn get_return_address(&self) -> *const u8 {
        StackFrame::for_fp(self.fp).return_address()
    }

    #[inline]
    fn get_return_value_address(&self) -> *mut Value {
        StackFrame::for_fp(self.fp).return_value_address()
    }

    #[inline]
    pub fn scope(&self) -> HeapPtr<Scope> {
        StackFrame::for_fp(self.fp).scope()
    }

    #[inline]
    pub fn closure(&self) -> HeapPtr<Closure> {
        StackFrame::for_fp(self.fp).closure()
    }

    #[inline]
    fn constant_table(&self) -> HeapPtr<ConstantTable> {
        StackFrame::for_fp(self.fp).constant_table()
    }

    #[inline]
    fn argc(&self) -> usize {
        StackFrame::for_fp(self.fp).argc()
    }

    #[inline]
    pub fn receiver(&self) -> Value {
        StackFrame::for_fp(self.fp).receiver()
    }

    #[inline]
    fn register_address<W: Width>(&self, reg: Register<W>) -> *mut Value {
        self.fp_offset(reg.value().to_isize()) as *mut Value
    }

    #[inline]
    fn read_register<W: Width>(&mut self, reg: Register<W>) -> Value {
        unsafe { *self.register_address(reg) }
    }

    #[inline]
    fn write_register<W: Width>(&mut self, reg: Register<W>, value: Value) {
        unsafe { *self.register_address(reg) = value }
    }

    #[inline]
    fn read_register_to_handle<W: Width>(&mut self, reg: Register<W>) -> Handle<Value> {
        self.read_register(reg).to_handle(self.cx)
    }

    #[inline]
    fn get_constant<W: Width>(&self, constant_index: ConstantIndex<W>) -> Value {
        self.constant_table()
            .get_constant(constant_index.value().to_usize())
    }

    #[inline]
    fn get_constant_offset<W: Width>(&self, constant_index: ConstantIndex<W>) -> isize {
        // Constant offsets are encoded as a raw isize, not a value
        self.get_constant(constant_index).as_raw_bits() as isize
    }

    /// Set the PC to the jump target, specified as a relative offset immediate.
    #[inline]
    fn jump_immediate<W: Width>(&mut self, offset: SInt<W>) {
        self.pc = unsafe { self.pc.offset(offset.value().to_isize()) };
    }

    // Set the PC to the jump target, specified as a relative offset in the constant table.
    #[inline]
    fn jump_constant<W: Width>(&mut self, constant_index: ConstantIndex<W>) {
        let offset = self.get_constant_offset(constant_index);
        self.pc = unsafe { self.pc.offset(offset) };
    }

    /// Execute an unconditional jump instruction
    #[inline]
    fn execute_jump<W: Width>(&mut self, instr: &JumpInstruction<W>) {
        self.jump_immediate(instr.offset());
    }

    /// Execute an unconditional jump constant instruction
    #[inline]
    fn execute_jump_constant<W: Width>(&mut self, instr: &JumpConstantInstruction<W>) {
        self.jump_constant(instr.constant_index());
    }

    /// Execute a conditional jump if true/false instruction
    fn execute_jump_boolean<W: Width, I: GenericJumpBooleanInstruction<W>>(&mut self, instr: &I) {
        let condition = self.read_register(instr.condition());
        if I::cond_function(condition) {
            self.jump_immediate(instr.offset());
        } else {
            self.set_pc_after(instr);
        }
    }

    /// Execute a conditional jump if true/false constant instruction
    fn execute_jump_boolean_constant<W: Width, I: GenericJumpBooleanConstantInstruction<W>>(
        &mut self,
        instr: &I,
    ) {
        let condition = self.read_register(instr.condition());
        if I::cond_function(condition) {
            self.jump_constant(instr.constant_index());
        } else {
            self.set_pc_after(instr);
        }
    }

    /// Execute a conditional jump if ToBoolean true/false instruction
    fn execute_jump_to_boolean<W: Width, I: GenericJumpToBooleanInstruction<W>>(
        &mut self,
        instr: &I,
    ) {
        let condition = self.read_register(instr.condition());
        if I::cond_function(to_boolean(condition)) {
            self.jump_immediate(instr.offset());
        } else {
            self.set_pc_after(instr);
        }
    }

    /// Execute a conditional jump if ToBoolean true/false constant instruction
    fn execute_jump_to_boolean_constant<W: Width, I: GenericJumpToBooleanConstantInstruction<W>>(
        &mut self,
        instr: &I,
    ) {
        let condition = self.read_register(instr.condition());
        if I::cond_function(to_boolean(condition)) {
            self.jump_constant(instr.constant_index());
        } else {
            self.set_pc_after(instr);
        }
    }

    /// Execute a conditional jump if not undefined instruction
    fn execute_jump_undefined<W: Width, I: GenericJumpUndefinedInstruction<W>>(
        &mut self,
        instr: &I,
    ) {
        let condition = self.read_register(instr.condition());
        if I::cond_function(condition) {
            self.jump_immediate(instr.offset());
        } else {
            self.set_pc_after(instr);
        }
    }

    /// Execute a conditional jump if not undefined constant instruction
    fn execute_jump_undefined_constant<W: Width, I: GenericJumpUndefinedConstantInstruction<W>>(
        &mut self,
        instr: &I,
    ) {
        let condition = self.read_register(instr.condition());
        if I::cond_function(condition) {
            self.jump_constant(instr.constant_index());
        } else {
            self.set_pc_after(instr);
        }
    }

    /// Execute a conditional jump if nullish/not nullish instruction
    fn execute_jump_nullish<W: Width, I: GenericJumpNullishInstruction<W>>(&mut self, instr: &I) {
        let condition = self.read_register(instr.condition());
        if I::cond_function(condition) {
            self.jump_immediate(instr.offset());
        } else {
            self.set_pc_after(instr);
        }
    }

    /// Execute a conditional jump if nullish/not nullish constant instruction
    fn execute_jump_nullish_constant<W: Width, I: GenericJumpNullishConstantInstruction<W>>(
        &mut self,
        instr: &I,
    ) {
        let condition = self.read_register(instr.condition());
        if I::cond_function(condition) {
            self.jump_constant(instr.constant_index());
        } else {
            self.set_pc_after(instr);
        }
    }

    /// Call a function from the Rust runtime. Used for the initial function call, as well as any
    /// re-entrant function calls from within the Rust runtime.
    ///
    /// Assumes that the current PC is the instruction to be executed after this call completes.
    pub fn call_from_rust(
        &mut self,
        function: Handle<Value>,
        receiver: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        // Check whether the value is callable, potentially deferring to proxy.
        let closure_handle = match maybe!(self.check_value_is_callable(function.get())) {
            CallableObject::Closure(closure) => closure.to_handle(),
            CallableObject::Proxy(proxy) => {
                return proxy.to_handle().call(self.cx, receiver, arguments)
            }
            CallableObject::Error(error) => return EvalResult::Throw(error),
        };

        // Get the receiver to use. May allocate.
        let receiver =
            maybe!(self.generate_receiver(Some(receiver.get()), closure_handle.function_ptr()));
        let closure_ptr = closure_handle.get_();

        // Check if this is a call to a function in the Rust runtime
        if let Some(function_id) = closure_ptr.function_ptr().rust_runtime_function_id() {
            // Reuse closure handle for receiver
            let mut receiver_handle = closure_handle.cast::<Value>();
            receiver_handle.replace(receiver);

            // Call rust runtime function directly in its own handle scope
            HandleScope::new(self.cx, |_| {
                self.call_rust_runtime(closure_ptr, function_id, receiver_handle, arguments, None)
            })
        } else {
            // Otherwise this is a call to a JS function in the VM
            let args_rev_iter = arguments.iter().rev().map(Handle::deref);

            // Push the address of the return value
            let mut return_value = Value::undefined();
            let return_value_address = (&mut return_value) as *mut Value;

            // Push a stack frame for the function call, with return address set to return to Rust
            self.push_stack_frame(
                closure_ptr,
                receiver,
                args_rev_iter,
                arguments.len(),
                /* return_to_rust_runtime */ true,
                return_value_address,
            );

            // If a new.target is needed it is implicitly left as undefined

            // Start executing the dispatch loop from the start of the function, returning out of
            // dispatch loop when the marked return address is encountered.
            if let Err(error_value) = self.dispatch_loop() {
                return EvalResult::Throw(error_value.to_handle(self.cx));
            }

            return_value.to_handle(self.cx).into()
        }
    }

    /// Call a function from the Rust runtime. Used for re-entrant constructor calls from within the
    /// Rust runtime.
    ///
    /// Assumes that the current PC is the instruction to be executed after this call completes.
    pub fn construct_from_rust(
        &mut self,
        function: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Handle<ObjectValue>,
    ) -> EvalResult<Handle<ObjectValue>> {
        // Check whether the value is a constructor, potentially deferring to proxy.
        let closure_handle = match self.check_value_is_constructor(function.get()) {
            CallableObject::Closure(closure) => closure.to_handle(),
            // Proxy constructors call directly into the rust runtime
            CallableObject::Proxy(proxy) => {
                return proxy.to_handle().construct(self.cx, arguments, new_target);
            }
            CallableObject::Error(error) => return EvalResult::Throw(error),
        };

        // Create the receiver to use. Allocates.
        let is_base = closure_handle.function_ptr().is_base_constructor();
        let receiver = maybe!(self.generate_constructor_receiver(new_target, is_base));

        // Reuse function handle for receiver
        let closure_ptr = closure_handle.get_();
        let function_ptr = closure_ptr.function_ptr();

        let mut receiver_handle = closure_handle.cast::<Value>();
        receiver_handle.replace(receiver);

        // Check if this is a call to a function in the Rust runtime
        let return_value = if let Some(function_id) = function_ptr.rust_runtime_function_id() {
            // Call rust runtime function directly in its own handle scope
            maybe!(HandleScope::new(self.cx, |_| {
                self.call_rust_runtime(
                    closure_ptr,
                    function_id,
                    receiver_handle,
                    arguments,
                    Some(new_target),
                )
            }))
        } else {
            // Otherwise this is a call to a JS function in the VM
            let args_rev_iter = arguments.iter().rev().map(Handle::deref);

            // Push the address of the return value
            let mut return_value = Value::undefined();
            let return_value_address = (&mut return_value) as *mut Value;

            // Push a stack frame for the function call, with return address set to return to Rust
            self.push_stack_frame(
                closure_ptr,
                receiver,
                args_rev_iter,
                arguments.len(),
                /* return_to_rust_runtime */ true,
                return_value_address,
            );

            // Set the new target if one exists
            self.set_new_target(function_ptr, new_target.get_());

            // Start executing the dispatch loop from the start of the function, returning out of
            // dispatch loop when the marked return address is encountered. May allocate.
            if let Err(error_value) = self.dispatch_loop() {
                return EvalResult::Throw(error_value.to_handle(self.cx));
            }

            return_value.to_handle(self.cx)
        };

        // Use the function's return value if it is an object
        if return_value.is_object() {
            return_value.as_object().into()
        } else {
            self.constructor_non_object_return_value(receiver_handle, is_base)
        }
    }

    #[inline]
    fn execute_generic_call<W: Width>(
        &mut self,
        instr: &impl GenericCallInstruction<W>,
    ) -> EvalResult<()> {
        let function_value = self.read_register(instr.function());
        let receiver = instr.receiver().map(|reg| self.read_register(reg));
        let args = instr.args();

        // Find address of the return value register
        let return_value_address = self.register_address(instr.dest());

        // Check whether the value is callable, potentially deferring to proxy.
        let closure_ptr = match maybe!(self.check_value_is_callable(function_value)) {
            CallableObject::Closure(closure) => closure,
            // Proxy constructors call into the rust runtime
            CallableObject::Proxy(proxy) => {
                // Can default to undefined receiver, which will be eventually coerced by callee
                let receiver = receiver.unwrap_or(Value::undefined()).to_handle(self.cx);
                let arguments = self.prepare_rust_runtime_args(args);
                let return_value = maybe!(proxy.to_handle().call(self.cx, receiver, &arguments));
                unsafe { *return_value_address = return_value.get() };
                return ().into();
            }
            CallableObject::Error(error) => return EvalResult::Throw(error),
        };

        let function_ptr = closure_ptr.function_ptr();

        // Check if this is a call to a function in the Rust runtime
        if let Some(function_id) = function_ptr.rust_runtime_function_id() {
            let return_value = maybe!(HandleScope::new(self.cx, |_| {
                // Get the receiver to use. May allocate.
                let closure_handle = closure_ptr.to_handle();
                let receiver = maybe!(self.generate_receiver(receiver, function_ptr));
                let closure_ptr = closure_handle.get_();

                // Reuse function handle for receiver
                let mut receiver_handle = closure_handle.cast::<Value>();
                receiver_handle.replace(receiver);

                // Prepare arguments for the runtime call
                let arguments = self.prepare_rust_runtime_args(args);

                self.call_rust_runtime(closure_ptr, function_id, receiver_handle, &arguments, None)
            }));

            // Set the return value from the Rust runtime call
            unsafe { *return_value_address = return_value.get() };
        } else {
            // Otherwise this is a call to a JS function in the VM.

            // Get the receiver to use. May allocate.
            let closure_handle = closure_ptr.to_handle();
            let receiver = maybe!(self.generate_receiver(receiver, function_ptr));
            let closure_ptr = closure_handle.get_();

            // Set up the stack frame for the function call. Iterator should be over args in reverse
            // order.
            match self.get_args_slice(args) {
                ArgsSlice::Forward(slice) => {
                    self.push_stack_frame(
                        closure_ptr,
                        receiver.into(),
                        slice.iter().rev(),
                        slice.len(),
                        /* return_to_rust_runtime */ false,
                        return_value_address,
                    );
                }
                ArgsSlice::Reverse(slice) => {
                    self.push_stack_frame(
                        closure_ptr,
                        receiver.into(),
                        slice.iter(),
                        slice.len(),
                        /* return_to_rust_runtime */ false,
                        return_value_address,
                    );
                }
            }

            // If a new.target is needed it is implicitly left as undefined

            // Continue in dispatch loop, executing the first instruction of the function
        }

        ().into()
    }

    #[inline]
    fn execute_generic_construct<W: Width>(
        &mut self,
        instr: &impl GenericConstructInstruction<W>,
    ) -> EvalResult<()> {
        let function_value = self.read_register(instr.function());
        let args = instr.args();

        // Find address of the return value register
        let return_value_address = self.register_address(instr.dest());

        // TODO: Check if this cast is safe
        let new_target = self
            .read_register(instr.new_target())
            .to_handle(self.cx)
            .cast();

        // Check whether the value is a constructor, potentially deferring to proxy.
        let closure_ptr = match self.check_value_is_constructor(function_value) {
            CallableObject::Closure(closure) => closure,
            // Proxy constructors call into the rust runtime
            CallableObject::Proxy(proxy) => {
                let proxy = proxy.to_handle();
                let arguments = self.prepare_rust_runtime_args(args);
                let return_value = maybe!(proxy.construct(self.cx, &arguments, new_target));

                // Can directly return value as proxy constructor is guaranteed to return an object
                unsafe { *return_value_address = return_value.get_().into() };

                return ().into();
            }
            CallableObject::Error(error) => return EvalResult::Throw(error),
        };

        let function_ptr = closure_ptr.function_ptr();
        let is_base = function_ptr.is_base_constructor();

        // Check if this is a call to a function in the Rust runtime
        let return_value = if let Some(function_id) = function_ptr.rust_runtime_function_id() {
            let return_value: Handle<ObjectValue> = maybe!(HandleScope::new(self.cx, |_| {
                let closure_handle = closure_ptr.to_handle();

                // Create the receiver to use. Allocates.
                let receiver = maybe!(self.generate_constructor_receiver(new_target, is_base));

                // Reuse function handle for receiver
                let closure_ptr = closure_handle.get_();
                let mut receiver_handle = closure_handle.cast::<Value>();
                receiver_handle.replace(receiver);

                // Prepare arguments for the runtime call
                let arguments = self.prepare_rust_runtime_args(args);

                let return_value = maybe!(self.call_rust_runtime(
                    closure_ptr,
                    function_id,
                    receiver_handle,
                    &arguments,
                    Some(new_target)
                ));

                // Use the function's return value if it is an object
                if return_value.is_object() {
                    return_value.as_object().into()
                } else {
                    self.constructor_non_object_return_value(receiver_handle, is_base)
                }
            }));

            return_value.get_()
        } else {
            // Otherwise this is a call to a JS function in the VM.
            let closure_handle = closure_ptr.to_handle();
            let function_handle = function_ptr.to_handle();

            // Create the receiver to use. Allocates.
            let receiver = maybe!(self.generate_constructor_receiver(new_target, is_base));

            let closure_ptr = closure_handle.get_();
            let mut receiver_handle = closure_handle.cast::<Value>();
            receiver_handle.replace(receiver);

            // Push the address of the return value
            let mut return_value = Value::undefined();
            let inner_call_return_value_address = (&mut return_value) as *mut Value;

            // Set up the stack frame for the function call. Iterator should be over args in reverse
            // order.
            match self.get_args_slice(args) {
                ArgsSlice::Forward(slice) => {
                    self.push_stack_frame(
                        closure_ptr,
                        receiver,
                        slice.iter().rev(),
                        slice.len(),
                        /* return_to_rust_runtime */ true,
                        inner_call_return_value_address,
                    );
                }
                ArgsSlice::Reverse(slice) => {
                    self.push_stack_frame(
                        closure_ptr,
                        receiver,
                        slice.iter(),
                        slice.len(),
                        /* return_to_rust_runtime */ true,
                        inner_call_return_value_address,
                    );
                }
            }

            // Set the new target if one exists
            self.set_new_target(function_handle.get_(), new_target.get_());

            // Start executing the dispatch loop from the start of the function, returning out of
            // dispatch loop when the marked return address is encountered.
            if let Err(error_value) = self.dispatch_loop() {
                return EvalResult::Throw(error_value.to_handle(self.cx));
            }

            // Use the function's return value if it is an object
            if return_value.is_object() {
                return_value.as_object()
            } else {
                maybe!(self.constructor_non_object_return_value(receiver_handle, is_base)).get_()
            }
        };

        // Set the return value from the Rust runtime call
        unsafe { *return_value_address = return_value.into() };

        ().into()
    }

    /// Check that a value is a callable (either a closure or proxy object), returning the
    /// categorization of the callable object
    #[inline]
    fn check_value_is_callable(&mut self, value: Value) -> EvalResult<CallableObject> {
        if value.is_pointer() {
            let kind = value.as_pointer().descriptor().kind();
            if kind == ObjectKind::Closure {
                let closure = value.as_pointer().cast::<Closure>();

                // Class constructors cannot be called directly
                if closure.function_ptr().is_class_constructor() {
                    let function_realm = closure.function_ptr().realm_ptr();
                    let error_value = maybe!(TypeError::new_with_message_in_realm(
                        self.cx,
                        function_realm,
                        "cannot call class constructor",
                    ));

                    return CallableObject::Error(error_value.into()).into();
                }

                // All other closures all callable
                return CallableObject::Closure(closure).into();
            } else if kind == ObjectKind::Proxy {
                // Check if proxy is callable, and if so return the attached closure
                let proxy_object = value.as_pointer().cast::<ProxyObject>();
                if proxy_object.is_callable() {
                    return CallableObject::Proxy(proxy_object).into();
                }
            }
        }

        CallableObject::Error(type_error_value(self.cx, "value is not a function")).into()
    }

    /// Check that a value is a constructor (either a closure or proxy object), returning the
    /// categorization of the callable object
    #[inline]
    fn check_value_is_constructor(&self, value: Value) -> CallableObject {
        if value.is_pointer() {
            let kind = value.as_pointer().descriptor().kind();
            if kind == ObjectKind::Closure {
                // Check if closure is a constructor
                let closure = value.as_pointer().cast::<Closure>();
                if closure.function_ptr().is_constructor() {
                    return CallableObject::Closure(closure);
                }
            } else if kind == ObjectKind::Proxy {
                // Check if proxy is a constructor
                let proxy_object = value.as_pointer().cast::<ProxyObject>();
                if proxy_object.is_constructor() {
                    return CallableObject::Proxy(proxy_object);
                }
            }
        }

        CallableObject::Error(type_error_value(self.cx, "value is not a constructor"))
    }

    /// Create a new stack frame constructed for the following arguments.
    ///
    /// Also saves the current PC on the stack frame as the return address, setting the PC to the
    /// first instruction in the function.
    #[inline]
    fn push_stack_frame<'a, I: Iterator<Item = &'a Value>>(
        &mut self,
        closure: HeapPtr<Closure>,
        receiver: Value,
        args_rev_iter: I,
        argc: usize,
        return_to_rust_runtime: bool,
        return_value_address: *mut Value,
    ) {
        let bytecode_function = closure.function_ptr();
        let scope = closure.scope();

        // Push arguments
        self.push_call_arguments(bytecode_function, args_rev_iter, argc);

        // Push the receiver if one is supplied, or the default receiver otherwise
        self.push(receiver.as_raw_bits() as StackSlotValue);

        // Push argc
        self.push(argc);

        // Push the function
        self.push(closure.as_ptr() as StackSlotValue);

        // Push the constant table
        let constant_table = unsafe { std::mem::transmute(bytecode_function.constant_table_ptr()) };
        self.push(constant_table);

        // Push the current scope
        self.push(scope.as_ptr() as StackSlotValue);

        // Push the address of the return value register
        self.push(return_value_address as StackSlotValue);

        // Push the address of the next instruction as the return address
        let return_address_slot = if return_to_rust_runtime {
            StackFrame::return_address_from_rust(self.pc)
        } else {
            StackFrame::return_address_from_vm(self.pc)
        };
        self.push(return_address_slot as StackSlotValue);

        // Push the frame pointer, creating a new frame pointer
        self.push_fp();

        // Make room for function locals
        self.allocate_local_registers(bytecode_function.num_registers());

        // Start executing from the first instruction of the function
        self.pc = bytecode_function.bytecode().as_ptr();
    }

    /// Pop the current stack frame, restoring the previous frame pointer and PC.
    #[inline]
    fn pop_stack_frame(&mut self) {
        unsafe {
            // Handle under/overapplication of arguments.
            let num_formal_parameters = self.closure().function_ptr().num_parameters() as usize;
            let argc = self.argc();
            let num_arguments = argc.max(num_formal_parameters);

            self.pc = self.get_return_address();
            self.sp = self.fp.add(FIRST_ARGUMENT_SLOT_INDEX + num_arguments);
            self.fp = *self.fp as *mut StackSlotValue;
        }
    }

    #[inline]
    fn set_new_target(
        &mut self,
        function: HeapPtr<BytecodeFunction>,
        new_target: HeapPtr<ObjectValue>,
    ) {
        if let Some(index) = function.new_target_index() {
            // Set the new.target register to the provided new target
            self.write_register(Register::<ExtraWide>::local(index as usize), new_target.into());
        }
    }

    /// Find slice over the arguments given argv and argc, starting with last argument.
    #[inline]
    fn get_args_rev_slice<'a, 'b, W: Width>(
        &'a self,
        argv: Register<W>,
        argc: UInt<W>,
    ) -> &'b [Value] {
        self.get_reg_rev_slice(argv, argc.value().to_usize())
    }

    /// Find slice over a slice of registers given argv and argc, starting with last argument.
    #[inline]
    fn get_reg_rev_slice<'a, 'b, W: Width>(
        &'a self,
        argv: Register<W>,
        argc: usize,
    ) -> &'b [Value] {
        let argv = self.register_address(argv);

        unsafe {
            std::slice::from_raw_parts(argv.sub(argc.saturating_sub(1)) as *const Value, argc)
        }
    }

    /// Find slice over the arguments passed in the given array object.
    #[inline]
    fn get_varargs_slice<'a, 'b, W: Width>(&'a mut self, array: Register<W>) -> &'b [Value] {
        // Return slice directly over the dense properties of the array
        let array_properties = self.read_register(array).as_object().array_properties();

        // Return slice over populated slice of array
        debug_assert!(array_properties.is_dense());
        let dense_properties = array_properties.as_dense();
        let slice = &dense_properties.as_slice()[..(dense_properties.len() as usize)];

        // Break the lifetime since slice is over a slice of the managed heap
        unsafe { std::mem::transmute(slice) }
    }

    /// Find slice over the argument values given the generic call arguments.
    #[inline]
    fn get_args_slice<'a, 'b, W: Width>(&'a mut self, args: GenericCallArgs<W>) -> ArgsSlice<'b> {
        match args {
            // Find slice over the arguments, starting with last argument
            GenericCallArgs::Stack { argv, argc } => {
                ArgsSlice::Reverse(self.get_args_rev_slice(argv, argc))
            }
            GenericCallArgs::Varargs { array } => ArgsSlice::Forward(self.get_varargs_slice(array)),
        }
    }

    /// Convert arguments to the form expected by the rust runtime - a vector of the arguments
    /// behind handles in order.
    fn prepare_rust_runtime_args<W: Width>(
        &mut self,
        args: GenericCallArgs<W>,
    ) -> Vec<Handle<Value>> {
        let mut arguments = vec![];

        // Arguments should be iterated in order
        match self.get_args_slice(args) {
            ArgsSlice::Forward(slice) => {
                for arg in slice {
                    arguments.push(arg.to_handle(self.cx));
                }
            }
            ArgsSlice::Reverse(slice) => {
                for arg in slice.iter().rev() {
                    arguments.push(arg.to_handle(self.cx));
                }
            }
        }

        arguments
    }

    /// Push call arguments onto the stack, handling over/underapplication. Takes an iterator over
    /// the arguments in reverse order.
    ///
    /// Does not push the receiver.
    #[inline]
    fn push_call_arguments<'a, I: Iterator<Item = &'a Value>>(
        &mut self,
        func: HeapPtr<BytecodeFunction>,
        args_rev_iter: I,
        argc: usize,
    ) {
        let mut sp = self.sp;

        // Handle under application of arguments, pushing undefined for missing arguments. This
        // guarantees that the number of pushed arguments equals max(argc, func.num_parameters).
        let num_parameters = func.num_parameters() as usize;
        if argc < num_parameters {
            for _ in argc..num_parameters {
                unsafe {
                    sp = sp.sub(1);
                    *sp = Value::undefined().as_raw_bits() as StackSlotValue;
                }
            }
        }

        // First push arguments onto the stack in reverse order
        for arg in args_rev_iter {
            unsafe {
                sp = sp.sub(1);
                *sp = arg.as_raw_bits() as StackSlotValue;
            }
        }

        self.sp = sp;
    }

    /// Generate the receiver to be used given an optional explicit receiver value and the called
    /// function.
    #[inline]
    fn generate_receiver(
        &mut self,
        receiver: Option<Value>,
        function: HeapPtr<BytecodeFunction>,
    ) -> EvalResult<Value> {
        if let Some(receiver) = receiver {
            self.coerce_receiver(receiver, function)
        } else {
            self.default_receiver(function).into()
        }
    }

    /// Return the default receiver used for a function call when no receiver is provided.
    #[inline]
    fn default_receiver(&self, function: HeapPtr<BytecodeFunction>) -> Value {
        if function.is_strict() {
            Value::undefined()
        } else {
            // Global object is used
            function.realm_ptr().global_object_ptr().into()
        }
    }

    /// Generate the receiver to be used for a constructor call.
    #[inline]
    fn generate_constructor_receiver(
        &mut self,
        new_target: Handle<ObjectValue>,
        is_base: bool,
    ) -> EvalResult<Value> {
        if is_base {
            let new_object: Value = maybe!(object_create_from_constructor::<ObjectValue>(
                self.cx,
                new_target,
                ObjectKind::OrdinaryObject,
                Intrinsic::ObjectPrototype,
            ))
            .into();

            new_object.into()
        } else {
            // Receiver starts out as the empty sentinel value in derived constructors, and only
            // will be set once super() is called.
            Value::empty().into()
        }
    }

    /// Create the return value for a constructor call that doesn't return an object.
    #[inline]
    fn constructor_non_object_return_value(
        &mut self,
        receiver: Handle<Value>,
        is_base: bool,
    ) -> EvalResult<Handle<ObjectValue>> {
        if is_base {
            // Base classes always return the receiver. Receiver is guaranteed to be an object
            // created earlier in this construct call.
            receiver.as_object().into()
        } else {
            // Derived constructors can either return an object or undefined. The derived
            // constructor implementation will return `this` if the code syntactically returns
            // undefined, so if the return value wasn't an object we should error.
            type_error(self.cx, "derived constructor must return object or undefined")
        }
    }

    /// Return the coerced receiver that should be passed to a function call. No coercion is
    /// necessary in strict mode, otherwise receiver must be coerced to an object, using the
    /// global object if the receiver is nullish.
    #[inline]
    fn coerce_receiver(
        &mut self,
        receiver: Value,
        function: HeapPtr<BytecodeFunction>,
    ) -> EvalResult<Value> {
        let value = if function.is_strict() {
            receiver
        } else if receiver.is_nullish() {
            // Global object is used if receiver is nullish
            function.realm_ptr().global_object_ptr().into()
        } else {
            let receiver = receiver.to_handle(self.cx);
            let receiver_object = maybe!(to_object(self.cx, receiver));
            receiver_object.get_().into()
        };

        value.into()
    }

    /// Allocate space for local registers, initializing to undefined
    fn allocate_local_registers(&mut self, num_registers: u32) {
        let num_registers = num_registers as usize;
        self.sp = unsafe { self.sp.sub(num_registers) };
        let slice = unsafe { std::slice::from_raw_parts_mut(self.sp as *mut Value, num_registers) };
        slice.fill(Value::undefined());
    }

    /// Calls a function in the Rust runtime, returning the result.
    ///
    /// Sets up a minimal VM stack frame for the Rust runtime function that can be used when
    /// collecting a stack trace.
    #[inline]
    fn call_rust_runtime(
        &mut self,
        function: HeapPtr<Closure>,
        function_id: RustRuntimeFunctionId,
        receiver: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        // Push a minimal stack frame for the Rust runtime function. No arguments are pushed in.
        self.push_stack_frame(
            function,
            /* receiver */ Value::undefined(),
            /* arguments */ [].iter(),
            /* argc */ 0,
            /* return_to_rust_runtime */ true,
            /* return value address */ std::ptr::null_mut(),
        );

        // Perform the runtime call. May allocate.
        let rust_function = self.cx.rust_runtime_functions.get_function(function_id);
        let result = rust_function(self.cx, receiver, &arguments, new_target);

        // Clean up the stack frame
        self.pop_stack_frame();

        result
    }

    #[inline]
    fn execute_call_maybe_eval<W: Width>(
        &mut self,
        instr: &CallMaybeEvalInstruction<W>,
    ) -> EvalResult<()> {
        let callee = self.read_register(instr.function());

        // Check if the callee is the eval function, if so this is a direct eval
        let eval_function_ptr = self.cx.get_intrinsic_ptr(Intrinsic::Eval);
        if callee.is_object() && same_object_value(callee.as_object(), eval_function_ptr) {
            let argc = instr.argc().value().to_usize();
            let flags = EvalFlags::from_bits_retain(instr.flags().value().to_usize() as u8);
            let dest = instr.dest();

            // Return undefined if there are no arguments
            if argc == 0 {
                self.write_register(dest, Value::undefined());
                return ().into();
            }

            // Only the first argument is passed to eval
            let arg = self.read_register_to_handle(instr.argv());

            self.direct_eval(arg, dest, flags)
        } else {
            self.execute_generic_call(instr)
        }
    }

    #[inline]
    fn execute_call_maybe_eval_varargs<W: Width>(
        &mut self,
        instr: &CallMaybeEvalVarargsInstruction<W>,
    ) -> EvalResult<()> {
        let callee = self.read_register(instr.function());

        // Check if the callee is the eval function, if so this is a direct eval
        let eval_function_ptr = self.cx.get_intrinsic_ptr(Intrinsic::Eval);
        if callee.is_object() && same_object_value(callee.as_object(), eval_function_ptr) {
            let flags = EvalFlags::from_bits_retain(instr.flags().value().to_usize() as u8);
            let dest = instr.dest();

            // Extract the first argument from the array
            let args_slice = self.get_varargs_slice(instr.args());
            if args_slice.is_empty() {
                self.write_register(instr.dest(), Value::undefined());
                return ().into();
            }

            let arg = args_slice[0].to_handle(self.cx);

            self.direct_eval(arg, dest, flags)
        } else {
            self.execute_generic_call(instr)
        }
    }

    #[inline]
    fn direct_eval<W: Width>(
        &mut self,
        arg: Handle<Value>,
        dest: Register<W>,
        flags: EvalFlags,
    ) -> EvalResult<()> {
        let is_strict_caller = self.closure().function_ptr().is_strict();
        let scope = self.scope().to_handle();

        // Allocates
        let result = maybe!(perform_eval(self.cx, arg, is_strict_caller, Some(scope), flags));
        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_default_super_call<W: Width>(
        &mut self,
        _: &DefaultSuperCallInstruction<W>,
    ) -> EvalResult<()> {
        let super_constructor = must!(self
            .closure()
            .to_handle()
            .object()
            .get_prototype_of(self.cx));

        if super_constructor.is_none() || !is_callable_object(super_constructor.unwrap()) {
            return type_error(self.cx, "super must be a constructor");
        }
        let super_constructor = super_constructor.unwrap();

        // Place all arguments behind handles
        let args = StackFrame::for_fp(self.fp)
            .args()
            .iter()
            .map(|arg| arg.to_handle(self.cx))
            .collect::<Vec<_>>();

        // New target is in the first local register
        let new_target = self
            .read_register_to_handle(Register::<W>::local(0))
            .as_object();

        // Call the super constructor
        let this_value =
            maybe!(self.construct_from_rust(super_constructor.into(), &args, new_target));

        // Store result of super call to the `this` register, which will be returned at end of call
        self.write_register(Register::<W>::this(), this_value.get_().into());

        ().into()
    }

    #[inline]
    fn execute_mov<W: Width>(&mut self, instr: &MovInstruction<W>) {
        let src_value = self.read_register(instr.src());
        self.write_register(instr.dest(), src_value)
    }

    #[inline]
    fn execute_load_immediate<W: Width>(&mut self, instr: &LoadImmediateInstruction<W>) {
        let immediate = Value::smi(instr.immediate().value().to_i32());
        self.write_register(instr.dest(), immediate)
    }

    #[inline]
    fn execute_load_undefined<W: Width>(&mut self, instr: &LoadUndefinedInstruction<W>) {
        self.write_register(instr.dest(), Value::undefined())
    }

    #[inline]
    fn execute_load_empty<W: Width>(&mut self, instr: &LoadEmptyInstruction<W>) {
        self.write_register(instr.dest(), Value::empty())
    }

    #[inline]
    fn execute_load_null<W: Width>(&mut self, instr: &LoadNullInstruction<W>) {
        self.write_register(instr.dest(), Value::null())
    }

    #[inline]
    fn execute_load_true<W: Width>(&mut self, instr: &LoadTrueInstruction<W>) {
        self.write_register(instr.dest(), Value::bool(true))
    }

    #[inline]
    fn execute_load_false<W: Width>(&mut self, instr: &LoadFalseInstruction<W>) {
        self.write_register(instr.dest(), Value::bool(false))
    }

    #[inline]
    fn execute_load_constant<W: Width>(&mut self, instr: &LoadConstantInstruction<W>) {
        let constant = self.get_constant(instr.constant_index());
        self.write_register(instr.dest(), constant)
    }

    #[inline]
    fn execute_load_global<W: Width>(
        &mut self,
        instr: &LoadGlobalInstruction<W>,
    ) -> EvalResult<()> {
        self.execute_generic_load_global(
            instr.dest(),
            instr.constant_index(),
            /* error_on_unresolved */ true,
        )
    }

    #[inline]
    fn execute_load_global_or_unresolved<W: Width>(
        &mut self,
        instr: &LoadGlobalOrUnresolvedInstruction<W>,
    ) -> EvalResult<()> {
        self.execute_generic_load_global(
            instr.dest(),
            instr.constant_index(),
            /* error_on_unresolved */ false,
        )
    }

    #[inline]
    fn execute_generic_load_global<W: Width>(
        &mut self,
        dest: Register<W>,
        name_constant_index: ConstantIndex<W>,
        error_on_unresolved: bool,
    ) -> EvalResult<()> {
        HandleScope::new(self.cx, |cx| {
            let name = self.get_constant(name_constant_index);
            let name = name.as_string().to_handle();

            // May allocate, reuse name handle
            let name_key = PropertyKey::string(cx, name);
            let name_key = name.replace_into(name_key);

            let global_object = self.closure().global_object();

            let value = if maybe!(has_property(cx, global_object, name_key)) {
                // Get the property from the global object
                maybe!(get(cx, global_object, name_key)).get()
            } else if let Some(value) = self
                .closure()
                .realm()
                .get_lexical_name(name.as_flat().get_())
            {
                // Otherwise might be a lexical name in one of realm's global scopes
                value
            } else if error_on_unresolved {
                // Error if property is not found on the global object
                return err_not_defined(cx, name);
            } else {
                // If not erroring, return undefined for unresolved names
                self.write_register(dest, Value::undefined());
                return ().into();
            };

            self.write_register(dest, value);

            ().into()
        })
    }

    #[inline]
    fn execute_store_global<W: Width>(
        &mut self,
        instr: &StoreGlobalInstruction<W>,
    ) -> EvalResult<()> {
        HandleScope::new(self.cx, |cx| {
            let value = self.read_register_to_handle(instr.value());

            let name = self.get_constant(instr.constant_index());
            let name = name.as_string().to_handle();

            // May allocate, reuse name handle
            let name_key = PropertyKey::string(cx, name);
            let name_key = name.replace_into(name_key);

            let mut global_object = self.closure().global_object();

            let success = if maybe!(has_property(cx, global_object, name_key)) {
                // Check if there is a global var with the given name then set the property on the
                // global object.
                maybe!(global_object.set(cx, name_key, value, global_object.into()))
            } else if maybe!(self.closure().realm().set_lexical_name(
                self.cx,
                name.as_flat().get_(),
                value.get(),
            )) {
                // Set the global lexical binding with the given name if it exists
                true
            } else if self.closure().function_ptr().is_strict() {
                // Otherwise if in strict mode, error on unresolved name
                return err_not_defined(cx, name);
            } else {
                // Otherwise in sloppy mode create a new global property
                return set(cx, global_object, name_key, value, false);
            };

            // If property set failed and in strict mode then error, otherwise silently ignore
            // failure in sloppy mode.
            if !success && self.closure().function_ptr().is_strict() {
                return err_cannot_set_property(cx, name);
            }

            ().into()
        })
    }

    #[inline]
    fn execute_load_dynamic<W: Width>(
        &mut self,
        instr: &LoadDynamicInstruction<W>,
    ) -> EvalResult<()> {
        self.execute_generic_load_dynamic(
            instr.dest(),
            instr.name_index(),
            /* error_on_unresolved */ true,
        )
    }

    #[inline]
    fn execute_load_dynamic_or_unresolved<W: Width>(
        &mut self,
        instr: &LoadDynamicOrUnresolvedInstruction<W>,
    ) -> EvalResult<()> {
        self.execute_generic_load_dynamic(
            instr.dest(),
            instr.name_index(),
            /* error_on_unresolved */ false,
        )
    }

    #[inline]
    fn execute_generic_load_dynamic<W: Width>(
        &mut self,
        dest: Register<W>,
        name_constant_index: ConstantIndex<W>,
        error_on_unresolved: bool,
    ) -> EvalResult<()> {
        HandleScope::new(self.cx, |cx| {
            let name = self.get_constant(name_constant_index);
            let name = name.as_string().to_handle();

            let scope = self.scope().to_handle();

            if let Some(value) = maybe!(scope.lookup(cx, name)) {
                self.write_register(dest, value.get());
                ().into()
            } else {
                if error_on_unresolved {
                    // Error if name could not be resolved
                    err_not_defined(cx, name)
                } else {
                    // If not erroring, return undefined for unresolved names
                    self.write_register(dest, Value::undefined());
                    ().into()
                }
            }
        })
    }

    #[inline]
    fn execute_store_dynamic<W: Width>(
        &mut self,
        instr: &StoreDynamicInstruction<W>,
    ) -> EvalResult<()> {
        HandleScope::new(self.cx, |cx| {
            let value = self.read_register(instr.value()).to_handle(cx);

            let name = self.get_constant(instr.name_index());
            let name = name.as_string().to_handle();

            let mut scope = self.scope().to_handle();
            let is_strict = self.closure().function_ptr().is_strict();

            let found_name = maybe!(scope.lookup_store(cx, name, value, is_strict));

            if !found_name {
                if is_strict {
                    // In strict mode names must be resolved otherwise error
                    return err_not_defined(cx, name);
                } else {
                    // Name is an interned string (and cannot be a number) so is already a property
                    // key.
                    let name_key = name.cast::<PropertyKey>();

                    // If in sloppy mode, create a new property on the global object
                    let mut global_object = self.closure().global_object();
                    maybe!(global_object.set(cx, name_key, value, global_object.into()));
                }
            }

            ().into()
        })
    }

    #[inline]
    fn execute_add<W: Width>(&mut self, instr: &AddInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_add(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_sub<W: Width>(&mut self, instr: &SubInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_subtract(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_mul<W: Width>(&mut self, instr: &MulInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_multiply(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_div<W: Width>(&mut self, instr: &DivInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_divide(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_rem<W: Width>(&mut self, instr: &RemInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_remainder(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_exp<W: Width>(&mut self, instr: &ExpInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_exponentiation(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_bit_and<W: Width>(&mut self, instr: &BitAndInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_bitwise_and(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_bit_or<W: Width>(&mut self, instr: &BitOrInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_bitwise_or(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_bit_xor<W: Width>(&mut self, instr: &BitXorInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_bitwise_xor(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_shift_left<W: Width>(&mut self, instr: &ShiftLeftInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_shift_left(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_shift_right_arithmetic<W: Width>(
        &mut self,
        instr: &ShiftRightArithmeticInstruction<W>,
    ) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_shift_right_arithmetic(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_shift_right_logical<W: Width>(
        &mut self,
        instr: &ShiftRightLogicalInstruction<W>,
    ) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_shift_right_logical(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_loose_equal<W: Width>(
        &mut self,
        instr: &LooseEqualInstruction<W>,
    ) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(is_loosely_equal(self.cx, left_value, right_value));

        self.write_register(dest, Value::bool(result));

        ().into()
    }

    #[inline]
    fn execute_loose_not_equal<W: Width>(
        &mut self,
        instr: &LooseNotEqualInstruction<W>,
    ) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(is_loosely_equal(self.cx, left_value, right_value));

        self.write_register(dest, Value::bool(!result));

        ().into()
    }

    #[inline]
    fn execute_strict_equal<W: Width>(&mut self, instr: &StrictEqualInstruction<W>) {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = is_strictly_equal(left_value, right_value);

        self.write_register(dest, Value::bool(result));
    }

    #[inline]
    fn execute_strict_not_equal<W: Width>(&mut self, instr: &StrictNotEqualInstruction<W>) {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = is_strictly_equal(left_value, right_value);

        self.write_register(dest, Value::bool(!result));
    }

    #[inline]
    fn execute_less_than<W: Width>(&mut self, instr: &LessThanInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_less_than(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_less_than_or_equal<W: Width>(
        &mut self,
        instr: &LessThanOrEqualInstruction<W>,
    ) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_less_than_or_equal(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_greater_than<W: Width>(
        &mut self,
        instr: &GreaterThanInstruction<W>,
    ) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_greater_than(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_greater_than_or_equal<W: Width>(
        &mut self,
        instr: &GreaterThanOrEqualInstruction<W>,
    ) -> EvalResult<()> {
        let left_value = self.read_register_to_handle(instr.left());
        let right_value = self.read_register_to_handle(instr.right());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_greater_than_or_equal(self.cx, left_value, right_value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_neg<W: Width>(&mut self, instr: &NegInstruction<W>) -> EvalResult<()> {
        let value = self.read_register_to_handle(instr.value());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_negate(self.cx, value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_inc<W: Width>(&mut self, instr: &IncInstruction<W>) {
        let dest = instr.dest();
        let value = self.read_register(dest);

        // Assume that the value is numeric
        debug_assert!(value.is_number() || value.is_bigint());

        let new_value = if value.is_smi() {
            // Fast path for smis - but check if they would overflow into floats
            let smi_value = value.as_smi();
            if smi_value < i32::MAX {
                Value::smi(smi_value + 1)
            } else {
                Value::from(i32::MAX as f64 + 1.0)
            }
        } else if !value.is_pointer() {
            Value::number(value.as_number() + 1.0)
        } else {
            let inc_value = value.as_bigint().bigint() + 1;
            BigIntValue::new_ptr(self.cx, inc_value).into()
        };

        self.write_register(dest, new_value);
    }

    #[inline]
    fn execute_dec<W: Width>(&mut self, instr: &DecInstruction<W>) {
        let dest = instr.dest();
        let value = self.read_register(dest);

        // Assume that the value is numeric
        debug_assert!(value.is_number() || value.is_bigint());

        let new_value = if value.is_smi() {
            // Fast path for smis - but check if they would overflow into floats
            let smi_value = value.as_smi();
            if smi_value > i32::MIN {
                Value::smi(smi_value - 1)
            } else {
                Value::from(i32::MIN as f64 - 1.0)
            }
        } else if !value.is_pointer() {
            Value::number(value.as_number() - 1.0)
        } else {
            let inc_value = value.as_bigint().bigint() - 1;
            BigIntValue::new_ptr(self.cx, inc_value).into()
        };

        self.write_register(dest, new_value);
    }

    #[inline]
    fn execute_log_not<W: Width>(&mut self, instr: &LogNotInstruction<W>) {
        let value = self.read_register(instr.value());
        let result = Value::bool(!to_boolean(value));
        self.write_register(instr.dest(), result);
    }

    #[inline]
    fn execute_bit_not<W: Width>(&mut self, instr: &BitNotInstruction<W>) -> EvalResult<()> {
        let value = self.read_register_to_handle(instr.value());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_bitwise_not(self.cx, value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_typeof<W: Width>(&mut self, instr: &TypeOfInstruction<W>) {
        let value = self.read_register_to_handle(instr.value());
        let dest = instr.dest();

        // May allocate
        let result = eval_typeof(self.cx, value);

        self.write_register(dest, result.cast::<Value>().get());
    }

    #[inline]
    fn execute_in<W: Width>(&mut self, instr: &InInstruction<W>) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());
        let key = self.read_register_to_handle(instr.key());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_in_expression(self.cx, key, object));

        self.write_register(dest, Value::bool(result));

        ().into()
    }

    #[inline]
    fn execute_instance_of<W: Width>(
        &mut self,
        instr: &InstanceOfInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());
        let constructor = self.read_register_to_handle(instr.constructor());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_instanceof_expression(self.cx, object, constructor));

        self.write_register(dest, Value::bool(result));

        ().into()
    }

    #[inline]
    fn execute_to_number<W: Width>(&mut self, instr: &ToNumberInstruction<W>) -> EvalResult<()> {
        let value = self.read_register_to_handle(instr.value());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(to_number(self.cx, value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_to_numeric<W: Width>(&mut self, instr: &ToNumericInstruction<W>) -> EvalResult<()> {
        let value = self.read_register_to_handle(instr.value());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(to_numeric(self.cx, value));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_to_string<W: Width>(&mut self, instr: &ToStringInstruction<W>) -> EvalResult<()> {
        let value = self.read_register_to_handle(instr.value());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(to_string(self.cx, value));

        self.write_register(dest, result.get_().into());

        ().into()
    }

    #[inline]
    fn execute_to_property_key<W: Width>(
        &mut self,
        instr: &ToPropertyKeyInstruction<W>,
    ) -> EvalResult<()> {
        let value = self.read_register_to_handle(instr.value());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(to_property_key(self.cx, value));

        self.write_register(dest, result.cast::<Value>().get());

        ().into()
    }

    #[inline]
    fn execute_to_object<W: Width>(&mut self, instr: &ToObjectInstruction<W>) -> EvalResult<()> {
        let value = self.read_register_to_handle(instr.value());
        let dest = instr.dest();

        // May allocate
        let result = maybe!(to_object(self.cx, value));

        self.write_register(dest, result.get_().into());

        ().into()
    }

    #[inline]
    fn execute_new_closure<W: Width>(&mut self, instr: &NewClosureInstruction<W>) {
        let func = self.get_constant(instr.function_index());
        let func = func.to_handle(self.cx).cast::<BytecodeFunction>();

        let dest = instr.dest();
        let scope = self.scope().to_handle();

        // Allocates
        let closure = Closure::new(self.cx, func, scope);

        self.write_register(dest, Value::object(closure.get_().cast()));
    }

    #[inline]
    fn execute_new_generator<W: Width>(
        &mut self,
        instr: &NewGeneratorInstruction<W>,
    ) -> EvalResult<()> {
        let func = self.get_constant(instr.function_index());
        let func = func.to_handle(self.cx).cast::<BytecodeFunction>();

        let dest = instr.dest();
        let scope = self.scope().to_handle();

        // Allocates
        let func_proto = self.cx.get_intrinsic(Intrinsic::GeneratorFunctionPrototype);
        let closure = Closure::new_with_proto(self.cx, func, scope, func_proto);

        maybe!(GeneratorPrototype::install_on_generator_function(self.cx, closure));

        self.write_register(dest, Value::object(closure.get_().cast()));

        ().into()
    }

    #[inline]
    fn execute_new_object<W: Width>(&mut self, instr: &NewObjectInstruction<W>) {
        let dest = instr.dest();

        // Allocates
        let object = ordinary_object_create(self.cx);

        self.write_register(dest, object.cast::<Value>().get());
    }

    #[inline]
    fn execute_new_array<W: Width>(&mut self, instr: &NewArrayInstruction<W>) {
        let dest = instr.dest();

        // Allocates
        let array = must!(array_create(self.cx, 0, None));

        self.write_register(dest, array.cast::<Value>().get());
    }

    #[inline]
    fn execute_new_regexp<W: Width>(&mut self, instr: &NewRegExpInstruction<W>) -> EvalResult<()> {
        let compiled_regexp = self.get_constant(instr.regexp_index());
        let compiled_regexp = compiled_regexp
            .to_handle(self.cx)
            .cast::<CompiledRegExpObject>();

        let dest = instr.dest();

        // Allocates
        let regexp = maybe!(RegExpObject::new_from_compiled_regexp(self.cx, compiled_regexp));

        self.write_register(dest, Value::object(regexp.get_().cast()));

        ().into()
    }

    #[inline]
    fn execute_new_mapped_arguments<W: Width>(&mut self, instr: &NewMappedArgumentsInstruction<W>) {
        let dest = instr.dest();

        let closure = self.closure().to_handle();
        let scope = self.scope().to_handle();
        let num_parameters = closure.function_ptr().num_parameters() as usize;

        let arguments = StackFrame::for_fp(self.fp)
            .args()
            .iter()
            .map(|arg| arg.to_handle(self.cx))
            .collect::<Vec<_>>();

        // Allocates
        let arguments_object =
            MappedArgumentsObject::new(self.cx, closure, &arguments, scope, num_parameters);

        self.write_register(dest, arguments_object.cast::<Value>().get());
    }

    #[inline]
    fn execute_new_unmapped_arguments<W: Width>(
        &mut self,
        instr: &NewUnmappedArgumentsInstruction<W>,
    ) {
        let dest = instr.dest();

        // Place all arguments (up to argc) behind handles
        let arguments = StackFrame::for_fp(self.fp)
            .args()
            .iter()
            .map(|arg| arg.to_handle(self.cx))
            .collect::<Vec<_>>();

        // Allocates
        let arguments_object = create_unmapped_arguments_object(self.cx, &arguments);

        self.write_register(dest, arguments_object.get());
    }

    #[inline]
    fn execute_new_class<W: Width>(&mut self, instr: &NewClassInstruction<W>) -> EvalResult<()> {
        let dest = instr.dest();

        let class_names = self
            .get_constant(instr.class_names_index())
            .to_handle(self.cx)
            .cast::<ClassNames>();
        let constructor_function = self
            .get_constant(instr.constructor_function_index())
            .to_handle(self.cx)
            .cast::<BytecodeFunction>();

        let super_class = self.read_register_to_handle(instr.super_class());
        let super_class = if super_class.is_empty() {
            None
        } else {
            Some(super_class)
        };

        let method_arguments = self
            .get_reg_rev_slice(instr.methods(), class_names.num_arguments())
            .iter()
            .rev()
            .map(|value| value.to_handle(self.cx))
            .collect::<Vec<_>>();

        // Allocates
        let constructor = maybe!(new_class(
            self.cx,
            class_names,
            constructor_function,
            super_class,
            &method_arguments
        ));

        self.write_register(dest, constructor.cast::<Value>().get());

        ().into()
    }

    #[inline]
    fn execute_new_accessor<W: Width>(&mut self, instr: &NewAccessorInstruction<W>) {
        let dest = instr.dest();
        let getter = self.read_register_to_handle(instr.getter()).as_object();
        let setter = self.read_register_to_handle(instr.setter()).as_object();

        // Allocates
        let accessor = AccessorValue::new(self.cx, Some(getter), Some(setter));

        self.write_register(dest, accessor.get_().into());
    }

    #[inline]
    fn execute_new_private_symbol<W: Width>(&mut self, instr: &NewPrivateSymbolInstruction<W>) {
        let dest = instr.dest();
        let name = self
            .get_constant(instr.name_index())
            .as_string()
            .to_handle();

        // Allocates
        let private_symbol = SymbolValue::new(self.cx, Some(name), /* is_private */ true);

        self.write_register(dest, private_symbol.get_().into());
    }

    #[inline]
    fn execute_get_property<W: Width>(
        &mut self,
        instr: &GetPropertyInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());
        let key = self.read_register_to_handle(instr.key());
        let dest = instr.dest();
        let is_strict = self.closure().function_ptr().is_strict();

        // May allocate
        let property_key = maybe!(to_property_key(self.cx, key));
        let coerced_object = maybe!(to_object(self.cx, object));

        // Result of ToObject is used as receiver in sloppy mode
        let receiver = if is_strict {
            object
        } else {
            coerced_object.into()
        };

        let result = maybe!(coerced_object.get(self.cx, property_key, receiver));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_set_property<W: Width>(
        &mut self,
        instr: &SetPropertyInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());
        let key = self.read_register_to_handle(instr.key());
        let value = self.read_register_to_handle(instr.value());
        let is_strict = self.closure().function_ptr().is_strict();

        // May allocate
        let property_key = maybe!(to_property_key(self.cx, key));
        let mut coerced_object = maybe!(to_object(self.cx, object));

        if is_strict {
            let success = maybe!(coerced_object.set(self.cx, property_key, value, object));
            if !success {
                return err_cannot_set_property(self.cx, property_key);
            }
        } else {
            maybe!(coerced_object.set(self.cx, property_key, value, coerced_object.into()));
        }

        ().into()
    }

    #[inline]
    fn execute_define_property<W: Width>(
        &mut self,
        instr: &DefinePropertyInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());
        let key = self.read_register_to_handle(instr.key());
        let value = self.read_register_to_handle(instr.value());
        let flags = DefinePropertyFlags::from_bits_retain(instr.flags().value().to_usize() as u8);

        // May allocate
        let property_key = maybe!(to_property_key(self.cx, key));
        let object = maybe!(to_object(self.cx, object));

        // Uncommon cases when some flags are set, e.g. for accessors or named evaluation
        if !flags.is_empty() {
            // We only set flags when the value evaluates to a closure
            debug_assert!(
                value.is_pointer() && value.as_pointer().descriptor().kind() == ObjectKind::Closure
            );
            let mut closure = value.cast::<Closure>();

            // Since we did not statically know the key we must perform "named evaluation" here, meaning
            // we set the function name to the key.
            if flags.contains(DefinePropertyFlags::NEEDS_NAME) {
                let prefix = if flags.contains(DefinePropertyFlags::GETTER) {
                    Some("get")
                } else if flags.contains(DefinePropertyFlags::SETTER) {
                    Some("set")
                } else {
                    None
                };

                let name = build_function_name(self.cx, property_key, prefix);
                closure.set_lazy_function_name(self.cx, name);
            }

            // Create special property descriptors for accessors
            if flags.contains(DefinePropertyFlags::GETTER) {
                let desc = PropertyDescriptor::get_only(Some(closure.into()), true, true);
                return define_property_or_throw(self.cx, object, property_key, desc);
            } else if flags.contains(DefinePropertyFlags::SETTER) {
                let desc = PropertyDescriptor::set_only(Some(closure.into()), true, true);
                return define_property_or_throw(self.cx, object, property_key, desc);
            }
        }

        create_data_property_or_throw(self.cx, object, property_key, value)
    }

    #[inline]
    fn execute_get_named_property<W: Width>(
        &mut self,
        instr: &GetNamedPropertyInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());

        let key = self.get_constant(instr.name_constant_index());
        let key = key.as_string().to_handle();

        let dest = instr.dest();
        let is_strict = self.closure().function_ptr().is_strict();

        // May allocate, replace handle
        let property_key = PropertyKey::string(self.cx, key);
        let property_key = key.replace_into(property_key);

        let coerced_object = maybe!(to_object(self.cx, object));

        // Result of ToObject is used as receiver in sloppy mode
        let receiver = if is_strict {
            object
        } else {
            coerced_object.into()
        };

        let result = maybe!(coerced_object.get(self.cx, property_key, receiver));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_set_named_property<W: Width>(
        &mut self,
        instr: &SetNamedPropertyInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());

        let key = self.get_constant(instr.name_constant_index());
        let key = key.as_string().to_handle();

        let value = self.read_register_to_handle(instr.value());

        let is_strict = self.closure().function_ptr().is_strict();

        // May allocate
        let mut coerced_object = maybe!(to_object(self.cx, object));

        let property_key = PropertyKey::string(self.cx, key);
        let property_key = key.replace_into(property_key);

        if is_strict {
            let success = maybe!(coerced_object.set(self.cx, property_key, value, object));
            if !success {
                return err_cannot_set_property(self.cx, property_key);
            }
        } else {
            maybe!(coerced_object.set(self.cx, property_key, value, coerced_object.into()));
        }

        ().into()
    }

    #[inline]
    fn execute_define_named_property<W: Width>(
        &mut self,
        instr: &DefineNamedPropertyInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());

        let key = self.get_constant(instr.name_constant_index());
        let key = key.as_string().to_handle();

        let value = self.read_register_to_handle(instr.value());

        // May allocate
        let object = maybe!(to_object(self.cx, object));

        let property_key = PropertyKey::string(self.cx, key);
        let property_key = key.replace_into(property_key);

        create_data_property_or_throw(self.cx, object, property_key, value)
    }

    #[inline]
    fn execute_get_super_property<W: Width>(
        &mut self,
        instr: &GetSuperPropertyInstruction<W>,
    ) -> EvalResult<()> {
        let home_object = self
            .read_register_to_handle(instr.home_object())
            .as_object();
        let receiver = self.read_register_to_handle(instr.receiver());
        let key = self.read_register_to_handle(instr.key());
        let dest = instr.dest();

        // May allocate
        let property_key = maybe!(to_property_key(self.cx, key));
        let home_prototype = match maybe!(home_object.get_prototype_of(self.cx)) {
            None => return type_error(self.cx, "prototype is null"),
            Some(prototype) => prototype,
        };

        let result = maybe!(home_prototype.get(self.cx, property_key, receiver));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_get_named_super_property<W: Width>(
        &mut self,
        instr: &GetNamedSuperPropertyInstruction<W>,
    ) -> EvalResult<()> {
        let home_object = self
            .read_register_to_handle(instr.home_object())
            .as_object();
        let receiver = self.read_register_to_handle(instr.receiver());

        let key = self.get_constant(instr.name_constant_index());
        let key = key.as_string().to_handle();

        let dest = instr.dest();

        // May allocate, replace handle
        let property_key = PropertyKey::string(self.cx, key);
        let property_key = key.replace_into(property_key);

        let home_prototype = match maybe!(home_object.get_prototype_of(self.cx)) {
            None => return type_error(self.cx, "prototype is null"),
            Some(prototype) => prototype,
        };

        let result = maybe!(home_prototype.get(self.cx, property_key, receiver));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_set_super_property<W: Width>(
        &mut self,
        instr: &SetSuperPropertyInstruction<W>,
    ) -> EvalResult<()> {
        let home_object = self
            .read_register_to_handle(instr.home_object())
            .as_object();
        let receiver = self.read_register_to_handle(instr.receiver());
        let key = self.read_register_to_handle(instr.key());
        let value = self.read_register_to_handle(instr.value());
        let is_strict = self.closure().function_ptr().is_strict();

        // May allocate
        let property_key = maybe!(to_property_key(self.cx, key));
        let mut home_prototype = match maybe!(home_object.get_prototype_of(self.cx)) {
            None => return type_error(self.cx, "prototype is null"),
            Some(prototype) => prototype,
        };

        if is_strict {
            let success = maybe!(home_prototype.set(self.cx, property_key, value, receiver));
            if !success {
                return err_cannot_set_property(self.cx, property_key);
            }
        } else {
            maybe!(home_prototype.set(self.cx, property_key, value, receiver));
        }

        ().into()
    }

    #[inline]
    fn execute_delete_property<W: Width>(
        &mut self,
        instr: &DeletePropertyInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());
        let key = self.read_register_to_handle(instr.key());
        let dest = instr.dest();
        let is_strict = self.closure().function_ptr().is_strict();

        // May allocate
        let key = maybe!(to_property_key(self.cx, key));
        let delete_status = maybe!(eval_delete_property(self.cx, object, key, is_strict));

        self.write_register(dest, Value::bool(delete_status));

        ().into()
    }

    #[inline]
    fn execute_delete_binding<W: Width>(
        &mut self,
        instr: &DeleteBindingInstruction<W>,
    ) -> EvalResult<()> {
        let mut scope = self.scope().to_handle();
        let name = self
            .get_constant(instr.name_constant_index())
            .to_handle(self.cx)
            .as_string();
        let dest = instr.dest();

        // May allocate
        let delete_status = maybe!(scope.lookup_delete(self.cx, name));

        self.write_register(dest, Value::bool(delete_status));

        ().into()
    }

    #[inline]
    fn execute_get_private_property<W: Width>(
        &mut self,
        instr: &GetPrivatePropertyInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());
        let key = self.read_register_to_handle(instr.key()).as_symbol();
        let dest = instr.dest();

        // May allocate
        let coerced_object = maybe!(to_object(self.cx, object));
        let result = maybe!(private_get(self.cx, coerced_object, key));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_set_private_property<W: Width>(
        &mut self,
        instr: &SetPrivatePropertyInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());
        let key = self.read_register_to_handle(instr.key()).as_symbol();
        let value = self.read_register_to_handle(instr.value());

        // May allocate
        let coerced_object = maybe!(to_object(self.cx, object));
        maybe!(private_set(self.cx, coerced_object, key, value));

        ().into()
    }

    #[inline]
    fn execute_define_private_property<W: Width>(
        &mut self,
        instr: &DefinePrivatePropertyInstruction<W>,
    ) -> EvalResult<()> {
        let mut object = self.read_register_to_handle(instr.object()).as_object();
        let key = self.read_register_to_handle(instr.key()).as_symbol();
        let value = self.read_register_to_handle(instr.value());
        let flags =
            DefinePrivatePropertyFlags::from_bits_retain(instr.flags().value().to_usize() as u8);

        // May allocate
        let property = if flags == DefinePrivatePropertyFlags::empty() {
            Property::private_field(value)
        } else if flags == DefinePrivatePropertyFlags::METHOD {
            Property::private_method(value.as_object())
        } else if flags.contains(DefinePrivatePropertyFlags::GETTER) {
            if flags.contains(DefinePrivatePropertyFlags::SETTER) {
                Property::private_accessor(value.as_accessor())
            } else {
                Property::private_getter(self.cx, value.as_object())
            }
        } else {
            Property::private_setter(self.cx, value.as_object())
        };

        object.property_property_add(self.cx, key, property)
    }

    #[inline]
    fn execute_set_array_property<W: Width>(&mut self, instr: &SetArrayPropertyInstruction<W>) {
        let array = self
            .read_register_to_handle(instr.array())
            .cast::<ArrayObject>();
        let index = self.read_register_to_handle(instr.index());
        let value = self.read_register_to_handle(instr.value());

        // May allocate
        let index = index.replace_into(must!(PropertyKey::from_value(self.cx, index)));
        let desc = Property::data(value, true, true, true);
        array.object().set_property(self.cx, index, desc);
    }

    #[inline]
    fn execute_set_prototype_of<W: Width>(&mut self, instr: &SetPrototypeOfInstruction<W>) {
        let mut object = self.read_register_to_handle(instr.object()).as_object();
        let prototype = self.read_register_to_handle(instr.prototype());

        // May allocate
        if prototype.is_object() {
            must!(object.set_prototype_of(self.cx, Some(prototype.as_object())));
        } else if prototype.is_null() {
            must!(object.set_prototype_of(self.cx, None));
        }
    }

    #[inline]
    fn execute_copy_data_properties<W: Width>(
        &mut self,
        instr: &CopyDataPropertiesInstruction<W>,
    ) -> EvalResult<()> {
        let dest = self.read_register_to_handle(instr.dest()).as_object();
        let source = self.read_register_to_handle(instr.source());
        let excluded_property_keys = self
            .get_args_rev_slice(instr.argv(), instr.argc())
            .iter()
            .map(|v| v.to_handle(self.cx).cast::<PropertyKey>())
            .collect::<HashSet<_>>();

        // May allocate
        maybe!(copy_data_properties(self.cx, dest, source, &excluded_property_keys));

        ().into()
    }

    #[inline]
    fn execute_push_lexical_scope<W: Width>(&mut self, instr: &PushLexicalScopeInstruction<W>) {
        let scope = self.scope().to_handle();
        let scope_names = self
            .get_constant(instr.scope_names_index())
            .to_handle(self.cx)
            .cast::<ScopeNames>();

        // Allocates
        let lexical_scope = Scope::new_lexical(self.cx, scope, scope_names).get_();

        // Write the new scope to the stack
        *StackFrame::for_fp(self.fp).scope_mut() = lexical_scope;
    }

    #[inline]
    fn execute_push_function_scope<W: Width>(&mut self, instr: &PushFunctionScopeInstruction<W>) {
        let scope = self.scope().to_handle();
        let scope_names = self
            .get_constant(instr.scope_names_index())
            .to_handle(self.cx)
            .cast::<ScopeNames>();

        // Allocates
        let function_scope = Scope::new_function(self.cx, scope, scope_names).get_();

        // Write the new scope to the stack
        *StackFrame::for_fp(self.fp).scope_mut() = function_scope;
    }

    #[inline]
    fn execute_push_with_scope<W: Width>(
        &mut self,
        instr: &PushWithScopeInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());

        let scope = self.scope().to_handle();
        let scope_names = self
            .get_constant(instr.scope_names_index())
            .to_handle(self.cx)
            .cast::<ScopeNames>();

        // Allocates
        let object = maybe!(to_object(self.cx, object));
        let lexical_scope = Scope::new_with(self.cx, scope, scope_names, object).get_();

        // Write the new scope to the stack
        *StackFrame::for_fp(self.fp).scope_mut() = lexical_scope;

        ().into()
    }

    #[inline]
    fn execute_pop_scope<W: Width>(&mut self, _: &PopScopeInstruction<W>) {
        let parent_scope = self.scope().parent_ptr();

        // Write the new scope to the stack
        *StackFrame::for_fp(self.fp).scope_mut() = parent_scope;
    }

    #[inline]
    fn execute_dup_scope<W: Width>(&mut self, _: &DupScopeInstruction<W>) {
        let scope = self.scope().to_handle();

        // Allocates
        let dup_scope = scope.duplicate(self.cx);

        // Write the new scope to the stack
        *StackFrame::for_fp(self.fp).scope_mut() = dup_scope;
    }

    #[inline]
    fn scope_at_depth(&self, parent_depth: usize) -> HeapPtr<Scope> {
        let mut scope = self.scope();
        for _ in 0..parent_depth {
            scope = scope.parent_ptr();
        }

        scope
    }

    #[inline]
    pub fn store_to_scope_at_depth(&self, scope_index: usize, parent_depth: usize, value: Value) {
        // Find the scope at the given parent depth
        let mut scope = self.scope_at_depth(parent_depth);

        // Store the value in the scope at the given index
        scope.set_slot(scope_index, value);
    }

    #[inline]
    fn execute_load_from_scope<W: Width>(&mut self, instr: &LoadFromScopeInstruction<W>) {
        let scope_index = instr.scope_index().value().to_usize();
        let parent_depth = instr.parent_depth().value().to_usize();
        let dest = instr.dest();

        // Find the scope at the given parent depth
        let scope = self.scope_at_depth(parent_depth);

        // Extract the value at the given index
        let value = scope.get_slot(scope_index);

        self.write_register(dest, value);
    }

    #[inline]
    fn execute_store_to_scope<W: Width>(&mut self, instr: &StoreToScopeInstruction<W>) {
        let scope_index = instr.scope_index().value().to_usize();
        let parent_depth = instr.parent_depth().value().to_usize();
        let value = self.read_register(instr.value());

        self.store_to_scope_at_depth(scope_index, parent_depth, value);
    }

    #[inline]
    fn execute_rest_parameter<W: Width>(&mut self, instr: &RestParameterInstruction<W>) {
        let dest = instr.dest();

        // Allocates
        let rest_array = must!(array_create(self.cx, 0, None));

        // Handles are shared between iterations
        let mut array_key = PropertyKey::uninit().to_handle(self.cx);
        let mut value_handle = Value::uninit().to_handle(self.cx);

        // The arguments between the number of formal parameters and the actual argc supplied will
        // all be added to the rest array.
        let num_parameters = self.closure().function_ptr().num_parameters() as usize;

        // No rest parameter needed if there was underapplication of arguments
        if num_parameters <= self.argc() {
            let stack_frame = StackFrame::for_fp(self.fp);
            for (i, argument) in stack_frame.args()[num_parameters..].iter().enumerate() {
                array_key.replace(PropertyKey::array_index(self.cx, i as u32));
                value_handle.replace(*argument);

                let array_property = Property::data(value_handle, true, true, true);
                rest_array
                    .object()
                    .set_property(self.cx, array_key, array_property);
            }
        }

        self.write_register(dest, rest_array.cast::<Value>().get());
    }

    #[inline]
    fn execute_get_super_constructor<W: Width>(
        &mut self,
        instr: &GetSuperConstructorInstruction<W>,
    ) -> EvalResult<()> {
        let derived_constructor = self
            .read_register_to_handle(instr.derived_constructor())
            .cast::<ObjectValue>();
        let dest = instr.dest();

        // May allocate
        let super_constructor = must!(derived_constructor.get_prototype_of(self.cx));

        // Return null if there is no prototype
        let super_constructor = super_constructor
            .map(|o| o.get_().into())
            .unwrap_or(Value::null());

        self.write_register(dest, super_constructor);

        ().into()
    }

    #[inline]
    fn execute_check_tdz<W: Width>(&mut self, instr: &CheckTdzInstruction<W>) -> EvalResult<()> {
        let value = self.read_register(instr.value());

        // Binding in TDZ represented as an empty value
        if !value.is_empty() {
            return ().into();
        }

        let name = self.get_constant(instr.name_constant_index()).as_string();

        reference_error(self.cx, &format!("can't access `{}` before initialization", name))
    }

    #[inline]
    fn execute_check_this_initialized<W: Width>(
        &mut self,
        instr: &CheckThisInitializedInstruction<W>,
    ) -> EvalResult<()> {
        let value = self.read_register(instr.value());

        // Uninitialized `this` represented as an empty value
        if !value.is_empty() {
            return ().into();
        }

        reference_error(self.cx, "this is not initialized")
    }

    #[inline]
    fn execute_check_super_already_called<W: Width>(
        &mut self,
        instr: &CheckSuperAlreadyCalledInstruction<W>,
    ) -> EvalResult<()> {
        let value = self.read_register(instr.value());

        // Uninitialized `this` represented as an empty value
        if value.is_empty() {
            return ().into();
        }

        reference_error(self.cx, "super constructor called multiple times")
    }

    #[inline]
    fn execute_error_const<W: Width>(
        &mut self,
        instr: &ErrorConstInstruction<W>,
    ) -> EvalResult<()> {
        let name = self
            .get_constant(instr.name_constant_index())
            .as_string()
            .as_flat();

        err_assign_constant(self.cx, name)
    }

    #[inline]
    fn execute_error_delete_super_property<W: Width>(
        &mut self,
        _: &ErrorDeleteSuperPropertyInstruction<W>,
    ) -> EvalResult<()> {
        reference_error(self.cx, "cannot delete super property")
    }

    #[inline]
    fn execute_new_for_in_iterator<W: Width>(
        &mut self,
        instr: &NewForInIteratorInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());
        let dest = instr.dest();

        // May allocate
        let object = maybe!(to_object(self.cx, object));
        let iterator = maybe!(ForInIterator::new_for_object(self.cx, object));

        self.write_register(dest, iterator.cast::<ObjectValue>().into());

        ().into()
    }

    #[inline]
    fn execute_for_in_next<W: Width>(&mut self, instr: &ForInNextInstruction<W>) -> EvalResult<()> {
        let mut iterator = self
            .read_register_to_handle(instr.iterator())
            .cast::<ForInIterator>();
        let dest = instr.dest();

        // May allocate
        let result = maybe!(iterator.next(self.cx));

        self.write_register(dest, result);

        ().into()
    }

    #[inline]
    fn execute_get_iterator<W: Width>(
        &mut self,
        instr: &GetIteratorInstruction<W>,
    ) -> EvalResult<()> {
        let iterable = self.read_register_to_handle(instr.iterable());
        let iterator_dest = instr.iterator();
        let next_method_dest = instr.next_method();

        // May allocate
        let iterator_result = maybe!(get_iterator(self.cx, iterable, IteratorHint::Sync, None));

        self.write_register(iterator_dest, iterator_result.iterator.get_().into());
        self.write_register(next_method_dest, iterator_result.next_method.get());

        ().into()
    }

    #[inline]
    fn execute_iterator_next<W: Width>(
        &mut self,
        instr: &IteratorNextInstruction<W>,
    ) -> EvalResult<()> {
        let next_method = self.read_register_to_handle(instr.next_method());
        let iterator = self.read_register_to_handle(instr.iterator());
        let value_dest = instr.value();
        let is_done_dest = instr.is_done();

        // Call the iterator's next method. May allocate.
        let iterator_result = maybe!(call(self.cx, next_method, iterator, &[]));

        // Iterator's function must return an object, otherwise error
        if !iterator_result.is_object() {
            return type_error(self.cx, "iterator's next method must return an object");
        }
        let iterator_result = iterator_result.as_object();

        // Check if the iterator is done
        let is_done = maybe!(iterator_complete(self.cx, iterator_result));

        if is_done {
            // If done then no need to write value register as it will be ignored
            self.write_register(is_done_dest, Value::bool(true));
        } else {
            // If not done then extract the value and write it to the value register
            let value = maybe!(iterator_value(self.cx, iterator_result));

            self.write_register(is_done_dest, Value::bool(false));
            self.write_register(value_dest, value.get());
        }

        ().into()
    }

    #[inline]
    fn execute_iterator_close<W: Width>(
        &mut self,
        instr: &IteratorCloseInstruction<W>,
    ) -> EvalResult<()> {
        let iterator = self.read_register_to_handle(instr.iterator());
        let return_method = maybe!(get_method(self.cx, iterator, self.cx.names.return_()));

        // Check if there is a return method and call it
        if let Some(return_method) = return_method {
            let return_result = maybe!(call_object(self.cx, return_method, iterator, &[]));

            // Return method must return an object otherwise error
            if !return_result.is_object() {
                return type_error(self.cx, "iterator's return method must return an object");
            }
        }

        ().into()
    }

    /// Visit a stack frame while unwinding the stack for an exception.
    #[inline]
    fn visit_frame_for_exception_unwinding(
        &mut self,
        stack_frame: StackFrame,
        instr_addr: *const u8,
        error_value: Value,
    ) -> bool {
        let func = stack_frame.closure().function_ptr();
        if func.exception_handlers_ptr().is_none() {
            return false;
        }

        // Find the offset of the instruction in the instruction stream
        let instr_offset = unsafe { instr_addr.offset_from(func.bytecode().as_ptr()) as usize };

        // Find the innermost matching exception handler
        let mut innermost_matching_handler = None;
        for handler in func.exception_handlers_ptr().unwrap().iter() {
            // The saved return address points to the start of the next instruction, so treat the
            // handler bounds as (exclusive, inclusive].
            if handler.start() < instr_offset && instr_offset <= handler.end() {
                let handler_width = handler.end() - handler.start();

                // Use the innermost handler (aka the one with the smallest width)
                if !matches!(
                    innermost_matching_handler,
                    Some ((_, min_width)) if min_width < handler_width,
                ) {
                    innermost_matching_handler = Some((handler, handler_width));
                }
            }
        }

        if let Some((handler, _)) = innermost_matching_handler {
            // Find the absolute address of the start of the handler block and start executing
            // instructions from this address.
            let handler_addr = unsafe { func.bytecode().as_ptr().add(handler.handler()) };
            self.pc = handler_addr;

            // Unwind the stack to the frame that contains the exception handler
            self.fp = stack_frame.fp();
            self.sp = stack_frame.sp();

            // Write the error into the appropriate register in the new stack frame
            if let Some(error_register) = handler.error_register() {
                self.write_register(error_register, error_value);
            }

            return true;
        }

        false
    }

    /// Visit all heap roots in the VM during GC root collection. Rewrites the stack in place,
    /// taking care to rewrite the current PC and return addresses.
    pub fn visit_roots(&mut self, visitor: &mut impl HeapVisitor) {
        if !self.is_executing() {
            return;
        }

        let mut stack_frame = StackFrame::for_fp(self.fp);

        // The current PC points into the current stack frame's BytecodeFunction. Rewrite both.
        Self::rewrite_bytecode_function_and_address(visitor, stack_frame, self.pc, |addr| {
            self.pc = addr
        });

        // Walk the stack, visiting all pointers in each frame
        loop {
            // Visit all args, registers, and fixed values in stack frame
            stack_frame.visit_simple_pointers(visitor);

            // Move to the parent's stack frame
            if let Some(caller_stack_frame) = stack_frame.previous_frame() {
                // This stack frame's return address points into the caller stack frame's
                // BytecodeFunction. Rewrite both.
                Self::rewrite_bytecode_function_and_address(
                    visitor,
                    caller_stack_frame,
                    stack_frame.return_address(),
                    |addr| stack_frame.set_return_address(addr),
                );
                stack_frame = caller_stack_frame;
            } else {
                return;
            }
        }
    }

    /// Visit a BytecodeFunction during GC stack walking, potentially moving the BytecodeFunction in
    /// the heap. Also rewrite a pointer into the function's instructions with this move, since this
    /// is an internal pointer into a moved heap object.
    ///
    /// The pointer to rewrite may be either the current PC or the return adress
    fn rewrite_bytecode_function_and_address(
        visitor: &mut impl HeapVisitor,
        stack_frame: StackFrame,
        instruction_address: *const u8,
        set_instruction_address: impl FnOnce(*const u8),
    ) {
        let mut bytecode_function = stack_frame.closure().function_ptr();

        // Find the offset of the instruction in the BytecodeFunction
        let function_start = bytecode_function.as_ptr() as *const u8;
        let offset = unsafe { instruction_address.offset_from(function_start) };

        // Visit the caller's BytecodeFunction, moving it in the heap and rewriting this pointer
        visitor.visit_pointer(&mut bytecode_function);

        // Rewrite the instruction address using the new location of the BytecodeFunction
        let new_function_start = bytecode_function.as_ptr() as *const u8;
        let new_instruction_address = unsafe { new_function_start.offset(offset) };
        set_instruction_address(new_instruction_address);
    }
}

enum ArgsSlice<'a> {
    Forward(&'a [Value]),
    Reverse(&'a [Value]),
}

enum CallableObject {
    Closure(HeapPtr<Closure>),
    Proxy(HeapPtr<ProxyObject>),
    Error(Handle<Value>),
}
