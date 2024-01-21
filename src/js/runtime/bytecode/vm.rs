use std::ops::Deref;

use crate::{
    js::runtime::{
        abstract_operations::set,
        error::type_error_,
        eval::expression::{
            eval_add, eval_divide, eval_exponentiation, eval_greater_than,
            eval_greater_than_or_equal, eval_less_than, eval_less_than_or_equal, eval_multiply,
            eval_remainder, eval_subtract, eval_typeof,
        },
        gc::{HandleScope, HeapVisitor},
        get,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RustRuntimeFunctionId},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        type_utilities::{is_loosely_equal, is_strictly_equal, to_boolean, to_object},
        Context, EvalResult, Handle, HeapPtr, PropertyKey, Value,
    },
    maybe,
};

use super::{
    function::{BytecodeFunction, Closure},
    instruction::{
        extra_wide_prefix_index_to_opcode_index, wide_prefix_index_to_opcode_index, AddInstruction,
        CallInstruction, CallWithReceiverInstruction, ConstructInstruction, DivInstruction,
        ExpInstruction, GetNamedPropertyInstruction, GreaterThanInstruction,
        GreaterThanOrEqualInstruction, Instruction, JumpConstantInstruction,
        JumpFalseConstantInstruction, JumpFalseInstruction, JumpInstruction,
        JumpToBooleanFalseConstantInstruction, JumpToBooleanFalseInstruction,
        JumpToBooleanTrueConstantInstruction, JumpToBooleanTrueInstruction,
        JumpTrueConstantInstruction, JumpTrueInstruction, LessThanInstruction,
        LessThanOrEqualInstruction, LoadConstantInstruction, LoadFalseInstruction,
        LoadGlobalInstruction, LoadImmediateInstruction, LoadNullInstruction, LoadTrueInstruction,
        LoadUndefinedInstruction, LooseEqualInstruction, LooseNotEqualInstruction, MovInstruction,
        MulInstruction, NewClosureInstruction, OpCode, RemInstruction, RetInstruction,
        SetNamedPropertyInstruction, StoreGlobalInstruction, StrictEqualInstruction,
        StrictNotEqualInstruction, SubInstruction, ThrowInstruction, TypeOfInstruction,
    },
    instruction_traits::{
        GenericCallInstruction, GenericJumpBooleanConstantInstruction,
        GenericJumpBooleanInstruction, GenericJumpToBooleanConstantInstruction,
        GenericJumpToBooleanInstruction,
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

    /// Handle pool
    h1: Handle<Value>,
    h2: Handle<Value>,
    h3: Handle<Value>,

    /// Whether the VM is currently executing bytecode. VM is only walked for GC roots when this
    /// is true.
    is_executing: bool,

    stack: Vec<StackSlotValue>,
}

impl VM {
    pub fn new(cx: Context) -> Self {
        // Allocate uninitialized memory for stack
        let mut stack = Vec::<StackSlotValue>::with_capacity(NUM_STACK_SLOTS);
        unsafe { stack.set_len(NUM_STACK_SLOTS) };

        VM {
            cx,
            pc: std::ptr::null(),
            sp: std::ptr::null_mut(),
            fp: std::ptr::null_mut(),

            // Setup handle pool
            h1: Handle::empty(cx),
            h2: Handle::empty(cx),
            h3: Handle::empty(cx),

            is_executing: false,
            stack,
        }
    }

    /// Execute a function with the provided arguments. Starts a new execution of the VM,
    /// initializing stack from scratch.
    pub fn execute(
        &mut self,
        closure: Handle<Closure>,
        arguments: &[Handle<Value>],
    ) -> Result<Handle<Value>, Handle<Value>> {
        // Initialize stack
        self.sp = self.stack.as_ptr_range().end as *mut StackSlotValue;
        self.fp = std::ptr::null_mut();

        // Evaluate in the global scope
        let receiver = self.cx.get_global_object().into();

        // Evaluate the provided function
        self.is_executing = true;
        let eval_result = self.call_from_rust(closure.cast(), receiver, arguments);
        self.is_executing = false;

        eval_result.to_rust_result()
    }

    /// Dispatch instructions, one after another, until there are no more instructions to execute.
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

            // Execute a ret instruction
            macro_rules! execute_ret {
                ($get_instr:ident) => {{
                    let instr = $get_instr!(RetInstruction);

                    // Store the return value at the return value address
                    let return_value = self.read_register(instr.return_value());
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

            macro_rules! throw {
                ($error_value:expr) => {{
                    let error_value = $error_value;

                    // Walk the stack, looking for an exception handler that covers the current
                    // address.
                    let mut stack_frame = StackFrame::for_fp(self.fp);
                    if self.visit_frame_for_exception_unwinding(
                        stack_frame,
                        self.pc,
                        error_value,
                        true,
                    ) {
                        continue 'dispatch;
                    }

                    while let Some(caller_stack_frame) = stack_frame.previous_frame() {
                        // If the caller is the Rust runtime then return the thrown error
                        if stack_frame.is_rust_caller() {
                            return Err(error_value);
                        }

                        if self.visit_frame_for_exception_unwinding(
                            caller_stack_frame,
                            stack_frame.return_address(),
                            error_value,
                            false,
                        ) {
                            continue 'dispatch;
                        }

                        stack_frame = caller_stack_frame;
                    }

                    // Exception has unwound the entire stack, finish VM execution returning the
                    // thrown error.
                    return Err(error_value);
                }};
            }

            macro_rules! execute_throw {
                ($get_instr:ident) => {{
                    let instr = $get_instr!(ThrowInstruction);
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
                        OpCode::LoadNull => dispatch!(LoadNullInstruction, execute_load_null),
                        OpCode::LoadTrue => dispatch!(LoadTrueInstruction, execute_load_true),
                        OpCode::LoadFalse => dispatch!(LoadFalseInstruction, execute_load_false),
                        OpCode::LoadConstant => {
                            dispatch!(LoadConstantInstruction, execute_load_constant)
                        }
                        OpCode::LoadGlobal => {
                            dispatch_or_throw!(LoadGlobalInstruction, execute_load_global)
                        }
                        OpCode::StoreGlobal => {
                            dispatch_or_throw!(StoreGlobalInstruction, execute_store_global)
                        }
                        OpCode::Call => dispatch_or_throw!(CallInstruction, execute_generic_call),
                        OpCode::CallWithReceiver => {
                            dispatch_or_throw!(CallWithReceiverInstruction, execute_generic_call)
                        }
                        OpCode::Construct => {
                            dispatch_or_throw!(ConstructInstruction, execute_construct)
                        }
                        OpCode::Ret => execute_ret!(get_instr),
                        OpCode::Add => dispatch_or_throw!(AddInstruction, execute_add),
                        OpCode::Sub => dispatch_or_throw!(SubInstruction, execute_sub),
                        OpCode::Mul => dispatch_or_throw!(MulInstruction, execute_mul),
                        OpCode::Div => dispatch_or_throw!(DivInstruction, execute_div),
                        OpCode::Rem => dispatch_or_throw!(RemInstruction, execute_rem),
                        OpCode::Exp => dispatch_or_throw!(ExpInstruction, execute_exp),
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
                        OpCode::TypeOf => dispatch!(TypeOfInstruction, execute_typeof),
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
                        OpCode::NewClosure => dispatch!(NewClosureInstruction, execute_new_closure),
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
                        OpCode::Throw => execute_throw!(get_instr),
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
    fn get_function(&self) -> HeapPtr<BytecodeFunction> {
        StackFrame::for_fp(self.fp).bytecode_function()
    }

    #[inline]
    fn get_argc(&self) -> usize {
        StackFrame::for_fp(self.fp).argc()
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
    fn get_constant<W: Width>(
        &self,
        func: HeapPtr<BytecodeFunction>,
        constant_index: ConstantIndex<W>,
    ) -> Value {
        // If a constant index is referenced the constant table must exist
        let constant_table = unsafe { func.constant_table_ptr().unwrap_unchecked() };
        constant_table.get_constant(constant_index.value().to_usize())
    }

    #[inline]
    fn get_constant_offset<W: Width>(
        &self,
        func: HeapPtr<BytecodeFunction>,
        constant_index: ConstantIndex<W>,
    ) -> isize {
        // Constant offsets are encoded as a raw isize, not a value
        self.get_constant(func, constant_index).as_raw_bits() as isize
    }

    /// Set the PC to the jump target, specified as a relative offset immediate.
    #[inline]
    fn jump_immediate<W: Width>(&mut self, offset: SInt<W>) {
        self.pc = unsafe { self.pc.offset(offset.value().to_isize()) };
    }

    // Set the PC to the jump target, specified as a relative offset in the constant table.
    #[inline]
    fn jump_constant<W: Width>(&mut self, constant_index: ConstantIndex<W>) {
        let offset = self.get_constant_offset(self.get_function(), constant_index);
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
        // Check that the value is callable. Only allocates when throwing.
        let function_handle = maybe!(self.check_value_is_closure(function.get())).to_handle();

        // Get the receiver to use. May allocate.
        let receiver =
            maybe!(self.generate_receiver(Some(receiver.get()), function_handle.is_strict()));
        let function_ptr = function_handle.get_();

        // Check if this is a call to a function in the Rust runtime
        if let Some(function_id) = function_ptr.rust_runtime_function_id() {
            // Reuse function handle for receiver
            let mut receiver_handle = function_handle.cast::<Value>();
            receiver_handle.replace(receiver);

            // Call rust runtime function directly in its own handle scope
            HandleScope::new(self.cx, |_| {
                self.call_rust_runtime(function_ptr, function_id, receiver_handle, arguments, None)
            })
        } else {
            // Otherwise this is a call to a JS function in the VM
            let args_rev_iter = arguments.iter().rev().map(Handle::deref);

            // Push the address of the return value
            let mut return_value = Value::undefined();
            let return_value_address = (&mut return_value) as *mut Value;

            // Push a stack frame for the function call, with return address set to return to Rust
            self.push_stack_frame(
                function_ptr,
                receiver,
                args_rev_iter,
                arguments.len(),
                /* return_to_rust_runtime */ true,
                return_value_address,
            );

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
        // Check that the value is callable. Only allocates when throwing.
        let function_handle = maybe!(self.check_value_is_constructor(function.get())).to_handle();

        // Create the receiver to use. Allocates.
        let receiver = maybe!(self.generate_constructor_receiver(new_target));

        // Reuse function handle for receiver
        let function_ptr = function_handle.get_();
        let mut receiver_handle = function_handle.cast::<ObjectValue>();
        receiver_handle.replace(receiver.into());

        // Check if this is a call to a function in the Rust runtime
        let return_value = if let Some(function_id) = function_ptr.rust_runtime_function_id() {
            // Call rust runtime function directly in its own handle scope
            maybe!(HandleScope::new(self.cx, |_| {
                self.call_rust_runtime(
                    function_ptr,
                    function_id,
                    receiver_handle.cast(),
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
                function_ptr,
                receiver.into(),
                args_rev_iter,
                arguments.len(),
                /* return_to_rust_runtime */ true,
                return_value_address,
            );

            // Start executing the dispatch loop from the start of the function, returning out of
            // dispatch loop when the marked return address is encountered. May allocate.
            if let Err(error_value) = self.dispatch_loop() {
                return EvalResult::Throw(error_value.to_handle(self.cx));
            }

            return_value.to_handle(self.cx)
        };

        // Use the function's return value, otherwise fall back to the reciever
        if return_value.is_object() {
            return_value.as_object().into()
        } else {
            receiver_handle.into()
        }
    }

    #[inline]
    fn execute_generic_call<W: Width>(
        &mut self,
        instr: &impl GenericCallInstruction<W>,
    ) -> EvalResult<()> {
        let function_value = self.read_register(instr.function());
        let receiver = instr.receiver().map(|reg| self.read_register(reg));

        // Find slice over the arguments, starting with last argument
        let args_rev_slice = self.get_args_rev_slice(instr.argv(), instr.argc());

        // Find address of the return value register
        let return_value_address = self.register_address(instr.dest());

        // Check that the value is callable. Only allocates when throwing.
        let function_ptr = maybe!(self.check_value_is_closure(function_value));

        // Check if this is a call to a function in the Rust runtime
        if let Some(function_id) = function_ptr.rust_runtime_function_id() {
            let return_value = maybe!(HandleScope::new(self.cx, |_| {
                // Get the receiver to use. May allocate.
                let function_handle = function_ptr.to_handle();
                let receiver = maybe!(self.generate_receiver(receiver, function_ptr.is_strict()));
                let function_ptr = function_handle.get_();

                // Reuse function handle for receiver
                let mut receiver_handle = function_handle.cast::<Value>();
                receiver_handle.replace(receiver);

                // All arguments must be placed behind handles before calling into Rust
                let mut arguments = vec![];
                for arg in args_rev_slice.iter().rev() {
                    arguments.push(arg.to_handle(self.cx));
                }

                self.call_rust_runtime(function_ptr, function_id, receiver_handle, &arguments, None)
            }));

            // Set the return value from the Rust runtime call
            unsafe { *return_value_address = return_value.get() };
        } else {
            // Otherwise this is a call to a JS function in the VM.

            // Get the receiver to use. May allocate.
            let function_handle = function_ptr.to_handle();
            let receiver = maybe!(self.generate_receiver(receiver, function_ptr.is_strict()));
            let function_ptr = function_handle.get_();

            // Set up the stack frame for the function call
            self.push_stack_frame(
                function_ptr,
                receiver,
                args_rev_slice.iter(),
                args_rev_slice.len(),
                /* return_to_rust_runtime */ false,
                return_value_address,
            );

            // Continue in dispatch loop, executing the first instruction of the function
        }

        ().into()
    }

    #[inline]
    fn execute_construct<W: Width>(&mut self, instr: &ConstructInstruction<W>) -> EvalResult<()> {
        // Find slice over the arguments, starting with last argument
        let args_rev_slice = self.get_args_rev_slice(instr.argv(), instr.argc());

        // Find address of the return value register
        let return_value_address = self.register_address(instr.dest());

        // Check that the value is callable. Only allocates when throwing.
        let function_value = self.read_register(instr.function());
        let function_ptr = maybe!(self.check_value_is_constructor(function_value)).to_handle();

        // Check if this is a call to a function in the Rust runtime
        let return_value = if let Some(function_id) = function_ptr.rust_runtime_function_id() {
            let return_value: Handle<ObjectValue> = maybe!(HandleScope::new(self.cx, |_| {
                let function_handle = function_ptr.to_handle();

                // TODO: Check if this cast is safe
                let new_target = self
                    .read_register(instr.new_target())
                    .to_handle(self.cx)
                    .cast();

                // Create the receiver to use. Allocates.
                let receiver = maybe!(self.generate_constructor_receiver(new_target));

                // Reuse function handle for receiver
                let function_ptr = function_handle.get_();
                let mut receiver_handle = function_handle.cast::<ObjectValue>();
                receiver_handle.replace(receiver);

                // All arguments must be placed behind handles before calling into Rust
                let mut arguments = vec![];
                for arg in args_rev_slice.iter().rev() {
                    arguments.push(arg.to_handle(self.cx));
                }

                let return_value = maybe!(self.call_rust_runtime(
                    function_ptr,
                    function_id,
                    receiver_handle.cast(),
                    &arguments,
                    Some(new_target)
                ));

                // Use the function's return value, otherwise fall back to the reciever
                if return_value.is_object() {
                    return_value.as_object().into()
                } else {
                    receiver_handle.into()
                }
            }));

            return_value.get_()
        } else {
            // Otherwise this is a call to a JS function in the VM.
            let function_handle = function_ptr.to_handle();

            // TODO: Check if this cast is safe
            let new_target = self
                .read_register(instr.new_target())
                .to_handle(self.cx)
                .cast();

            // Create the receiver to use. Allocates.
            let receiver = maybe!(self.generate_constructor_receiver(new_target));

            let function_ptr = function_handle.get_();
            let mut receiver_handle = function_handle.cast::<ObjectValue>();
            receiver_handle.replace(receiver);

            // Push the address of the return value
            let mut return_value = Value::undefined();
            let inner_call_return_value_address = (&mut return_value) as *mut Value;

            // Set up the stack frame for the function call
            self.push_stack_frame(
                function_ptr,
                receiver.into(),
                args_rev_slice.iter(),
                args_rev_slice.len(),
                /* return_to_rust_runtime */ true,
                inner_call_return_value_address,
            );

            // Start executing the dispatch loop from the start of the function, returning out of
            // dispatch loop when the marked return address is encountered.
            if let Err(error_value) = self.dispatch_loop() {
                return EvalResult::Throw(error_value.to_handle(self.cx));
            }

            // Use the function's return value, otherwise fall back to the reciever
            if return_value.is_object() {
                return_value.as_object()
            } else {
                receiver_handle.get_()
            }
        };

        // Set the return value from the Rust runtime call
        unsafe { *return_value_address = return_value.into() };

        ().into()
    }

    /// Check that a value is a closure (aka the only callable value), returning the inner
    /// BytecodeFunction.
    ///
    /// Only allocates when throwing.
    #[inline]
    fn check_value_is_closure(&self, value: Value) -> EvalResult<HeapPtr<BytecodeFunction>> {
        if !value.is_pointer() || value.as_pointer().descriptor().kind() != ObjectKind::Closure {
            return type_error_(self.cx, "value is not a function");
        }

        value.as_pointer().cast::<Closure>().function_ptr().into()
    }

    /// Check that a value is a constructor, returning the inner BytecodeFunction.
    ///
    /// Only allocates when throwing.
    #[inline]
    fn check_value_is_constructor(&self, value: Value) -> EvalResult<HeapPtr<BytecodeFunction>> {
        if !value.is_pointer() || value.as_pointer().descriptor().kind() != ObjectKind::Closure {
            return type_error_(self.cx, "value is not a constructor");
        }

        let func = value.as_pointer().cast::<Closure>().function_ptr();
        if !func.is_constructor() {
            return type_error_(self.cx, "value is not a constructor");
        }

        func.into()
    }

    /// Create a new stack frame constructed for the following arguments.
    ///
    /// Also saves the current PC on the stack frame as the return address, setting the PC to the
    /// first instruction in the function.
    #[inline]
    fn push_stack_frame<'a, I: Iterator<Item = &'a Value>>(
        &mut self,
        func: HeapPtr<BytecodeFunction>,
        receiver: Value,
        args_rev_iter: I,
        argc: usize,
        return_to_rust_runtime: bool,
        return_value_address: *mut Value,
    ) {
        // Push arguments
        let argc_to_push = self.push_call_arguments(func, args_rev_iter, argc);

        // Push the receiver if one is supplied, or the default receiver otherwise
        self.push(receiver.as_raw_bits() as StackSlotValue);

        // Push argc
        self.push(argc_to_push);

        // Push the function
        self.push(func.as_ptr() as StackSlotValue);

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
        self.allocate_local_registers(func.num_registers());

        // Start executing from the first instruction of the function
        self.pc = func.bytecode().as_ptr();
    }

    /// Pop the current stack frame, restoring the previous frame pointer and PC.
    #[inline]
    fn pop_stack_frame(&mut self) {
        unsafe {
            self.pc = self.get_return_address();
            self.sp = self.fp.add(FIRST_ARGUMENT_SLOT_INDEX + self.get_argc());
            self.fp = *self.fp as *mut StackSlotValue;
        }
    }

    /// Find slice over the arguments given argv and argc, starting with last argument/
    #[inline]
    fn get_args_rev_slice<'a, 'b, W: Width>(
        &'a self,
        argv: Register<W>,
        argc: UInt<W>,
    ) -> &'b [Value] {
        let argv = self.register_address(argv);
        let argc = argc.value().to_usize();

        unsafe {
            std::slice::from_raw_parts(argv.sub(argc.saturating_sub(1)) as *const Value, argc)
        }
    }

    /// Push call arguments onto the stack, returning the argc which the caller should push onto
    /// the stack. Takes an iterator over the arguments in reverse order.
    ///
    /// Does not push the receiver.
    #[inline]
    fn push_call_arguments<'a, I: Iterator<Item = &'a Value>>(
        &mut self,
        func: HeapPtr<BytecodeFunction>,
        args_rev_iter: I,
        argc: usize,
    ) -> usize {
        let mut sp = self.sp;

        // Handle under application of arguments, pushing undefined for missing arguments
        let num_parameters = func.num_parameters() as usize;
        let num_arguments = if argc < num_parameters {
            for _ in argc..num_parameters {
                unsafe {
                    sp = sp.sub(1);
                    *sp = Value::undefined().as_raw_bits() as StackSlotValue;
                }
            }

            num_parameters
        } else {
            argc
        };

        // First push arguments onto the stack in reverse order
        for arg in args_rev_iter {
            unsafe {
                sp = sp.sub(1);
                *sp = arg.as_raw_bits() as StackSlotValue;
            }
        }

        self.sp = sp;

        num_arguments
    }

    /// Generate the receiver to be used given an optional explicit receiver value and whether the
    /// function is in strict mode.
    #[inline]
    fn generate_receiver(&mut self, receiver: Option<Value>, is_strict: bool) -> EvalResult<Value> {
        if let Some(receiver) = receiver {
            self.coerce_receiver(receiver, is_strict)
        } else {
            self.default_receiver(is_strict).into()
        }
    }

    /// Return the default receiver used for a function call when no receiver is provided.
    #[inline]
    fn default_receiver(&self, is_strict: bool) -> Value {
        if is_strict {
            Value::undefined()
        } else {
            self.cx.get_global_object_ptr().into()
        }
    }

    /// Generate the receiver to be used for a constructor call.
    #[inline]
    fn generate_constructor_receiver(
        &mut self,
        new_target: Handle<ObjectValue>,
    ) -> EvalResult<HeapPtr<ObjectValue>> {
        object_create_from_constructor::<ObjectValue>(
            self.cx,
            new_target,
            ObjectKind::OrdinaryObject,
            Intrinsic::ObjectPrototype,
        )
    }

    /// Return the coerced receiver that should be passed to a function call. No coercion is
    /// necessary in strict mode, otherwise receiver must be coerced to an object, using the
    /// global object if the receiver is nullish.
    #[inline]
    fn coerce_receiver(&mut self, receiver: Value, is_strict: bool) -> EvalResult<Value> {
        let value = if is_strict {
            receiver
        } else if receiver.is_nullish() {
            self.cx.get_global_object_ptr().into()
        } else {
            self.h1.replace(receiver);
            let receiver = maybe!(to_object(self.cx, self.h1));
            receiver.get_().into()
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
        function: HeapPtr<BytecodeFunction>,
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
            /* return_to_rust_runtiem */ true,
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
        let constant = self.get_constant(self.get_function(), instr.constant_index());
        self.write_register(instr.dest(), constant)
    }

    #[inline]
    fn execute_load_global<W: Width>(
        &mut self,
        instr: &LoadGlobalInstruction<W>,
    ) -> EvalResult<()> {
        let name = self.get_constant(self.get_function(), instr.constant_index());
        self.h1.replace(name);
        let name = self.h1.cast();

        let dest = instr.dest();

        // TODO: Use global scope with new scope system
        HandleScope::new(self.cx, |cx| {
            // May allocate
            let key = PropertyKey::string(cx, name).to_handle(cx);
            let value = maybe!(get(cx, cx.get_global_object(), key));

            self.write_register(dest, value.get());

            ().into()
        })
    }

    #[inline]
    fn execute_store_global<W: Width>(
        &mut self,
        instr: &StoreGlobalInstruction<W>,
    ) -> EvalResult<()> {
        let value = self.read_register(instr.value());
        self.h1.replace(value);
        let value = self.h1;

        let name = self.get_constant(self.get_function(), instr.constant_index());
        self.h2.replace(name);
        let name = self.h2.cast();

        // TODO: Use global scope with new scope system
        HandleScope::new(self.cx, |cx| {
            // May allocate
            let key = PropertyKey::string(cx, name).to_handle(cx);
            maybe!(set(cx, cx.get_global_object(), key, value, false));

            ().into()
        })
    }

    #[inline]
    fn execute_add<W: Width>(&mut self, instr: &AddInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_add(self.cx, self.h1, self.h2));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_sub<W: Width>(&mut self, instr: &SubInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_subtract(self.cx, self.h1, self.h2));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_mul<W: Width>(&mut self, instr: &MulInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_multiply(self.cx, self.h1, self.h2));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_div<W: Width>(&mut self, instr: &DivInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_divide(self.cx, self.h1, self.h2));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_rem<W: Width>(&mut self, instr: &RemInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_remainder(self.cx, self.h1, self.h2));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_exp<W: Width>(&mut self, instr: &ExpInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_exponentiation(self.cx, self.h1, self.h2));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_loose_equal<W: Width>(
        &mut self,
        instr: &LooseEqualInstruction<W>,
    ) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let dest = instr.dest();

        // May allocate
        let result = maybe!(is_loosely_equal(self.cx, self.h1, self.h2));

        self.write_register(dest, Value::bool(result));

        ().into()
    }

    #[inline]
    fn execute_loose_not_equal<W: Width>(
        &mut self,
        instr: &LooseNotEqualInstruction<W>,
    ) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let dest = instr.dest();

        // May allocate
        let result = maybe!(is_loosely_equal(self.cx, self.h1, self.h2));

        self.write_register(dest, Value::bool(!result));

        ().into()
    }

    #[inline]
    fn execute_strict_equal<W: Width>(&mut self, instr: &StrictEqualInstruction<W>) {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let result = is_strictly_equal(self.h1, self.h2);
        self.write_register(instr.dest(), Value::bool(result));
    }

    #[inline]
    fn execute_strict_not_equal<W: Width>(&mut self, instr: &StrictNotEqualInstruction<W>) {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let result = is_strictly_equal(self.h1, self.h2);
        self.write_register(instr.dest(), Value::bool(!result));
    }

    #[inline]
    fn execute_less_than<W: Width>(&mut self, instr: &LessThanInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_less_than(self.cx, self.h1, self.h2));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_less_than_or_equal<W: Width>(
        &mut self,
        instr: &LessThanOrEqualInstruction<W>,
    ) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_less_than_or_equal(self.cx, self.h1, self.h2));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_greater_than<W: Width>(
        &mut self,
        instr: &GreaterThanInstruction<W>,
    ) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_greater_than(self.cx, self.h1, self.h2));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_greater_than_or_equal<W: Width>(
        &mut self,
        instr: &GreaterThanOrEqualInstruction<W>,
    ) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let dest = instr.dest();

        // May allocate
        let result = maybe!(eval_greater_than_or_equal(self.cx, self.h1, self.h2));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_typeof<W: Width>(&mut self, instr: &TypeOfInstruction<W>) {
        let value = self.read_register(instr.value());
        self.h1.replace(value);

        let dest = instr.dest();

        // May allocate
        let result = eval_typeof(self.cx, self.h1);

        self.write_register(dest, result.cast::<Value>().get());
    }

    #[inline]
    fn execute_new_closure<W: Width>(&mut self, instr: &NewClosureInstruction<W>) {
        let func = self.get_constant(self.get_function(), instr.function_index());
        self.h1.replace(func);

        let dest = instr.dest();

        // Allocates
        let closure = Closure::new(self.cx, self.h1.cast::<BytecodeFunction>());

        self.write_register(dest, Value::object(closure.get_().cast()));
    }

    #[inline]
    fn execute_get_named_property<W: Width>(
        &mut self,
        instr: &GetNamedPropertyInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register(instr.object());
        self.h1.replace(object);

        let key = self.get_constant(self.get_function(), instr.name_constant_index());
        self.h2.replace(key);

        let dest = instr.dest();

        // May allocate
        let key = PropertyKey::string(self.cx, self.h2.as_string());
        self.h2.replace(key.as_string().into());

        let object = maybe!(to_object(self.cx, self.h1));
        let result = maybe!(get(self.cx, object, self.h2.cast()));

        self.write_register(dest, result.get());

        ().into()
    }

    #[inline]
    fn execute_set_named_property<W: Width>(
        &mut self,
        instr: &SetNamedPropertyInstruction<W>,
    ) -> EvalResult<()> {
        // Object may still be h1, so cannot reuse handle
        let object = self.read_register(instr.object());
        self.h1.replace(object);
        let object = maybe!(to_object(self.cx, self.h1));

        let key = self.get_constant(self.get_function(), instr.name_constant_index());
        self.h2.replace(key);

        let value = self.read_register(instr.value());
        self.h3.replace(value);

        // May allocate
        let key = PropertyKey::string(self.cx, self.h2.as_string());
        self.h2.replace(key.as_string().into());

        let is_strict = self.get_function().is_strict();

        set(self.cx, object, self.h2.cast(), self.h3, is_strict)
    }

    /// Visit a stack frame while unwinding the stack for an exception.
    #[inline]
    fn visit_frame_for_exception_unwinding(
        &mut self,
        stack_frame: StackFrame,
        instr_addr: *const u8,
        error_value: Value,
        is_current_frame: bool,
    ) -> bool {
        let func = stack_frame.bytecode_function();
        if func.exception_handlers_ptr().is_none() {
            return false;
        }

        // Find the offset of the instruction in the instruction stream
        let instr_offset = unsafe { instr_addr.offset_from(func.bytecode().as_ptr()) as usize };

        for handler in func.exception_handlers_ptr().unwrap().iter() {
            // In the current frame the PC is set to the start of the current instruction so treat
            // the handler bounds as [inclusive, exclusive).
            //
            // In caller frames the saved return address points to the start of the next
            // instruction, so treat the handler bounds as (exclusive, inclusive].
            let instr_in_range = if is_current_frame {
                handler.start() <= instr_offset && instr_offset < handler.end()
            } else {
                handler.start() < instr_offset && instr_offset <= handler.end()
            };

            if instr_in_range {
                // Find the absolute address of the start of the handler block and start executing
                // instructions from this address.
                let handler_addr = unsafe { func.bytecode().as_ptr().add(handler.handler()) };
                self.pc = handler_addr;

                // Unwind the stack to the frame that contains the exception handler
                if !is_current_frame {
                    self.fp = stack_frame.fp();
                    self.sp = stack_frame.sp();
                }

                // Write the error into the appropriate register in the new stack frame
                if let Some(error_register) = handler.error_register() {
                    self.write_register(error_register, error_value);
                }

                return true;
            }
        }

        false
    }

    /// Visit all heap roots in the VM during GC root collection. Rewrites the stack in place,
    /// taking care to rewrite the current PC and return addresses.
    pub fn visit_roots(&mut self, visitor: &mut impl HeapVisitor) {
        if !self.is_executing {
            return;
        }

        let mut stack_frame = StackFrame::for_fp(self.fp);

        // The current PC points into the current stack frame's BytecodeFunction. Rewrite both.
        Self::rewrite_bytecode_function_and_address(visitor, stack_frame, self.pc, |addr| {
            self.pc = addr
        });

        // Walk the stack, visiting all pointers in each frame
        loop {
            // Visit all args and registers in stack frame
            for arg in stack_frame.args_with_receiver_mut() {
                visitor.visit_value(arg);
            }

            for register in stack_frame.registers_mut() {
                visitor.visit_value(register);
            }

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
        mut stack_frame: StackFrame,
        instruction_address: *const u8,
        set_instruction_address: impl FnOnce(*const u8),
    ) {
        // Find the offset of the instruction in the BytecodeFunction
        let function_start = stack_frame.bytecode_function().as_ptr() as *const u8;
        let offset = unsafe { instruction_address.offset_from(function_start) };

        // Visit the caller's BytecodeFunction, moving it in the heap
        visitor.visit_pointer(stack_frame.bytecode_function_mut());

        // Rewrite the instruction address using the new location of the BytecodeFunction
        let new_function_start = stack_frame.bytecode_function().as_ptr() as *const u8;
        let new_instruction_address = unsafe { new_function_start.offset(offset) };
        set_instruction_address(new_instruction_address);
    }
}
