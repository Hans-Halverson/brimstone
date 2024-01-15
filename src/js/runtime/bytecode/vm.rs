use std::ops::{Deref, Not};

use crate::{
    js::runtime::{
        abstract_operations::set,
        eval::expression::{
            eval_add, eval_divide, eval_exponentiation, eval_greater_than,
            eval_greater_than_or_equal, eval_less_than, eval_less_than_or_equal, eval_multiply,
            eval_remainder, eval_subtract,
        },
        gc::{HandleScope, HeapVisitor},
        get,
        intrinsics::rust_runtime::decode_rust_runtime_id,
        type_utilities::{is_loosely_equal, is_strictly_equal, to_boolean, to_object},
        Context, EvalResult, Handle, HeapPtr, PropertyKey, Value,
    },
    maybe,
};

use super::{
    function::{BytecodeFunction, Closure},
    instruction::{
        extra_wide_prefix_index_to_opcode_index, wide_prefix_index_to_opcode_index, AddInstruction,
        CallInstruction, CallRustRuntimeInstruction, CallWithReceiverInstruction, DivInstruction,
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
        StrictNotEqualInstruction, SubInstruction, ThrowInstruction,
    },
    operand::{ConstantIndex, Register, SInt},
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

    pub fn execute(
        &mut self,
        closure: Handle<Closure>,
        arguments: &[Handle<Value>],
    ) -> Result<Handle<Value>, Handle<Value>> {
        self.is_executing = true;

        let func = closure.function_ptr();
        self.pc = func.bytecode().as_ptr();
        self.sp = self.stack.as_ptr_range().end as *mut StackSlotValue;
        self.fp = std::ptr::null_mut();

        // Create the initial stack frame

        // Push arguments
        let argc = self.push_call_arguments(
            func,
            arguments.iter().rev().map(Handle::deref),
            arguments.len(),
        );

        // Push the initial receiver
        let receiver = self.default_receiver(func.is_strict());
        self.push(receiver.as_raw_bits() as StackSlotValue);

        // Push argc
        self.push(argc);

        // Push the function
        self.push(func.as_ptr() as StackSlotValue);

        // Push the address of the return value
        let mut return_value = Value::undefined();
        self.push(&mut return_value as *mut _ as StackSlotValue);

        // Push a dummy return address
        self.push(0);

        // Create the frame pointer, pointing to null initial FP which marks the top of the stack
        self.push_fp();

        // Make room for function locals
        self.allocate_local_registers(func.num_registers());

        // Start the dispatch loop
        if let Err(error_value) = self.dispatch_loop() {
            return Err(error_value.to_handle(self.cx));
        }

        self.is_executing = false;

        Ok(return_value.to_handle(self.cx))
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
                            self.$func::<$width>(instr);
                            self.set_pc_after(instr);
                        }};
                    }

                    macro_rules! dispatch_or_throw {
                        ($instr:ident, $func:ident) => {{
                            let instr = get_instr!($instr);
                            maybe_throw!(self.$func::<$width>(instr));
                            self.set_pc_after(instr)
                        }};
                    }
                };
            }

            // Execute a call instruction
            macro_rules! execute_call {
                ($get_instr:ident, $instr:ident, $receiver_fn:expr) => {{
                    let instr = $get_instr!($instr);

                    let closure = self
                        .read_register(instr.function())
                        .as_object()
                        .cast::<Closure>();
                    let func = closure.function_ptr();

                    let argv = self.register_address(instr.argv());
                    let argc = instr.argc().value().to_usize();

                    // Push arguments
                    let args_slice = unsafe {
                        std::slice::from_raw_parts(
                            argv.sub(argc.saturating_sub(1)) as *const Value,
                            argc,
                        )
                    };
                    let argc_to_push = self.push_call_arguments(func, args_slice.iter(), argc);

                    // Push the receiver if one is supplied, otherwise push undefined
                    let receiver = if let Some(receiver_fn) = $receiver_fn {
                        let receiver = self.read_register(receiver_fn(instr));
                        maybe_throw!(self.coerce_receiver(receiver, func.is_strict()))
                    } else {
                        self.default_receiver(func.is_strict())
                    };
                    self.push(receiver.as_raw_bits() as StackSlotValue);

                    // Push argc
                    self.push(argc_to_push);

                    // Push the function
                    self.push(func.as_ptr() as StackSlotValue);

                    // Push the address of the return value register
                    let return_value_address = self.register_address(instr.dest());
                    self.push(return_value_address as StackSlotValue);

                    // Push the address of the next instruction as the return address
                    let return_address = self.get_pc_after(instr);
                    self.push(return_address as StackSlotValue);

                    // Push the frame pointer, creating a new frame pointer
                    self.push_fp();

                    // Make room for function locals
                    self.allocate_local_registers(func.num_registers());

                    // Set the PC to the start of the callee function's bytecode
                    self.pc = func.bytecode().as_ptr();
                }};
            }

            // Execute a call into the Rust runtime
            macro_rules! execute_call_rust_runtime {
                ($get_instr:ident) => {{
                    let instr = $get_instr!(CallRustRuntimeInstruction);

                    // Decode function id and use it to fetch function
                    let id_high_byte = instr.func_id1().value().to_usize() as u8;
                    let id_low_byte = instr.func_id2().value().to_usize() as u8;
                    let function_id = decode_rust_runtime_id(id_high_byte, id_low_byte);
                    let rust_function = self.cx.rust_runtime_functions.get_function(function_id);

                    // Runtime call is wrapped in its own handle scope
                    maybe_throw!(HandleScope::new(self.cx, |cx| {
                        // Use the same arguments and receiver as the caller function
                        let argv = self.register_address(Register::<Narrow>::argument(0));
                        let argc = self.get_argc();
                        let args_slice =
                            unsafe { std::slice::from_raw_parts(argv as *const Value, argc) };

                        // All arguments must be placed behind handles before calling into Rust
                        let mut arguments = vec![];
                        for arg in args_slice {
                            arguments.push(arg.to_handle(cx));
                        }

                        // Receiver must be placed behind handle and passed separately from
                        // arguments.
                        self.h1.replace(self.get_receiver());
                        let receiver = self.h1;

                        // Call rust function
                        // TODO: Handle new target
                        let result = maybe!(rust_function(cx, receiver, &arguments, None));

                        // Write return register
                        self.write_register(instr.dest(), result.get());
                        self.set_pc_after(instr);

                        ().into()
                    }));
                }};
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
                    self.pc = self.get_return_address();

                    let argc = self.get_argc();

                    // Destroy the stack frame
                    unsafe {
                        self.sp = self.fp.add(FIRST_ARGUMENT_SLOT_INDEX + argc);
                        self.fp = *self.fp as *mut StackSlotValue;
                    }

                    // A null FP indicates that we have reached the top of the stack
                    if self.fp.is_null() {
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
                    // TODO: Check for throwing out of re-entrant VM call
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

            // Execute an unconditional jump instruction
            macro_rules! execute_jump {
                ($get_instr:ident) => {{
                    let instr = $get_instr!(JumpInstruction);
                    self.jump_immediate(instr.offset());
                }};
            }

            // Execute an unconditional jump constant instruction
            macro_rules! execute_jump_constant {
                ($get_instr:ident) => {{
                    let instr = $get_instr!(JumpConstantInstruction);
                    self.jump_constant(instr.constant_index());
                }};
            }

            // Execute a conditional jump if true/false instruction
            macro_rules! execute_jump_boolean {
                ($get_instr:ident, $instr:ident, $cond_fn:expr) => {{
                    let instr = $get_instr!($instr);

                    let condition = self.read_register(instr.condition());
                    if $cond_fn(&condition) {
                        self.jump_immediate(instr.offset());
                    } else {
                        self.set_pc_after(instr);
                    }
                }};
            }

            // Execute a conditional jump if false constant instruction
            macro_rules! execute_jump_boolean_constant {
                ($get_instr:ident, $instr:ident, $cond_fn:expr) => {{
                    let instr = $get_instr!($instr);

                    let condition = self.read_register(instr.condition());
                    if $cond_fn(&condition) {
                        self.jump_constant(instr.constant_index());
                    } else {
                        self.set_pc_after(instr);
                    }
                }};
            }

            // Execute a conditional jump if ToBoolean false instruction
            macro_rules! execute_jump_to_boolean {
                ($get_instr:ident, $instr:ident, $cond_fn:expr) => {{
                    let instr = $get_instr!($instr);

                    let condition = self.read_register(instr.condition());
                    if $cond_fn(to_boolean(condition)) {
                        self.set_pc_after(instr);
                    } else {
                        self.jump_immediate(instr.offset());
                    }
                }};
            }

            // Execute a conditional jump if ToBoolean false constant instruction
            macro_rules! execute_jump_to_boolean_constant {
                ($get_instr:ident, $instr:ident, $cond_fn:expr) => {{
                    let instr = $get_instr!($instr);

                    let condition = self.read_register(instr.condition());
                    if $cond_fn(to_boolean(condition)) {
                        self.set_pc_after(instr);
                    } else {
                        self.jump_constant(instr.constant_index());
                    }
                }};
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
                        OpCode::Call => {
                            let receiver_fn: Option<fn(_) -> Register<$width>> = None;
                            execute_call!(get_instr, CallInstruction, receiver_fn)
                        }
                        OpCode::CallWithReceiver => {
                            let receiver_fn =
                                |instr: &CallWithReceiverInstruction<$width>| -> Register<$width> {
                                    instr.receiver()
                                };
                            execute_call!(get_instr, CallWithReceiverInstruction, Some(receiver_fn))
                        }
                        OpCode::CallRustRuntime => execute_call_rust_runtime!(get_instr),
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
                        OpCode::Jump => execute_jump!(get_instr),
                        OpCode::JumpConstant => execute_jump_constant!(get_instr),
                        OpCode::JumpTrue => {
                            execute_jump_boolean!(get_instr, JumpTrueInstruction, Value::is_true)
                        }
                        OpCode::JumpTrueConstant => execute_jump_boolean_constant!(
                            get_instr,
                            JumpTrueConstantInstruction,
                            Value::is_true
                        ),
                        OpCode::JumpToBooleanTrue => execute_jump_to_boolean!(
                            get_instr,
                            JumpToBooleanTrueInstruction,
                            bool::not
                        ),
                        OpCode::JumpToBooleanTrueConstant => {
                            execute_jump_to_boolean_constant!(
                                get_instr,
                                JumpToBooleanTrueConstantInstruction,
                                bool::not
                            )
                        }
                        OpCode::JumpFalse => {
                            execute_jump_boolean!(get_instr, JumpFalseInstruction, Value::is_false)
                        }
                        OpCode::JumpFalseConstant => execute_jump_boolean_constant!(
                            get_instr,
                            JumpFalseConstantInstruction,
                            Value::is_false
                        ),
                        OpCode::JumpToBooleanFalse => execute_jump_to_boolean!(
                            get_instr,
                            JumpToBooleanFalseInstruction,
                            |b| b
                        ),
                        OpCode::JumpToBooleanFalseConstant => {
                            execute_jump_to_boolean_constant!(
                                get_instr,
                                JumpToBooleanFalseConstantInstruction,
                                |b| b
                            )
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
    fn get_receiver(&self) -> Value {
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

    /// Return the default receiver used for a function call when no receiver is provided.
    #[inline]
    fn default_receiver(&self, is_strict: bool) -> Value {
        if is_strict {
            Value::undefined()
        } else {
            self.cx.get_global_object_ptr().into()
        }
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

        // TODO: Use global scope with new scope system
        HandleScope::new(self.cx, |cx| {
            let key = PropertyKey::string(cx, name).to_handle(cx);
            let value = maybe!(get(cx, cx.get_global_object(), key));
            self.write_register(instr.dest(), value.get());

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

        let result = maybe!(eval_add(self.cx, self.h1, self.h2));
        self.write_register(instr.dest(), result.get());

        ().into()
    }

    #[inline]
    fn execute_sub<W: Width>(&mut self, instr: &SubInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let result = maybe!(eval_subtract(self.cx, self.h1, self.h2));
        self.write_register(instr.dest(), result.get());

        ().into()
    }

    #[inline]
    fn execute_mul<W: Width>(&mut self, instr: &MulInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let result = maybe!(eval_multiply(self.cx, self.h1, self.h2));
        self.write_register(instr.dest(), result.get());

        ().into()
    }

    #[inline]
    fn execute_div<W: Width>(&mut self, instr: &DivInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let result = maybe!(eval_divide(self.cx, self.h1, self.h2));
        self.write_register(instr.dest(), result.get());

        ().into()
    }

    #[inline]
    fn execute_rem<W: Width>(&mut self, instr: &RemInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let result = maybe!(eval_remainder(self.cx, self.h1, self.h2));
        self.write_register(instr.dest(), result.get());

        ().into()
    }

    #[inline]
    fn execute_exp<W: Width>(&mut self, instr: &ExpInstruction<W>) -> EvalResult<()> {
        let left_value = self.read_register(instr.left());
        self.h1.replace(left_value);

        let right_value = self.read_register(instr.right());
        self.h2.replace(right_value);

        let result = maybe!(eval_exponentiation(self.cx, self.h1, self.h2));
        self.write_register(instr.dest(), result.get());

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

        let result = maybe!(is_loosely_equal(self.cx, self.h1, self.h2));
        self.write_register(instr.dest(), Value::bool(result));

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

        let result = maybe!(is_loosely_equal(self.cx, self.h1, self.h2));
        self.write_register(instr.dest(), Value::bool(!result));

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

        let result = maybe!(eval_less_than(self.cx, self.h1, self.h2));
        self.write_register(instr.dest(), result.get());

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

        let result = maybe!(eval_less_than_or_equal(self.cx, self.h1, self.h2));
        self.write_register(instr.dest(), result.get());

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

        let result = maybe!(eval_greater_than(self.cx, self.h1, self.h2));
        self.write_register(instr.dest(), result.get());

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

        let result = maybe!(eval_greater_than_or_equal(self.cx, self.h1, self.h2));
        self.write_register(instr.dest(), result.get());

        ().into()
    }

    #[inline]
    fn execute_new_closure<W: Width>(&mut self, instr: &NewClosureInstruction<W>) {
        let func = self.get_constant(self.get_function(), instr.function_index());
        self.h1.replace(func);

        let closure = Closure::new_ptr(self.cx, self.h1.cast::<BytecodeFunction>());
        self.write_register(instr.dest(), Value::object(closure.cast()))
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

        let key = PropertyKey::string(self.cx, self.h2.as_string());
        self.h2.replace(key.as_string().into());

        let object = maybe!(to_object(self.cx, self.h1));
        let result = maybe!(get(self.cx, object, self.h2.cast()));

        self.write_register(instr.dest(), result.get());

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

        let key = PropertyKey::string(self.cx, self.h2.as_string());
        self.h2.replace(key.as_string().into());

        let value = self.read_register(instr.value());
        self.h3.replace(value);

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
        Self::rewrite_bytecode_function_and_address(visitor, stack_frame, &mut self.pc);

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
                    stack_frame.return_address_mut(),
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
        instruction_address: &mut *const u8,
    ) {
        // Find the offset of the instruction in the BytecodeFunction
        let function_start = stack_frame.bytecode_function().as_ptr() as *const u8;
        let offset = unsafe { (*instruction_address).offset_from(function_start) };

        // Visit the caller's BytecodeFunction, moving it in the heap
        visitor.visit_pointer(stack_frame.bytecode_function_mut());

        // Rewrite the instruction address using the new location of the BytecodeFunction
        let new_function_start = stack_frame.bytecode_function().as_ptr() as *const u8;
        let new_instruction_address = unsafe { new_function_start.offset(offset) };
        *instruction_address = new_instruction_address;
    }
}
