use std::ops::Deref;

use crate::{
    js::runtime::{
        abstract_operations::set,
        bytecode::{
            instruction::{
                extra_wide_prefix_index_to_opcode_index, wide_prefix_index_to_opcode_index,
            },
            stack_frame::FIRST_ARGUMENT_SLOT_INDEX,
        },
        eval::expression::{eval_add, eval_less_than, eval_subtract},
        gc::HandleScope,
        get, Context, EvalResult, Handle, HeapPtr, PropertyKey, Value,
    },
    maybe,
};

use super::{
    function::{BytecodeFunction, Closure},
    instruction::{
        AddInstruction, CallInstruction, Instruction, JumpConstantInstruction,
        JumpFalseConstantInstruction, JumpFalseInstruction, JumpInstruction, LessThanInstruction,
        LoadConstantInstruction, LoadFalseInstruction, LoadGlobalInstruction,
        LoadImmediateInstruction, LoadNullInstruction, LoadTrueInstruction,
        LoadUndefinedInstruction, MovInstruction, NewClosureInstruction, OpCode, RetInstruction,
        StoreGlobalInstruction, SubInstruction,
    },
    operand::{ConstantIndex, Register, SInt},
    stack_frame::{
        StackSlotValue, ARGC_SLOT_INDEX, FUNCTION_SLOT_INDEX, NUM_STACK_SLOTS,
        RETURN_ADDRESS_SLOT_INDEX, RETURN_VALUE_ADDRESS_INDEX,
    },
    width::{ExtraWide, Narrow, SignedWidthRepr, UnsignedWidthRepr, Wide, Width},
};

/// The virtual machine that executes bytecode.
pub struct VM {
    cx: Context,

    // The program counter (instruction pointer)
    pc: *const u8,

    // The stack pointer
    sp: *mut StackSlotValue,

    // The frame pointer
    fp: *mut StackSlotValue,

    // Handle pool
    h1: Handle<Value>,
    h2: Handle<Value>,

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

            stack,
        }
    }

    pub fn execute(
        &mut self,
        closure: Handle<Closure>,
        arguments: &[Handle<Value>],
    ) -> Handle<Value> {
        let func = closure.function_ptr();
        self.pc = func.bytecode().as_ptr();
        self.sp = self.stack.as_ptr_range().end as *mut StackSlotValue;
        self.fp = std::ptr::null_mut();

        // Create the initial stack frame

        // Push arguments and argc
        self.push_call_arguments(func, arguments.iter().rev().map(Handle::deref), arguments.len());

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
        self.dispatch_loop();

        return_value.to_handle(self.cx)
    }

    /// Dispatch instructions, one after another, until there are no more instructions to execute.
    fn dispatch_loop(&mut self) {
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
                        match self.$func::<$width>(instr) {
                            EvalResult::Ok(_) => self.set_pc_after(instr),
                            EvalResult::Throw(_) => unimplemented!("Throwing in VM"),
                        }
                    }};
                }
            };
        }

        // Execute a call instruction
        macro_rules! execute_call {
            ($get_instr:ident) => {{
                let instr = $get_instr!(CallInstruction);

                let closure = self
                    .read_register(instr.function())
                    .as_object()
                    .cast::<Closure>();
                let func = closure.function_ptr();

                let argv = self.register_address(instr.argv());
                let argc = instr.argc().value().to_usize();

                // Push arguments and argc
                let args_slice =
                    unsafe { std::slice::from_raw_parts(argv.sub(argc - 1) as *const Value, argc) };
                self.push_call_arguments(func, args_slice.iter(), argc);

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
                    return;
                }
            }};
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

        // Execute a conditional jump if false instruction
        macro_rules! execute_jump_false {
            ($get_instr:ident) => {{
                let instr = $get_instr!(JumpFalseInstruction);

                let condition = self.read_register(instr.condition());
                if condition.is_false() {
                    self.jump_immediate(instr.offset());
                } else {
                    self.set_pc_after(instr);
                }
            }};
        }

        // Execute a conditional jump if false constant instruction
        macro_rules! execute_jump_false_constant {
            ($get_instr:ident) => {{
                let instr = $get_instr!(JumpFalseConstantInstruction);

                let condition = self.read_register(instr.condition());
                if condition.is_false() {
                    self.jump_constant(instr.constant_index());
                } else {
                    self.set_pc_after(instr);
                }
            }};
        }

        macro_rules! create_dispatch_table {
            ($width:ident, $opcode:ident, $opcode_pc:ident) => {
                create_dispatch_macros!($width, $opcode_pc);

                match $opcode {
                    // A prefix cannot follow the initial wide prefix
                    OpCode::WidePrefix => panic!("A prefix cannot appear at this position"),
                    OpCode::ExtraWidePrefix => panic!("A prefix cannot appear at this position"),
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
                    OpCode::Call => execute_call!(get_instr),
                    OpCode::Ret => execute_ret!(get_instr),
                    OpCode::Add => dispatch_or_throw!(AddInstruction, execute_add),
                    OpCode::Sub => dispatch_or_throw!(SubInstruction, execute_sub),
                    OpCode::LessThan => dispatch_or_throw!(LessThanInstruction, execute_less_than),
                    OpCode::Jump => execute_jump!(get_instr),
                    OpCode::JumpConstant => execute_jump_constant!(get_instr),
                    OpCode::JumpFalse => execute_jump_false!(get_instr),
                    OpCode::JumpFalseConstant => execute_jump_false_constant!(get_instr),
                    OpCode::NewClosure => dispatch!(NewClosureInstruction, execute_new_closure),
                }
            };
        }

        loop {
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
        unsafe { *self.fp_offset(RETURN_ADDRESS_SLOT_INDEX as isize) as *const u8 }
    }

    #[inline]
    fn get_return_value_address(&self) -> *mut Value {
        unsafe { *self.fp_offset(RETURN_VALUE_ADDRESS_INDEX as isize) as *mut Value }
    }

    #[inline]
    fn get_function(&self) -> HeapPtr<BytecodeFunction> {
        unsafe {
            let ptr = *self.fp_offset(FUNCTION_SLOT_INDEX as isize) as *mut BytecodeFunction;
            HeapPtr::from_ptr(ptr)
        }
    }

    #[inline]
    fn get_argc(&self) -> usize {
        unsafe { *self.fp_offset(ARGC_SLOT_INDEX as isize) as usize }
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

    /// Push call arguments onto the stack, followed by argc. Takes an iterator over the arguments
    /// in reverse order.
    #[inline]
    fn push_call_arguments<'a, I: Iterator<Item = &'a Value>>(
        &mut self,
        func: HeapPtr<BytecodeFunction>,
        args_rev_iter: I,
        argc: usize,
    ) {
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

        // Push argc
        self.push(num_arguments);
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
    fn execute_new_closure<W: Width>(&mut self, instr: &NewClosureInstruction<W>) {
        let func = self.get_constant(self.get_function(), instr.function_index());
        self.h1.replace(func);

        let closure = Closure::new_ptr(self.cx, self.h1.cast::<BytecodeFunction>());
        self.write_register(instr.dest(), Value::object(closure.cast()))
    }
}
