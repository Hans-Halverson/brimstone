use std::{collections::HashSet, ops::Deref};

use crate::{
    js::runtime::{
        abstract_operations::{
            copy_data_properties, create_data_property_or_throw, define_property_or_throw, set,
        },
        array_object::{array_create, ArrayObject},
        error::{reference_error_, type_error_},
        eval::expression::{
            eval_add, eval_bitwise_and, eval_bitwise_not, eval_bitwise_or, eval_bitwise_xor,
            eval_delete_property, eval_divide, eval_exponentiation, eval_greater_than,
            eval_greater_than_or_equal, eval_in_expression, eval_instanceof_expression,
            eval_less_than, eval_less_than_or_equal, eval_multiply, eval_negate, eval_remainder,
            eval_shift_left, eval_shift_right_arithmetic, eval_shift_right_logical, eval_subtract,
            eval_typeof,
        },
        for_in_iterator::ForInIterator,
        function::build_function_name,
        gc::{HandleScope, HeapVisitor},
        get,
        intrinsics::{
            intrinsics::Intrinsic, regexp_constructor::RegExpObject,
            rust_runtime::RustRuntimeFunctionId,
        },
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{object_create_from_constructor, ordinary_object_create},
        property::Property,
        regexp::compiled_regexp::CompiledRegExpObject,
        to_string,
        type_utilities::{
            is_loosely_equal, is_strictly_equal, to_boolean, to_number, to_numeric, to_object,
            to_property_key,
        },
        value::BigIntValue,
        Context, EvalResult, Handle, HeapPtr, PropertyDescriptor, PropertyKey, Value,
    },
    maybe, must,
};

use super::{
    constant_table::ConstantTable,
    function::{BytecodeFunction, Closure},
    instruction::{
        extra_wide_prefix_index_to_opcode_index, wide_prefix_index_to_opcode_index, AddInstruction,
        BitAndInstruction, BitNotInstruction, BitOrInstruction, BitXorInstruction, CallInstruction,
        CallWithReceiverInstruction, CheckTdzInstruction, ConstructInstruction,
        CopyDataPropertiesInstruction, DecInstruction, DefineNamedPropertyInstruction,
        DefinePropertyFlags, DefinePropertyInstruction, DeletePropertyInstruction, DivInstruction,
        ExpInstruction, ForInNextInstruction, GetNamedPropertyInstruction, GetPropertyInstruction,
        GreaterThanInstruction, GreaterThanOrEqualInstruction, InInstruction, IncInstruction,
        InstanceOfInstruction, Instruction, JumpConstantInstruction, JumpFalseConstantInstruction,
        JumpFalseInstruction, JumpInstruction, JumpNotNullishConstantInstruction,
        JumpNotNullishInstruction, JumpNotUndefinedConstantInstruction,
        JumpNotUndefinedInstruction, JumpNullishConstantInstruction, JumpNullishInstruction,
        JumpToBooleanFalseConstantInstruction, JumpToBooleanFalseInstruction,
        JumpToBooleanTrueConstantInstruction, JumpToBooleanTrueInstruction,
        JumpTrueConstantInstruction, JumpTrueInstruction, LessThanInstruction,
        LessThanOrEqualInstruction, LoadConstantInstruction, LoadEmptyInstruction,
        LoadFalseInstruction, LoadGlobalInstruction, LoadImmediateInstruction, LoadNullInstruction,
        LoadTrueInstruction, LoadUndefinedInstruction, LogNotInstruction, LooseEqualInstruction,
        LooseNotEqualInstruction, MovInstruction, MulInstruction, NegInstruction,
        NewArrayInstruction, NewClosureInstruction, NewForInIteratorInstruction,
        NewObjectInstruction, NewRegExpInstruction, OpCode, RemInstruction,
        RestParameterInstruction, RetInstruction, SetArrayPropertyInstruction,
        SetNamedPropertyInstruction, SetPropertyInstruction, SetPrototypeOfInstruction,
        ShiftLeftInstruction, ShiftRightArithmeticInstruction, ShiftRightLogicalInstruction,
        StoreGlobalInstruction, StrictEqualInstruction, StrictNotEqualInstruction, SubInstruction,
        ThrowInstruction, ToNumberInstruction, ToNumericInstruction, ToPropertyKeyInstruction,
        ToStringInstruction, TypeOfInstruction,
    },
    instruction_traits::{
        GenericCallInstruction, GenericJumpBooleanConstantInstruction,
        GenericJumpBooleanInstruction, GenericJumpNullishConstantInstruction,
        GenericJumpNullishInstruction, GenericJumpToBooleanConstantInstruction,
        GenericJumpToBooleanInstruction, GenericJumpUndefinedConstantInstruction,
        GenericJumpUndefinedInstruction,
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

    /// Execute a function with the provided arguments. Starts a new execution of the VM,
    /// initializing stack from scratch.
    pub fn execute(
        &mut self,
        closure: Handle<Closure>,
        arguments: &[Handle<Value>],
    ) -> Result<Handle<Value>, Handle<Value>> {
        // Evaluate in the global scope
        let receiver = self.cx.get_global_object().into();

        // Evaluate the provided function
        let eval_result = self.call_from_rust(closure.cast(), receiver, arguments);

        eval_result.to_rust_result()
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
                        OpCode::NewObject => dispatch!(NewObjectInstruction, execute_new_object),
                        OpCode::NewArray => dispatch!(NewArrayInstruction, execute_new_array),
                        OpCode::NewRegExp => {
                            dispatch_or_throw!(NewRegExpInstruction, execute_new_regexp)
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
                        OpCode::DeleteProperty => {
                            dispatch_or_throw!(DeletePropertyInstruction, execute_delete_property)
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
                        OpCode::Throw => execute_throw!(get_instr),
                        OpCode::RestParameter => {
                            dispatch!(RestParameterInstruction, execute_rest_parameter)
                        }
                        OpCode::CheckTdz => {
                            dispatch_or_throw!(CheckTdzInstruction, execute_check_tdz)
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
    pub fn closure(&self) -> HeapPtr<Closure> {
        StackFrame::for_fp(self.fp).closure()
    }

    #[inline]
    fn constant_table(&self) -> HeapPtr<ConstantTable> {
        StackFrame::for_fp(self.fp).constant_table()
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
        // Check that the value is callable. Only allocates when throwing.
        let closure_handle = maybe!(self.check_value_is_closure(function.get())).to_handle();

        // Get the receiver to use. May allocate.
        let is_strict = closure_handle.function_ptr().is_strict();
        let receiver = maybe!(self.generate_receiver(Some(receiver.get()), is_strict));
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
        let closure_handle = maybe!(self.check_value_is_constructor(function.get())).to_handle();

        // Create the receiver to use. Allocates.
        let receiver = maybe!(self.generate_constructor_receiver(new_target));

        // Reuse function handle for receiver
        let closure_ptr = closure_handle.get_();
        let function_ptr = closure_ptr.function_ptr();

        let mut receiver_handle = closure_handle.cast::<ObjectValue>();
        receiver_handle.replace(receiver.into());

        // Check if this is a call to a function in the Rust runtime
        let return_value = if let Some(function_id) = function_ptr.rust_runtime_function_id() {
            // Call rust runtime function directly in its own handle scope
            maybe!(HandleScope::new(self.cx, |_| {
                self.call_rust_runtime(
                    closure_ptr,
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
                closure_ptr,
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
        let closure_ptr = maybe!(self.check_value_is_closure(function_value));
        let function_ptr = closure_ptr.function_ptr();

        // Check if this is a call to a function in the Rust runtime
        if let Some(function_id) = function_ptr.rust_runtime_function_id() {
            let return_value = maybe!(HandleScope::new(self.cx, |_| {
                // Get the receiver to use. May allocate.
                let closure_handle = closure_ptr.to_handle();
                let receiver = maybe!(self.generate_receiver(receiver, function_ptr.is_strict()));
                let closure_ptr = closure_handle.get_();

                // Reuse function handle for receiver
                let mut receiver_handle = closure_handle.cast::<Value>();
                receiver_handle.replace(receiver);

                // All arguments must be placed behind handles before calling into Rust
                let mut arguments = vec![];
                for arg in args_rev_slice.iter().rev() {
                    arguments.push(arg.to_handle(self.cx));
                }

                self.call_rust_runtime(closure_ptr, function_id, receiver_handle, &arguments, None)
            }));

            // Set the return value from the Rust runtime call
            unsafe { *return_value_address = return_value.get() };
        } else {
            // Otherwise this is a call to a JS function in the VM.

            // Get the receiver to use. May allocate.
            let closure_handle = closure_ptr.to_handle();
            let receiver = maybe!(self.generate_receiver(receiver, function_ptr.is_strict()));
            let closure_ptr = closure_handle.get_();

            // Set up the stack frame for the function call
            self.push_stack_frame(
                closure_ptr,
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
        let closure_ptr = maybe!(self.check_value_is_constructor(function_value)).to_handle();
        let function_ptr = closure_ptr.function_ptr();

        // Check if this is a call to a function in the Rust runtime
        let return_value = if let Some(function_id) = function_ptr.rust_runtime_function_id() {
            let return_value: Handle<ObjectValue> = maybe!(HandleScope::new(self.cx, |_| {
                let closure_handle = closure_ptr.to_handle();

                // TODO: Check if this cast is safe
                let new_target = self
                    .read_register(instr.new_target())
                    .to_handle(self.cx)
                    .cast();

                // Create the receiver to use. Allocates.
                let receiver = maybe!(self.generate_constructor_receiver(new_target));

                // Reuse function handle for receiver
                let closure_ptr = closure_handle.get_();
                let mut receiver_handle = closure_handle.cast::<ObjectValue>();
                receiver_handle.replace(receiver);

                // All arguments must be placed behind handles before calling into Rust
                let mut arguments = vec![];
                for arg in args_rev_slice.iter().rev() {
                    arguments.push(arg.to_handle(self.cx));
                }

                let return_value = maybe!(self.call_rust_runtime(
                    closure_ptr,
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
            let closure_handle = closure_ptr.to_handle();

            // TODO: Check if this cast is safe
            let new_target = self
                .read_register(instr.new_target())
                .to_handle(self.cx)
                .cast();

            // Create the receiver to use. Allocates.
            let receiver = maybe!(self.generate_constructor_receiver(new_target));

            let closure_ptr = closure_handle.get_();
            let mut receiver_handle = closure_handle.cast::<ObjectValue>();
            receiver_handle.replace(receiver);

            // Push the address of the return value
            let mut return_value = Value::undefined();
            let inner_call_return_value_address = (&mut return_value) as *mut Value;

            // Set up the stack frame for the function call
            self.push_stack_frame(
                closure_ptr,
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
    fn check_value_is_closure(&self, value: Value) -> EvalResult<HeapPtr<Closure>> {
        if !value.is_pointer() || value.as_pointer().descriptor().kind() != ObjectKind::Closure {
            return type_error_(self.cx, "value is not a function");
        }

        value.as_pointer().cast::<Closure>().into()
    }

    /// Check that a value is a constructor, returning the inner BytecodeFunction.
    ///
    /// Only allocates when throwing.
    #[inline]
    fn check_value_is_constructor(&self, value: Value) -> EvalResult<HeapPtr<Closure>> {
        if !value.is_pointer() || value.as_pointer().descriptor().kind() != ObjectKind::Closure {
            return type_error_(self.cx, "value is not a constructor");
        }

        let closure = value.as_pointer().cast::<Closure>();
        if !closure.function_ptr().is_constructor() {
            return type_error_(self.cx, "value is not a constructor");
        }

        closure.into()
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

        // Push arguments
        let argc_to_push = self.push_call_arguments(bytecode_function, args_rev_iter, argc);

        // Push the receiver if one is supplied, or the default receiver otherwise
        self.push(receiver.as_raw_bits() as StackSlotValue);

        // Push argc
        self.push(argc_to_push);

        // Push the function
        self.push(closure.as_ptr() as StackSlotValue);

        // Push the constant table
        let constant_table = unsafe { std::mem::transmute(bytecode_function.constant_table_ptr()) };
        self.push(constant_table);

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
            self.pc = self.get_return_address();
            self.sp = self.fp.add(FIRST_ARGUMENT_SLOT_INDEX + self.get_argc());
            self.fp = *self.fp as *mut StackSlotValue;
        }
    }

    /// Find slice over the arguments given argv and argc, starting with last argument.
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
        let name = self.get_constant(instr.constant_index());
        let name = name.as_string().to_handle();

        let dest = instr.dest();

        // TODO: Use global scope with new scope system
        HandleScope::new(self.cx, |cx| {
            // May allocate, reuse name handle
            let key = PropertyKey::string(cx, name);
            let key = name.replace_into(key);

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
        let value = self.read_register_to_handle(instr.value());

        let name = self.get_constant(instr.constant_index());
        let name = name.as_string().to_handle();

        // TODO: Use global scope with new scope system
        HandleScope::new(self.cx, |cx| {
            // May allocate, reuse name handle
            let key = PropertyKey::string(cx, name);
            let key = name.replace_into(key);

            maybe!(set(cx, cx.get_global_object(), key, value, false));

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
    fn execute_new_closure<W: Width>(&mut self, instr: &NewClosureInstruction<W>) {
        let func = self.get_constant(instr.function_index());
        let func = func.to_handle(self.cx).cast::<BytecodeFunction>();

        let dest = instr.dest();

        // Allocates
        let closure = Closure::new(self.cx, func);

        self.write_register(dest, Value::object(closure.get_().cast()));
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
    fn execute_get_property<W: Width>(
        &mut self,
        instr: &GetPropertyInstruction<W>,
    ) -> EvalResult<()> {
        let object = self.read_register_to_handle(instr.object());
        let key = self.read_register_to_handle(instr.key());
        let dest = instr.dest();

        // May allocate
        let property_key = maybe!(to_property_key(self.cx, key));
        let object = maybe!(to_object(self.cx, object));

        let result = maybe!(get(self.cx, object, property_key));

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
        let object = maybe!(to_object(self.cx, object));

        set(self.cx, object, property_key, value, is_strict)
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

        // Uncommon cases when some flags are set, e.g. for accessors or named evaluation"
        if !flags.is_empty() {
            // We only set flags when the value evaluates to a closure
            debug_assert!(
                value.is_pointer() && value.as_pointer().descriptor().kind() == ObjectKind::Closure
            );
            let closure = value.cast::<Closure>();

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

                // Perform a raw set of the property, overwriting the previous value even though it was
                // not writable. This will preserve the order of the properties, as the function name
                // was initially added but defaulted to the empty string.
                let property = Property::data(name.into(), false, false, true);
                closure
                    .cast::<ObjectValue>()
                    .set_property(self.cx, self.cx.names.name(), property);
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

        // May allocate, replace handle
        let property_key = PropertyKey::string(self.cx, key);
        let property_key = key.replace_into(property_key);

        let object = maybe!(to_object(self.cx, object));
        let result = maybe!(get(self.cx, object, property_key));

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
        let object = maybe!(to_object(self.cx, object));

        let property_key = PropertyKey::string(self.cx, key);
        let property_key = key.replace_into(property_key);

        set(self.cx, object, property_key, value, is_strict)
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
        let stack_frame = StackFrame::for_fp(self.fp);

        for (i, argument) in stack_frame.args()[num_parameters..].iter().enumerate() {
            array_key.replace(PropertyKey::array_index(self.cx, i as u32));
            value_handle.replace(*argument);

            let array_property = Property::data(value_handle, true, true, true);
            rest_array
                .object()
                .set_property(self.cx, array_key, array_property);
        }

        self.write_register(dest, rest_array.cast::<Value>().get());
    }

    #[inline]
    fn execute_check_tdz<W: Width>(&mut self, instr: &CheckTdzInstruction<W>) -> EvalResult<()> {
        let value = self.read_register(instr.value());

        // Binding in TDZ represented as an empty value
        if !value.is_empty() {
            return ().into();
        }

        let name = self.get_constant(instr.name_constant_index()).as_string();

        reference_error_(self.cx, &format!("can't access `{}` before initialization", name))
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

        for handler in func.exception_handlers_ptr().unwrap().iter() {
            // The saved return address points to the start of the next instruction, so treat the
            // handler bounds as (exclusive, inclusive].
            if handler.start() < instr_offset && instr_offset <= handler.end() {
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
            // Visit all args and registers in stack frame
            for arg in stack_frame.args_with_receiver_mut() {
                visitor.visit_value(arg);
            }

            for register in stack_frame.registers_mut() {
                visitor.visit_value(register);
            }

            visitor.visit_pointer(stack_frame.closure_mut());
            visitor.visit_pointer(stack_frame.constant_table_mut());

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
        // Find the offset of the instruction in the BytecodeFunction
        let function_start = stack_frame.closure().function_ptr().as_ptr() as *const u8;
        let offset = unsafe { instruction_address.offset_from(function_start) };

        // Visit the caller's BytecodeFunction, moving it in the heap and rewriting this pointer
        let mut bytecode_function = stack_frame.closure().function_ptr();
        visitor.visit_pointer(&mut bytecode_function);

        // Rewrite the instruction address using the new location of the BytecodeFunction
        let new_function_start = bytecode_function.as_ptr() as *const u8;
        let new_instruction_address = unsafe { new_function_start.offset(offset) };
        set_instruction_address(new_instruction_address);
    }
}
