use crate::js::runtime::{scope::Scope, HeapPtr, Value};

use super::{constant_table::ConstantTable, function::Closure};

/// Stack frame layout:
///
/// Stack grows downwards towards lower addresses.
///
///     +------------------+
///     |       argn       |  (last arg)
///     +------------------+
///     |       ...        |
///     +------------------+
///     |       arg0       |  (first arg)
/// +64 +------------------+                     ^                ^
///     |     receiver     |  (receiver)         | caller's frame |
/// +56 +------------------+                     +----------------+
///     |       argc       |                     | callee's frame |
/// +48 +------------------+                     v                v
///     |      closure     |  (closure of the caller)
/// +40 +------------------+
///     |  constant_table  |  (constant table of the called closure)
/// +32 +------------------+
///     |       scope      |  (current VM scope)
/// +24 +------------------+
///     |  return val addr |  (address of the return value)
/// +16 +------------------+
///     |  return address  |  (caller's saved pc)
///  +8 +------------------+
///     |     saved fp     |  (caller's saved fp)
///   0 +------------------+  <- fp
///     |       reg0       |  (first register)
///  -8 +------------------+
///     |       reg1       |
///     +------------------+
///     |       ...        |
///     +------------------+                     ^                ^
///     |       regn       |  (last register)    | callee's frame |
///     +------------------+  <- sp              +----------------+
#[derive(Clone, Copy)]
pub struct StackFrame {
    /// Stack frames are centered around the frame pointer
    fp: *const StackSlotValue,
}

impl StackFrame {
    /// Create a new stack frame centered around a frame pointer.
    #[inline]
    pub fn for_fp(fp: *mut StackSlotValue) -> Self {
        Self { fp: fp.cast_const() }
    }

    /// Return the previous stack frame, or None if this is the first frame on the stack.
    #[inline]
    pub fn previous_frame(&self) -> Option<StackFrame> {
        let prev_fp = unsafe { *self.fp as *mut StackSlotValue };
        if prev_fp.is_null() {
            return None;
        }

        Some(StackFrame::for_fp(prev_fp))
    }

    /// Return the frame pointer for this stack frame.
    #[inline]
    pub fn fp(&self) -> *mut StackSlotValue {
        self.fp.cast_mut()
    }

    /// Return the stack pointer for this stack frame.
    #[inline]
    pub fn sp(&self) -> *mut StackSlotValue {
        let num_registers = self.closure().function_ptr().num_registers() as usize;
        unsafe { self.fp.offset(-1 - num_registers as isize).cast_mut() }
    }

    /// Highest bit of the return address slot indicates whether the caller is the Rust runtime.
    const IS_RUST_CALLER_TAG: usize = 1 << (usize::BITS - 1);

    /// The return address slot holds both the return address and a flag (in the topmost bit)
    /// indicating whether the caller is the Rust runtime.
    #[inline]
    fn encode_return_address_slot(return_address: *const u8, is_rust_caller: bool) -> usize {
        if is_rust_caller {
            (return_address as usize) | Self::IS_RUST_CALLER_TAG
        } else {
            return_address as usize
        }
    }

    /// Encode the return address slot for a call from Rust.
    #[inline]
    pub fn return_address_from_rust(return_address: *const u8) -> usize {
        Self::encode_return_address_slot(return_address, true)
    }

    /// Encode the return addres slot for a call from the JS VM.
    #[inline]
    pub fn return_address_from_vm(return_address: *const u8) -> usize {
        Self::encode_return_address_slot(return_address, false)
    }

    /// Whether the caller of the function in the current stack frame is the Rust runtime.
    #[inline]
    pub fn is_rust_caller(&self) -> bool {
        let encoded_value = unsafe { *self.fp.add(RETURN_ADDRESS_SLOT_INDEX) };
        (encoded_value & Self::IS_RUST_CALLER_TAG) != 0
    }

    /// The return address stored within this stack frame. This return address points to the next
    /// instruction to execute within the caller function.
    #[inline]
    pub fn return_address(&self) -> *const u8 {
        let encoded_value = unsafe { *self.fp.add(RETURN_ADDRESS_SLOT_INDEX) };
        ((encoded_value as usize) & !Self::IS_RUST_CALLER_TAG) as *const u8
    }

    /// Set the return address, preserving the Rust caller flag.
    #[inline]
    pub fn set_return_address(&mut self, addr: *const u8) {
        let is_rust_caller = self.is_rust_caller();
        let encoded_address = Self::encode_return_address_slot(addr, is_rust_caller);
        unsafe { *(self.fp.add(RETURN_ADDRESS_SLOT_INDEX).cast_mut()) = encoded_address }
    }

    /// Address where the return value should be stored.
    #[inline]
    pub fn return_value_address(&self) -> *mut Value {
        unsafe { *self.fp.add(RETURN_VALUE_ADDRESS_INDEX) as *mut Value }
    }

    #[inline]
    pub fn scope(&self) -> HeapPtr<Scope> {
        let ptr = unsafe { *self.fp.add(SCOPE_SLOT_INDEX) };
        HeapPtr::from_ptr(ptr as *mut Scope)
    }

    /// A mutable reference to the constant table of the callee function in this stack frame.
    #[inline]
    pub fn scope_mut(&mut self) -> &mut HeapPtr<Scope> {
        unsafe { &mut *(self.fp.add(SCOPE_SLOT_INDEX) as *mut HeapPtr<Scope>) }
    }

    /// The constant table of the callee function in this stack frame.
    #[inline]
    pub fn constant_table(&self) -> HeapPtr<ConstantTable> {
        let ptr = unsafe { *self.fp.add(CONSTANT_TABLE_SLOT_INDEX) };
        HeapPtr::from_ptr(ptr as *mut ConstantTable)
    }

    /// A mutable reference to the constant table of the callee function in this stack frame.
    #[inline]
    pub fn constant_table_mut(&mut self) -> &mut HeapPtr<ConstantTable> {
        unsafe { &mut *(self.fp.add(CONSTANT_TABLE_SLOT_INDEX) as *mut HeapPtr<ConstantTable>) }
    }

    /// The callee function in this stack frame.
    #[inline]
    pub fn closure(&self) -> HeapPtr<Closure> {
        let ptr = unsafe { *self.fp.add(CLOSURE_SLOT_INDEX) };
        HeapPtr::from_ptr(ptr as *mut Closure)
    }

    /// A mutable reference to the callee function in this stack frame.
    #[inline]
    pub fn closure_mut(&mut self) -> &mut HeapPtr<Closure> {
        unsafe { &mut *(self.fp.add(CLOSURE_SLOT_INDEX) as *mut HeapPtr<Closure>) }
    }

    /// The number of arguments in this stack frame, not including the receiver.
    #[inline]
    pub fn argc(&self) -> usize {
        unsafe { *self.fp.add(ARGC_SLOT_INDEX) }
    }

    /// Slice over args portion of frame, not including the receiver.
    #[inline]
    pub fn args(&self) -> &[Value] {
        unsafe {
            let argc = self.argc();
            let first_arg_ptr = self.fp.add(FIRST_ARGUMENT_SLOT_INDEX) as *const Value;
            std::slice::from_raw_parts(first_arg_ptr, argc)
        }
    }

    /// Mutable slice over args and receiver portion of frame, starting at receiver followed by
    /// the first argument.
    #[inline]
    pub fn args_with_receiver_mut(&self) -> &mut [Value] {
        unsafe {
            let argc_with_receiver = self.argc() + 1;
            let receiver_ptr = self.fp.add(RECEIVER_SLOT_INDEX) as *mut Value;
            std::slice::from_raw_parts_mut(receiver_ptr, argc_with_receiver)
        }
    }

    /// Mutable slice over registers portion of frame. Registers are in reverse order on stack, so
    /// starts at the last register.
    #[inline]
    pub fn registers_mut(&self) -> &mut [Value] {
        unsafe {
            let num_registers = self.closure().function_ptr().num_registers() as usize;
            let last_register_ptr = self.fp.sub(num_registers) as *mut Value;
            std::slice::from_raw_parts_mut(last_register_ptr, num_registers)
        }
    }
}

/// Generic value stored in a stack slot.
pub type StackSlotValue = usize;

/// Total size of the stack, 4MB.
const STACK_SIZE: usize = 4 * 1024 * 1024;

/// Total number of stack slots that fit in the stack.
pub const NUM_STACK_SLOTS: usize = STACK_SIZE / std::mem::size_of::<StackSlotValue>();

const RETURN_ADDRESS_SLOT_INDEX: usize = 1;

const RETURN_VALUE_ADDRESS_INDEX: usize = 2;

pub const SCOPE_SLOT_INDEX: usize = 3;

const CONSTANT_TABLE_SLOT_INDEX: usize = 4;

const CLOSURE_SLOT_INDEX: usize = 5;

const ARGC_SLOT_INDEX: usize = 6;

pub const RECEIVER_SLOT_INDEX: usize = 7;

pub const FIRST_ARGUMENT_SLOT_INDEX: usize = 8;
