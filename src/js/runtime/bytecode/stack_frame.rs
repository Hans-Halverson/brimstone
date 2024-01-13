use crate::js::runtime::{HeapPtr, Value};

use super::function::BytecodeFunction;

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
/// +48 +------------------+                     ^                ^
///     |     receiver     |  (receiver)         | caller's frame |
/// +40 +------------------+                     +----------------+
///     |       argc       |                     | callee's frame |
/// +32 +------------------+                     v                v
///     |   bytecode func  |  (bytecode function of the called closure)
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

    /// The return address stored within this stack frame. This return address points to the next
    /// instruction to execute within the caller function.
    #[inline]
    pub fn return_address(&self) -> *const u8 {
        unsafe { *self.fp.add(RETURN_ADDRESS_SLOT_INDEX) as *const u8 }
    }

    /// A mutable reference to the return address stored within this stack frame. This return
    /// address points to the next instruction to execute within the caller function.
    #[inline]
    pub fn return_address_mut(&mut self) -> &mut *const u8 {
        unsafe { &mut *(self.fp.add(RETURN_ADDRESS_SLOT_INDEX) as *mut *const u8) }
    }

    /// Address where the return value should be stored.
    #[inline]
    pub fn return_value_address(&self) -> *mut Value {
        unsafe { *self.fp.add(RETURN_VALUE_ADDRESS_INDEX) as *mut Value }
    }

    /// The BytecodeFunction for the callee function in this stack frame.
    #[inline]
    pub fn bytecode_function(&self) -> HeapPtr<BytecodeFunction> {
        let ptr = unsafe { *self.fp.add(FUNCTION_SLOT_INDEX) };
        HeapPtr::from_ptr(ptr as *mut BytecodeFunction)
    }

    /// A mutable reference to the BytecodeFunction for the callee function in this stack frame.
    #[inline]
    pub fn bytecode_function_mut(&mut self) -> &mut HeapPtr<BytecodeFunction> {
        unsafe { &mut *(self.fp.add(FUNCTION_SLOT_INDEX) as *mut HeapPtr<BytecodeFunction>) }
    }

    /// The number of arguments in this stack frame, not including the receiver.
    #[inline]
    pub fn argc(&self) -> usize {
        unsafe { *self.fp.add(ARGC_SLOT_INDEX) }
    }

    /// The receiver value for this stack frame.
    #[inline]
    pub fn receiver(&self) -> Value {
        unsafe { *self.fp.add(RECEIVER_SLOT_INDEX).cast::<Value>() }
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
            let num_registers = self.bytecode_function().num_registers() as usize;
            let last_register_ptr = self.fp.offset(-1 - num_registers as isize) as *mut Value;
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

const FUNCTION_SLOT_INDEX: usize = 3;

const ARGC_SLOT_INDEX: usize = 4;

pub const RECEIVER_SLOT_INDEX: usize = 5;

pub const FIRST_ARGUMENT_SLOT_INDEX: usize = 6;
