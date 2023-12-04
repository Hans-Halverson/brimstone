/// Stack frame layout:
///
/// Stack grows downwards towards lower addresses.
///
///     +------------------+
///     |       argn       |  (last arg)
///     +------------------+
///     |       ...        |
///     +------------------+
///     |       arg1       |  (first arg)
///     +------------------+                     ^                ^
///     |       arg0       |  (receiver)         | caller's frame |
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
struct _StackFrame;

/// Generic value stored in a stack slot.
pub type StackSlotValue = usize;

/// Total size of the stack, 4MB.
const STACK_SIZE: usize = 4 * 1024 * 1024;

/// Total number of stack slots that fit in the stack.
pub const NUM_STACK_SLOTS: usize = STACK_SIZE / std::mem::size_of::<StackSlotValue>();

pub const RETURN_ADDRESS_SLOT_INDEX: usize = 1;

pub const RETURN_VALUE_ADDRESS_INDEX: usize = 2;

pub const FUNCTION_SLOT_INDEX: usize = 3;

pub const ARGC_SLOT_INDEX: usize = 4;

pub const FIRST_ARGUMENT_SLOT_INDEX: usize = 5;
