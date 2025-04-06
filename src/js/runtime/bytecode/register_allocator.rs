use crate::runtime::bytecode::{generator::EmitError, operand::Register, width::ExtraWide};

use super::generator::{EmitResult, GenRegister};

/// Allocates a stack of temporary registers during bytecode generation.
pub struct TemporaryRegisterAllocator {
    /// Total number of local registers used for this function. Temporary registers start after the
    /// last local register.
    num_local_registers: u32,
    /// Total number of registers (including local registers) currently allocated. This is the
    /// next temporary register to allocate.
    num_allocated: u32,
    /// Maximum number of temporary registers allocated at once.
    max_allocated: u32,
}

impl TemporaryRegisterAllocator {
    pub fn new(num_local_registers: u32) -> Self {
        Self {
            num_allocated: num_local_registers,
            num_local_registers,
            max_allocated: num_local_registers,
        }
    }

    /// Allocate a new temporary register.
    pub fn allocate(&mut self) -> EmitResult<GenRegister> {
        let register_index = self.num_allocated;
        if register_index >= Register::<ExtraWide>::MAX_LOCAL_INDEX as u32 {
            return Err(EmitError::TooManyRegisters);
        }

        self.num_allocated += 1;
        self.max_allocated = self.max_allocated.max(self.num_allocated);

        Ok(GenRegister::local(register_index as usize))
    }

    /// Release a temporary register. This must be the most recently allocated temporary register.
    ///
    /// If the given register is a local or argument register this function is a noop.
    pub fn release(&mut self, register: GenRegister) {
        if !self.is_temporary_register(register) {
            return;
        }

        // Check that released register is the most recently allocated temporary register
        // before releasing.
        debug_assert!(register.local_index() as u32 == self.num_allocated - 1);
        self.num_allocated -= 1;
    }

    /// Return whether the given register is a temporary register (vs an argument, non-temporary
    /// local, etc).
    #[inline]
    pub fn is_temporary_register(&self, register: GenRegister) -> bool {
        register.is_local() && register.local_index() as u32 >= self.num_local_registers
    }

    /// Return the maximum number of register allocated at once (including both local and temporary
    /// registers).
    pub fn max_allocated(&self) -> u32 {
        self.max_allocated
    }

    /// Return whether there are no temporary registers allocated.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.num_allocated == self.num_local_registers
    }

    /// Return the total number of registers allocated, including both locals and temporaries.
    #[inline]
    pub fn num_allocated(&self) -> u32 {
        self.num_allocated
    }
}
