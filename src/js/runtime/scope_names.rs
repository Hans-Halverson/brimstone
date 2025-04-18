use bitflags::bitflags;

use crate::{field_offset, runtime::object_descriptor::ObjectKind, set_uninit};

use super::{
    collections::InlineArray,
    gc::{HeapObject, HeapVisitor},
    object_descriptor::ObjectDescriptor,
    string_value::{FlatString, StringValue},
    Context, Handle, HeapPtr,
};

#[repr(C)]
pub struct ScopeNames {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Flags for this scope overall.
    flags: ScopeFlags,
    /// Inline array of names for the slots.
    names: InlineArray<HeapPtr<FlatString>>,
    /// Compressed flag data for each slot, with one byte per slot.
    _name_flags: [u64; 1],
}

bitflags! {
    #[derive(Clone, Copy)]
    pub struct ScopeFlags: u8 {
        /// Whether this scope is a "var scope", meaning sloppy eval will create vars in this scope.
        const IS_VAR_SCOPE = 1 << 0;
        /// Whether this is the scope that contains function params, separate from the function
        /// body because the function params contains expressions.
        const IS_FUNCTION_PARAMETERS_SCOPE = 1 << 1;
        /// Whether this scope is a class body scope.
        const IS_CLASS_SCOPE = 1 << 2;
    }
}

bitflags! {
    #[derive(Clone, Copy)]
    pub struct ScopeNameFlags: u8 {
        /// Whether this is an immutable binding
        const IS_IMMUTABLE = 1 << 0;
        /// Whether this is a lexically scoped binding.
        const IS_LEXICAL = 1 << 1;
        /// Whether this is a function parameter binding.
        const IS_FUNCTION_PARAMETER = 1 << 2;
        /// Whether this if a function expression name binding.
        const IS_FUNCTION_EXPRESSION_NAME = 1 << 3;
        /// Whether this is a private name binding.
        const IS_PRIVATE_NAME = 1 << 4;
        /// Whether this is a module binding.
        const IS_MODULE_BINDING = 1 << 5;
    }
}

impl ScopeNames {
    pub fn new(
        cx: Context,
        flags: ScopeFlags,
        names: &[Handle<FlatString>],
        name_flags: &[ScopeNameFlags],
    ) -> Handle<ScopeNames> {
        let size = Self::calculate_size_in_bytes(names.len());
        let mut scope_names = cx.alloc_uninit_with_size::<ScopeNames>(size);

        set_uninit!(scope_names.descriptor, cx.base_descriptors.get(ObjectKind::ScopeNames));
        set_uninit!(scope_names.flags, flags);

        // Copy names into inline names array
        scope_names.names.init_with_uninit(names.len());
        for (i, name) in names.iter().enumerate() {
            scope_names.names.set_unchecked(i, **name);
        }

        // Copy name flags into name flags section
        let name_flags_ptr = scope_names.get_name_flags_ptr() as *mut u8;
        unsafe {
            std::ptr::copy_nonoverlapping(
                name_flags.as_ptr().cast::<u8>(),
                name_flags_ptr,
                name_flags.len(),
            )
        };

        scope_names.to_handle()
    }

    const NAMES_OFFSET: usize = field_offset!(ScopeNames, names);

    fn calculate_size_in_bytes(num_slots: usize) -> usize {
        Self::name_flags_offset(num_slots) + Self::calculate_name_flags_size(num_slots)
    }

    fn name_flags_offset(num_slots: usize) -> usize {
        Self::NAMES_OFFSET + InlineArray::<StringValue>::calculate_size_in_bytes(num_slots)
    }

    pub fn calculate_name_flags_size(num_slots: usize) -> usize {
        // One byte per slot
        num_slots
    }

    fn get_name_flags_ptr(&self) -> *const u8 {
        let ptr = self as *const ScopeNames as *const u8;
        unsafe { ptr.add(Self::name_flags_offset(self.len())) }
    }

    pub fn len(&self) -> usize {
        self.names.len()
    }

    pub fn is_var_scope(&self) -> bool {
        self.flags.contains(ScopeFlags::IS_VAR_SCOPE)
    }

    pub fn is_function_parameters_scope(&self) -> bool {
        self.flags
            .contains(ScopeFlags::IS_FUNCTION_PARAMETERS_SCOPE)
    }

    pub fn is_class_scope(&self) -> bool {
        self.flags.contains(ScopeFlags::IS_CLASS_SCOPE)
    }

    pub fn name_ptrs(&self) -> &[HeapPtr<FlatString>] {
        self.names.as_slice()
    }

    pub fn get_slot_name(&self, index: usize) -> HeapPtr<FlatString> {
        self.names.as_slice()[index]
    }

    /// Look up a name, returning its index in the scope if present. The name must be an interned
    /// string.
    pub fn lookup_name(&self, name: HeapPtr<FlatString>) -> Option<usize> {
        self.names.as_slice().iter().position(|n| n.ptr_eq(&name))
    }

    fn get_name_flags(&self, index: usize) -> ScopeNameFlags {
        unsafe {
            *self
                .get_name_flags_ptr()
                .add(index)
                .cast::<ScopeNameFlags>()
        }
    }

    /// Return whether the binding at index is an immutable binding.
    pub fn is_immutable(&self, index: usize) -> bool {
        self.get_name_flags(index)
            .contains(ScopeNameFlags::IS_IMMUTABLE)
    }

    /// Return whether the binding at index is a lexical binding.
    pub fn is_lexical(&self, index: usize) -> bool {
        self.get_name_flags(index)
            .contains(ScopeNameFlags::IS_LEXICAL)
    }

    /// Return whether the binding at index is a function parameter binding.
    pub fn is_function_parameter(&self, index: usize) -> bool {
        self.get_name_flags(index)
            .contains(ScopeNameFlags::IS_FUNCTION_PARAMETER)
    }

    /// Return whether the binding at index is a function expression name binding.
    pub fn is_function_expression_name(&self, index: usize) -> bool {
        self.get_name_flags(index)
            .contains(ScopeNameFlags::IS_FUNCTION_EXPRESSION_NAME)
    }

    /// Return whether the binding at index is a private name binding.
    pub fn is_private_name(&self, index: usize) -> bool {
        self.get_name_flags(index)
            .contains(ScopeNameFlags::IS_PRIVATE_NAME)
    }

    /// Return whether the binding at index is a module binding.
    pub fn is_module_binding(&self, index: usize) -> bool {
        self.get_name_flags(index)
            .contains(ScopeNameFlags::IS_MODULE_BINDING)
    }
}

impl HeapObject for HeapPtr<ScopeNames> {
    fn byte_size(&self) -> usize {
        ScopeNames::calculate_size_in_bytes(self.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        for name in self.names.as_mut_slice() {
            visitor.visit_pointer(name);
        }
    }
}
