use std::mem::size_of;

use crate::{
    extend_object, field_offset,
    js::runtime::{
        bytecode::instruction::debug_print_instructions,
        collections::InlineArray,
        gc::{HeapObject, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::{ObjectDescriptor, ObjectKind},
        ordinary_object::object_create,
        string_value::StringValue,
        Context, Handle, HeapPtr,
    },
    set_uninit,
};

use super::constant_table::ConstantTable;

// A closure is a pair of a function and it's scope. Represents the instantiation of a function's
// bytecode "template" in a particular scope, and is the callable object.
extend_object! {
    pub struct Closure {
        function: HeapPtr<BytecodeFunction>,
        // TODO: Add runtime scope
    }
}

impl Closure {
    pub fn new(cx: Context, function: Handle<BytecodeFunction>) -> Handle<Closure> {
        Self::new_ptr(cx, function).to_handle()
    }

    pub fn new_ptr(cx: Context, function: Handle<BytecodeFunction>) -> HeapPtr<Closure> {
        // TODO: Handle different function prototypes
        let mut object =
            object_create::<Closure>(cx, ObjectKind::Closure, Intrinsic::FunctionPrototype);

        set_uninit!(object.function, function.get_());

        object
    }

    pub fn function_ptr(&self) -> HeapPtr<BytecodeFunction> {
        self.function
    }
}

impl HeapObject for HeapPtr<Closure> {
    fn byte_size(&self) -> usize {
        size_of::<Closure>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.function);
    }
}

/// The bytecode representation of a function. This is the "template" containing all the statically
/// known information about a function. Cannot be called directly - must be first be combined with
/// a runtime scope to create a closure.
#[repr(C)]
pub struct BytecodeFunction {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Constants referenced by the function (or raw jump offsets)
    constant_table: Option<HeapPtr<ConstantTable>>,
    /// Number of local registers (and temporaries) needed by the function.
    num_registers: u32,
    /// Number of parameters to the function, not counting the rest parameter.
    num_parameters: u32,
    /// Name of the function, used for debugging.
    debug_name: Option<HeapPtr<StringValue>>,
    /// Inlined bytecode array for the function.
    bytecode: InlineArray<u8>,
}

impl BytecodeFunction {
    pub fn new(
        cx: Context,
        bytecode: Vec<u8>,
        constant_table: Option<Handle<ConstantTable>>,
        num_registers: u32,
        num_parameters: u32,
        debug_name: Option<Handle<StringValue>>,
    ) -> Handle<BytecodeFunction> {
        let size = Self::calculate_size_in_bytes(bytecode.len());
        let mut object = cx.alloc_uninit_with_size::<BytecodeFunction>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::BytecodeFunction));
        set_uninit!(object.constant_table, constant_table.map(|c| c.get_()));
        set_uninit!(object.num_registers, num_registers);
        set_uninit!(object.num_parameters, num_parameters);
        set_uninit!(object.debug_name, debug_name.map(|n| n.get_()));
        object.bytecode.init_from_vec(bytecode);

        object.to_handle()
    }

    const BYTECODE_BYTE_OFFSET: usize = field_offset!(BytecodeFunction, bytecode);

    fn calculate_size_in_bytes(bytecode_len: usize) -> usize {
        Self::BYTECODE_BYTE_OFFSET + InlineArray::<u8>::calculate_size_in_bytes(bytecode_len)
    }

    pub fn bytecode(&self) -> &[u8] {
        self.bytecode.as_slice()
    }

    pub fn constant_table(&self) -> Option<HeapPtr<ConstantTable>> {
        self.constant_table
    }

    pub fn num_parameters(&self) -> u32 {
        self.num_parameters
    }

    pub fn num_registers(&self) -> u32 {
        self.num_registers
    }
}

impl Handle<BytecodeFunction> {
    pub fn debug_print(&self) -> String {
        let name = if let Some(name) = self.debug_name {
            name.to_string()
        } else {
            "<anonymous>".to_string()
        };

        format!("Bytecode for {}:\n{}", name, debug_print_instructions(self.bytecode()))
    }
}

impl HeapObject for HeapPtr<BytecodeFunction> {
    fn byte_size(&self) -> usize {
        BytecodeFunction::calculate_size_in_bytes(self.bytecode.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        visitor.visit_pointer_opt(&mut self.constant_table);
        visitor.visit_pointer_opt(&mut self.debug_name);
    }
}
