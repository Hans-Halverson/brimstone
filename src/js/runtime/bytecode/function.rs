use std::mem::size_of;

use crate::{
    extend_object, field_offset,
    js::runtime::{
        bytecode::instruction::debug_format_instructions,
        collections::InlineArray,
        debug_print::{DebugPrint, DebugPrintMode, DebugPrinter},
        gc::{HeapObject, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::{ObjectDescriptor, ObjectKind},
        object_value::ObjectValue,
        ordinary_object::{object_create, object_create_with_proto},
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

    pub fn new_builtin(
        cx: Context,
        function: Handle<BytecodeFunction>,
        prototype: Handle<ObjectValue>,
    ) -> Handle<Closure> {
        let mut object = object_create_with_proto::<Closure>(cx, ObjectKind::Closure, prototype);

        set_uninit!(object.function, function.get_());

        object.to_handle()
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
    /// Whether this function is in strict mode.
    is_strict: bool,
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
        is_strict: bool,
        debug_name: Option<Handle<StringValue>>,
    ) -> Handle<BytecodeFunction> {
        let size = Self::calculate_size_in_bytes(bytecode.len());
        let mut object = cx.alloc_uninit_with_size::<BytecodeFunction>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::BytecodeFunction));
        set_uninit!(object.constant_table, constant_table.map(|c| c.get_()));
        set_uninit!(object.num_registers, num_registers);
        set_uninit!(object.num_parameters, num_parameters);
        set_uninit!(object.is_strict, is_strict);
        set_uninit!(object.debug_name, debug_name.map(|n| n.get_()));
        object.bytecode.init_from_vec(bytecode);

        object.to_handle()
    }

    const BYTECODE_BYTE_OFFSET: usize = field_offset!(BytecodeFunction, bytecode);

    fn calculate_size_in_bytes(bytecode_len: usize) -> usize {
        Self::BYTECODE_BYTE_OFFSET + InlineArray::<u8>::calculate_size_in_bytes(bytecode_len)
    }

    #[inline]
    pub fn bytecode(&self) -> &[u8] {
        self.bytecode.as_slice()
    }

    #[inline]
    pub fn constant_table_ptr(&self) -> Option<HeapPtr<ConstantTable>> {
        self.constant_table
    }

    #[inline]
    pub fn num_parameters(&self) -> u32 {
        self.num_parameters
    }

    #[inline]
    pub fn num_registers(&self) -> u32 {
        self.num_registers
    }

    #[inline]
    pub fn is_strict(&self) -> bool {
        self.is_strict
    }
}

impl DebugPrint for HeapPtr<BytecodeFunction> {
    /// Debug print this function only.
    fn debug_format(&self, printer: &mut DebugPrinter) {
        let name = if let Some(name) = self.debug_name {
            name.to_string()
        } else {
            "<anonymous>".to_owned()
        };

        if printer.is_short_mode() {
            printer.write_heap_item_with_context(self.cast(), &name);
            return;
        }

        // Write the function name and indent body
        printer.write_heap_item_with_context(self.cast(), &name);
        printer.write(" {\n");
        printer.inc_indent();

        // Followed by some function metadata
        printer.write_indent();
        printer.write(&format!(
            "Parameters: {}, Registers: {}\n",
            self.num_parameters(),
            self.num_registers()
        ));

        // Followed by instructions which are further indented
        printer.inc_indent();
        debug_format_instructions(self.bytecode(), printer);
        printer.dec_indent();

        // Followed by the constant table if present
        if let Some(constant_table) = self.constant_table_ptr() {
            printer.write_indent();
            constant_table.debug_format(printer);
        }

        printer.dec_indent();
        printer.write("}\n");
    }
}

impl HeapPtr<BytecodeFunction> {
    /// Debug print this function and all its child functions.
    pub fn debug_print_recursive(&self, ignore_raw_bytes: bool) -> String {
        let mut printer = DebugPrinter::new(DebugPrintMode::Verbose);
        printer.set_ignore_raw_bytes(ignore_raw_bytes);

        let mut stack = vec![*self];

        while let Some(function) = stack.pop() {
            if !printer.is_empty() {
                printer.write("\n");
            }

            function.debug_format(&mut printer);

            // Constant table contains all child functions in declaration order. Push them in
            // reverse order so they are popped in depth-first declaration order.
            if let Some(constant_table) = function.constant_table_ptr() {
                for (i, constant) in constant_table.as_slice().iter().enumerate().rev() {
                    if constant_table.is_value(i) && constant.is_pointer() {
                        let heap_item = constant.as_pointer();
                        if heap_item.descriptor().kind() == ObjectKind::BytecodeFunction {
                            stack.push(heap_item.cast::<BytecodeFunction>());
                        }
                    }
                }
            }
        }

        printer.finish()
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
