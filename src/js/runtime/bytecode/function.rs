use std::mem::size_of;

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object, field_offset,
    js::runtime::{
        abstract_operations::define_property_or_throw,
        builtin_function::BuiltinFunctionPtr,
        collections::InlineArray,
        debug_print::{DebugPrint, DebugPrintMode, DebugPrinter},
        function::{set_function_length, set_function_name},
        gc::{HeapObject, HeapVisitor},
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RustRuntimeFunctionId},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        object_value::{ObjectValue, VirtualObject},
        ordinary_object::{object_create, object_create_with_proto, ordinary_object_create},
        scope::Scope,
        string_value::StringValue,
        Context, EvalResult, Handle, HeapPtr, PropertyDescriptor, PropertyKey, Realm, Value,
    },
    must, set_uninit,
};

use super::{
    constant_table::ConstantTable, exception_handlers::ExceptionHandlers,
    instruction::debug_format_instructions,
};

// A closure is a pair of a function and its scope. Represents the instantiation of a function's
// bytecode "template" in a particular scope, and is the callable object.
extend_object! {
    pub struct Closure {
        function: HeapPtr<BytecodeFunction>,
        scope: HeapPtr<Scope>,
    }
}

impl Closure {
    pub fn new(
        cx: Context,
        function: Handle<BytecodeFunction>,
        scope: Handle<Scope>,
    ) -> Handle<Closure> {
        // TODO: Handle different function prototypes
        let mut object =
            object_create::<Closure>(cx, ObjectKind::Closure, Intrinsic::FunctionPrototype);

        set_uninit!(object.function, function.get_());
        set_uninit!(object.scope, scope.get_());

        let closure = object.to_handle();
        Self::init_common_properties(cx, closure, function);

        closure
    }

    pub fn new_in_realm(
        cx: Context,
        function: Handle<BytecodeFunction>,
        scope: Handle<Scope>,
        realm: Handle<Realm>,
    ) -> Handle<Closure> {
        let proto = realm.get_intrinsic(Intrinsic::FunctionPrototype);
        let mut object = object_create_with_proto::<Closure>(cx, ObjectKind::Closure, proto);

        set_uninit!(object.function, function.get_());
        set_uninit!(object.scope, scope.get_());

        let closure = object.to_handle();
        Self::init_common_properties(cx, closure, function);

        closure
    }

    pub fn new_builtin(
        cx: Context,
        function: Handle<BytecodeFunction>,
        scope: Handle<Scope>,
        prototype: Handle<ObjectValue>,
    ) -> Handle<Closure> {
        let mut object = object_create_with_proto::<Closure>(cx, ObjectKind::Closure, prototype);

        set_uninit!(object.function, function.get_());
        set_uninit!(object.scope, scope.get_());

        // Does not need to the `name` and `length` properties as these will be set by caller
        object.to_handle()
    }

    pub fn init_extra_fields(
        &mut self,
        function: HeapPtr<BytecodeFunction>,
        scope: HeapPtr<Scope>,
    ) {
        set_uninit!(self.function, function);
        set_uninit!(self.scope, scope);
    }

    #[inline]
    pub fn function_ptr(&self) -> HeapPtr<BytecodeFunction> {
        self.function
    }

    #[inline]
    pub fn scope(&self) -> Handle<Scope> {
        self.scope.to_handle()
    }

    #[inline]
    pub fn global_object(&self) -> Handle<ObjectValue> {
        self.function_ptr().realm_ptr().global_object()
    }

    #[inline]
    pub fn realm(&self) -> Handle<Realm> {
        self.function_ptr().realm()
    }

    /// Iniialize the common properties of all functions - `name`, `length`, and `prototype` if
    /// this is a constructor.
    fn init_common_properties(
        cx: Context,
        closure: Handle<Closure>,
        function: Handle<BytecodeFunction>,
    ) {
        set_function_length(cx, closure.into(), function.function_length());

        // Default to the empty string if a name was not provided
        let name = if let Some(name) = function.name {
            PropertyKey::string(cx, name.to_handle()).to_handle(cx)
        } else {
            cx.names.empty_string()
        };
        set_function_name(cx, closure.into(), name, None);

        // 10.2.5 MakeConstructor
        if function.is_constructor() {
            let prototype = ordinary_object_create(cx);

            let desc = PropertyDescriptor::data(closure.into(), true, false, true);
            must!(define_property_or_throw(cx, prototype, cx.names.constructor(), desc));

            let desc = PropertyDescriptor::data(prototype.into(), true, false, false);
            must!(define_property_or_throw(cx, closure.into(), cx.names.prototype(), desc));
        }
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<Closure> {
    fn is_callable(&self) -> bool {
        true
    }
}

impl HeapObject for HeapPtr<Closure> {
    fn byte_size(&self) -> usize {
        size_of::<Closure>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.function);
        visitor.visit_pointer(&mut self.scope);
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
    /// Exception handlers in this function.
    exception_handlers: Option<HeapPtr<ExceptionHandlers>>,
    /// The realm this function was defined in.
    realm: HeapPtr<Realm>,
    /// Number of local registers (and temporaries) needed by the function.
    num_registers: u32,
    /// Number of parameters to the function, not counting the rest parameter.
    num_parameters: u32,
    /// Value of the `length` property
    function_length: u32,
    /// Whether this function is in strict mode.
    is_strict: bool,
    /// Whether this function is a constructor
    is_constructor: bool,
    /// Index of the new.target register, if a new.target is needed.
    new_target_index: Option<u32>,
    /// Name of the function, used for debugging and the `name` property of non-runtime functions.
    name: Option<HeapPtr<StringValue>>,
    /// This function may be a stub function back into the Rust runtime. If this is set then this
    /// function has an empty bytecode array and default values for many other fields.
    rust_runtime_function_id: Option<RustRuntimeFunctionId>,
    /// Inlined bytecode array for the function.
    bytecode: InlineArray<u8>,
}

impl BytecodeFunction {
    pub fn new(
        cx: Context,
        bytecode: Vec<u8>,
        constant_table: Option<Handle<ConstantTable>>,
        exception_handlers: Option<Handle<ExceptionHandlers>>,
        realm: Handle<Realm>,
        num_registers: u32,
        num_parameters: u32,
        function_length: u32,
        is_strict: bool,
        is_constructor: bool,
        new_target_index: Option<u32>,
        name: Option<Handle<StringValue>>,
    ) -> Handle<BytecodeFunction> {
        let size = Self::calculate_size_in_bytes(bytecode.len());
        let mut object = cx.alloc_uninit_with_size::<BytecodeFunction>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::BytecodeFunction));
        set_uninit!(object.constant_table, constant_table.map(|c| c.get_()));
        set_uninit!(object.exception_handlers, exception_handlers.map(|h| h.get_()));
        set_uninit!(object.realm, realm.get_());
        set_uninit!(object.num_registers, num_registers);
        set_uninit!(object.num_parameters, num_parameters);
        set_uninit!(object.function_length, function_length);
        set_uninit!(object.is_strict, is_strict);
        set_uninit!(object.is_constructor, is_constructor);
        set_uninit!(object.new_target_index, new_target_index);
        set_uninit!(object.name, name.map(|n| n.get_()));
        set_uninit!(object.rust_runtime_function_id, None);
        object.bytecode.init_from_vec(bytecode);

        object.to_handle()
    }

    pub fn new_rust_runtime_function(
        cx: Context,
        builtin_func: BuiltinFunctionPtr,
        realm: Handle<Realm>,
        is_constructor: bool,
    ) -> Handle<BytecodeFunction> {
        let function_id = *cx.rust_runtime_functions.get_id(builtin_func).unwrap();

        let size = Self::calculate_size_in_bytes(0);
        let mut object = cx.alloc_uninit_with_size::<BytecodeFunction>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::BytecodeFunction));
        set_uninit!(object.constant_table, None);
        set_uninit!(object.exception_handlers, None);
        set_uninit!(object.realm, realm.get_());
        set_uninit!(object.num_registers, 0);
        set_uninit!(object.num_parameters, 0);
        set_uninit!(object.function_length, 0);
        set_uninit!(object.is_strict, true);
        set_uninit!(object.is_constructor, is_constructor);
        set_uninit!(object.new_target_index, None);
        set_uninit!(object.name, None);
        set_uninit!(object.rust_runtime_function_id, Some(function_id));
        object.bytecode.init_from_vec(vec![]);

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
    pub fn exception_handlers_ptr(&self) -> Option<HeapPtr<ExceptionHandlers>> {
        self.exception_handlers
    }

    #[inline]
    pub fn realm_ptr(&self) -> HeapPtr<Realm> {
        self.realm
    }

    #[inline]
    pub fn realm(&self) -> Handle<Realm> {
        self.realm.to_handle()
    }

    #[inline]
    pub fn num_parameters(&self) -> u32 {
        self.num_parameters
    }

    #[inline]
    pub fn function_length(&self) -> u32 {
        self.function_length
    }

    #[inline]
    pub fn num_registers(&self) -> u32 {
        self.num_registers
    }

    #[inline]
    pub fn is_strict(&self) -> bool {
        self.is_strict
    }

    #[inline]
    pub fn is_constructor(&self) -> bool {
        self.is_constructor
    }

    #[inline]
    pub fn new_target_index(&self) -> Option<u32> {
        self.new_target_index
    }

    #[inline]
    pub fn rust_runtime_function_id(&self) -> Option<RustRuntimeFunctionId> {
        self.rust_runtime_function_id
    }
}

impl DebugPrint for HeapPtr<BytecodeFunction> {
    /// Debug print this function only.
    fn debug_format(&self, printer: &mut DebugPrinter) {
        let name = if let Some(name) = self.name {
            name.to_string()
        } else {
            "<anonymous>".to_owned()
        };

        // Write the function name and stop if in short mode
        printer.write_heap_item_with_context(self.cast(), &name);

        if printer.is_short_mode() {
            return;
        }

        // Entire body is indented
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

        // Followed by the exception handlers if present
        if let Some(exception_handlers) = self.exception_handlers_ptr() {
            printer.write_indent();
            exception_handlers.debug_format(printer);
        }

        printer.dec_indent();
        printer.write_indent();
        printer.write("}\n");
    }
}

pub fn dump_bytecode_function(cx: Context, func: HeapPtr<BytecodeFunction>) {
    let bytecode_string = func.debug_print_recursive(false);

    if let Some(mut dump_buffer) = cx.options.dump_buffer() {
        dump_buffer.push_str(&bytecode_string);
        dump_buffer.push_str("\n");
    } else {
        println!("{}", func.debug_print_recursive(false));
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
        visitor.visit_pointer_opt(&mut self.exception_handlers);
        visitor.visit_pointer(&mut self.realm);
        visitor.visit_pointer_opt(&mut self.name);
    }
}
