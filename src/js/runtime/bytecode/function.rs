use std::{mem::size_of, ops::Range};

use crate::{
    extend_object, field_offset, must,
    parser::loc::Pos,
    runtime::{
        abstract_operations::define_property_or_throw,
        collections::{array::ByteArray, InlineArray},
        debug_print::{DebugPrint, DebugPrintMode, DebugPrinter},
        function::{set_function_length, set_function_name},
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
        intrinsics::{
            intrinsics::Intrinsic,
            rust_runtime::{RustRuntimeFunction, RustRuntimeFunctionId},
        },
        object_value::ObjectValue,
        ordinary_object::{object_create, object_create_with_proto},
        property::Property,
        scope::Scope,
        source_file::SourceFile,
        string_value::StringValue,
        Context, Handle, HeapPtr, PropertyDescriptor, PropertyKey, Realm,
    },
    set_uninit,
};

use super::{
    constant_table::ConstantTable, exception_handlers::ExceptionHandlers,
    instruction::debug_format_instructions, source_map::BytecodeSourceMap,
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
        let mut object =
            object_create::<Closure>(cx, HeapItemKind::Closure, Intrinsic::FunctionPrototype);

        set_uninit!(object.function, *function);
        set_uninit!(object.scope, *scope);

        let closure = object.to_handle();
        Self::init_common_properties(cx, closure, function, cx.current_realm());

        closure
    }

    pub fn new_with_proto(
        cx: Context,
        function: Handle<BytecodeFunction>,
        scope: Handle<Scope>,
        prototype: Handle<ObjectValue>,
    ) -> Handle<Closure> {
        let mut object = object_create_with_proto::<Closure>(cx, HeapItemKind::Closure, prototype);

        set_uninit!(object.function, *function);
        set_uninit!(object.scope, *scope);

        let closure = object.to_handle();
        Self::init_common_properties(cx, closure, function, cx.current_realm());

        closure
    }

    pub fn new_in_realm(
        cx: Context,
        function: Handle<BytecodeFunction>,
        scope: Handle<Scope>,
        realm: Handle<Realm>,
    ) -> Handle<Closure> {
        let proto = realm.get_intrinsic(Intrinsic::FunctionPrototype);
        let mut object = object_create_with_proto::<Closure>(cx, HeapItemKind::Closure, proto);

        set_uninit!(object.function, *function);
        set_uninit!(object.scope, *scope);

        let closure = object.to_handle();
        Self::init_common_properties(cx, closure, function, realm);

        closure
    }

    pub fn new_builtin(
        cx: Context,
        function: Handle<BytecodeFunction>,
        scope: Handle<Scope>,
        prototype: Handle<ObjectValue>,
    ) -> Handle<Closure> {
        let mut object = object_create_with_proto::<Closure>(cx, HeapItemKind::Closure, prototype);

        set_uninit!(object.function, *function);
        set_uninit!(object.scope, *scope);

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
    pub fn function(&self) -> Handle<BytecodeFunction> {
        self.function.to_handle()
    }

    #[inline]
    pub fn scope_ptr(&self) -> HeapPtr<Scope> {
        self.scope
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
        realm: Handle<Realm>,
    ) {
        set_function_length(cx, closure.into(), function.function_length());

        // Default to the empty string if a name was not provided
        let name = if let Some(name) = function.name {
            PropertyKey::string(cx, name.to_handle()).to_handle(cx)
        } else {
            cx.names.empty_string()
        };
        set_function_name(cx, closure.into(), name, None);

        // MakeConstructor (https://tc39.es/ecma262/#sec-makeconstructor)
        if function.is_constructor() {
            let proto = realm.get_intrinsic(Intrinsic::ObjectPrototype);
            let prototype =
                object_create_with_proto::<ObjectValue>(cx, HeapItemKind::OrdinaryObject, proto)
                    .to_handle();

            let desc = PropertyDescriptor::data(closure.into(), true, false, true);
            must!(define_property_or_throw(cx, prototype, cx.names.constructor(), desc));

            let desc = PropertyDescriptor::data(prototype.into(), true, false, false);
            must!(define_property_or_throw(cx, closure.into(), cx.names.prototype(), desc));
        }
    }
}

impl Handle<Closure> {
    /// Set the function name for this closure if the function name was not set when the closure
    /// was first created.
    ///
    /// Performs a raw set of the property, overwriting the previous value even though it was not
    /// writable. This will preserve the order of the properties, as the function name was initially
    /// added but defaulted to the empty string.
    pub fn set_lazy_function_name(&mut self, cx: Context, name: Handle<StringValue>) {
        let property = Property::data(name.into(), false, false, true);
        self.as_object().set_property(cx, cx.names.name(), property);
    }
}

impl HeapItem for HeapPtr<Closure> {
    fn byte_size(&self) -> usize {
        size_of::<Closure>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut self.function);
        visitor.visit_pointer(&mut self.scope);
    }
}

/// The bytecode representation of a function. This is the "template" containing all the statically
/// known information about a function. Cannot be called directly - must be first be combined with
/// a runtime scope to create a closure.
#[repr(C)]
pub struct BytecodeFunction {
    descriptor: HeapPtr<HeapItemDescriptor>,
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
    /// Whether this function is a constructor.
    is_constructor: bool,
    /// Whether this function is a class constructor.
    is_class_constructor: bool,
    /// Whether this function is a base class constructor. If this function is a constructor but
    /// not a base constructor then it must be a derived constructor.
    is_base_constructor: bool,
    /// Whether this function is async
    is_async: bool,
    /// Index of the new.target register, if a new.target is needed.
    new_target_index: Option<u32>,
    /// Index of the generator register, if this is a generator function.
    generator_index: Option<u32>,
    /// Name of the function, used for debugging and the `name` property of non-runtime functions.
    name: Option<HeapPtr<StringValue>>,
    /// Source file this function was defined in. None if there is no source file, like for builtins
    source_file: Option<HeapPtr<SourceFile>>,
    /// The source map that maps from bytecode offsets to source code positions. None if there is
    /// no source file, like for builtins.
    ///
    /// `source_map` is None iff `source_file` is None
    source_map: Option<HeapPtr<ByteArray>>,
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
        is_class_constructor: bool,
        is_base_constructor: bool,
        is_async: bool,
        new_target_index: Option<u32>,
        generator_index: Option<u32>,
        name: Option<Handle<StringValue>>,
        source_file: Handle<SourceFile>,
        source_map: Handle<ByteArray>,
    ) -> Handle<BytecodeFunction> {
        let size = Self::calculate_size_in_bytes(bytecode.len());
        let mut object = cx.alloc_uninit_with_size::<BytecodeFunction>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(HeapItemKind::BytecodeFunction));
        set_uninit!(object.constant_table, constant_table.map(|c| *c));
        set_uninit!(object.exception_handlers, exception_handlers.map(|h| *h));
        set_uninit!(object.realm, *realm);
        set_uninit!(object.num_registers, num_registers);
        set_uninit!(object.num_parameters, num_parameters);
        set_uninit!(object.function_length, function_length);
        set_uninit!(object.is_strict, is_strict);
        set_uninit!(object.is_constructor, is_constructor);
        set_uninit!(object.is_class_constructor, is_class_constructor);
        set_uninit!(object.is_base_constructor, is_base_constructor);
        set_uninit!(object.is_async, is_async);
        set_uninit!(object.new_target_index, new_target_index);
        set_uninit!(object.generator_index, generator_index);
        set_uninit!(object.name, name.map(|n| *n));
        set_uninit!(object.source_file, Some(*source_file));
        set_uninit!(object.source_map, Some(*source_map));
        set_uninit!(object.rust_runtime_function_id, None);
        object.bytecode.init_from_slice(&bytecode);

        object.to_handle()
    }

    pub fn new_rust_runtime_function(
        cx: Context,
        builtin_func: RustRuntimeFunction,
        realm: Handle<Realm>,
        is_constructor: bool,
        name: Option<Handle<StringValue>>,
    ) -> Handle<BytecodeFunction> {
        let function_id = cx.rust_runtime_functions.get_id(builtin_func).unwrap();

        let size = Self::calculate_size_in_bytes(0);
        let mut object = cx.alloc_uninit_with_size::<BytecodeFunction>(size);

        let mut num_registers = 0;
        let mut new_target_index = None;

        // Native constructors store the new target in the first register
        if is_constructor {
            num_registers = 1;
            new_target_index = Some(0);
        }

        set_uninit!(object.descriptor, cx.base_descriptors.get(HeapItemKind::BytecodeFunction));
        set_uninit!(object.constant_table, None);
        set_uninit!(object.exception_handlers, None);
        set_uninit!(object.realm, *realm);
        set_uninit!(object.num_registers, num_registers);
        set_uninit!(object.num_parameters, 0);
        set_uninit!(object.function_length, 0);
        set_uninit!(object.is_strict, true);
        set_uninit!(object.is_constructor, is_constructor);
        set_uninit!(object.is_class_constructor, false);
        set_uninit!(object.is_base_constructor, true);
        set_uninit!(object.is_async, false);
        set_uninit!(object.new_target_index, new_target_index);
        set_uninit!(object.generator_index, None);
        set_uninit!(object.name, name.map(|n| *n));
        set_uninit!(object.source_file, None);
        set_uninit!(object.source_map, None);
        set_uninit!(object.rust_runtime_function_id, Some(function_id));
        object.bytecode.init_from_slice(&[]);

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
    pub fn is_class_constructor(&self) -> bool {
        self.is_class_constructor
    }

    #[inline]
    pub fn is_base_constructor(&self) -> bool {
        self.is_base_constructor
    }

    #[inline]
    pub fn is_async(&self) -> bool {
        self.is_async
    }

    #[inline]
    pub fn new_target_index(&self) -> Option<u32> {
        self.new_target_index
    }

    #[inline]
    pub fn name(&self) -> Option<Handle<StringValue>> {
        self.name.map(|n| n.to_handle())
    }

    #[inline]
    pub fn source_file_ptr(&self) -> Option<HeapPtr<SourceFile>> {
        self.source_file
    }

    #[inline]
    pub fn source_map_ptr(&self) -> Option<HeapPtr<ByteArray>> {
        self.source_map
    }

    #[inline]
    pub fn source_range(&self) -> Option<Range<Pos>> {
        self.source_map.map(BytecodeSourceMap::get_function_range)
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
    let mut printer = DebugPrinter::new(DebugPrintMode::Verbose);
    printer.set_ignore_raw_bytes(/* ignore_raw_bytes */ true);

    func.debug_format(&mut printer);
    let bytecode_string = printer.finish();

    cx.print_or_add_to_dump_buffer(&bytecode_string);
}

impl HeapItem for HeapPtr<BytecodeFunction> {
    fn byte_size(&self) -> usize {
        BytecodeFunction::calculate_size_in_bytes(self.bytecode.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        visitor.visit_pointer_opt(&mut self.constant_table);
        visitor.visit_pointer_opt(&mut self.exception_handlers);
        visitor.visit_pointer(&mut self.realm);
        visitor.visit_pointer_opt(&mut self.name);
        visitor.visit_pointer_opt(&mut self.source_file);
        visitor.visit_pointer_opt(&mut self.source_map);
    }
}
