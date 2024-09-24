use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    rc::Rc,
};

use crate::js::{
    common::{options::Options, wtf_8::Wtf8String},
    runtime::gc::HandleScope,
};

use super::{
    array_properties::{ArrayProperties, DenseArrayProperties},
    builtin_names::{BuiltinNames, BuiltinSymbols},
    bytecode::{generator::BytecodeScript, vm::VM},
    collections::{BsHashMap, BsHashMapField},
    error::{panic_if_rejects, print_error_and_exit_if_rejects},
    gc::{Heap, HeapVisitor},
    interned_strings::InternedStrings,
    intrinsics::{
        finalization_registry_object::FinalizerCallback, intrinsics::Intrinsic,
        rust_runtime::RustRuntimeFunctionRegistry,
    },
    module::{
        execute::{execute_module, ExecuteOnReject},
        source_text_module::SourceTextModule,
    },
    object_descriptor::{BaseDescriptors, ObjectKind},
    object_value::{NamedPropertiesMap, ObjectValue},
    realm::Realm,
    string_value::FlatString,
    tasks::TaskQueue,
    value::SymbolValue,
    Handle, HeapPtr, Value,
};

/// Top level context for the JS runtime. Contains the heap, execution contexts, etc.
/// Must never be moved, as there may be internal pointers held.
///
/// Includes properties from section Agent (https://tc39.es/ecma262/#sec-agents)
///
/// Contexts are always represented by a pointer to the Context itself. A mutable reference to a
/// Context can be obtained from any reference to the heap. To avoid breaking Rust's mutable
/// aliasing rules, we must pass around a Context pointer that allows deref access to its individual
/// fields instead of passing around a `&mut Context`. This allows us to safely interweave Context
/// mutations from different Context pointers.
///
/// Note that Contexts are not automatically dropped. Contexts must be manually droppd with
/// Context::drop or Context::execute_then_drop.
#[derive(Copy, Clone)]
pub struct Context {
    ptr: NonNull<ContextCell>,
}

pub struct ContextCell {
    pub heap: Heap,
    global_symbol_registry: HeapPtr<GlobalSymbolRegistry>,
    pub names: BuiltinNames,
    pub well_known_symbols: BuiltinSymbols,
    pub base_descriptors: BaseDescriptors,
    pub rust_runtime_functions: RustRuntimeFunctionRegistry,

    /// The virtual machine used to execute bytecode.
    pub vm: Option<Box<VM>>,

    /// The task queue of all pending tasks.
    task_queue: TaskQueue,

    // Canonical values
    undefined: Value,
    null: Value,
    empty: Value,
    true_: Value,
    false_: Value,

    /// Canonical string values for strings that appear in the AST
    pub interned_strings: InternedStrings,

    /// Cache modules by their canonical absolute path
    pub modules: HashMap<String, HeapPtr<SourceTextModule>>,

    /// An empty named properties map to use as the initial value for named properties
    pub default_named_properties: HeapPtr<NamedPropertiesMap>,

    /// An empty, dense array properties object to use as the initial value for array properties
    pub default_array_properties: HeapPtr<ArrayProperties>,

    /// All pending finalizer callbacks for garbage collected values.
    /// TODO: Call finalizer callbacks
    finalizer_callbacks: Vec<FinalizerCallback>,

    /// Options passed to this program.
    pub options: Rc<Options>,
}

type GlobalSymbolRegistry = BsHashMap<HeapPtr<FlatString>, HeapPtr<SymbolValue>>;

impl Context {
    /// Create a context. All allocations that occur during the init callback will be placed in
    /// the permanent heap.
    pub fn new<R>(options: Rc<Options>, mut init: impl FnMut(Context) -> R) -> (Context, R) {
        let cx_cell = Box::new(ContextCell {
            heap: Heap::new(),
            global_symbol_registry: HeapPtr::uninit(),
            names: BuiltinNames::uninit(),
            well_known_symbols: BuiltinSymbols::uninit(),
            base_descriptors: BaseDescriptors::uninit(),
            rust_runtime_functions: RustRuntimeFunctionRegistry::new(),
            vm: None,
            task_queue: TaskQueue::new(),
            undefined: Value::undefined(),
            null: Value::null(),
            empty: Value::empty(),
            true_: Value::bool(true),
            false_: Value::bool(false),
            interned_strings: InternedStrings::uninit(),
            modules: HashMap::new(),
            default_named_properties: HeapPtr::uninit(),
            default_array_properties: HeapPtr::uninit(),
            finalizer_callbacks: vec![],
            options,
        });

        let mut cx = unsafe { Context::from_ptr(NonNull::new_unchecked(Box::leak(cx_cell))) };

        cx.heap.info().set_context(cx);
        cx.vm = Some(Box::new(VM::new(cx)));

        HandleScope::new(cx, |mut cx| {
            // Initialize all uninitialized fields
            cx.base_descriptors = BaseDescriptors::new(cx);
            InternedStrings::init(cx);

            cx.init_builtin_names();
            cx.init_builtin_symbols();

            cx.global_symbol_registry =
                GlobalSymbolRegistry::new_initial(cx, ObjectKind::GlobalSymbolRegistryMap);

            cx.default_array_properties = DenseArrayProperties::new(cx, 0).cast();
            cx.default_named_properties =
                NamedPropertiesMap::new(cx, ObjectKind::ObjectNamedPropertiesMap, 0);
        });

        // Execute the init callback, and turn its allocations into the permanent heap
        let result = init(cx);
        cx.heap.mark_current_semispace_as_permanent();

        (cx, result)
    }

    pub fn from_ptr(ptr: NonNull<ContextCell>) -> Context {
        Context { ptr }
    }

    /// Execute a function then drop this Context.
    pub fn execute_then_drop<R>(self, f: impl FnOnce(Context) -> R) -> R {
        let result = f(self);
        self.drop();
        result
    }

    pub fn drop(self) {
        unsafe { drop(Box::from_raw(self.ptr.as_ptr())) }
    }

    pub fn vm(&mut self) -> &mut VM {
        self.vm.as_mut().unwrap()
    }

    pub fn task_queue(&mut self) -> &mut TaskQueue {
        &mut self.task_queue
    }

    /// Execute a program, running until the task queue is empty.
    pub fn run_script(&mut self, bytecode_script: BytecodeScript) -> Result<(), Handle<Value>> {
        self.vm().execute_script(bytecode_script)?;
        self.run_all_tasks().into_rust_result()?;

        Ok(())
    }

    /// Execute a module, loading and executing all dependencies. Run until the task queue is empty.
    ///
    /// Must provide the action to take if the promise for the module execution rejects.
    pub fn run_module(
        &mut self,
        module: Handle<SourceTextModule>,
        on_reject: ExecuteOnReject,
    ) -> Result<(), Handle<Value>> {
        // Loading, linking, and evaluation should all have a current realm set as some objects
        // needing a realm will be created.
        let realm = module.program_function_ptr().realm_ptr();
        self.vm().push_initial_realm_stack_frame(realm);

        let promise = execute_module(*self, module);

        // Set up action taken if the module execution rejects.
        match on_reject {
            ExecuteOnReject::PrintAndExit => print_error_and_exit_if_rejects(*self, promise),
            ExecuteOnReject::Panic => panic_if_rejects(*self, promise),
        }

        self.run_all_tasks().into_rust_result()?;

        self.vm().pop_initial_realm_stack_frame();

        Ok(())
    }

    pub fn alloc_uninit<T>(&self) -> HeapPtr<T> {
        Heap::alloc_uninit::<T>(*self)
    }

    pub fn alloc_uninit_with_size<T>(&self, size: usize) -> HeapPtr<T> {
        Heap::alloc_uninit_with_size::<T>(*self, size)
    }

    #[inline]
    pub fn current_realm_ptr(&self) -> HeapPtr<Realm> {
        self.vm
            .as_ref()
            .unwrap()
            .closure()
            .function_ptr()
            .realm_ptr()
    }

    #[inline]
    pub fn current_realm(&self) -> Handle<Realm> {
        self.current_realm_ptr().to_handle()
    }

    /// Return an intrinsic for the current realm.
    #[inline]
    pub fn get_intrinsic_ptr(&self, intrinsic: Intrinsic) -> HeapPtr<ObjectValue> {
        self.current_realm().get_intrinsic_ptr(intrinsic)
    }

    #[inline]
    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> Handle<ObjectValue> {
        self.current_realm().get_intrinsic(intrinsic)
    }

    pub fn current_function(&mut self) -> Handle<ObjectValue> {
        self.vm().closure().to_handle().into()
    }

    pub fn global_symbol_registry(&self) -> HeapPtr<GlobalSymbolRegistry> {
        self.global_symbol_registry
    }

    pub fn global_symbol_registry_field(&mut self) -> GlobalSymbolRegistryField {
        GlobalSymbolRegistryField
    }

    pub fn add_finalizer_callbacks(&mut self, finalizer_callbacks: Vec<FinalizerCallback>) {
        self.finalizer_callbacks.extend(finalizer_callbacks);
    }

    #[inline]
    pub fn alloc_string_ptr(&mut self, str: &str) -> HeapPtr<FlatString> {
        FlatString::from_wtf8(*self, str.as_bytes())
    }

    #[inline]
    pub fn alloc_wtf8_string_ptr(&mut self, str: &Wtf8String) -> HeapPtr<FlatString> {
        FlatString::from_wtf8(*self, str.as_bytes())
    }

    #[inline]
    pub fn alloc_string(&mut self, str: &str) -> Handle<FlatString> {
        self.alloc_string_ptr(str).to_handle()
    }

    #[inline]
    pub fn alloc_wtf8_string(&mut self, str: &Wtf8String) -> Handle<FlatString> {
        self.alloc_wtf8_string_ptr(str).to_handle()
    }

    #[inline]
    pub fn undefined(&self) -> Handle<Value> {
        Handle::<Value>::from_fixed_non_heap_ptr(&self.undefined)
    }

    #[inline]
    pub fn null(&self) -> Handle<Value> {
        Handle::<Value>::from_fixed_non_heap_ptr(&self.null)
    }

    #[inline]
    pub fn empty(&self) -> Handle<Value> {
        Handle::<Value>::from_fixed_non_heap_ptr(&self.empty)
    }

    #[inline]
    pub fn bool(&self, value: bool) -> Handle<Value> {
        if value {
            Handle::<Value>::from_fixed_non_heap_ptr(&self.true_)
        } else {
            Handle::<Value>::from_fixed_non_heap_ptr(&self.false_)
        }
    }

    pub fn visit_roots(&mut self, visitor: &mut impl HeapVisitor) {
        self.heap.visit_roots(visitor);

        visitor.visit_pointer(&mut self.global_symbol_registry);
        self.task_queue.visit_roots(visitor);
        self.interned_strings.visit_roots(visitor);

        for module in self.modules.values_mut() {
            visitor.visit_pointer(module);
        }

        for finalizer_callback in self.finalizer_callbacks.iter_mut() {
            visitor.visit_pointer(&mut finalizer_callback.cleanup_callback);
            visitor.visit_value(&mut finalizer_callback.held_value);
        }

        if let Some(vm) = &mut self.vm {
            vm.visit_roots(visitor);
        }

        // The following fields must be in the permanent semispace so they do not need to be visited
        //
        // - All builtin names
        // - All builtin symbols
        // - All builtin descriptors
        // - default_named_properties
        // - default_array_properties
    }

    #[cfg(feature = "gc_stress_test")]
    pub fn enable_gc_stress_test(&mut self) {
        self.heap.gc_stress_test = true;
    }
}

impl Deref for Context {
    type Target = ContextCell;

    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl DerefMut for Context {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }
}

pub struct GlobalSymbolRegistryField;

impl BsHashMapField<HeapPtr<FlatString>, HeapPtr<SymbolValue>> for GlobalSymbolRegistryField {
    fn new_map(&self, cx: Context, capacity: usize) -> HeapPtr<GlobalSymbolRegistry> {
        GlobalSymbolRegistry::new(cx, ObjectKind::GlobalSymbolRegistryMap, capacity)
    }

    fn get(&self, cx: Context) -> HeapPtr<GlobalSymbolRegistry> {
        cx.global_symbol_registry
    }

    fn set(&mut self, mut cx: Context, map: HeapPtr<GlobalSymbolRegistry>) {
        cx.global_symbol_registry = map;
    }
}

impl GlobalSymbolRegistryField {
    pub fn byte_size(map: &HeapPtr<GlobalSymbolRegistry>) -> usize {
        GlobalSymbolRegistry::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(map: &mut HeapPtr<GlobalSymbolRegistry>, visitor: &mut impl HeapVisitor) {
        map.visit_pointers(visitor);

        for (key, value) in map.iter_mut_gc_unsafe() {
            visitor.visit_pointer(key);
            visitor.visit_pointer(value);
        }
    }
}
