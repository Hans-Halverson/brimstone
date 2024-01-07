use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
    rc::Rc,
};

use crate::js::{
    common::{options::Options, wtf_8::Wtf8String},
    parser::{
        parser::{ParseFunctionResult, ParseProgramResult},
        source::Source,
    },
    runtime::gc::HandleScope,
};

use super::{
    array_properties::{ArrayProperties, DenseArrayProperties},
    builtin_function::ClosureEnvironment,
    builtin_names::{BuiltinNames, BuiltinSymbols},
    bytecode::{function::Closure, vm::VM},
    collections::{BsHashMap, BsHashMapField},
    environment::{
        declarative_environment::DeclarativeEnvironment, environment::HeapDynEnvironment,
    },
    execution_context::{ExecutionContext, ScriptOrModule},
    gc::{Heap, HeapVisitor},
    interned_strings::InternedStrings,
    intrinsics::{finalization_registry_object::FinalizerCallback, intrinsics::Intrinsic},
    object_descriptor::{BaseDescriptors, ObjectKind},
    object_value::{NamedPropertiesMap, ObjectValue},
    realm::Realm,
    string_value::{FlatString, StringValue},
    value::SymbolValue,
    Handle, HeapPtr, Value,
};

/// Top level context for the JS runtime. Contains the heap, execution contexts, etc.
/// Must never be moved, as there may be internal pointers held.
///
/// Includes properties from section 8.6 Agent.
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
    execution_context_stack: Vec<HeapPtr<ExecutionContext>>,
    pub heap: Heap,
    global_symbol_registry: HeapPtr<GlobalSymbolRegistry>,
    pub names: BuiltinNames,
    pub well_known_symbols: BuiltinSymbols,
    pub base_descriptors: BaseDescriptors,

    /// The virtual machine used to execute bytecode.
    pub vm: Option<Box<VM>>,

    // Canonical values
    undefined: Value,
    null: Value,
    empty: Value,
    true_: Value,
    false_: Value,

    // Canonical string values for strings that appear in the AST
    pub interned_strings: InternedStrings,

    // Stack of closure environments for all builtin functions currently being evaluated
    pub closure_environments: Vec<Option<HeapPtr<ClosureEnvironment>>>,

    // An empty named properties map to use as the initial value for named properties
    pub default_named_properties: HeapPtr<NamedPropertiesMap>,

    // An empty, dense array properties object to use as the initial value for array properties
    pub default_array_properties: HeapPtr<ArrayProperties>,

    // An empty environment to be used as an uninitialized value
    pub uninit_environment: HeapDynEnvironment,

    // All pending finalizer callbacks for garbage collected values.
    // TODO: Call finalizer callbacks
    finalizer_callbacks: Vec<FinalizerCallback>,

    /// Options passed to this program.
    pub options: Rc<Options>,

    // All ASTs produced by eval and function constructors in this context. Saved here so that they
    // are not freed while the context is still running, as they may be needed e.g. due to functions
    // returned from an eval.
    pub eval_asts: Vec<ParseProgramResult>,
    pub function_constructor_asts: Vec<(ParseFunctionResult, Rc<Source>)>,
}

type GlobalSymbolRegistry = BsHashMap<HeapPtr<FlatString>, HeapPtr<SymbolValue>>;

impl Context {
    /// Create a context. All allocations that occur during the init callback will be placed in
    /// the permanent heap.
    pub fn new<R>(options: Rc<Options>, mut init: impl FnMut(Context) -> R) -> (Context, R) {
        let cx_cell = Box::new(ContextCell {
            execution_context_stack: vec![],
            heap: Heap::new(),
            global_symbol_registry: HeapPtr::uninit(),
            names: BuiltinNames::uninit(),
            well_known_symbols: BuiltinSymbols::uninit(),
            base_descriptors: BaseDescriptors::uninit(),
            vm: None,
            undefined: Value::undefined(),
            null: Value::null(),
            empty: Value::empty(),
            true_: Value::bool(true),
            false_: Value::bool(false),
            interned_strings: InternedStrings::uninit(),
            closure_environments: vec![],
            default_named_properties: HeapPtr::uninit(),
            default_array_properties: HeapPtr::uninit(),
            uninit_environment: HeapDynEnvironment::uninit(),
            finalizer_callbacks: vec![],
            options,
            eval_asts: vec![],
            function_constructor_asts: vec![],
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
            cx.uninit_environment = DeclarativeEnvironment::uninit(cx).into_dyn_env().to_heap();
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

    pub fn execute_bytecode(
        &mut self,
        closure: Handle<Closure>,
        arguments: &[Handle<Value>],
    ) -> Handle<Value> {
        self.vm.as_deref_mut().unwrap().execute(closure, arguments)
    }

    pub fn alloc_uninit<T>(&self) -> HeapPtr<T> {
        Heap::alloc_uninit::<T>(*self)
    }

    pub fn alloc_uninit_with_size<T>(&self, size: usize) -> HeapPtr<T> {
        Heap::alloc_uninit_with_size::<T>(*self, size)
    }

    pub fn push_execution_context(&mut self, exec_ctx: Handle<ExecutionContext>) {
        self.execution_context_stack.push(exec_ctx.get_())
    }

    pub fn pop_execution_context(&mut self) {
        self.execution_context_stack.pop();
    }

    #[inline]
    pub fn current_execution_context_ptr(&self) -> HeapPtr<ExecutionContext> {
        *self.execution_context_stack.last().unwrap()
    }

    #[inline]
    pub fn current_execution_context(&self) -> Handle<ExecutionContext> {
        self.current_execution_context_ptr().to_handle()
    }

    pub fn current_realm_ptr(&self) -> HeapPtr<Realm> {
        self.current_execution_context_ptr().realm_ptr()
    }

    pub fn current_realm(&self) -> Handle<Realm> {
        self.current_execution_context_ptr().realm()
    }

    /// Return an intrinsic for the current realm.
    pub fn get_intrinsic_ptr(&self, intrinsic: Intrinsic) -> HeapPtr<ObjectValue> {
        self.current_execution_context_ptr()
            .get_intrinsic_ptr(intrinsic)
    }

    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> Handle<ObjectValue> {
        self.current_execution_context_ptr()
            .get_intrinsic(intrinsic)
    }

    /// 9.4.6 GetGlobalObject. Return the global object for the current realm.
    pub fn get_global_object(&self) -> Handle<ObjectValue> {
        self.current_execution_context_ptr().global_object()
    }

    // 9.4.1 GetActiveScriptOrModule
    pub fn get_active_script_or_module(&self) -> Option<ScriptOrModule> {
        self.execution_context_stack
            .iter()
            .rev()
            .find_map(|exec_ctx| exec_ctx.script_or_module())
    }

    pub fn global_symbol_registry(&self) -> HeapPtr<GlobalSymbolRegistry> {
        self.global_symbol_registry
    }

    pub fn global_symbol_registry_field(&mut self) -> GlobalSymbolRegistryField {
        GlobalSymbolRegistryField
    }

    pub fn push_closure_environment(&mut self, env: Option<HeapPtr<ClosureEnvironment>>) {
        self.closure_environments.push(env)
    }

    pub fn pop_closure_environment(&mut self) {
        self.closure_environments.pop();
    }

    pub fn get_closure_environment_ptr<T>(&self) -> HeapPtr<T> {
        let closure_environment = self.closure_environments.last().unwrap().unwrap();
        closure_environment.cast::<T>()
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
    pub fn alloc_string(&mut self, str: &str) -> Handle<StringValue> {
        self.alloc_string_ptr(str).as_string().to_handle()
    }

    #[inline]
    pub fn alloc_wtf8_string(&mut self, str: &Wtf8String) -> Handle<StringValue> {
        self.alloc_wtf8_string_ptr(str).as_string().to_handle()
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
        for execution_context in self.execution_context_stack.iter_mut() {
            visitor.visit_pointer(execution_context);
        }

        self.heap.visit_roots(visitor);

        visitor.visit_pointer(&mut self.global_symbol_registry);
        self.interned_strings.visit_roots(visitor);

        for closure_environment in self.closure_environments.iter_mut() {
            visitor.visit_pointer_opt(closure_environment);
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
        // - uninit_environment
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
    fn new(&self, cx: Context, capacity: usize) -> HeapPtr<GlobalSymbolRegistry> {
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
