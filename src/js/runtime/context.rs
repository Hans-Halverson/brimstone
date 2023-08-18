use crate::js::{common::wtf_8::Wtf8String, parser::ast, runtime::gc::HandleScope};

use super::{
    array_properties::{ArrayProperties, DenseArrayProperties},
    builtin_function::ClosureEnvironment,
    builtin_names::{BuiltinNames, BuiltinSymbols},
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
pub struct Context {
    execution_context_stack: Vec<HeapPtr<ExecutionContext>>,
    pub heap: Heap,
    global_symbol_registry: HeapPtr<GlobalSymbolRegistry>,
    pub names: BuiltinNames,
    pub well_known_symbols: BuiltinSymbols,
    pub base_descriptors: BaseDescriptors,

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

    // All ASTs produced by eval and function constructors in this context. Saved here so that they
    // are not freed while the context is still running, as they may be needed e.g. due to functions
    // returned from an eval.
    pub eval_asts: Vec<ast::Program>,
    pub function_constructor_asts: Vec<ast::P<ast::Function>>,
}

type GlobalSymbolRegistry = BsHashMap<HeapPtr<FlatString>, HeapPtr<SymbolValue>>;

impl Context {
    pub fn new() -> Box<Context> {
        let mut heap = Heap::new();

        // Context does not yet exist, so handle scope must directly reference heap
        let handle_scope = HandleScope::enter_with_heap(&mut heap);

        // Initialize some fields, leave others uninitialized and initialize later
        let names = BuiltinNames::uninit();
        let well_known_symbols = BuiltinSymbols::uninit();

        let mut cx = Box::new(Context {
            execution_context_stack: vec![],
            heap,
            global_symbol_registry: HeapPtr::uninit(),
            names,
            well_known_symbols,
            base_descriptors: BaseDescriptors::uninit(),
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
            eval_asts: vec![],
            function_constructor_asts: vec![],
        });

        cx.heap.info().set_context(&mut cx);

        // Initialize all uninitialized fields
        cx.base_descriptors = BaseDescriptors::new(&mut cx);
        InternedStrings::init(&mut cx);

        cx.init_builtin_names();
        cx.init_builtin_symbols();

        cx.global_symbol_registry =
            GlobalSymbolRegistry::new_initial(&mut cx, ObjectKind::GlobalSymbolRegistryMap);

        cx.default_array_properties = DenseArrayProperties::new(&mut cx, 0).cast();
        cx.default_named_properties =
            NamedPropertiesMap::new(&mut cx, ObjectKind::ObjectNamedPropertiesMap, 0);
        cx.uninit_environment = DeclarativeEnvironment::uninit(&mut cx)
            .into_dyn_env()
            .to_heap();

        // Clean up handles from Context creation
        handle_scope.exit();

        cx
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
        FlatString::from_wtf8(self, str.as_bytes())
    }

    #[inline]
    pub fn alloc_wtf8_string_ptr(&mut self, str: &Wtf8String) -> HeapPtr<FlatString> {
        FlatString::from_wtf8(self, str.as_bytes())
    }

    #[inline]
    pub fn alloc_string(&mut self, str: &str) -> Handle<StringValue> {
        self.alloc_string_ptr(str).as_string().to_handle()
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

        self.names.visit_roots(visitor);
        self.well_known_symbols.visit_roots(visitor);
        self.base_descriptors.visit_roots(visitor);
        self.interned_strings.visit_roots(visitor);

        for closure_environment in self.closure_environments.iter_mut() {
            visitor.visit_pointer_opt(closure_environment);
        }

        visitor.visit_pointer(&mut self.default_named_properties);
        visitor.visit_pointer(&mut self.default_array_properties);
        self.uninit_environment.visit_pointers(visitor);

        for finalizer_callback in self.finalizer_callbacks.iter_mut() {
            visitor.visit_pointer(&mut finalizer_callback.cleanup_callback);
            visitor.visit_value(&mut finalizer_callback.held_value);
        }
    }
}

pub struct GlobalSymbolRegistryField;

impl BsHashMapField<HeapPtr<FlatString>, HeapPtr<SymbolValue>> for GlobalSymbolRegistryField {
    fn new(&self, cx: &mut Context, capacity: usize) -> HeapPtr<GlobalSymbolRegistry> {
        GlobalSymbolRegistry::new(cx, ObjectKind::GlobalSymbolRegistryMap, capacity)
    }

    fn get(&self, cx: &mut Context) -> HeapPtr<GlobalSymbolRegistry> {
        cx.global_symbol_registry
    }

    fn set(&mut self, cx: &mut Context, map: HeapPtr<GlobalSymbolRegistry>) {
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
