use std::{
    hash::{Hash, Hasher},
    num::NonZeroUsize,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    rc::Rc,
};

use rand::{SeedableRng, rngs::StdRng};
use timezone_provider::tzif::CompiledTzdbProvider;

use crate::{
    common::{
        constants::NANOSECONDS_IN_ONE_MILLISECOND,
        filesystem::FileNameReserver,
        numeric::Numeric,
        options::Options,
        serialized_heap::SerializedHeap,
        time::{get_current_unix_time_millis, get_current_unix_time_nanos},
        wtf_8::{Wtf8Str, Wtf8String},
    },
    eval_err, handle_scope, impl_hash_map_instance, must_a,
    parser::{
        ParseContext, analyze::analyze, parse_module, parse_script, print_program, source::Source,
    },
    runtime::{
        EvalResult, Handle, HeapPtr, SymbolValue, Value,
        alloc_error::AllocResult,
        annex_b::init_annex_b_methods,
        array_properties::{ArrayProperties, DenseArrayProperties},
        builtin_names::{BuiltinNames, BuiltinSymbols},
        bytecode::{
            generator::{BytecodeProgramGenerator, BytecodeScript},
            vm::VM,
        },
        collections::{HashMapInstance, hash_map::BsHashMapField, index_map::IndexMapInstance},
        error::BsResult,
        gc::{GarbageCollector, Heap, HeapItem, HeapRootsDeserializer, HeapVisitor},
        interned_strings::InternedStrings,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RustRuntimeFunctionRegistry},
        module::{
            execute::execute_module,
            import_attributes::ImportAttributes,
            module::{DynModule, HeapDynModule},
            source_text_module::SourceTextModule,
        },
        object_value::{NamedPropertiesMap, ObjectValue},
        realm::Realm,
        shape_registry::ShapeRegistry,
        string_value::{FlatString, StringValue},
        tasks::TaskQueue,
    },
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
    pub names: BuiltinNames,
    pub symbols: BuiltinSymbols,
    pub shapes: ShapeRegistry,
    pub rust_runtime_functions: RustRuntimeFunctionRegistry,

    /// The virtual machine used to execute bytecode.
    pub vm: Option<Box<VM>>,

    /// The initial realm for this context. Either provided by the host environment or set up during
    /// context initialization.
    initial_realm: HeapPtr<Realm>,

    /// The task queue of all pending tasks.
    task_queue: TaskQueue,

    // Canonical values
    undefined: Value,
    null: Value,
    empty: Value,
    true_: Value,
    false_: Value,
    zero: Value,
    one: Value,
    negative_one: Value,
    nan: Value,

    /// Canonical string values for strings that appear in the AST
    pub interned_strings: InternedStrings,

    /// All symbols that have been registered under a specific key with `Symbol.for`.
    global_symbol_registry: HeapPtr<GlobalSymbolRegistryMap>,

    /// Cache modules by their canonical absolute path and import attributes
    pub modules: HeapPtr<ModuleCacheMap>,

    /// An empty named properties map to use as the initial value for named properties
    pub default_named_properties: HeapPtr<NamedPropertiesMap>,

    /// An empty, dense array properties object to use as the initial value for array properties
    pub default_array_properties: HeapPtr<ArrayProperties>,

    /// Options passed to this program.
    pub options: Rc<Options>,

    /// Reserves unique file paths for any debug files written during this session.
    pub debug_file_name_reserver: FileNameReserver,

    /// Set once module resolution has been completed.
    pub has_finished_module_resolution: bool,

    /// Counter for the [[AsyncEvaluation]] slot of SourceTextModule
    pub async_evaluation_counter: NonZeroUsize,

    /// Random number generator used within this context.
    pub rand: StdRng,

    /// If set, this is the unix time in nanoseconds.
    mocked_unix_time_nanos: Option<u128>,

    /// Time zone provider used for Temporal operations.
    temporal_provider: CompiledTzdbProvider,
}

impl Context {
    fn new(options: Rc<Options>) -> AllocResult<Context> {
        let cx_cell = Box::new(ContextCell {
            heap: Heap::new(options.min_heap_size),
            global_symbol_registry: HeapPtr::uninit(),
            names: BuiltinNames::uninit(),
            symbols: BuiltinSymbols::uninit(),
            shapes: ShapeRegistry::uninit_empty(),
            rust_runtime_functions: RustRuntimeFunctionRegistry::new(),
            vm: None,
            initial_realm: HeapPtr::uninit(),
            task_queue: TaskQueue::new(),
            undefined: Value::undefined(),
            null: Value::null(),
            empty: Value::empty(),
            true_: Value::bool(true),
            false_: Value::bool(false),
            zero: Value::smi(0),
            one: Value::smi(1),
            negative_one: Value::smi(-1),
            nan: Value::nan(),
            interned_strings: InternedStrings::uninit(),
            modules: HeapPtr::uninit(),
            default_named_properties: HeapPtr::uninit(),
            default_array_properties: HeapPtr::uninit(),
            options: options.clone(),
            debug_file_name_reserver: FileNameReserver::new(),
            has_finished_module_resolution: false,
            async_evaluation_counter: NonZeroUsize::MIN,
            // We want the initial heap generation to be deterministic so use seeded PRNG. After
            // initial heap has been set up switch to a PRNG seeded from a random source.
            rand: StdRng::from_seed([0; 32]),
            mocked_unix_time_nanos: None,
            temporal_provider: CompiledTzdbProvider::default(),
        });

        let mut cx = unsafe { Context::from_ptr(NonNull::new_unchecked(Box::leak(cx_cell))) };

        cx.heap.info().set_context(cx);
        cx.vm = Some(Box::new(VM::new(cx)));

        if let Some(serialized_heap) = options.serialized_heap {
            cx.init_heap_from_serialized(serialized_heap);
        } else {
            cx.init_heap_allocated_context_fields()?;
        }

        // Stop using deterministic PRNG
        cx.rand = StdRng::from_entropy();

        // Annex B methods may not be included in the serialized heap so they must be initialized
        // separately.
        if options.annex_b {
            init_annex_b_methods(cx, cx.initial_realm())?;
        }

        Ok(cx)
    }

    fn init_heap_allocated_context_fields(&mut self) -> AllocResult<()> {
        let mut cx = *self;

        handle_scope!(cx, {
            // Initialize all uninitialized fields
            cx.shapes = ShapeRegistry::new(cx)?;
            InternedStrings::init(cx)?;

            cx.init_builtin_names()?;
            cx.init_builtin_symbols()?;

            cx.global_symbol_registry = GlobalSymbolRegistryMap::new_initial(cx)?;
            cx.modules = ModuleCacheMap::new_initial(cx)?;

            cx.default_array_properties = DenseArrayProperties::new(cx, 0)?.cast();
            cx.default_named_properties = NamedPropertiesMap::new(cx, 0)?;

            cx.initial_realm = *Realm::new(cx)?;

            Ok(())
        })?;

        // Stop allocating into the permanent heap
        cx.heap.mark_current_semispace_as_permanent();

        Ok(())
    }

    /// Initialize this context from a serialized heap.
    ///
    /// Deserializes heap including fixing up heap roots for all heap allocated fields in the
    /// context.
    fn init_heap_from_serialized(&mut self, serialized: &SerializedHeap) {
        let mut cx = *self;

        // Initialize all uninitialized fields
        cx.shapes = ShapeRegistry::uninit();

        // Deserialize the heap roots
        HeapRootsDeserializer::deserialize(cx, serialized);
        self.heap.init_from_serialized(cx, serialized);
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

    #[inline]
    pub fn initial_realm_ptr(&self) -> HeapPtr<Realm> {
        self.initial_realm
    }

    #[inline]
    pub fn initial_realm(&self) -> Handle<Realm> {
        self.initial_realm_ptr().to_handle()
    }

    pub fn evaluate_script(&mut self, source: Rc<Source>) -> BsResult<()> {
        // Parse script and perform semantic analysis
        let pcx = ParseContext::new(source);
        let parse_result = parse_script(&pcx, self.options.clone())?;

        if self.options.print_ast {
            println!("{}", print_program(&parse_result));
        }

        if self.options.parse_stats {
            println!("{:#?}", pcx.stats());
        }

        let analyzed_result = analyze(parse_result)?;

        // Generate bytecode for the program
        let bytecode_script = BytecodeProgramGenerator::generate_from_parse_script_result(
            *self,
            &analyzed_result,
            self.initial_realm(),
        )?;

        // Execute in the bytecode interpreter
        self.run_script(bytecode_script)?;

        Ok(())
    }

    pub fn evaluate_module(&mut self, source: Rc<Source>) -> BsResult<()> {
        // Parse module and perform semantic analysis
        let pcx = ParseContext::new(source);
        let parse_result = parse_module(&pcx, self.options.clone())?;

        if self.options.print_ast {
            println!("{}", print_program(&parse_result));
        }

        if self.options.parse_stats {
            println!("{:#?}", pcx.stats());
        }

        let analyzed_result = analyze(parse_result)?;

        // Generate bytecode for the program
        let module = BytecodeProgramGenerator::generate_from_parse_module_result(
            *self,
            &analyzed_result,
            self.initial_realm(),
        )?;

        // Load modules and execute in the bytecode interpreter
        self.run_module(module)?;

        Ok(())
    }

    /// Execute a program, running until the task queue is empty.
    pub fn run_script(&mut self, bytecode_script: BytecodeScript) -> EvalResult<()> {
        self.with_initial_realm_stack_frame(self.initial_realm_ptr(), |mut cx| {
            cx.vm().execute_script(bytecode_script)
        })?;

        self.run_all_tasks()?;

        Ok(())
    }

    /// Execute a module, loading and executing all dependencies. Run until the task queue is empty.
    pub fn run_module(&mut self, module: Handle<SourceTextModule>) -> EvalResult<()> {
        // Loading, linking, and evaluation should all have a current realm set as some objects
        // needing a realm will be created.
        let promise = self
            .with_initial_realm_stack_frame(module.program_function_ptr().realm_ptr(), |cx| {
                Ok(execute_module(cx, module)?)
            })?;

        self.run_all_tasks()?;

        debug_assert!(!promise.is_pending());

        if let Some(value) = promise.rejected_value() {
            return eval_err!(value.to_handle(*self));
        }

        Ok(())
    }

    pub fn with_initial_realm_stack_frame<T>(
        &mut self,
        realm: HeapPtr<Realm>,
        f: impl FnOnce(Context) -> EvalResult<T>,
    ) -> EvalResult<T> {
        self.vm().debug_assert_stack_empty();

        let push_frame_result = self.vm().push_initial_realm_stack_frame(realm);
        assert!(push_frame_result.is_ok(), "Initial realm frame overflowed stack");

        // Always mark the top of the stack trace under the initial realm frame
        self.vm().mark_stack_trace_top();

        let result = f(*self);

        self.vm().pop_initial_realm_stack_frame();

        result
    }

    pub fn insert_module(
        &mut self,
        cache_key: ModuleCacheKey,
        module: DynModule,
    ) -> AllocResult<()> {
        ModuleCacheField
            .maybe_grow_for_insertion(*self)?
            .insert_without_growing(cache_key.into_heap(), module.to_heap());

        Ok(())
    }

    pub fn alloc_uninit<T>(&self) -> AllocResult<HeapPtr<T>> {
        Heap::alloc_uninit::<T>(*self)
    }

    pub fn alloc_uninit_with_size<T>(&self, size: usize) -> AllocResult<HeapPtr<T>> {
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

    pub fn current_new_target(&mut self) -> Option<Handle<ObjectValue>> {
        let new_target_index = self.vm().closure().function().new_target_index();
        if let Some(index) = new_target_index {
            let new_target = self.vm().get_register_at_index(index);
            if new_target.is_undefined() {
                return None;
            }

            debug_assert!(new_target.is_object());
            Some(new_target.as_object().to_handle())
        } else {
            None
        }
    }

    pub fn global_symbol_registry(&self) -> HeapPtr<GlobalSymbolRegistryMap> {
        self.global_symbol_registry
    }

    pub fn global_symbol_registry_field(&mut self) -> GlobalSymbolRegistryField {
        GlobalSymbolRegistryField
    }

    /// Returns the current unix time in milliseconds, which may be mocked.
    pub fn current_unix_time_millis(&self) -> u128 {
        if let Some(mocked_unix_time_nanos) = self.mocked_unix_time_nanos {
            mocked_unix_time_nanos / (NANOSECONDS_IN_ONE_MILLISECOND as u128)
        } else {
            get_current_unix_time_millis()
        }
    }

    /// Returns the current unix time in nanoseconds, which may be mocked.
    pub fn current_unix_time_nanos(&self) -> u128 {
        if let Some(mocked_unix_time_nanos) = self.mocked_unix_time_nanos {
            mocked_unix_time_nanos
        } else {
            get_current_unix_time_nanos()
        }
    }

    pub fn temporal_provider(&self) -> &CompiledTzdbProvider {
        &self.temporal_provider
    }

    pub fn print_or_add_to_dump_buffer(&self, str: &str) {
        if let Some(mut buffer) = self.options.dump_buffer() {
            if !buffer.is_empty() {
                buffer.push('\n');
            }

            buffer.push_str(str);
        } else {
            println!("{str}");
        }
    }

    #[inline]
    pub fn alloc_string_ptr(&mut self, str: &str) -> EvalResult<HeapPtr<FlatString>> {
        FlatString::from_wtf8(*self, str.as_bytes())
    }

    #[inline]
    pub fn alloc_static_string_ptr(
        &mut self,
        str: &'static str,
    ) -> AllocResult<HeapPtr<FlatString>> {
        // Assumes that all static strings are less than the maximum string length
        Ok(must_a!(self.alloc_string_ptr(str)))
    }

    #[inline]
    pub fn alloc_wtf8_string_ptr(&mut self, str: &Wtf8String) -> EvalResult<HeapPtr<FlatString>> {
        FlatString::from_wtf8(*self, str.as_bytes())
    }

    #[inline]
    pub fn alloc_wtf8_str_ptr(&mut self, str: &Wtf8Str) -> EvalResult<HeapPtr<FlatString>> {
        FlatString::from_wtf8(*self, str.as_bytes())
    }

    #[inline]
    pub fn alloc_static_wtf8_str_ptr(
        &mut self,
        str: &'static Wtf8Str,
    ) -> AllocResult<HeapPtr<FlatString>> {
        // Assumes that all static strings are less than the maximum string length
        Ok(must_a!(self.alloc_wtf8_str_ptr(str)))
    }

    #[inline]
    pub fn alloc_string(&mut self, str: &str) -> EvalResult<Handle<StringValue>> {
        Ok(self.alloc_string_ptr(str)?.as_string().to_handle())
    }

    #[inline]
    pub fn alloc_static_string(&mut self, str: &'static str) -> AllocResult<Handle<StringValue>> {
        Ok(self.alloc_static_string_ptr(str)?.as_string().to_handle())
    }

    #[inline]
    pub fn alloc_flat_string(&mut self, str: &str) -> EvalResult<Handle<FlatString>> {
        Ok(self.alloc_string_ptr(str)?.to_handle())
    }

    #[inline]
    pub fn alloc_wtf8_string(&mut self, str: &Wtf8String) -> EvalResult<Handle<FlatString>> {
        Ok(self.alloc_wtf8_string_ptr(str)?.to_handle())
    }

    #[inline]
    pub fn alloc_wtf8_str(&mut self, str: &Wtf8Str) -> EvalResult<Handle<FlatString>> {
        Ok(self.alloc_wtf8_str_ptr(str)?.to_handle())
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

    #[inline]
    pub fn zero(&self) -> Handle<Value> {
        Handle::<Value>::from_fixed_non_heap_ptr(&self.zero)
    }

    #[inline]
    pub fn one(&self) -> Handle<Value> {
        Handle::<Value>::from_fixed_non_heap_ptr(&self.one)
    }

    #[inline]
    pub fn negative_one(&self) -> Handle<Value> {
        Handle::<Value>::from_fixed_non_heap_ptr(&self.negative_one)
    }

    #[inline]
    pub fn nan(&self) -> Handle<Value> {
        Handle::<Value>::from_fixed_non_heap_ptr(&self.nan)
    }

    #[inline]
    pub fn smi<T: Numeric>(&self, value: T) -> Handle<Value> {
        Value::smi(value).to_handle(*self)
    }

    #[inline]
    pub fn number<T: Numeric>(&self, value: T) -> Handle<Value> {
        Value::number(value).to_handle(*self)
    }

    /// Visit all heap roots for a garbage collection. This optionally visits pointers that are
    /// guaranteed to be in the permanent semispace.
    pub fn visit_roots_for_gc(&mut self, gc: &mut GarbageCollector) {
        self.visit_common_roots(gc);
        self.visit_post_initialization_roots(gc);

        // Only need to visit permanent roots if growing the heap, otherwise permanent space is
        // guaranteed to not move and .
        if gc.is_resizing() {
            self.visit_permanent_roots(gc);
        }
    }

    /// Visit all heap roots that are needed for heap serialization. This includes all pointers to
    /// the permanent semispace.
    pub fn visit_roots_for_serialization(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_common_roots(visitor);
        self.visit_permanent_roots(visitor);

        // Intentionally do not need to visit_post_initialization_roots
    }

    /// Visit all heap roots that should always be visited.
    fn visit_common_roots(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.global_symbol_registry);
        self.interned_strings.visit_roots(visitor);
        visitor.visit_pointer(&mut self.modules);
    }

    /// Visit all heap roots that are guaranteed to point to the permanent semispace.
    fn visit_permanent_roots(&mut self, visitor: &mut impl HeapVisitor) {
        self.names.visit_roots(visitor);
        self.symbols.visit_roots(visitor);
        self.shapes.visit_roots(visitor);
        visitor.visit_pointer(&mut self.initial_realm);

        visitor.visit_pointer(&mut self.default_named_properties);
        visitor.visit_pointer(&mut self.default_array_properties);
    }

    /// Visit all heap roots that can only actually contain roots after the context has been
    /// initialized.
    fn visit_post_initialization_roots(&mut self, visitor: &mut impl HeapVisitor) {
        self.heap.visit_roots(visitor);
        self.task_queue.visit_roots(visitor);

        if let Some(vm) = &mut self.vm {
            vm.visit_roots(visitor);
        }
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

pub struct ContextBuilder {
    options: Option<Rc<Options>>,
    mocked_unix_time_nanos: Option<u128>,
}

impl ContextBuilder {
    pub fn new() -> Self {
        Self { options: None, mocked_unix_time_nanos: None }
    }

    pub fn build(self) -> AllocResult<Context> {
        // Create default options if none were provided
        let options = self.options.unwrap_or_else(|| Rc::new(Options::default()));

        // Create default realm if one was not provided
        let mut cx = Context::new(options)?;

        cx.mocked_unix_time_nanos = self.mocked_unix_time_nanos;

        Ok(cx)
    }

    pub fn set_options(mut self, options: Rc<Options>) -> Self {
        self.options = Some(options);
        self
    }

    pub fn mock_unix_time_nanos(mut self, time: u128) -> Self {
        self.mocked_unix_time_nanos = Some(time);
        self
    }
}

/// Modules are cached by their canonical path and import attributes.
#[derive(Clone, Eq, PartialEq)]
pub struct HeapModuleCacheKey {
    path: String,
    attributes: Option<HeapPtr<ImportAttributes>>,
}

pub struct ModuleCacheKey {
    path: String,
    attributes: Option<Handle<ImportAttributes>>,
}

impl ModuleCacheKey {
    pub fn new(path: String, attributes: Option<Handle<ImportAttributes>>) -> Self {
        Self { path, attributes }
    }

    pub fn into_heap(self) -> HeapModuleCacheKey {
        HeapModuleCacheKey::new(self.path, self.attributes.map(|attr| *attr))
    }
}

impl HeapModuleCacheKey {
    pub fn new(path: String, attributes: Option<HeapPtr<ImportAttributes>>) -> Self {
        Self { path, attributes }
    }
}

impl Hash for HeapModuleCacheKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Attributes are intentionally not included in hash
        self.path.hash(state);
    }
}

impl_hash_map_instance!(ModuleCacheMap, HeapModuleCacheKey, HeapDynModule);

pub struct ModuleCacheField;

impl BsHashMapField<ModuleCacheMap> for ModuleCacheField {
    fn get(&self, cx: Context) -> HeapPtr<ModuleCacheMap> {
        cx.modules
    }

    fn set_new(
        &mut self,
        mut cx: Context,
        capacity: usize,
    ) -> AllocResult<HeapPtr<ModuleCacheMap>> {
        let map = ModuleCacheMap::new(cx, capacity)?;
        cx.modules = map;
        Ok(map)
    }
}

impl HeapItem for ModuleCacheMap {
    fn byte_size(map: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(map.capacity())
    }

    fn visit_pointers(mut map: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        map.visit_map_pointers(visitor);

        for (cache_key, module) in map.iter_mut_gc_unsafe() {
            visitor.visit_pointer_opt(&mut cache_key.attributes);
            module.visit_pointers(visitor);
        }
    }
}

impl_hash_map_instance!(GlobalSymbolRegistryMap, HeapPtr<FlatString>, HeapPtr<SymbolValue>);

pub struct GlobalSymbolRegistryField;

impl BsHashMapField<GlobalSymbolRegistryMap> for GlobalSymbolRegistryField {
    fn get(&self, cx: Context) -> HeapPtr<GlobalSymbolRegistryMap> {
        cx.global_symbol_registry
    }

    fn set_new(
        &mut self,
        mut cx: Context,
        capacity: usize,
    ) -> AllocResult<HeapPtr<GlobalSymbolRegistryMap>> {
        let map = GlobalSymbolRegistryMap::new(cx, capacity)?;
        cx.global_symbol_registry = map;
        Ok(map)
    }
}

impl HeapItem for GlobalSymbolRegistryMap {
    fn byte_size(map: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(map.capacity())
    }

    fn visit_pointers(mut map: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        map.visit_map_pointers(visitor);

        for (key, value) in map.iter_mut_gc_unsafe() {
            visitor.visit_pointer(key);
            visitor.visit_pointer(value);
        }
    }
}
