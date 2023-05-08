use std::collections::HashMap;

use crate::js::parser::ast;

use super::{
    array_properties::{ArrayProperties, DenseArrayProperties},
    builtin_function::ClosureEnvironment,
    builtin_names::{BuiltinNames, BuiltinSymbols},
    environment::{declarative_environment::DeclarativeEnvironment, environment::DynEnvironment},
    execution_context::{ExecutionContext, ScriptOrModule},
    gc::{HandleValue, Heap},
    interned_strings::InternedStrings,
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::BaseDescriptors,
    object_value::ObjectValue,
    realm::Realm,
    string_value::StringValue,
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
    pub global_symbol_registry: HashMap<HeapPtr<StringValue>, HeapPtr<SymbolValue>>,
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

    // An empty, dense array properties object to use as the initial value for array properties
    pub default_array_properties: HeapPtr<ArrayProperties>,

    // An empty environment to be used as an uninitialized value
    pub uninit_environment: DynEnvironment,

    // All ASTs produced by eval and function constructors in this context. Saved here so that they
    // are not freed while the context is still running, as they may be needed e.g. due to functions
    // returned from an eval.
    pub eval_asts: Vec<ast::Program>,
    pub function_constructor_asts: Vec<ast::P<ast::Function>>,
}

impl Context {
    pub fn new() -> Context {
        let mut heap = Heap::new();
        let names = BuiltinNames::uninit();
        let well_known_symbols = BuiltinSymbols::uninit();
        let base_descriptors = BaseDescriptors::new(&mut heap);
        let uninit_environment =
            DeclarativeEnvironment::uninit(&mut heap, &base_descriptors).into_dyn_env();

        let mut cx = Context {
            execution_context_stack: vec![],
            heap,
            global_symbol_registry: HashMap::new(),
            names,
            well_known_symbols,
            base_descriptors,
            undefined: Value::undefined(),
            null: Value::null(),
            empty: Value::empty(),
            true_: Value::bool(true),
            false_: Value::bool(false),
            interned_strings: InternedStrings::new(),
            closure_environments: vec![],
            default_array_properties: HeapPtr::uninit(),
            uninit_environment,
            eval_asts: vec![],
            function_constructor_asts: vec![],
        };

        cx.heap.info().set_context(&mut cx);

        cx.init_builtin_names();
        cx.init_builtin_symbols();
        cx.default_array_properties = DenseArrayProperties::new(&mut cx, 0).cast();

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
        Handle::from_heap(self.current_execution_context_ptr())
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

    #[inline]
    pub fn alloc_string_ptr(&mut self, str: String) -> HeapPtr<StringValue> {
        StringValue::from_utf8(self, str)
    }

    #[inline]
    pub fn alloc_string(&mut self, str: String) -> Handle<StringValue> {
        Handle::from_heap(self.alloc_string_ptr(str))
    }

    #[inline]
    pub fn undefined(&self) -> HandleValue {
        self.undefined
    }

    #[inline]
    pub fn null(&self) -> HandleValue {
        self.null
    }

    #[inline]
    pub fn empty(&self) -> HandleValue {
        self.empty
    }

    #[inline]
    pub fn bool(&self, value: bool) -> HandleValue {
        if value {
            self.true_
        } else {
            self.false_
        }
    }
}
