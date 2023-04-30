use std::collections::HashMap;

use crate::js::parser::ast;

use super::{
    array_properties::{ArrayProperties, DenseArrayProperties},
    builtin_function::ClosureEnvironment,
    builtin_names::{BuiltinNames, BuiltinSymbols},
    environment::{declarative_environment::DeclarativeEnvironment, environment::DynEnvironment},
    execution_context::{ExecutionContext, ScriptOrModule},
    gc::{Gc, Heap},
    interned_strings::InternedStrings,
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::BaseDescriptors,
    object_value::ObjectValue,
    realm::Realm,
    string_value::StringValue,
    value::SymbolValue,
};

/// Top level context for the JS runtime. Contains the heap, execution contexts, etc.
///
/// Includes properties from section 8.6 Agent.
pub struct Context {
    execution_context_stack: Vec<Gc<ExecutionContext>>,
    pub heap: Heap,
    pub global_symbol_registry: HashMap<Gc<StringValue>, Gc<SymbolValue>>,
    pub names: BuiltinNames,
    pub well_known_symbols: BuiltinSymbols,
    pub base_descriptors: BaseDescriptors,

    // Canonical string values for strings that appear in the AST
    pub interned_strings: InternedStrings,

    // Stack of closure environments for all builtin functions currently being evaluated
    pub closure_environments: Vec<Option<Gc<ClosureEnvironment>>>,

    // An empty, dense array properties object to use as the initial value for array properties
    pub default_array_properties: Gc<ArrayProperties>,

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
            DeclarativeEnvironment::uninit(&mut heap, &base_descriptors).into_dyn();

        let mut cx = Context {
            execution_context_stack: vec![],
            heap,
            global_symbol_registry: HashMap::new(),
            names,
            well_known_symbols,
            base_descriptors,
            interned_strings: InternedStrings::new(),
            closure_environments: vec![],
            default_array_properties: Gc::uninit(),
            uninit_environment,
            eval_asts: vec![],
            function_constructor_asts: vec![],
        };

        cx.init_builtin_names();
        cx.init_builtin_symbols();
        cx.default_array_properties = DenseArrayProperties::new(&mut cx, 0).cast();

        cx
    }

    pub fn push_execution_context(&mut self, exec_ctx: Gc<ExecutionContext>) {
        self.execution_context_stack.push(exec_ctx)
    }

    pub fn pop_execution_context(&mut self) {
        self.execution_context_stack.pop();
    }

    pub fn current_execution_context(&mut self) -> Gc<ExecutionContext> {
        *self.execution_context_stack.last().unwrap()
    }

    pub fn current_realm(&mut self) -> Gc<Realm> {
        self.current_execution_context().realm()
    }

    /// Return an intrinsic for the current realm.
    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> Gc<ObjectValue> {
        let current_execution_context = *self.execution_context_stack.last().unwrap();
        current_execution_context.get_intrinsic(intrinsic)
    }

    /// 9.4.6 GetGlobalObject. Return the global object for the current realm.
    pub fn get_global_object(&self) -> Gc<ObjectValue> {
        let current_execution_context = *self.execution_context_stack.last().unwrap();
        current_execution_context.global_object()
    }

    // 9.4.1 GetActiveScriptOrModule
    pub fn get_active_script_or_module(&self) -> Option<ScriptOrModule> {
        self.execution_context_stack
            .iter()
            .rev()
            .find_map(|exec_ctx| exec_ctx.script_or_module())
    }

    pub fn push_closure_environment(&mut self, env: Option<Gc<ClosureEnvironment>>) {
        self.closure_environments.push(env)
    }

    pub fn pop_closure_environment(&mut self) {
        self.closure_environments.pop();
    }

    pub fn get_closure_environment<T>(&self) -> Gc<T> {
        let closure_environment = self.closure_environments.last().unwrap().unwrap();
        closure_environment.cast::<T>()
    }

    pub fn alloc_string(&mut self, str: String) -> Gc<StringValue> {
        StringValue::from_utf8(self, str)
    }
}
