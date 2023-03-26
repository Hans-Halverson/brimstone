use wrap_ordinary_object::wrap_ordinary_object;

use crate::{extend_object, impl_gc_into, maybe};

use super::{
    completion::EvalResult,
    environment::private_environment::PrivateNameId,
    execution_context::{ExecutionContext, ScriptOrModule},
    function::{set_function_length, set_function_name},
    gc::{Gc, GcDeref},
    intrinsics::intrinsics::Intrinsic,
    object_value::{extract_object_vtable, Object, ObjectValue},
    ordinary_object::object_ordinary_init,
    property::{PrivateProperty, Property},
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    realm::Realm,
    string_value::StringValue,
    value::Value,
    Context,
};

// 10.3 Built-in Function Object
extend_object! {
    pub struct BuiltinFunction {
        realm: Gc<Realm>,
        script_or_module: Option<ScriptOrModule>,
        pub initial_name: Option<Gc<StringValue>>,
        builtin_func: BuiltinFunctionPtr,
        closure_environment: Option<Gc<ClosureEnvironment>>,
        has_constructor: bool,
    }
}

// Function pointer to a builtin function
pub type BuiltinFunctionPtr = fn(
    cx: &mut Context,
    this_value: Value,
    arguments: &[Value],
    new_target: Option<Gc<ObjectValue>>,
) -> EvalResult<Value>;

// Generic storage for variables captured by function if it is a closure. Must be cast to specific
// type for stored variables at each use.
pub struct ClosureEnvironment {}

impl GcDeref for BuiltinFunction {}

impl_gc_into!(BuiltinFunction, ObjectValue);

impl BuiltinFunction {
    const VTABLE: *const () = extract_object_vtable::<BuiltinFunction>();

    // 10.3.3 CreateBuiltinFunction
    pub fn create(
        cx: &mut Context,
        builtin_func: BuiltinFunctionPtr,
        length: i32,
        name: &PropertyKey,
        realm: Option<Gc<Realm>>,
        prototype: Option<Gc<ObjectValue>>,
        prefix: Option<&str>,
    ) -> Gc<BuiltinFunction> {
        let func = BuiltinFunction::create_without_properties(cx, builtin_func, realm, prototype);

        set_function_length(cx, func.into(), length);
        set_function_name(cx, func.into(), name, prefix);

        func
    }

    pub fn create_without_properties(
        cx: &mut Context,
        builtin_func: BuiltinFunctionPtr,
        realm: Option<Gc<Realm>>,
        prototype: Option<Gc<ObjectValue>>,
    ) -> Gc<BuiltinFunction> {
        let realm = realm.unwrap_or_else(|| cx.current_realm());
        let prototype =
            prototype.unwrap_or_else(|| realm.get_intrinsic(Intrinsic::FunctionPrototype));

        let mut object = cx.heap.alloc_uninit::<BuiltinFunction>();
        object._vtable = Self::VTABLE;

        object_ordinary_init(object.object_mut(), prototype);

        object.realm = realm;
        object.script_or_module = None;
        object.initial_name = None;
        object.builtin_func = builtin_func;
        object.closure_environment = None;
        object.has_constructor = false;
        object.script_or_module = None;

        object
    }

    pub fn set_is_constructor(&mut self) {
        self.has_constructor = true;
    }

    pub fn set_closure_environment<T>(&mut self, closure_environment: Gc<T>) {
        self.closure_environment =
            Some(Gc::from_ptr(closure_environment.as_ptr().cast::<ClosureEnvironment>()));
    }

    pub fn set_property(&mut self, key: &PropertyKey, value: Property) {
        self.object_mut().set_property(key, value);
    }

    pub fn intrinsic_frozen_property(&mut self, key: &PropertyKey, value: Value) {
        self.object_mut().intrinsic_frozen_property(key, value);
    }

    pub fn intrinsic_func(
        &mut self,
        cx: &mut Context,
        name: &PropertyKey,
        func: BuiltinFunctionPtr,
        length: i32,
        realm: Gc<Realm>,
    ) {
        self.object_mut()
            .intrinsic_func(cx, name, func, length, realm);
    }

    pub fn intrinsic_getter(
        &mut self,
        cx: &mut Context,
        name: &PropertyKey,
        func: BuiltinFunctionPtr,
        realm: Gc<Realm>,
    ) {
        self.object_mut().intrinsic_getter(cx, name, func, realm)
    }
}

#[wrap_ordinary_object]
impl Object for BuiltinFunction {
    // 10.3.1 [[Call]]
    fn call(
        &self,
        cx: &mut Context,
        this_argument: Value,
        arguments: &[Value],
    ) -> EvalResult<Value> {
        let current_execution_context = cx.current_execution_context();
        let callee_context = cx.heap.alloc(ExecutionContext {
            function: Some(self.into()),
            realm: self.realm,
            script_or_module: None,
            lexical_env: cx.uninit_environment,
            variable_env: cx.uninit_environment,
            private_env: None,
            is_strict_mode: current_execution_context.is_strict_mode,
        });

        cx.push_closure_environment(self.closure_environment);
        cx.push_execution_context(callee_context);

        let result = (self.builtin_func)(cx, this_argument, arguments, None);

        cx.pop_execution_context();
        cx.pop_closure_environment();

        result
    }

    // 10.3.2 [[Construct]]
    fn construct(
        &self,
        cx: &mut Context,
        arguments: &[Value],
        new_target: Gc<ObjectValue>,
    ) -> EvalResult<Gc<ObjectValue>> {
        let current_execution_context = cx.current_execution_context();
        let callee_context = cx.heap.alloc(ExecutionContext {
            function: Some(self.into()),
            realm: self.realm,
            script_or_module: None,
            lexical_env: cx.uninit_environment,
            variable_env: cx.uninit_environment,
            private_env: None,
            is_strict_mode: current_execution_context.is_strict_mode,
        });

        cx.push_closure_environment(self.closure_environment);
        cx.push_execution_context(callee_context);

        let result = (self.builtin_func)(cx, Value::undefined(), arguments, Some(new_target));

        cx.pop_execution_context();
        cx.pop_closure_environment();

        maybe!(result).as_object().into()
    }

    fn as_builtin_function_opt(&self) -> Option<Gc<BuiltinFunction>> {
        Some(self.into())
    }

    fn is_callable(&self) -> bool {
        true
    }

    fn is_constructor(&self) -> bool {
        self.has_constructor
    }

    fn get_realm(&self, _: &mut Context) -> EvalResult<Gc<Realm>> {
        self.realm.into()
    }
}

impl Into<Gc<BuiltinFunction>> for &BuiltinFunction {
    fn into(self) -> Gc<BuiltinFunction> {
        Gc::from_ptr(self as *const _ as *mut BuiltinFunction)
    }
}
