use wrap_ordinary_object::wrap_ordinary_object;

use crate::{extend_object, maybe};

use super::{
    completion::EvalResult,
    execution_context::{ExecutionContext, ScriptOrModule},
    function::{set_function_length, set_function_name},
    gc::Gc,
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::ObjectKind,
    object_value::{ObjectValue, VirtualObject},
    ordinary_object::object_ordinary_init,
    property::Property,
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

impl BuiltinFunction {
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
        object.descriptor = cx.base_descriptors.get(ObjectKind::BuiltinFunction);

        object_ordinary_init(cx, object.object(), prototype);

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
        self.closure_environment = Some(closure_environment.cast::<ClosureEnvironment>());
    }
}

impl Gc<BuiltinFunction> {
    pub fn set_property(&mut self, cx: &mut Context, key: &PropertyKey, value: Property) {
        self.object().set_property(cx, key, value);
    }

    pub fn intrinsic_frozen_property(&mut self, cx: &mut Context, key: &PropertyKey, value: Value) {
        self.object().intrinsic_frozen_property(cx, key, value);
    }

    pub fn intrinsic_func(
        &mut self,
        cx: &mut Context,
        name: &PropertyKey,
        func: BuiltinFunctionPtr,
        length: i32,
        realm: Gc<Realm>,
    ) {
        self.object().intrinsic_func(cx, name, func, length, realm);
    }

    pub fn intrinsic_getter(
        &mut self,
        cx: &mut Context,
        name: &PropertyKey,
        func: BuiltinFunctionPtr,
        realm: Gc<Realm>,
    ) {
        self.object().intrinsic_getter(cx, name, func, realm)
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Gc<BuiltinFunction> {
    // 10.3.1 [[Call]]
    fn call(
        &self,
        cx: &mut Context,
        this_argument: Value,
        arguments: &[Value],
    ) -> EvalResult<Value> {
        let current_execution_context = cx.current_execution_context();
        let callee_context = ExecutionContext::new(
            cx,
            /* function */ Some(self.object()),
            self.realm,
            /* script_or_module */ None,
            /* lexical_env */ cx.uninit_environment,
            /* variable_env */ cx.uninit_environment,
            /* private_env */ None,
            current_execution_context.is_strict_mode,
        );

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
        let callee_context = ExecutionContext::new(
            cx,
            /* function */ Some(self.object()),
            self.realm,
            /* script_or_module */ None,
            /* lexical_env */ cx.uninit_environment,
            /* variable_env */ cx.uninit_environment,
            /* private_env */ None,
            current_execution_context.is_strict_mode,
        );

        cx.push_closure_environment(self.closure_environment);
        cx.push_execution_context(callee_context);

        let result = (self.builtin_func)(cx, Value::undefined(), arguments, Some(new_target));

        cx.pop_execution_context();
        cx.pop_closure_environment();

        maybe!(result).as_object().into()
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

// TODO: For some reason this suppresses seemingly unrelated unused function warnings. Keeping for
// now until this is figured out and the functions are cleaned up.
impl Into<Gc<()>> for &() {
    fn into(self) -> Gc<()> {
        unimplemented!("")
    }
}
