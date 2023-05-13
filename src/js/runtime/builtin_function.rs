use wrap_ordinary_object::wrap_ordinary_object;

use crate::{extend_object, maybe, set_uninit};

use super::{
    completion::EvalResult,
    execution_context::ExecutionContext,
    function::{set_function_length, set_function_name},
    gc::{HandleScope, HeapPtr, IsHeapObject},
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::ObjectKind,
    object_value::{ObjectValue, VirtualObject},
    ordinary_object::object_create_with_proto,
    property::Property,
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    realm::Realm,
    string_value::StringValue,
    Context, Handle, Value,
};

// 10.3 Built-in Function Object
extend_object! {
    pub struct BuiltinFunction {
        realm: HeapPtr<Realm>,
        initial_name: Option<HeapPtr<StringValue>>,
        builtin_func: BuiltinFunctionPtr,
        closure_environment: Option<HeapPtr<ClosureEnvironment>>,
        has_constructor: bool,
    }
}

// Function pointer to a builtin function
pub type BuiltinFunctionPtr = fn(
    cx: &mut Context,
    this_value: Handle<Value>,
    arguments: &[Handle<Value>],
    new_target: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>>;

// Generic storage for variables captured by function if it is a closure. Must be cast to specific
// type for stored variables at each use.
pub struct ClosureEnvironment {}

impl BuiltinFunction {
    // 10.3.3 CreateBuiltinFunction
    pub fn create(
        cx: &mut Context,
        builtin_func: BuiltinFunctionPtr,
        length: i32,
        name: Handle<PropertyKey>,
        realm: Option<Handle<Realm>>,
        prototype: Option<Handle<ObjectValue>>,
        prefix: Option<&str>,
    ) -> Handle<BuiltinFunction> {
        let func = BuiltinFunction::create_without_properties(cx, builtin_func, realm, prototype);

        set_function_length(cx, func.into(), length);
        set_function_name(cx, func.into(), name, prefix);

        func
    }

    pub fn create_without_properties(
        cx: &mut Context,
        builtin_func: BuiltinFunctionPtr,
        realm: Option<Handle<Realm>>,
        prototype: Option<Handle<ObjectValue>>,
    ) -> Handle<BuiltinFunction> {
        let realm = realm.unwrap_or_else(|| cx.current_realm());
        let prototype =
            prototype.unwrap_or_else(|| realm.get_intrinsic(Intrinsic::FunctionPrototype));

        let mut object =
            object_create_with_proto::<BuiltinFunction>(cx, ObjectKind::BuiltinFunction, prototype);

        set_uninit!(object.realm, realm.get_());
        set_uninit!(object.initial_name, None);
        set_uninit!(object.builtin_func, builtin_func);
        set_uninit!(object.closure_environment, None);
        set_uninit!(object.has_constructor, false);

        object.to_handle()
    }

    fn realm(&self) -> Handle<Realm> {
        self.realm.to_handle()
    }

    pub fn set_is_constructor(&mut self) {
        self.has_constructor = true;
    }

    pub fn set_closure_environment<T: IsHeapObject>(&mut self, closure_environment: Handle<T>) {
        self.closure_environment = Some(closure_environment.get_().cast::<ClosureEnvironment>());
    }

    pub fn set_initial_name(&mut self, initial_name: Option<Handle<StringValue>>) {
        self.initial_name = initial_name.map(|s| s.get_());
    }
}

impl Handle<BuiltinFunction> {
    pub fn set_property(&mut self, cx: &mut Context, key: Handle<PropertyKey>, value: Property) {
        self.object().set_property(cx, key, value);
    }

    pub fn intrinsic_frozen_property(
        &mut self,
        cx: &mut Context,
        key: Handle<PropertyKey>,
        value: Handle<Value>,
    ) {
        self.object().intrinsic_frozen_property(cx, key, value);
    }

    pub fn intrinsic_func(
        &mut self,
        cx: &mut Context,
        name: Handle<PropertyKey>,
        func: BuiltinFunctionPtr,
        length: i32,
        realm: Handle<Realm>,
    ) {
        self.object().intrinsic_func(cx, name, func, length, realm);
    }

    pub fn intrinsic_getter(
        &mut self,
        cx: &mut Context,
        name: Handle<PropertyKey>,
        func: BuiltinFunctionPtr,
        realm: Handle<Realm>,
    ) {
        self.object().intrinsic_getter(cx, name, func, realm)
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<BuiltinFunction> {
    // 10.3.1 [[Call]]
    fn call(
        &self,
        cx: &mut Context,
        this_argument: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        HandleScope::new(cx, |cx| {
            let is_strict_mode = cx.current_execution_context_ptr().is_strict_mode();
            let callee_context = ExecutionContext::new(
                cx,
                /* function */ Some(self.object()),
                self.realm(),
                /* script_or_module */ None,
                /* lexical_env */ cx.uninit_environment,
                /* variable_env */ cx.uninit_environment,
                /* private_env */ None,
                is_strict_mode,
            );

            cx.push_closure_environment(self.closure_environment);
            cx.push_execution_context(callee_context);

            let result = (self.builtin_func)(cx, this_argument, arguments, None);

            cx.pop_execution_context();
            cx.pop_closure_environment();

            result
        })
    }

    // 10.3.2 [[Construct]]
    fn construct(
        &self,
        cx: &mut Context,
        arguments: &[Handle<Value>],
        new_target: Handle<ObjectValue>,
    ) -> EvalResult<Handle<ObjectValue>> {
        HandleScope::new(cx, |cx| {
            let is_strict_mode = cx.current_execution_context_ptr().is_strict_mode();
            let callee_context = ExecutionContext::new(
                cx,
                /* function */ Some(self.object()),
                self.realm(),
                /* script_or_module */ None,
                /* lexical_env */ cx.uninit_environment,
                /* variable_env */ cx.uninit_environment,
                /* private_env */ None,
                is_strict_mode,
            );

            cx.push_closure_environment(self.closure_environment);
            cx.push_execution_context(callee_context);

            let result = (self.builtin_func)(cx, cx.undefined(), arguments, Some(new_target));

            cx.pop_execution_context();
            cx.pop_closure_environment();

            maybe!(result).as_object().into()
        })
    }

    fn is_callable(&self) -> bool {
        true
    }

    fn is_constructor(&self) -> bool {
        self.has_constructor
    }

    fn get_realm(&self, _: &mut Context) -> EvalResult<HeapPtr<Realm>> {
        self.realm.into()
    }
}

// TODO: For some reason this suppresses seemingly unrelated unused function warnings. Keeping for
// now until this is figured out and the functions are cleaned up.
impl Into<HeapPtr<()>> for &() {
    fn into(self) -> HeapPtr<()> {
        unimplemented!("")
    }
}
