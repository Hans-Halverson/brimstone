use wrap_ordinary_object::wrap_ordinary_object;

use crate::impl_gc_into;

use super::{
    completion::EvalResult,
    environment::private_environment::PrivateNameId,
    execution_context::{ExecutionContext, ScriptOrModule},
    function::{set_function_length, set_function_name},
    gc::{Gc, GcDeref},
    intrinsics::intrinsics::Intrinsic,
    object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
    ordinary_object::OrdinaryObject,
    property::{PrivateProperty, Property},
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    realm::Realm,
    value::{StringValue, Value},
    Context,
};

// 10.3 Built-in Function Object
#[repr(C)]
pub struct BuiltinFunction {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    realm: Gc<Realm>,
    script_or_module: Option<ScriptOrModule>,
    pub initial_name: Option<Gc<StringValue>>,
    builtin_func: BuiltinFunctionPtr,
    has_constructor: bool,
}

// Function pointer to a builtin function
pub type BuiltinFunctionPtr = fn(
    cx: &mut Context,
    this_value: Value,
    arguments: &[Value],
    new_target: Option<Gc<ObjectValue>>,
) -> EvalResult<Value>;

impl GcDeref for BuiltinFunction {}

impl_gc_into!(BuiltinFunction, ObjectValue);

const VTABLE: *const () = extract_object_vtable::<BuiltinFunction>();

impl BuiltinFunction {
    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }

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

        cx.heap.alloc(BuiltinFunction {
            _vtable: VTABLE,
            object: OrdinaryObject::new(Some(prototype), true),
            realm,
            script_or_module: None,
            initial_name: None,
            builtin_func,
            has_constructor: false,
        })
    }

    pub fn set_is_constructor(&mut self) {
        self.has_constructor = true;
    }

    pub fn set_property(&mut self, key: &PropertyKey, value: Property) {
        self.object.set_property(key, value);
    }

    pub fn intrinsic_func(
        &mut self,
        cx: &mut Context,
        name: &PropertyKey,
        func: BuiltinFunctionPtr,
        length: i32,
        realm: Gc<Realm>,
    ) {
        self.object.intrinsic_func(cx, name, func, length, realm);
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
            realm: current_execution_context.realm,
            script_or_module: None,
            lexical_env: cx.uninit_environment,
            variable_env: cx.uninit_environment,
            private_env: None,
            is_strict_mode: current_execution_context.is_strict_mode,
        });

        cx.push_execution_context(callee_context);
        let result = (self.builtin_func)(cx, this_argument, arguments, None);
        cx.pop_execution_context();

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
            realm: current_execution_context.realm,
            script_or_module: None,
            lexical_env: cx.uninit_environment,
            variable_env: cx.uninit_environment,
            private_env: None,
            is_strict_mode: current_execution_context.is_strict_mode,
        });

        cx.push_execution_context(callee_context);
        let result = crate::maybe!((self.builtin_func)(
            cx,
            Value::undefined(),
            arguments,
            Some(new_target)
        ));
        cx.pop_execution_context();

        result.as_object().into()
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
}

impl Into<Gc<BuiltinFunction>> for &BuiltinFunction {
    fn into(self) -> Gc<BuiltinFunction> {
        Gc::from_ptr(self as *const _ as *mut BuiltinFunction)
    }
}
