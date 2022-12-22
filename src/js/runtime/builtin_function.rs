use wrap_ordinary_object::wrap_ordinary_object;

use crate::{impl_gc_into, maybe_};

use super::{
    completion::AbstractResult,
    execution_context::{ExecutionContext, ScriptOrModule},
    function::{set_function_length, set_function_name},
    gc::{Gc, GcDeref},
    intrinsics::intrinsics::Intrinsic,
    object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
    ordinary_object::OrdinaryObject,
    property_descriptor::PropertyDescriptor,
    realm::Realm,
    value::Value,
    Context,
};

// 10.3 Built-in Function Object
#[repr(C)]
pub struct BuiltinFunction {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
    realm: Gc<Realm>,
    script_or_module: Option<ScriptOrModule>,
    pub initial_name: Option<String>,
    builtin_func: BuiltinFunctionPtr,
}

// Function pointer to a builtin function
type BuiltinFunctionPtr = fn(
    cx: &mut Context,
    this_value: Value,
    arguments: Vec<Value>,
    new_target: Option<Gc<ObjectValue>>,
) -> AbstractResult<Value>;

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
        length: u32,
        name: &str,
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
        })
    }
}

#[wrap_ordinary_object]
impl Object for BuiltinFunction {
    // 10.3.1 [[Call]]
    fn call(
        &self,
        cx: &mut Context,
        this_argument: Value,
        arguments: Vec<Value>,
    ) -> AbstractResult<Value> {
        let realm = cx.current_realm();
        let callee_context = cx.heap.alloc(ExecutionContext {
            function: Some(self.into()),
            realm,
            script_or_module: None,
            lexical_env: cx.uninit_environment,
            variable_env: cx.uninit_environment,
            private_env: None,
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
        arguments: Vec<Value>,
        new_target: Gc<ObjectValue>,
    ) -> AbstractResult<Gc<ObjectValue>> {
        let realm = cx.current_realm();
        let callee_context = cx.heap.alloc(ExecutionContext {
            function: Some(self.into()),
            realm,
            script_or_module: None,
            lexical_env: cx.uninit_environment,
            variable_env: cx.uninit_environment,
            private_env: None,
        });

        cx.push_execution_context(callee_context);
        let result = maybe_!((self.builtin_func)(
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
}

impl Into<Gc<BuiltinFunction>> for &BuiltinFunction {
    fn into(self) -> Gc<BuiltinFunction> {
        Gc::from_ptr(self as *const _ as *mut BuiltinFunction)
    }
}
