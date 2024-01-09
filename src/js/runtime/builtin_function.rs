use std::mem::size_of;

use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object,
    js::runtime::bytecode::{function::Closure, generator::BytecodeFunctionGenerator},
    maybe, set_uninit,
};

use super::{
    completion::EvalResult,
    execution_context::ExecutionContext,
    function::{set_function_length, set_function_name},
    gc::{HandleScope, HeapObject, HeapPtr, HeapVisitor, IsHeapObject},
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::ObjectKind,
    object_value::{ObjectValue, VirtualObject},
    ordinary_object::object_create_with_proto,
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
    cx: Context,
    this_value: Handle<Value>,
    arguments: &[Handle<Value>],
    new_target: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>>;

// Generic storage for variables captured by function if it is a closure. Must be cast to specific
// type for stored variables at each use.
pub struct ClosureEnvironment {}

impl BuiltinFunction {
    pub fn create(
        cx: Context,
        builtin_func: BuiltinFunctionPtr,
        length: i32,
        name: Handle<PropertyKey>,
        realm: Option<Handle<Realm>>,
        prototype: Option<Handle<ObjectValue>>,
        prefix: Option<&str>,
    ) -> Handle<ObjectValue> {
        if cx.options.bytecode {
            Self::create_builtin_bytecode_function(
                cx,
                builtin_func,
                length,
                name,
                realm,
                prototype,
                prefix,
            )
            .into()
        } else {
            Self::create_builtin_legacy_function(
                cx,
                builtin_func,
                length,
                name,
                realm,
                prototype,
                prefix,
            )
            .into()
        }
    }

    pub fn create_builtin_function_without_properties(
        cx: Context,
        builtin_func: BuiltinFunctionPtr,
        length: i32,
        realm: Option<Handle<Realm>>,
        prototype: Option<Handle<ObjectValue>>,
    ) -> Handle<ObjectValue> {
        if cx.options.bytecode {
            Self::create_builtin_bytecode_function_without_properties(
                cx,
                builtin_func,
                length,
                realm,
                prototype,
            )
            .into()
        } else {
            Self::create_builtin_legacy_function_without_properties(
                cx,
                builtin_func,
                realm,
                prototype,
            )
            .into()
        }
    }

    // 10.3.3 CreateBuiltinFunction
    fn create_builtin_legacy_function(
        cx: Context,
        builtin_func: BuiltinFunctionPtr,
        length: i32,
        name: Handle<PropertyKey>,
        realm: Option<Handle<Realm>>,
        prototype: Option<Handle<ObjectValue>>,
        prefix: Option<&str>,
    ) -> Handle<BuiltinFunction> {
        let func = Self::create_builtin_legacy_function_without_properties(
            cx,
            builtin_func,
            realm,
            prototype,
        );
        Self::install_common_properties(cx, func.into(), length, name, prefix);

        func
    }

    fn create_builtin_bytecode_function(
        cx: Context,
        builtin_func: BuiltinFunctionPtr,
        length: i32,
        name: Handle<PropertyKey>,
        realm: Option<Handle<Realm>>,
        prototype: Option<Handle<ObjectValue>>,
        prefix: Option<&str>,
    ) -> Handle<Closure> {
        let func = Self::create_builtin_bytecode_function_without_properties(
            cx,
            builtin_func,
            length,
            realm,
            prototype,
        );
        Self::install_common_properties(cx, func.into(), length, name, prefix);

        func
    }

    fn install_common_properties(
        cx: Context,
        func: Handle<ObjectValue>,
        length: i32,
        name: Handle<PropertyKey>,
        prefix: Option<&str>,
    ) {
        set_function_length(cx, func, length);
        set_function_name(cx.into(), func, name, prefix);
    }

    fn create_builtin_legacy_function_without_properties(
        cx: Context,
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

    fn create_builtin_bytecode_function_without_properties(
        cx: Context,
        builtin_func: BuiltinFunctionPtr,
        length: i32,
        realm: Option<Handle<Realm>>,
        prototype: Option<Handle<ObjectValue>>,
    ) -> Handle<Closure> {
        let function_id = *cx.rust_runtime_functions.get_id(builtin_func).unwrap();
        let bytecode_function = BytecodeFunctionGenerator::generate_rust_runtime_function(
            cx,
            function_id,
            length as u32,
        )
        .unwrap();

        // TOOD: Use global object scope from realm
        let realm = realm.unwrap_or_else(|| cx.current_realm());
        let prototype =
            prototype.unwrap_or_else(|| realm.get_intrinsic(Intrinsic::FunctionPrototype));

        Closure::new_builtin(cx, bytecode_function, prototype)
    }

    /// Create the constructor function for an intrinsic.
    pub fn intrinsic_constructor(
        cx: Context,
        builtin_func: BuiltinFunctionPtr,
        length: i32,
        name: Handle<PropertyKey>,
        realm: Option<Handle<Realm>>,
        prototype: Option<Handle<ObjectValue>>,
    ) -> Handle<ObjectValue> {
        if cx.options.bytecode {
            // TODO: Mark as a constructor
            Self::create_builtin_bytecode_function(
                cx,
                builtin_func,
                length,
                name,
                realm,
                prototype,
                None,
            )
            .into()
        } else {
            let mut constructor_function = Self::create_builtin_legacy_function(
                cx,
                builtin_func,
                length,
                name,
                realm,
                prototype,
                None,
            );
            constructor_function.set_is_constructor();
            constructor_function.into()
        }
    }

    /// Intrinsic closures are builtins that use a rust closure environment. Can not yet be emitted
    /// as bytecode.
    pub fn intrinsic_closure(
        cx: Context,
        builtin_func: BuiltinFunctionPtr,
        length: i32,
        name: Handle<PropertyKey>,
    ) -> Handle<BuiltinFunction> {
        if cx.options.bytecode {
            unimplemented!("Intrinsic closures can not yet be emitted as bytecode");
        }

        Self::create_builtin_legacy_function(cx, builtin_func, length, name, None, None, None)
    }

    fn realm(&self) -> Handle<Realm> {
        self.realm.to_handle()
    }

    pub fn initial_name(&self) -> Option<Handle<StringValue>> {
        self.initial_name.map(|s| s.to_handle())
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

#[wrap_ordinary_object]
impl VirtualObject for Handle<BuiltinFunction> {
    // 10.3.1 [[Call]]
    fn call(
        &self,
        cx: Context,
        this_argument: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        HandleScope::new(cx, |mut cx| {
            let is_strict_mode = cx.current_execution_context_ptr().is_strict_mode();
            let callee_context = ExecutionContext::new(
                cx,
                /* function */ Some(self.object()),
                self.realm(),
                /* script_or_module */ None,
                /* lexical_env */ None,
                /* variable_env */ None,
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
        cx: Context,
        arguments: &[Handle<Value>],
        new_target: Handle<ObjectValue>,
    ) -> EvalResult<Handle<ObjectValue>> {
        HandleScope::new(cx, |mut cx| {
            let is_strict_mode = cx.current_execution_context_ptr().is_strict_mode();
            let callee_context = ExecutionContext::new(
                cx,
                /* function */ Some(self.object()),
                self.realm(),
                /* script_or_module */ None,
                /* lexical_env */ None,
                /* variable_env */ None,
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

    fn get_realm(&self, _: Context) -> EvalResult<HeapPtr<Realm>> {
        self.realm.into()
    }
}

impl HeapObject for HeapPtr<BuiltinFunction> {
    fn byte_size(&self) -> usize {
        size_of::<BuiltinFunction>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.cast::<ObjectValue>().visit_pointers(visitor);
        visitor.visit_pointer(&mut self.realm);
        visitor.visit_pointer_opt(&mut self.initial_name);
        visitor.visit_pointer_opt(&mut self.closure_environment);
    }
}
