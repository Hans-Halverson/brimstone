use crate::js::runtime::bytecode::function::Closure;

use super::{
    bytecode::function::BytecodeFunction,
    completion::EvalResult,
    function::{build_function_name, set_function_length, set_function_name},
    intrinsics::intrinsics::Intrinsic,
    object_value::ObjectValue,
    property_key::PropertyKey,
    realm::Realm,
    Context, Handle, Value,
};

// 10.3 Built-in Function Object
pub struct BuiltinFunction {}

// Function pointer to a builtin function
pub type BuiltinFunctionPtr = fn(
    cx: Context,
    this_value: Handle<Value>,
    arguments: &[Handle<Value>],
    new_target: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>>;

impl BuiltinFunction {
    /// Create a new builtin function. Function is not a constructor.
    pub fn create(
        cx: Context,
        builtin_func: BuiltinFunctionPtr,
        length: u32,
        name: Handle<PropertyKey>,
        realm: Handle<Realm>,
        prototype: Option<Handle<ObjectValue>>,
        prefix: Option<&str>,
    ) -> Handle<ObjectValue> {
        Self::create_builtin_function(
            cx,
            builtin_func,
            length,
            name,
            realm,
            prototype,
            prefix,
            /* is_constructor */ false,
        )
        .into()
    }

    fn create_builtin_function(
        cx: Context,
        builtin_func: BuiltinFunctionPtr,
        length: u32,
        name: Handle<PropertyKey>,
        realm: Handle<Realm>,
        prototype: Option<Handle<ObjectValue>>,
        prefix: Option<&str>,
        is_constructor: bool,
    ) -> Handle<Closure> {
        let func = Self::create_builtin_function_without_properties(
            cx,
            builtin_func,
            Some(name),
            realm,
            prototype,
            is_constructor,
        );
        Self::install_common_properties(cx, func.into(), length, name, prefix);

        func
    }

    fn install_common_properties(
        cx: Context,
        func: Handle<ObjectValue>,
        length: u32,
        name: Handle<PropertyKey>,
        prefix: Option<&str>,
    ) {
        set_function_length(cx, func, length);
        set_function_name(cx.into(), func, name, prefix);
    }

    pub fn create_builtin_function_without_properties(
        cx: Context,
        builtin_func: BuiltinFunctionPtr,
        name: Option<Handle<PropertyKey>>,
        realm: Handle<Realm>,
        prototype: Option<Handle<ObjectValue>>,
        is_constructor: bool,
    ) -> Handle<Closure> {
        let name = name.map(|name| build_function_name(cx, name, None));
        let bytecode_function = BytecodeFunction::new_rust_runtime_function(
            cx,
            builtin_func,
            realm,
            is_constructor,
            name,
        );

        let prototype =
            prototype.unwrap_or_else(|| realm.get_intrinsic(Intrinsic::FunctionPrototype));

        Closure::new_builtin(cx, bytecode_function, realm.default_global_scope(), prototype)
    }

    /// Create the constructor function for an intrinsic.
    pub fn intrinsic_constructor(
        cx: Context,
        builtin_func: BuiltinFunctionPtr,
        length: u32,
        name: Handle<PropertyKey>,
        realm: Handle<Realm>,
        prototype: Option<Handle<ObjectValue>>,
    ) -> Handle<ObjectValue> {
        Self::create_builtin_function(
            cx,
            builtin_func,
            length,
            name,
            realm,
            prototype,
            None,
            /* is_constructor */ true,
        )
        .into()
    }
}
