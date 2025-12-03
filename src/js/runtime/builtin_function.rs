use crate::runtime::bytecode::function::Closure;

use super::{
    bytecode::function::BytecodeFunction,
    function::{build_function_name, set_function_length, set_function_name},
    intrinsics::{intrinsics::Intrinsic, rust_runtime::RustRuntimeFunction},
    object_value::ObjectValue,
    property_key::PropertyKey,
    realm::Realm,
    Context, Handle,
};

/// Built-in Function Object (https://tc39.es/ecma262/#sec-built-in-function-objects)
pub struct BuiltinFunction {}

impl BuiltinFunction {
    /// Create a new builtin function. Function is not a constructor.
    pub fn create(
        cx: Context,
        builtin_func: RustRuntimeFunction,
        length: u32,
        name: Handle<PropertyKey>,
        realm: Handle<Realm>,
        prefix: Option<&str>,
    ) -> Handle<ObjectValue> {
        Self::create_builtin_function(
            cx,
            builtin_func,
            length,
            name,
            realm,
            // Default to Function.prototype
            Some(realm.get_intrinsic(Intrinsic::FunctionPrototype)),
            prefix,
            /* is_constructor */ false,
        )
        .into()
    }

    fn create_builtin_function(
        cx: Context,
        builtin_func: RustRuntimeFunction,
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
        set_function_name(cx, func, name, prefix);
    }

    /// Create a function with the given internal slots but without installing the `length` and
    /// `name` properties.
    ///
    /// Prototype is the raw value for the [[Prototype]] internal slot - n.
    pub fn create_builtin_function_without_properties(
        cx: Context,
        builtin_func: RustRuntimeFunction,
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

        Closure::new_builtin(cx, bytecode_function, realm.default_global_scope(), prototype)
    }

    /// Create the constructor function for an intrinsic.
    pub fn intrinsic_constructor(
        cx: Context,
        builtin_func: RustRuntimeFunction,
        length: u32,
        name: Handle<PropertyKey>,
        realm: Handle<Realm>,
        prototype: Intrinsic,
    ) -> Handle<ObjectValue> {
        Self::create_builtin_function(
            cx,
            builtin_func,
            length,
            name,
            realm,
            Some(realm.get_intrinsic(prototype)),
            None,
            /* is_constructor */ true,
        )
        .into()
    }
}
