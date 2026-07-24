use crate::{
    must_a,
    runtime::{
        Context, Handle,
        alloc_error::AllocResult,
        bytecode::function::{BytecodeFunction, ClosureObject},
        function::{build_function_name, set_function_length, set_function_name},
        intrinsics::{
            intrinsics::Intrinsic,
            rust_runtime::{RuntimeFunction, RuntimeFunctionId},
        },
        object_value::ObjectValue,
        property_key::PropertyKey,
        realm::Realm,
    },
};

/// Built-in Function Object (https://tc39.es/ecma262/#sec-built-in-function-objects)
pub struct BuiltinFunction {}

impl BuiltinFunction {
    /// Create a new builtin function. Function is not a constructor.
    pub fn create(
        cx: Context,
        builtin_func: RuntimeFunction,
        length: u32,
        name: Handle<PropertyKey>,
        realm: Handle<Realm>,
        prefix: Option<&str>,
    ) -> AllocResult<Handle<ObjectValue>> {
        Self::create_impl(cx, builtin_func.to_id(), length, name, realm, prefix)
    }

    /// Create a new builtin function for a custom runtime function. Id is generated when runtime
    /// function is registered.
    ///
    /// Function is not a constructor.
    pub fn create_custom(
        cx: Context,
        builtin_func: RuntimeFunctionId,
        length: u32,
        name: Handle<PropertyKey>,
        realm: Handle<Realm>,
        prefix: Option<&str>,
    ) -> AllocResult<Handle<ObjectValue>> {
        Self::create_impl(cx, builtin_func, length, name, realm, prefix)
    }

    fn create_impl(
        cx: Context,
        builtin_func: RuntimeFunctionId,
        length: u32,
        name: Handle<PropertyKey>,
        realm: Handle<Realm>,
        prefix: Option<&str>,
    ) -> AllocResult<Handle<ObjectValue>> {
        // Assumes that the name property for all built-in functions is within the string length
        // limit, otherwise panic.
        let name = must_a!(build_function_name(cx, name, prefix));

        let bytecode_function = BytecodeFunction::new_rust_runtime_function(
            cx,
            builtin_func,
            realm,
            /* is_constructor */ false,
            Some(name),
            length,
        )?;

        let closure =
            ClosureObject::new(cx, bytecode_function, realm.default_global_scope(), realm)?;

        Ok(closure.as_object())
    }

    /// Create a function with the given internal slots but without installing the `length` and
    /// `name` properties.
    pub fn create_builtin_function_without_properties(
        cx: Context,
        builtin_func: RuntimeFunctionId,
        name: Option<Handle<PropertyKey>>,
        realm: Handle<Realm>,
        prototype: Option<Handle<ObjectValue>>,
        is_constructor: bool,
    ) -> AllocResult<Handle<ClosureObject>> {
        // Assumes that the name property for all built-in functions is within the string length
        // limit, otherwise panic.
        let name = name
            .map(|name| Ok(must_a!(build_function_name(cx, name, None))))
            .transpose()?;
        let bytecode_function = BytecodeFunction::new_rust_runtime_function(
            cx,
            builtin_func,
            realm,
            is_constructor,
            name,
            // Function length is set later by caller if needed
            0,
        )?;

        ClosureObject::new_without_properties(
            cx,
            bytecode_function,
            realm.default_global_scope(),
            prototype,
        )
    }

    /// Create the constructor function for an intrinsic.
    pub fn intrinsic_constructor(
        cx: Context,
        builtin_func: RuntimeFunction,
        length: u32,
        name: Handle<PropertyKey>,
        realm: Handle<Realm>,
        prototype: Intrinsic,
    ) -> AllocResult<Handle<ObjectValue>> {
        let closure = Self::create_builtin_function_without_properties(
            cx,
            builtin_func.to_id(),
            Some(name),
            realm,
            Some(realm.get_intrinsic(prototype)),
            /* is_constructor */ true,
        )?;

        set_function_length(cx, closure.into(), length)?;

        // Assumes that the name property for all built-in functions is within the string length
        // limit, otherwise panic.
        must_a!(set_function_name(cx, closure.into(), name, None));

        Ok(closure.as_object())
    }
}
