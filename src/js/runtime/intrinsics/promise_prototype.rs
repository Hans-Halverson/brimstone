use crate::{
    eval_err, intrinsic_methods,
    runtime::{
        Context, Handle, Value,
        abstract_operations::{call_object, invoke, species_constructor},
        alloc_error::AllocResult,
        builtin_function::BuiltinFunction,
        error::type_error,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction},
        object_value::ObjectValue,
        promise_object::{PromiseCapability, PromiseObject, promise_resolve},
        realm::Realm,
        type_utilities::is_callable,
    },
    runtime_fn,
};

pub struct PromisePrototype;

impl PromisePrototype {
    /// Properties of the Promise Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-promise-prototype-object)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        // Constructor property is added once PromiseConstructor has been created
        intrinsic_methods!(cx, builder, {
            catch   PromisePrototype_catch   (1),
            finally PromisePrototype_finally (1),
            then    PromisePrototype_then    (2),
        });

        // Promise.prototype [ @@toStringTag ] (https://tc39.es/ecma262/#sec-promise.prototype-%symbol.tostringtag%)
        builder.to_string_tag(cx.names.promise())?;

        builder.build()
    }

    runtime_fn! {
    /// Promise.prototype.catch (https://tc39.es/ecma262/#sec-promise.prototype.catch)
    fn catch(cx, this_value, arguments) {
        let on_rejected = arguments.get(cx, 0);
        invoke(cx, this_value, cx.names.then(), &[cx.undefined(), on_rejected])
    }}

    runtime_fn! {
    /// Promise.prototype.finally (https://tc39.es/ecma262/#sec-promise.prototype.finally)
    fn finally(cx, this_value, arguments) {
        if !this_value.is_object() {
            return type_error(cx, "Promise.prototype.finally must be called on an object");
        }
        let promise = this_value.as_object();

        let on_finally = arguments.get(cx, 0);

        let constructor = species_constructor(cx, promise, Intrinsic::PromiseConstructor)?;

        let then_finally;
        let catch_finally;
        if !is_callable(on_finally) {
            then_finally = on_finally;
            catch_finally = on_finally;
        } else {
            let on_finally = on_finally.as_object();

            // Create then and catch functions
            let finally_then = BuiltinFunction::create(
                cx,
                RuntimeFunction::PromisePrototype_finally_then,
                1,
                cx.names.empty_string(),
                cx.current_realm(),
                None,
            )?;

            let finally_catch = BuiltinFunction::create(
                cx,
                RuntimeFunction::PromisePrototype_finally_catch,
                1,
                cx.names.empty_string(),
                cx.current_realm(),
                None,
            )?;

            // And attach private properties
            Self::set_constructor(cx, finally_then, constructor)?;
            Self::set_on_finally(cx, finally_then, on_finally)?;

            Self::set_constructor(cx, finally_catch, constructor)?;
            Self::set_on_finally(cx, finally_catch, on_finally)?;

            then_finally = finally_then.into();
            catch_finally = finally_catch.into();
        }

        invoke(cx, promise.into(), cx.names.then(), &[then_finally, catch_finally])
    }}

    fn get_constructor(cx: Context, function: Handle<ObjectValue>) -> Handle<ObjectValue> {
        function
            .private_element_find(cx, cx.symbols.constructor().cast())
            .unwrap()
            .value()
            .as_object()
    }

    fn set_constructor(
        cx: Context,
        mut function: Handle<ObjectValue>,
        value: Handle<ObjectValue>,
    ) -> AllocResult<()> {
        function.private_element_set(cx, cx.symbols.constructor().cast(), value.into())
    }

    fn get_on_finally(cx: Context, function: Handle<ObjectValue>) -> Handle<ObjectValue> {
        function
            .private_element_find(cx, cx.symbols.on_finally().cast())
            .unwrap()
            .value()
            .as_object()
    }

    fn set_on_finally(
        cx: Context,
        mut function: Handle<ObjectValue>,
        value: Handle<ObjectValue>,
    ) -> AllocResult<()> {
        function.private_element_set(cx, cx.symbols.on_finally().cast(), value.into())
    }

    fn get_value(cx: Context, function: Handle<ObjectValue>) -> Handle<Value> {
        function
            .private_element_find(cx, cx.symbols.values().cast())
            .unwrap()
            .value()
    }

    fn set_value(
        cx: Context,
        mut function: Handle<ObjectValue>,
        value: Handle<Value>,
    ) -> AllocResult<()> {
        function.private_element_set(cx, cx.symbols.values().cast(), value)
    }

    runtime_fn! {
    fn finally_then(cx, _, arguments) {
        let current_function = cx.current_function();

        let constructor = Self::get_constructor(cx, current_function);
        let on_finally = Self::get_on_finally(cx, current_function);

        let result = call_object(cx, on_finally, cx.undefined(), &[])?;
        let promise = promise_resolve(cx, constructor.into(), result)?;

        // Continue to a function that returns the value
        let continue_function = BuiltinFunction::create(
            cx,
            RuntimeFunction::PromisePrototype_finally_then_continue,
            0,
            cx.names.empty_string(),
            cx.current_realm(),
            None,
        )?;

        let value = arguments.get(cx, 0);
        Self::set_value(cx, continue_function, value)?;

        invoke(cx, promise.into(), cx.names.then(), &[continue_function.into()])
    }}

    runtime_fn! {
    fn finally_then_continue(cx, _, _) {
        let current_function = cx.current_function();
        Ok(Self::get_value(cx, current_function))
    }}

    runtime_fn! {
    fn finally_catch(cx, _, arguments) {
        let current_function = cx.current_function();

        let constructor = Self::get_constructor(cx, current_function);
        let on_finally = Self::get_on_finally(cx, current_function);

        let result = call_object(cx, on_finally, cx.undefined(), &[])?;
        let promise = promise_resolve(cx, constructor.into(), result)?;

        // Continue to a function that throws the value
        let continue_function = BuiltinFunction::create(
            cx,
            RuntimeFunction::PromisePrototype_finally_catch_continue,
            0,
            cx.names.empty_string(),
            cx.current_realm(),
            None,
        )?;

        let value = arguments.get(cx, 0);
        Self::set_value(cx, continue_function, value)?;

        invoke(cx, promise.into(), cx.names.then(), &[continue_function.into()])
    }}

    runtime_fn! {
    fn finally_catch_continue(cx, _, _) {
        let current_function = cx.current_function();
        let value = Self::get_value(cx, current_function);

        eval_err!(value)
    }}

    runtime_fn! {
    /// Promise.prototype.then (https://tc39.es/ecma262/#sec-promise.prototype.then)
    fn then(cx, this_value, arguments) {
        let Some(promise) = this_value.as_opt::<PromiseObject>() else {
            return type_error(cx, "Promise.prototype.then must be called on a Promise");
        };

        let constructor = species_constructor(cx, promise.into(), Intrinsic::PromiseConstructor)?;
        let capability = PromiseCapability::new(cx, constructor.into())?;

        let on_fulfilled = arguments.get(cx, 0);
        let on_rejected = arguments.get(cx, 1);

        Ok(perform_promise_then(cx, promise, on_fulfilled, on_rejected, Some(capability))?)
    }}
}

/// PerformPromiseThen (https://tc39.es/ecma262/#sec-performpromisethen) with a capability provided.
pub fn perform_promise_then(
    cx: Context,
    mut promise: Handle<PromiseObject>,
    fulfill_handler: Handle<Value>,
    reject_handler: Handle<Value>,
    capability: Option<Handle<PromiseCapability>>,
) -> AllocResult<Handle<Value>> {
    let fulfill_handler = if is_callable(fulfill_handler) {
        Some(fulfill_handler.as_object())
    } else {
        None
    };

    let reject_handler = if is_callable(reject_handler) {
        Some(reject_handler.as_object())
    } else {
        None
    };

    promise.add_then_reaction(cx, fulfill_handler, reject_handler, capability)?;

    if let Some(capability) = capability {
        Ok(capability.promise().into())
    } else {
        Ok(cx.undefined())
    }
}
