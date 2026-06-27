use crate::{
    must,
    runtime::{
        Context,
        abstract_operations::create_data_property_or_throw,
        alloc_error::AllocResult,
        builtin_function::BuiltinFunction,
        error::type_error,
        gc::Handle,
        intrinsics::{intrinsics::Intrinsic, rust_runtime::RuntimeFunction},
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create,
        proxy_object::{ProxyObject, proxy_create},
        realm::Realm,
    },
    runtime_fn,
};

pub struct ProxyConstructor;

impl ProxyConstructor {
    /// Properties of the Proxy Constructor (https://tc39.es/ecma262/#sec-properties-of-the-proxy-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            RuntimeFunction::ProxyConstructor_construct,
            2,
            cx.names.proxy(),
            realm,
            Intrinsic::FunctionPrototype,
        )?;

        func.intrinsic_func(
            cx,
            cx.names.revocable(),
            RuntimeFunction::ProxyConstructor_revocable,
            2,
            realm,
        )?;

        Ok(func)
    }

    runtime_fn! {
    /// Proxy (https://tc39.es/ecma262/#sec-proxy-target-handler)
    fn construct(cx, _, arguments) {
        if cx.current_new_target().is_none() {
            return type_error(cx, "Proxy constructor must be called with new");
        }

        let target = arguments.get(cx, 0);
        let handler = arguments.get(cx, 1);

        Ok(proxy_create(cx, target, handler)?.as_value())
    }}

    runtime_fn! {
    /// Proxy.revocable (https://tc39.es/ecma262/#sec-proxy.revocable)
    fn revocable(cx, _, arguments) {
        let target = arguments.get(cx, 0);
        let handler = arguments.get(cx, 1);
        let proxy = proxy_create(cx, target, handler)?;

        let realm = cx.current_realm();
        let mut revoker = BuiltinFunction::create(
            cx,
            RuntimeFunction::proxy_constructor_revoke,
            0,
            cx.names.empty_string(),
            realm,
            None,
        )?;

        // Attach the proxy to the revoker so it can be accessed when the revoker is called
        revoker.private_element_set(
            cx,
            cx.well_known_symbols.revocable_proxy().cast(),
            proxy.into(),
        )?;

        let result = ordinary_object_create(cx)?;

        must!(create_data_property_or_throw(cx, result, cx.names.proxy_(), proxy.into()));
        must!(create_data_property_or_throw(cx, result, cx.names.revoke(), revoker.into()));

        Ok(result.as_value())
    }}
}

runtime_fn! {
fn revoke(cx, _, _) {
    // Find the proxy object attached to this closure via a private property
    let mut revoke_function = cx.current_function();
    let proxy_object_property =
        revoke_function.private_element_find(cx, cx.well_known_symbols.revocable_proxy().cast());

    // Revoke the proxy object and remove from closure
    if let Some(proxy_object_property) = proxy_object_property {
        revoke_function.remove_property(cx.well_known_symbols.revocable_proxy());

        let mut proxy_object = proxy_object_property.value().cast::<ProxyObject>();
        proxy_object.revoke();
    }

    Ok(cx.undefined())
}}
