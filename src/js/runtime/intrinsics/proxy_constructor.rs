use crate::{
    must,
    runtime::{
        abstract_operations::create_data_property_or_throw,
        builtin_function::BuiltinFunction,
        error::type_error,
        eval_result::EvalResult,
        function::get_argument,
        gc::Handle,
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create,
        proxy_object::{proxy_create, ProxyObject},
        realm::Realm,
        Context, Value,
    },
};

pub struct ProxyConstructor;

impl ProxyConstructor {
    /// Properties of the Proxy Constructor (https://tc39.es/ecma262/#sec-properties-of-the-proxy-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            2,
            cx.names.proxy(),
            realm,
            None,
        );

        func.intrinsic_func(cx, cx.names.revocable(), Self::revocable, 2, realm);

        func
    }

    /// Proxy (https://tc39.es/ecma262/#sec-proxy-target-handler)
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        if cx.current_new_target().is_none() {
            return type_error(cx, "Proxy is a constructor");
        }

        let target = get_argument(cx, arguments, 0);
        let handler = get_argument(cx, arguments, 1);

        Ok(proxy_create(cx, target, handler)?.as_value())
    }

    /// Proxy.revocable (https://tc39.es/ecma262/#sec-proxy.revocable)
    pub fn revocable(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        let handler = get_argument(cx, arguments, 1);
        let proxy = proxy_create(cx, target, handler)?;

        let realm = cx.current_realm();
        let mut revoker =
            BuiltinFunction::create(cx, revoke, 0, cx.names.empty_string(), realm, None, None);

        // Attach the proxy to the revoker so it can be accessed when the revoker is called
        revoker.private_element_set(
            cx,
            cx.well_known_symbols.revocable_proxy().cast(),
            proxy.into(),
        );

        let result = ordinary_object_create(cx);

        must!(create_data_property_or_throw(cx, result, cx.names.proxy_(), proxy.into()));
        must!(create_data_property_or_throw(cx, result, cx.names.revoke(), revoker.into()));

        Ok(result.as_value())
    }
}

pub fn revoke(mut cx: Context, _: Handle<Value>, _: &[Handle<Value>]) -> EvalResult<Handle<Value>> {
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
}
