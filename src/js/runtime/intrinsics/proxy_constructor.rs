use std::mem::size_of;

use crate::{
    js::runtime::{
        abstract_operations::create_data_property_or_throw,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        gc::{Handle, HeapObject, HeapVisitor},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create,
        proxy_object::{proxy_create, ProxyObject},
        realm::Realm,
        Context, HeapPtr, Value,
    },
    maybe, must, set_uninit,
};

pub struct ProxyConstructor;

impl ProxyConstructor {
    // 28.2.2 Properties of the Proxy Constructor
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

    // 28.2.1.1 Proxy
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if new_target.is_none() {
            return type_error_(cx, "Proxy is a constructor");
        }

        let target = get_argument(cx, arguments, 0);
        let handler = get_argument(cx, arguments, 1);

        maybe!(proxy_create(cx, target, handler)).into()
    }

    // 28.2.2.1 Proxy.revocable
    pub fn revocable(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        let handler = get_argument(cx, arguments, 1);
        let proxy = maybe!(proxy_create(cx, target, handler));

        let realm = cx.current_realm();
        let mut revoker =
            BuiltinFunction::create(cx, revoke, 0, cx.names.empty_string(), realm, None, None);

        // Attach the proxy to the revoker so it can be accessed when the revoker is called
        if cx.options.bytecode {
            revoker.private_element_set(
                cx,
                cx.well_known_symbols.revocable_proxy().cast(),
                proxy.into(),
            );
        } else {
            let revoke_environment = RevokeProxyClosureEnvironment::new(cx, Some(proxy));
            revoker
                .cast::<BuiltinFunction>()
                .set_closure_environment(revoke_environment);
        }

        let result = ordinary_object_create(cx);

        must!(create_data_property_or_throw(cx, result, cx.names.proxy_(), proxy.into()));
        must!(create_data_property_or_throw(cx, result, cx.names.revoke(), revoker.into()));

        result.into()
    }
}

pub fn revoke(
    mut cx: Context,
    _: Handle<Value>,
    _: &[Handle<Value>],
    _: Option<Handle<ObjectValue>>,
) -> EvalResult<Handle<Value>> {
    if cx.options.bytecode {
        // Find the proxy object attached to this closure via a private property
        let mut revoke_function = cx.current_function();
        let proxy_object_property = revoke_function
            .private_element_find(cx, cx.well_known_symbols.revocable_proxy().cast());

        // Revoke the proxy object and remove from closure
        if let Some(proxy_object_property) = proxy_object_property {
            revoke_function.remove_property(cx.well_known_symbols.revocable_proxy());

            let mut proxy_object = proxy_object_property.value().cast::<ProxyObject>();
            proxy_object.revoke();
        }
    } else {
        let mut closure_environment_ptr =
            cx.get_closure_environment_ptr::<RevokeProxyClosureEnvironment>();
        let revocable_proxy_ptr = closure_environment_ptr.revocable_proxy_ptr();

        if revocable_proxy_ptr.is_none() {
            return cx.undefined().into();
        }

        closure_environment_ptr.revocable_proxy = None;
        revocable_proxy_ptr.unwrap().revoke();
    }

    cx.undefined().into()
}

#[repr(C)]
pub struct RevokeProxyClosureEnvironment {
    descriptor: HeapPtr<ObjectDescriptor>,
    revocable_proxy: Option<HeapPtr<ProxyObject>>,
}

impl RevokeProxyClosureEnvironment {
    fn new(
        cx: Context,
        revocable_proxy: Option<Handle<ProxyObject>>,
    ) -> Handle<RevokeProxyClosureEnvironment> {
        let mut env = cx.alloc_uninit::<RevokeProxyClosureEnvironment>();

        set_uninit!(
            env.descriptor,
            cx.base_descriptors
                .get(ObjectKind::RevokeProxyClosureEnvironment)
        );
        set_uninit!(env.revocable_proxy, revocable_proxy.map(|p| p.get_()));

        env.to_handle()
    }

    fn revocable_proxy_ptr(&self) -> Option<HeapPtr<ProxyObject>> {
        self.revocable_proxy
    }
}

impl HeapObject for HeapPtr<RevokeProxyClosureEnvironment> {
    fn byte_size(&self) -> usize {
        size_of::<RevokeProxyClosureEnvironment>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer_opt(&mut self.revocable_proxy);
    }
}
