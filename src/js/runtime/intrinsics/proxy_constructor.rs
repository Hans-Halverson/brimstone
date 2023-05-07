use crate::{
    js::runtime::{
        abstract_operations::create_data_property_or_throw,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        gc::{Gc, GcDeref, Handle},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        object_value::ObjectValue,
        ordinary_object::ordinary_object_create,
        proxy_object::{proxy_create, ProxyObject},
        realm::Realm,
        value::Value,
        Context,
    },
    maybe, must, set_uninit,
};

pub struct ProxyConstructor;

impl ProxyConstructor {
    // 28.2.2 Properties of the Proxy Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            2,
            cx.names.proxy(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();

        func.intrinsic_func(cx, cx.names.revocable(), Self::revocable, 2, realm);

        func
    }

    // 28.2.1.1 Proxy
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if new_target.is_none() {
            return type_error_(cx, "Proxy is a constructor");
        }

        let target = get_argument(arguments, 0);
        let handler = get_argument(arguments, 1);

        maybe!(proxy_create(cx, target, handler)).into()
    }

    // 28.2.2.1 Proxy.revocable
    fn revocable(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        let handler = get_argument(arguments, 1);
        let proxy = maybe!(proxy_create(cx, target, handler));

        // Create the revoker abstract closure
        let revoke = |cx: &mut Context,
                      _: Value,
                      _: &[Value],
                      _: Option<Gc<ObjectValue>>|
         -> EvalResult<Value> {
            let mut closure_environment = cx.get_closure_environment::<RevokeEnvironment>();
            let revocable_proxy = closure_environment.revocable_proxy;

            if revocable_proxy.is_none() {
                return Value::undefined().into();
            }

            closure_environment.revocable_proxy = None;
            revocable_proxy.unwrap().revoke();

            Value::undefined().into()
        };

        let revoke_environment = RevokeEnvironment::new(cx, Some(proxy));

        let mut revoker =
            BuiltinFunction::create(cx, revoke, 0, cx.names.empty_string(), None, None, None);
        revoker.set_closure_environment(revoke_environment);

        let result = ordinary_object_create(cx);

        must!(create_data_property_or_throw(cx, result, cx.names.proxy_(), proxy.into()));
        must!(create_data_property_or_throw(cx, result, cx.names.revoke(), revoker.into()));

        result.into()
    }
}

#[repr(C)]
struct RevokeEnvironment {
    descriptor: Gc<ObjectDescriptor>,
    revocable_proxy: Option<Gc<ProxyObject>>,
}

impl GcDeref for RevokeEnvironment {}

impl RevokeEnvironment {
    fn new(
        cx: &mut Context,
        revocable_proxy: Option<Handle<ProxyObject>>,
    ) -> Handle<RevokeEnvironment> {
        let mut env = cx.heap.alloc_uninit::<RevokeEnvironment>();

        set_uninit!(
            env.descriptor,
            cx.base_descriptors
                .get(ObjectKind::RevokeProxyClosureEnvironment)
        );
        set_uninit!(env.revocable_proxy, revocable_proxy.map(|p| p.get_()));

        env
    }
}
