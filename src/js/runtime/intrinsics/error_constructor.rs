use crate::{
    js::runtime::{
        abstract_operations::{create_non_enumerable_data_property_or_throw, get, has_property},
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        gc::HandleValue,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_create_from_constructor,
        property::Property,
        realm::Realm,
        type_utilities::to_string,
        Context, Handle,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

pub struct ErrorObject;

impl ErrorObject {
    fn new_from_constructor(
        cx: &mut Context,
        constructor: Handle<ObjectValue>,
    ) -> EvalResult<Handle<ObjectValue>> {
        let object = maybe!(object_create_from_constructor::<ObjectValue>(
            cx,
            constructor,
            ObjectKind::ErrorObject,
            Intrinsic::ErrorPrototype
        ));

        Handle::from_heap(object).into()
    }
}

pub struct ErrorConstructor;

impl ErrorConstructor {
    // 20.5.2 Properties of the Error Constructor
    pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            cx.names.error(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            cx,
            cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::ErrorPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func
    }

    // 20.5.1.1 Error
    fn construct(
        cx: &mut Context,
        _: HandleValue,
        arguments: &[HandleValue],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            cx.current_execution_context_ptr().function()
        };

        let object = maybe!(ErrorObject::new_from_constructor(cx, new_target));

        let message = get_argument(arguments, 0);
        if !message.is_undefined() {
            let message_string = maybe!(to_string(cx, message));
            create_non_enumerable_data_property_or_throw(
                cx,
                object,
                cx.names.message(),
                message_string.into(),
            );
        }

        maybe!(install_error_cause(cx, object, get_argument(arguments, 1)));

        object.into()
    }
}

// 20.5.8.1 InstallErrorCause
pub fn install_error_cause(
    cx: &mut Context,
    object: Handle<ObjectValue>,
    options: HandleValue,
) -> EvalResult<()> {
    if options.is_object() {
        let options = options.as_object();
        if maybe!(has_property(cx, options, cx.names.cause())) {
            let cause = maybe!(get(cx, options, cx.names.cause()));
            create_non_enumerable_data_property_or_throw(cx, object, cx.names.cause(), cause);
        }
    }

    ().into()
}
