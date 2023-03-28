use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object,
    js::runtime::{
        abstract_operations::{create_non_enumerable_data_property_or_throw, get, has_property},
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        function::get_argument,
        gc::Gc,
        object_value::{extract_object_vtable, HasObject, Object, ObjectValue},
        ordinary_object::object_ordinary_init_from_constructor,
        property::{PrivateProperty, Property},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::to_string,
        value::Value,
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

extend_object! {
    pub struct ErrorObject {}
}

impl ErrorObject {
    const VTABLE: *const () = extract_object_vtable::<ErrorObject>();

    fn new_from_constructor(
        cx: &mut Context,
        constructor: Gc<ObjectValue>,
    ) -> EvalResult<Gc<ErrorObject>> {
        let mut object = cx.heap.alloc_uninit::<ErrorObject>();
        object._vtable = Self::VTABLE;

        maybe!(object_ordinary_init_from_constructor(
            cx,
            object.object_mut(),
            constructor,
            Intrinsic::ErrorPrototype
        ));

        object.into()
    }
}

#[wrap_ordinary_object]
impl Object for ErrorObject {
    fn is_error(&self) -> bool {
        true
    }
}

pub struct ErrorConstructor;

impl ErrorConstructor {
    // 20.5.2 Properties of the Error Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            &cx.names.error(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            &cx.names.prototype(),
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
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            cx.current_execution_context().function.unwrap()
        };

        let object: Gc<ObjectValue> =
            maybe!(ErrorObject::new_from_constructor(cx, new_target)).into();

        let message = get_argument(arguments, 0);
        if !message.is_undefined() {
            let message_string = maybe!(to_string(cx, message));
            create_non_enumerable_data_property_or_throw(
                cx,
                object,
                &cx.names.message(),
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
    object: Gc<ObjectValue>,
    options: Value,
) -> EvalResult<()> {
    if options.is_object() {
        let options = options.as_object();
        if maybe!(has_property(cx, options, &cx.names.cause())) {
            let cause = maybe!(get(cx, options, &cx.names.cause()));
            create_non_enumerable_data_property_or_throw(cx, object, &cx.names.cause(), cause);
        }
    }

    ().into()
}
