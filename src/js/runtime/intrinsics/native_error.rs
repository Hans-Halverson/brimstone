use crate::{
    js::runtime::{
        abstract_operations::create_non_enumerable_data_property_or_throw,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        gc::HandleValue,
        intrinsics::error_constructor::install_error_cause,
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{object_create, object_create_from_constructor},
        property::Property,
        realm::Realm,
        type_utilities::to_string,
        Context, Handle,
    },
    maybe,
};

macro_rules! create_native_error {
    ($native_error:ident, $rust_name:ident, $prototype:ident, $constructor:ident) => {
        pub struct $native_error;

        impl $native_error {
            #[allow(dead_code)]
            pub fn new_with_message(cx: &mut Context, message: String) -> Handle<ObjectValue> {
                // Be sure to allocate before creating object
                let message_value = cx.alloc_string(message).into();

                let object = object_create::<ObjectValue>(
                    cx,
                    ObjectKind::ErrorObject,
                    Intrinsic::$prototype,
                );

                let mut object = object.to_handle();

                object.intrinsic_data_prop(cx, cx.names.message(), message_value);

                object
            }

            pub fn new_from_constructor(
                cx: &mut Context,
                constructor: Handle<ObjectValue>,
            ) -> EvalResult<Handle<ObjectValue>> {
                let object = maybe!(object_create_from_constructor::<ObjectValue>(
                    cx,
                    constructor,
                    ObjectKind::ErrorObject,
                    Intrinsic::$prototype
                ));

                object.to_handle().into()
            }
        }

        pub struct $constructor;

        impl $constructor {
            // 20.5.6.2 Properties of the NativeError Constructors
            pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<BuiltinFunction> {
                let error_constructor = realm.get_intrinsic(Intrinsic::ErrorConstructor);
                let mut func = BuiltinFunction::create(
                    cx,
                    Self::construct,
                    1,
                    cx.names.$rust_name(),
                    Some(realm),
                    Some(error_constructor),
                    None,
                );

                func.set_is_constructor();
                func.set_property(
                    cx,
                    cx.names.prototype(),
                    Property::data(
                        realm.get_intrinsic(Intrinsic::$prototype).into(),
                        false,
                        false,
                        false,
                    ),
                );

                func
            }

            // 20.5.6.1.1 NativeError
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

                let object = maybe!($native_error::new_from_constructor(cx, new_target));

                let message = get_argument(cx, arguments, 0);
                if !message.is_undefined() {
                    let message_string = maybe!(to_string(cx, message));
                    create_non_enumerable_data_property_or_throw(
                        cx,
                        object,
                        cx.names.message(),
                        message_string.into(),
                    );
                }

                let options_arg = get_argument(cx, arguments, 1);
                maybe!(install_error_cause(cx, object, options_arg));

                object.into()
            }
        }

        pub struct $prototype;

        impl $prototype {
            // 20.5.6.3 Properties of the NativeError Prototype Objects
            pub fn new(cx: &mut Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
                let proto = realm.get_intrinsic(Intrinsic::ErrorPrototype);
                let mut object = ObjectValue::new(cx, Some(proto), true);

                // Constructor property is added once NativeErrorConstructor has been created
                object.intrinsic_name_prop(cx, stringify!($native_error));
                object.intrinsic_data_prop(
                    cx,
                    cx.names.message(),
                    cx.names.empty_string().as_string().into(),
                );

                object
            }
        }
    };
}

create_native_error!(EvalError, eval_error, EvalErrorPrototype, EvalErrorConstructor);
create_native_error!(RangeError, range_error, RangeErrorPrototype, RangeErrorConstructor);
create_native_error!(
    ReferenceError,
    reference_error,
    ReferenceErrorPrototype,
    ReferenceErrorConstructor
);
create_native_error!(SyntaxError, syntax_error, SyntaxErrorPrototype, SyntaxErrorConstructor);
create_native_error!(TypeError, type_error, TypeErrorPrototype, TypeErrorConstructor);
create_native_error!(URIError, uri_error, URIErrorPrototype, URIErrorConstructor);
