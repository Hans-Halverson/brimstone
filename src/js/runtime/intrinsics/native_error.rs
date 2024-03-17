use crate::{
    js::runtime::{
        abstract_operations::create_non_enumerable_data_property_or_throw,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        intrinsics::error_constructor::{install_error_cause, ErrorObject},
        intrinsics::intrinsics::Intrinsic,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{object_create, object_create_from_constructor},
        realm::Realm,
        type_utilities::to_string,
        Context, Handle, Value,
    },
    maybe,
};

macro_rules! create_native_error {
    ($native_error:ident, $rust_name:ident, $prototype:ident, $constructor:ident) => {
        pub struct $native_error;

        impl $native_error {
            #[allow(dead_code)]
            pub fn new_with_message(mut cx: Context, message: String) -> Handle<ErrorObject> {
                // Be sure to allocate before creating object
                let message_value = cx.alloc_string(&message).into();

                let object = object_create::<ErrorObject>(
                    cx,
                    ObjectKind::ErrorObject,
                    Intrinsic::$prototype,
                );

                let object = object.to_handle();

                object
                    .object()
                    .intrinsic_data_prop(cx, cx.names.message(), message_value);

                object
            }

            pub fn new_from_constructor(
                cx: Context,
                constructor: Handle<ObjectValue>,
            ) -> EvalResult<Handle<ErrorObject>> {
                let object = maybe!(object_create_from_constructor::<ErrorObject>(
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
            pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
                let error_constructor = realm.get_intrinsic(Intrinsic::ErrorConstructor);
                let mut func = BuiltinFunction::intrinsic_constructor(
                    cx,
                    Self::construct,
                    1,
                    cx.names.$rust_name(),
                    realm,
                    Some(error_constructor),
                );

                func.intrinsic_frozen_property(
                    cx,
                    cx.names.prototype(),
                    realm.get_intrinsic(Intrinsic::$prototype).into(),
                );

                func
            }

            // 20.5.6.1.1 NativeError
            pub fn construct(
                mut cx: Context,
                _: Handle<Value>,
                arguments: &[Handle<Value>],
                new_target: Option<Handle<ObjectValue>>,
            ) -> EvalResult<Handle<Value>> {
                let new_target = if let Some(new_target) = new_target {
                    new_target
                } else {
                    cx.current_function()
                };

                let object = maybe!($native_error::new_from_constructor(cx, new_target));

                let message = get_argument(cx, arguments, 0);
                if !message.is_undefined() {
                    let message_string = maybe!(to_string(cx, message));
                    create_non_enumerable_data_property_or_throw(
                        cx,
                        object.into(),
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
            pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
                let proto = realm.get_intrinsic(Intrinsic::ErrorPrototype);
                let mut object = ObjectValue::new(cx, Some(proto), true);

                // Constructor property is added once NativeErrorConstructor has been created
                object.intrinsic_data_prop(
                    cx,
                    cx.names.name(),
                    cx.names.$rust_name().as_string().into(),
                );
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
