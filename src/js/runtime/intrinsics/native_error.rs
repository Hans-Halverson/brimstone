use crate::runtime::{
    abstract_operations::{construct, create_non_enumerable_data_property_or_throw},
    builtin_function::BuiltinFunction,
    eval_result::EvalResult,
    function::get_argument,
    intrinsics::{
        error_constructor::{install_error_cause, ErrorObject},
        intrinsics::Intrinsic,
    },
    object_value::ObjectValue,
    realm::Realm,
    type_utilities::to_string,
    Context, Handle, HeapPtr, Value,
};

macro_rules! create_native_error {
    ($native_error:ident, $rust_name:ident, $prototype:ident, $constructor:ident) => {
        pub struct $native_error;

        impl $native_error {
            #[allow(dead_code)]
            pub fn new_with_message(mut cx: Context, message: String) -> Handle<ErrorObject> {
                // Be sure to allocate before creating object
                let message_value = cx.alloc_string(&message).into();

                let object = ErrorObject::new(
                    cx,
                    Intrinsic::$prototype,
                    /* skip_current_frame */ false,
                );

                object
                    .as_object()
                    .intrinsic_data_prop(cx, cx.names.message(), message_value);

                object
            }

            #[allow(dead_code)]
            pub fn new_with_message_in_realm(
                mut cx: Context,
                realm: HeapPtr<Realm>,
                message: &str,
            ) -> EvalResult<Handle<ObjectValue>> {
                let type_error_constructor = realm.get_intrinsic(Intrinsic::$constructor);
                let message_value = cx.alloc_string(message).into();
                construct(cx, type_error_constructor, &[message_value], None)
            }
        }

        pub struct $constructor;

        impl $constructor {
            /// Properties of the NativeError Constructors (https://tc39.es/ecma262/#sec-properties-of-the-nativeerror-constructors)
            pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
                let mut func = BuiltinFunction::intrinsic_constructor(
                    cx,
                    Self::construct,
                    1,
                    cx.names.$rust_name(),
                    realm,
                    Intrinsic::ErrorConstructor,
                );

                func.intrinsic_frozen_property(
                    cx,
                    cx.names.prototype(),
                    realm.get_intrinsic(Intrinsic::$prototype).into(),
                );

                func
            }

            /// NativeError (https://tc39.es/ecma262/#sec-nativeerror)
            pub fn construct(
                mut cx: Context,
                _: Handle<Value>,
                arguments: &[Handle<Value>],
            ) -> EvalResult<Handle<Value>> {
                let new_target = if let Some(new_target) = cx.current_new_target() {
                    new_target
                } else {
                    cx.current_function()
                };

                let object = ErrorObject::new_from_constructor(
                    cx,
                    new_target,
                    Intrinsic::$prototype,
                    /* skip_current_frame */ true,
                )?;

                let message = get_argument(cx, arguments, 0);
                if !message.is_undefined() {
                    let message_string = to_string(cx, message)?;
                    create_non_enumerable_data_property_or_throw(
                        cx,
                        object.into(),
                        cx.names.message(),
                        message_string.into(),
                    );
                }

                let options_arg = get_argument(cx, arguments, 1);
                install_error_cause(cx, object, options_arg)?;

                Ok(object.as_value())
            }
        }

        pub struct $prototype;

        impl $prototype {
            /// Properties of the NativeError Prototype Objects (https://tc39.es/ecma262/#sec-properties-of-the-nativeerror-prototype-objects)
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
