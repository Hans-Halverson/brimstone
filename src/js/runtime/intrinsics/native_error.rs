use crate::runtime::{
    Context, Handle, HeapPtr,
    abstract_operations::{construct, create_non_enumerable_data_property_or_throw},
    alloc_error::AllocResult,
    common_shapes::CommonShape,
    eval_result::EvalResult,
    intrinsic_builder::IntrinsicBuilder,
    intrinsics::{
        error_constructor::install_error_cause, error_object::ErrorObject, intrinsics::Intrinsic,
        rust_runtime::RuntimeFunction,
    },
    object_value::ObjectValue,
    realm::Realm,
    string_value::StringValue,
    type_utilities::to_string,
};

macro_rules! create_native_error {
    ($native_error:ident, $rust_name:ident, $prototype:ident, $constructor:ident, $construct_fn:expr) => {
        pub struct $native_error;

        impl $native_error {
            #[allow(dead_code)]
            pub fn new_with_message(
                mut cx: Context,
                message: String,
            ) -> EvalResult<Handle<ErrorObject>> {
                // Be sure to allocate before creating object
                let message_value = cx.alloc_string(&message)?;
                Ok(Self::new_with_message_value(cx, message_value)?)
            }

            #[allow(dead_code)]
            pub fn new_with_message_value(
                cx: Context,
                message: Handle<StringValue>,
            ) -> AllocResult<Handle<ErrorObject>> {
                ErrorObject::new_with_message(
                    cx,
                    CommonShape::$native_error,
                    message,
                    /* skip_current_frame */ false,
                )
            }

            #[allow(dead_code)]
            pub fn new_with_message_in_realm(
                mut cx: Context,
                realm: HeapPtr<Realm>,
                message: &str,
            ) -> EvalResult<Handle<ObjectValue>> {
                let type_error_constructor = realm.get_intrinsic(Intrinsic::$constructor);
                let message_value = cx.alloc_string(message)?.into();
                construct(cx, type_error_constructor, &[message_value], None)
            }
        }

        pub struct $constructor;

        impl $constructor {
            /// Properties of the NativeError Constructors (https://tc39.es/ecma262/#sec-properties-of-the-nativeerror-constructors)
            pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
                let mut builder = IntrinsicBuilder::constructor(
                    cx,
                    realm,
                    $construct_fn,
                    1,
                    cx.names.$rust_name(),
                    Intrinsic::ErrorConstructor,
                )?;

                builder.prototype(Intrinsic::$prototype)?;

                builder.build()
            }

            $crate::runtime_fn! {
            /// NativeError (https://tc39.es/ecma262/#sec-nativeerror)
            fn construct(cx, _, arguments) {
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

                let message = arguments.get(cx, 0);
                if !message.is_undefined() {
                    let message_string = to_string(cx, message)?;
                    create_non_enumerable_data_property_or_throw(
                        cx,
                        object.into(),
                        cx.names.message(),
                        message_string.into(),
                    )?;
                }

                let options_arg = arguments.get(cx, 1);
                install_error_cause(cx, object, options_arg)?;

                Ok(object.as_value())
            }}
        }

        pub struct $prototype;

        impl $prototype {
            /// Properties of the NativeError Prototype Objects (https://tc39.es/ecma262/#sec-properties-of-the-nativeerror-prototype-objects)
            pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
                let mut builder =
                    IntrinsicBuilder::new_object(cx, realm, Intrinsic::ErrorPrototype)?;

                // Constructor property is added once NativeErrorConstructor has been created
                builder.data(cx.names.name(), cx.names.$rust_name().as_string().into())?;
                builder.data(cx.names.message(), cx.names.empty_string().as_string().into())?;

                builder.build()
            }
        }
    };
}

create_native_error!(
    EvalError,
    eval_error,
    EvalErrorPrototype,
    EvalErrorConstructor,
    RuntimeFunction::EvalErrorConstructor_construct
);
create_native_error!(
    RangeError,
    range_error,
    RangeErrorPrototype,
    RangeErrorConstructor,
    RuntimeFunction::RangeErrorConstructor_construct
);
create_native_error!(
    ReferenceError,
    reference_error,
    ReferenceErrorPrototype,
    ReferenceErrorConstructor,
    RuntimeFunction::ReferenceErrorConstructor_construct
);
create_native_error!(
    SyntaxError,
    syntax_error,
    SyntaxErrorPrototype,
    SyntaxErrorConstructor,
    RuntimeFunction::SyntaxErrorConstructor_construct
);
create_native_error!(
    TypeError,
    type_error,
    TypeErrorPrototype,
    TypeErrorConstructor,
    RuntimeFunction::TypeErrorConstructor_construct
);
create_native_error!(
    URIError,
    uri_error,
    URIErrorPrototype,
    URIErrorConstructor,
    RuntimeFunction::URIErrorConstructor_construct
);
