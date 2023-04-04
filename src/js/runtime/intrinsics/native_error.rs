use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object,
    js::runtime::{
        abstract_operations::create_non_enumerable_data_property_or_throw,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        function::get_argument,
        gc::Gc,
        intrinsics::error_constructor::install_error_cause,
        object_descriptor::ObjectKind,
        object_value::{HasObject, Object, ObjectValue},
        ordinary_object::{
            object_ordinary_init, object_ordinary_init_from_constructor, OrdinaryObject,
        },
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

macro_rules! create_native_error {
    ($native_error:ident, $rust_name:ident, $prototype:ident, $constructor:ident) => {
        extend_object! {
            pub struct $native_error {}
        }

        impl $native_error {
            pub fn new_with_message(cx: &mut Context, message: String) -> Gc<$native_error> {
                let prototype = cx.current_realm().get_intrinsic(Intrinsic::$prototype);

                let mut object = cx.heap.alloc_uninit::<$native_error>();
                object.descriptor = cx.base_descriptors.get(ObjectKind::ErrorObject);

                object_ordinary_init(object.object_mut(), prototype);

                object
                    .object_mut()
                    .intrinsic_data_prop(&cx.names.message(), cx.heap.alloc_string(message).into());

                object
            }

            pub fn new_from_constructor(
                cx: &mut Context,
                constructor: Gc<ObjectValue>,
            ) -> EvalResult<Gc<$native_error>> {
                let mut object = cx.heap.alloc_uninit::<$native_error>();
                object.descriptor = cx.base_descriptors.get(ObjectKind::ErrorObject);

                maybe!(object_ordinary_init_from_constructor(
                    cx,
                    object.object_mut(),
                    constructor,
                    Intrinsic::$prototype
                ));

                object.into()
            }
        }

        #[wrap_ordinary_object]
        impl Object for $native_error {
            fn is_error(&self) -> bool {
                true
            }
        }

        pub struct $constructor;

        impl $constructor {
            // 20.5.6.2 Properties of the NativeError Constructors
            pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
                let error_constructor = realm.get_intrinsic(Intrinsic::ErrorConstructor);
                let mut func = BuiltinFunction::create(
                    cx,
                    Self::construct,
                    1,
                    &cx.names.$rust_name(),
                    Some(realm),
                    Some(error_constructor),
                    None,
                );

                func.set_is_constructor();
                func.set_property(
                    &cx.names.prototype(),
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
                    maybe!($native_error::new_from_constructor(cx, new_target)).into();

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

        pub struct $prototype;

        impl $prototype {
            // 20.5.6.3 Properties of the NativeError Prototype Objects
            pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
                let proto = realm.get_intrinsic(Intrinsic::ErrorPrototype);
                let mut object = OrdinaryObject::new(cx, Some(proto), true);

                // Constructor property is added once NativeErrorConstructor has been created
                object.intrinsic_name_prop(cx, stringify!($native_error));
                object.intrinsic_data_prop(
                    &cx.names.message(),
                    cx.names.empty_string().as_string().into(),
                );

                object.into()
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
