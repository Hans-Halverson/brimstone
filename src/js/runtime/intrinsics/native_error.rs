use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        abstract_operations::create_non_enumerable_data_property_or_throw,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        gc::Gc,
        intrinsics::error_constructor::install_error_cause,
        object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
        ordinary_object::{ordinary_create_from_constructor, OrdinaryObject},
        property::Property,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        type_utilities::to_string,
        value::Value,
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

macro_rules! create_native_error {
    ($native_error:ident, $prototype:ident, $constructor:ident) => {
        #[repr(C)]
        pub struct $native_error {
            _vtable: ObjectValueVtable,
            object: OrdinaryObject,
        }

        impl_gc_into!($native_error, ObjectValue);

        impl $native_error {
            const VTABLE: *const () = extract_object_vtable::<$native_error>();

            fn new(object: OrdinaryObject) -> $native_error {
                $native_error {
                    _vtable: Self::VTABLE,
                    object,
                }
            }

            pub fn new_with_message(cx: &mut Context, message: String) -> Gc<$native_error> {
                let prototype = cx.current_realm().get_intrinsic(Intrinsic::$prototype);
                let mut object = OrdinaryObject::new(Some(prototype), true);

                object.intrinsic_data_prop("message", cx.heap.alloc_string(message).into());

                cx.heap.alloc(Self::new(object))
            }

            #[inline]
            fn object(&self) -> &OrdinaryObject {
                &self.object
            }

            #[inline]
            fn object_mut(&mut self) -> &mut OrdinaryObject {
                &mut self.object
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
                    stringify!($native_error),
                    Some(realm),
                    Some(error_constructor),
                    None,
                );

                func.set_is_constructor();
                func.set_property(
                    "prototype".to_owned(),
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

                let ordinary_object = maybe!(ordinary_create_from_constructor(
                    cx,
                    new_target,
                    Intrinsic::$prototype
                ));
                let object: Gc<ObjectValue> =
                    cx.heap.alloc($native_error::new(ordinary_object)).into();

                let message = get_argument(arguments, 0);
                if !message.is_undefined() {
                    let message_string = maybe!(to_string(cx, message));
                    create_non_enumerable_data_property_or_throw(
                        cx,
                        object,
                        "message",
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
                let mut object =
                    OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ErrorPrototype)), true);

                // Constructor property is added once NativeErrorConstructor has been created
                object.intrinsic_name_prop(cx, stringify!($native_error));
                object.intrinsic_data_prop("message", cx.heap.alloc_string(String::new()).into());

                cx.heap.alloc(object).into()
            }
        }
    };
}

create_native_error!(EvalError, EvalErrorPrototype, EvalErrorConstructor);
create_native_error!(RangeError, RangeErrorPrototype, RangeErrorConstructor);
create_native_error!(
    ReferenceError,
    ReferenceErrorPrototype,
    ReferenceErrorConstructor
);
create_native_error!(SyntaxError, SyntaxErrorPrototype, SyntaxErrorConstructor);
create_native_error!(TypeError, TypeErrorPrototype, TypeErrorConstructor);
create_native_error!(URIError, URIErrorPrototype, URIErrorConstructor);
