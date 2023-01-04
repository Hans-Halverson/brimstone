use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        abstract_operations::{create_non_enumerable_data_property_or_throw, get, has_property},
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        gc::Gc,
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

#[repr(C)]
pub struct ErrorObject {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
}

impl_gc_into!(ErrorObject, ObjectValue);

impl ErrorObject {
    const VTABLE: *const () = extract_object_vtable::<ErrorObject>();

    fn new(object: OrdinaryObject) -> ErrorObject {
        ErrorObject {
            _vtable: Self::VTABLE,
            object,
        }
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
impl Object for ErrorObject {
    fn is_error(&self) -> bool {
        true
    }
}

pub struct ErrorConstructor;

impl ErrorConstructor {
    // 20.5.2 Properties of the Error Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func =
            BuiltinFunction::create(cx, Self::construct, 1, "Error", Some(realm), None, None);

        func.set_is_constructor();
        func.set_property(
            "prototype".to_owned(),
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

        let ordinary_object = maybe!(ordinary_create_from_constructor(
            cx,
            new_target,
            Intrinsic::ErrorPrototype
        ));
        let object: Gc<ObjectValue> = cx.heap.alloc(ErrorObject::new(ordinary_object)).into();

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

// 20.5.8.1 InstallErrorCause
pub fn install_error_cause(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    options: Value,
) -> EvalResult<()> {
    if options.is_object() {
        let options = options.as_object();
        if maybe!(has_property(options, "cause")) {
            let cause = maybe!(get(cx, options, "cause"));
            create_non_enumerable_data_property_or_throw(cx, object, "cause", cause);
        }
    }

    ().into()
}
