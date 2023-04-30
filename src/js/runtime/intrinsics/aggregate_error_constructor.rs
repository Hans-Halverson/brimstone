use crate::{
    js::runtime::{
        abstract_operations::{
            create_non_enumerable_data_property_or_throw, define_property_or_throw, get,
            has_property,
        },
        array_object::create_array_from_list,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        function::get_argument,
        gc::Gc,
        iterator::iter_iterator_values,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_ordinary_init_from_constructor,
        property::Property,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        type_utilities::to_string,
        value::Value,
        Context,
    },
    maybe, must,
};

use super::intrinsics::Intrinsic;

// 20.5.7 AggregateError Objects
pub struct AggregateErrorObject;

impl AggregateErrorObject {
    fn new_from_constructor(
        cx: &mut Context,
        constructor: Gc<ObjectValue>,
    ) -> EvalResult<Gc<ObjectValue>> {
        let object = cx.heap.alloc_uninit::<ObjectValue>();
        maybe!(object_ordinary_init_from_constructor(
            cx,
            object.object(),
            constructor,
            ObjectKind::ErrorObject,
            Intrinsic::AggregateErrorPrototype
        ));

        object.into()
    }
}

pub struct AggregateErrorConstructor;

impl AggregateErrorConstructor {
    // 20.5.7.2 Properties of the AggregateError Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
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

    // 20.5.7.1.1 AggregateError
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let new_target = if let Some(new_target) = new_target {
            new_target
        } else {
            cx.current_execution_context().function().unwrap()
        };

        let object: Gc<ObjectValue> =
            maybe!(AggregateErrorObject::new_from_constructor(cx, new_target)).into();

        let errors = get_argument(arguments, 0);
        let message = get_argument(arguments, 1);
        if !message.is_undefined() {
            let message_string = maybe!(to_string(cx, message));
            create_non_enumerable_data_property_or_throw(
                cx,
                object,
                cx.names.message(),
                message_string.into(),
            );
        }

        maybe!(install_error_cause(cx, object, get_argument(arguments, 2)));

        // Collect errors in iterable and create a new array containing all errors
        let mut errors_list = vec![];
        let completion = iter_iterator_values(cx, errors, &mut |_, value| {
            errors_list.push(value);
            None
        });

        maybe!(completion.into_eval_result());

        let errors_array: Gc<ObjectValue> = create_array_from_list(cx, &errors_list).into();

        let errors_desc = PropertyDescriptor::data(errors_array.into(), true, false, true);
        must!(define_property_or_throw(cx, object, cx.names.errors(), errors_desc));

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
        if maybe!(has_property(cx, options, cx.names.cause())) {
            let cause = maybe!(get(cx, options, cx.names.cause()));
            create_non_enumerable_data_property_or_throw(cx, object, cx.names.cause(), cause);
        }
    }

    ().into()
}
