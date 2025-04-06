use crate::{
    must,
    runtime::{
        abstract_operations::{
            create_data_property_or_throw, create_non_enumerable_data_property_or_throw,
            define_property_or_throw,
        },
        array_object::create_array_from_list,
        builtin_function::BuiltinFunction,
        eval_result::EvalResult,
        function::get_argument,
        intrinsics::error_constructor::install_error_cause,
        iterator::iter_iterator_values,
        object_value::ObjectValue,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        type_utilities::to_string,
        Context, Handle, Value,
    },
};

use super::{error_constructor::ErrorObject, intrinsics::Intrinsic};

/// AggregateError Objects (https://tc39.es/ecma262/#sec-aggregate-error-objects)
pub struct AggregateErrorObject;

impl AggregateErrorObject {
    pub fn new(cx: Context, errors: Handle<Value>) -> Handle<ErrorObject> {
        let object = ErrorObject::new(
            cx,
            Intrinsic::AggregateErrorPrototype,
            /* skip_current_frame */ true,
        );

        must!(create_data_property_or_throw(cx, object.into(), cx.names.errors(), errors));

        object
    }
}

pub struct AggregateErrorConstructor;

impl AggregateErrorConstructor {
    /// Properties of the AggregateError Constructor (https://tc39.es/ecma262/#sec-properties-of-the-aggregate-error-constructors)
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let error_constructor = realm.get_intrinsic(Intrinsic::ErrorConstructor);
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            2,
            cx.names.aggregate_error(),
            realm,
            Some(error_constructor),
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm
                .get_intrinsic(Intrinsic::AggregateErrorPrototype)
                .into(),
        );

        func
    }

    /// AggregateError (https://tc39.es/ecma262/#sec-aggregate-error)
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
            Intrinsic::AggregateErrorPrototype,
            /* skip_current_frame */ true,
        )?;

        let errors = get_argument(cx, arguments, 0);
        let message = get_argument(cx, arguments, 1);
        if !message.is_undefined() {
            let message_string = to_string(cx, message)?;
            create_non_enumerable_data_property_or_throw(
                cx,
                object.into(),
                cx.names.message(),
                message_string.into(),
            );
        }

        let options_arg = get_argument(cx, arguments, 2);
        install_error_cause(cx, object, options_arg)?;

        // Collect errors in iterable and create a new array containing all errors
        let mut errors_list = vec![];
        iter_iterator_values(cx, errors, &mut |_, value| {
            errors_list.push(value);
            None
        })?;

        let errors_array = create_array_from_list(cx, &errors_list).as_object();

        let errors_desc = PropertyDescriptor::data(errors_array.into(), true, false, true);
        must!(define_property_or_throw(cx, object.into(), cx.names.errors(), errors_desc));

        Ok(object.as_value())
    }
}
