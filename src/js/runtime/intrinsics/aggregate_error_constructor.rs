use crate::{
    must,
    runtime::{
        Context, Handle,
        abstract_operations::{
            create_non_enumerable_data_property_or_throw, define_property_or_throw,
        },
        alloc_error::AllocResult,
        array_object::create_array_from_list,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            error_constructor::install_error_cause, error_object::ErrorObject,
            intrinsics::Intrinsic, rust_runtime::RuntimeFunction,
        },
        iterator::iter_iterator_values,
        object_value::ObjectValue,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        type_utilities::to_string,
    },
    runtime_fn,
};

pub struct AggregateErrorConstructor;

impl AggregateErrorConstructor {
    /// Properties of the AggregateError Constructor (https://tc39.es/ecma262/#sec-properties-of-the-aggregate-error-constructors)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::AggregateErrorConstructor_construct,
            2,
            cx.names.aggregate_error(),
            Intrinsic::ErrorConstructor,
        )?;

        builder.prototype(Intrinsic::AggregateErrorPrototype)?;

        builder.build()
    }

    runtime_fn! {
    /// AggregateError (https://tc39.es/ecma262/#sec-aggregate-error)
    fn construct(cx, _, arguments) {
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

        let errors = arguments.get(cx, 0);
        let message = arguments.get(cx, 1);
        if !message.is_undefined() {
            let message_string = to_string(cx, message)?;
            create_non_enumerable_data_property_or_throw(
                cx,
                object.into(),
                cx.names.message(),
                message_string.into(),
            )?;
        }

        let options_arg = arguments.get(cx, 2);
        install_error_cause(cx, object, options_arg)?;

        // Collect errors in iterable and create a new array containing all errors
        let mut errors_list = vec![];
        iter_iterator_values(cx, errors, &mut |_, value| {
            errors_list.push(value);
            None
        })?;

        let errors_array = create_array_from_list(cx, &errors_list)?.as_object();

        let errors_desc = PropertyDescriptor::non_enumerable_data(errors_array.into());
        must!(define_property_or_throw(cx, object.into(), cx.names.errors(), errors_desc));

        Ok(object.as_value())
    }}
}
