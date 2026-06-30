use crate::{
    common::error::SourceInfo,
    intrinsic_methods,
    runtime::{
        Context, Handle, Value,
        abstract_operations::{create_non_enumerable_data_property_or_throw, get, has_property},
        alloc_error::AllocResult,
        eval_result::EvalResult,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            error_object::{CachedStackTraceInfo, ErrorObject},
            intrinsics::Intrinsic,
            rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::to_string,
    },
    runtime_fn,
};

pub struct ErrorConstructor;

impl ErrorConstructor {
    /// Properties of the Error Constructor (https://tc39.es/ecma262/#sec-properties-of-the-error-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::ErrorConstructor_construct,
            1,
            cx.names.error(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::ErrorPrototype)?;

        intrinsic_methods!(cx, builder, {
            is_error ErrorConstructor_is_error (1),
        });

        builder.build()
    }

    runtime_fn! {
    /// Error (https://tc39.es/ecma262/#sec-error-message)
    fn construct(cx, _, arguments) {
        let new_target = if let Some(new_target) = cx.current_new_target() {
            new_target
        } else {
            cx.current_function()
        };

        let object = ErrorObject::new_from_constructor(
            cx,
            new_target,
            Intrinsic::ErrorPrototype,
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

    runtime_fn! {
    /// Error.isError (https://tc39.es/ecma262/#sec-error.iserror)
    fn is_error(cx, _, arguments) {
        let arg = arguments.get(cx, 0);

        if !arg.is_object() {
            return Ok(cx.bool(false));
        }

        Ok(cx.bool(arg.is::<ErrorObject>()))
    }}
}

/// InstallErrorCause (https://tc39.es/ecma262/#sec-installerrorcause)
pub fn install_error_cause(
    cx: Context,
    object: Handle<ErrorObject>,
    options: Handle<Value>,
) -> EvalResult<()> {
    if options.is_object() {
        let options = options.as_object();
        if has_property(cx, options, cx.names.cause())? {
            let cause = get(cx, options, cx.names.cause())?;
            create_non_enumerable_data_property_or_throw(
                cx,
                object.into(),
                cx.names.cause(),
                cause,
            )?;
        }
    }

    Ok(())
}

pub fn new_heap_source_info(
    cx: Context,
    stack_trace_info: &CachedStackTraceInfo,
) -> AllocResult<Option<SourceInfo>> {
    let (mut source_file, line, col) =
        if let Some((source_file, line, col)) = &stack_trace_info.source_file_line_col {
            (source_file.to_handle(), *line, *col)
        } else {
            return Ok(None);
        };

    let name = source_file.display_name().to_string();
    let snippet = source_file.get_line(cx, line - 1)?;

    Ok(Some(SourceInfo::new(name, line, col, snippet)))
}
