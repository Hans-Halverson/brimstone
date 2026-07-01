use crate::{
    common::{
        error::{ErrorFormatter, FormatOptions},
        terminal::stdout_should_use_colors,
    },
    intrinsic_methods,
    runtime::{
        Arguments, Context, Handle, HeapItemKind, Value,
        alloc_error::AllocResult,
        eval_result::EvalResult,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            error_constructor::new_heap_source_info,
            error_object::ErrorObject,
            error_prototype::{error_message, error_name},
            intrinsics::Intrinsic,
        },
        object_value::ObjectValue,
        realm::Realm,
        type_utilities::number_to_string,
        value::{BOOL_TAG, NULL_TAG, UNDEFINED_TAG},
    },
    runtime_fn,
};

enum ConsoleLogLevel {
    Debug,
    Error,
    Info,
    Log,
    Warn,
}

pub struct ConsoleObject;

impl ConsoleObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::object(cx, realm, Intrinsic::ObjectPrototype)?;

        intrinsic_methods!(cx, builder, {
            debug  ConsoleObject_debug (0),
            error_ ConsoleObject_error (0),
            info   ConsoleObject_info  (0),
            log    ConsoleObject_log   (0),
            warn   ConsoleObject_warn  (0),
        });

        builder.build()
    }

    runtime_fn! {
    fn debug(cx, _, arguments) {
        console_method_impl(cx, ConsoleLogLevel::Debug, arguments)
    }}

    runtime_fn! {
    fn error(cx, _, arguments) {
        console_method_impl(cx, ConsoleLogLevel::Error, arguments)
    }}

    runtime_fn! {
    fn info(cx, _, arguments) {
        console_method_impl(cx, ConsoleLogLevel::Info, arguments)
    }}

    runtime_fn! {
    fn log(cx, _, arguments) {
        console_method_impl(cx, ConsoleLogLevel::Log, arguments)
    }}

    runtime_fn! {
    fn warn(cx, _, arguments) {
        console_method_impl(cx, ConsoleLogLevel::Warn, arguments)
    }}
}

fn console_method_impl(
    cx: Context,
    _: ConsoleLogLevel,
    arguments: Arguments,
) -> EvalResult<Handle<Value>> {
    let use_colors = stdout_should_use_colors(&cx.options);
    let opts = FormatOptions::new(use_colors);

    let mut formatted = vec![];
    for argument in arguments.iter() {
        formatted.push(to_console_string(cx, *argument, &opts)?);
    }

    println!("{}", formatted.join(" "));

    Ok(cx.undefined())
}

/// Format for printing value to console
pub fn to_console_string(
    cx: Context,
    value: Handle<Value>,
    opts: &FormatOptions,
) -> AllocResult<String> {
    let result = if value.is_pointer() {
        match value.as_pointer().descriptor().kind() {
            HeapItemKind::StringValue => value.as_string().format()?,
            HeapItemKind::SymbolValue => match value.as_symbol().description_ptr() {
                None => String::from("Symbol()"),
                Some(description) => format!("Symbol({description})"),
            },
            HeapItemKind::BigIntValue => format!("{}n", value.as_bigint().bigint()),
            // Otherwise must be an object
            _ => {
                let object = value.as_object();

                if let Some(error) = object.as_opt::<ErrorObject>() {
                    error_to_console_string(cx, error, opts)?
                } else if object.is_callable() {
                    "[Function]".to_owned()
                } else {
                    "[Object]".to_owned()
                }
            }
        }
    } else {
        match value.get_tag() {
            NULL_TAG => "null".to_owned(),
            UNDEFINED_TAG => "undefined".to_owned(),
            BOOL_TAG => {
                if value.as_bool() {
                    "true".to_owned()
                } else {
                    "false".to_owned()
                }
            }
            // Otherwise must be a number, either a double or smi
            _ => number_to_string(value.as_number()),
        }
    };

    Ok(result)
}

fn error_to_console_string(
    cx: Context,
    mut error: Handle<ErrorObject>,
    opts: &FormatOptions,
) -> AllocResult<String> {
    let name = error_name(cx, error).format()?;
    let mut formatter = ErrorFormatter::new(name, opts);

    if let Some(message) = error_message(cx, error)? {
        formatter.set_message(message);
    }

    let stack_trace = error.get_stack_trace(cx)?;
    formatter.set_stack_trace(stack_trace.frames.to_string());

    if let Some(source_info) = new_heap_source_info(cx, &stack_trace)? {
        formatter.set_source_info(source_info);
    }

    Ok(formatter.build())
}
