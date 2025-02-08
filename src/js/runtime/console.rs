use crate::js::common::{
    error::{ErrorFormatter, FormatOptions, SourceInfo},
    terminal::stdout_should_use_colors,
};

use super::{
    eval_result::EvalResult,
    intrinsics::{
        error_constructor::{CachedStackTraceInfo, ErrorObject},
        error_prototype::{error_message, error_name},
        intrinsics::Intrinsic,
    },
    object_descriptor::ObjectKind,
    object_value::ObjectValue,
    realm::Realm,
    type_utilities::number_to_string,
    value::{BOOL_TAG, NULL_TAG, UNDEFINED_TAG},
    Context, Handle, Value,
};

pub struct ConsoleObject;

impl ConsoleObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        object.intrinsic_func(cx, cx.names.log(), Self::log, 0, realm);

        object.to_handle()
    }

    pub fn log(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let use_colors = stdout_should_use_colors(&cx.options);
        let opts = FormatOptions::new(use_colors);

        let formatted = arguments
            .iter()
            .map(|argument| to_console_string(cx, *argument, &opts))
            .collect::<Vec<String>>()
            .join(" ");

        println!("{}", formatted);

        Ok(cx.undefined())
    }
}

/// Format for printing value to console
pub fn to_console_string(cx: Context, value: Handle<Value>, opts: &FormatOptions) -> String {
    if value.is_pointer() {
        match value.as_pointer().descriptor().kind() {
            ObjectKind::String => format!("{}", value.as_string()),
            ObjectKind::Symbol => match value.as_symbol().description_ptr() {
                None => String::from("Symbol()"),
                Some(description) => format!("Symbol({})", description),
            },
            ObjectKind::BigInt => format!("{}n", value.as_bigint().bigint()),
            // Otherwise must be an object
            _ => {
                let object = value.as_object();

                if let Some(error) = object.as_error() {
                    error_to_console_string(cx, error, opts)
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
    }
}

fn error_to_console_string(
    cx: Context,
    mut error: Handle<ErrorObject>,
    opts: &FormatOptions,
) -> String {
    let name = error_name(cx, error).to_string();
    let mut formatter = ErrorFormatter::new(name, opts);

    if let Some(message) = error_message(cx, error) {
        formatter.set_message(message);
    }

    let stack_trace = error.get_stack_trace(cx);
    formatter.set_stack_trace(stack_trace.frames.to_string());

    if let Some(source_info) = new_heap_source_info(cx, &stack_trace) {
        formatter.set_source_info(source_info);
    }

    formatter.build()
}

fn new_heap_source_info(
    cx: Context,
    stack_trace_info: &CachedStackTraceInfo,
) -> Option<SourceInfo> {
    let (mut source_file, line, col) =
        if let Some((source_file, line, col)) = &stack_trace_info.source_file_line_col {
            (source_file.to_handle(), *line, *col)
        } else {
            return None;
        };

    let name = source_file.display_name().to_string();
    let snippet = source_file.get_line(cx, line - 1);

    Some(SourceInfo::new(name, line, col, snippet))
}
