use super::{
    abstract_operations::get,
    completion::EvalResult,
    intrinsics::intrinsics::Intrinsic,
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
        let formatted = arguments
            .iter()
            .map(|argument| to_console_string(cx, *argument))
            .collect::<Vec<String>>()
            .join(" ");

        println!("{}", formatted);

        Ok(cx.undefined())
    }
}

/// Format for printing value to console
pub fn to_console_string(cx: Context, value: Handle<Value>) -> String {
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

                if object.is_error() {
                    match get(cx, object, cx.names.stack()) {
                        // Try to use the stack property if it exists and is a string
                        Ok(stack_value) if stack_value.is_string() => {
                            stack_value.as_string().to_string()
                        }
                        // Otherwise use default one line error formatting
                        _ => format_error_one_line(cx, object),
                    }
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

/// Format an error object into a one line string containing name and message
pub fn format_error_one_line(cx: Context, object: Handle<ObjectValue>) -> String {
    let name = match get(cx, object, cx.names.name()) {
        Ok(name_value) if name_value.is_string() => name_value.as_string(),
        _ => cx.names.error().as_string(),
    };

    match get(cx, object, cx.names.message()) {
        Ok(message_value) => {
            format!("{}: {}", name, to_console_string(cx, message_value))
        }
        Err(_) => format!("{}", name),
    }
}
