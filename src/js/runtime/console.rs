use super::{
    abstract_operations::get,
    completion::EvalResult,
    gc::Gc,
    intrinsics::intrinsics::Intrinsic,
    object_value::ObjectValue,
    realm::Realm,
    type_utilities::number_to_string,
    value::{Value, BIGINT_TAG, BOOL_TAG, NULL_TAG, STRING_TAG, SYMBOL_TAG, UNDEFINED_TAG},
    Context,
};

pub struct ConsoleObject;

impl ConsoleObject {
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        object.intrinsic_func(cx, &cx.names.log(), Self::log, 0, realm);

        object.into()
    }

    fn log(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let formatted = arguments
            .iter()
            .map(|argument| to_console_string(cx, *argument))
            .collect::<Vec<String>>()
            .join(" ");

        println!("{}", formatted);
        return Value::undefined().into();
    }
}

/// Format for printing value to console
pub fn to_console_string(cx: &mut Context, value: Value) -> String {
    if value.is_number() {
        return number_to_string(value.as_number());
    }

    match value.get_tag() {
        NULL_TAG => return "null".to_owned(),
        UNDEFINED_TAG => return "undefined".to_owned(),
        STRING_TAG => return format!("{}", value.as_string()),
        BOOL_TAG => {
            return if value.as_bool() {
                "true".to_owned()
            } else {
                "false".to_owned()
            }
        }
        SYMBOL_TAG => {
            return match value.as_symbol().description() {
                None => String::from("Symbol()"),
                Some(description) => format!("Symbol({})", description),
            };
        }
        BIGINT_TAG => {
            return format!("{}n", value.as_bigint().bigint().to_string());
        }
        _ => {}
    };

    // Value must be an object
    let object = value.as_object();

    if object.is_error() {
        let name = match get(cx, object, &cx.names.name()) {
            EvalResult::Ok(name_value) if name_value.is_string() => name_value.as_string(),
            _ => cx.names.error.as_string(),
        };

        match get(cx, object, &cx.names.message()) {
            EvalResult::Ok(message_value) => {
                format!("{}: {}", name, to_console_string(cx, message_value))
            }
            EvalResult::Throw(_) => format!("{}", name),
        }
    } else if object.is_callable() {
        "[Function]".to_owned()
    } else {
        "[Object]".to_owned()
    }
}
