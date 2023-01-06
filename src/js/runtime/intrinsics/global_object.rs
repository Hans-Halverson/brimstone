use crate::{
    js::runtime::{
        abstract_operations::define_property_or_throw, builtin_function::BuiltinFunction,
        console::ConsoleObject, function::get_argument, object_value::ObjectValue,
        property_descriptor::PropertyDescriptor, type_utilities::to_number, Context, EvalResult,
        Gc, Realm, Value,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 9.3.4 SetDefaultGlobalBindings
pub fn set_default_global_bindings(cx: &mut Context, realm: Gc<Realm>) -> EvalResult<()> {
    macro_rules! value_prop {
        ($name:expr, $value:expr, $is_writable:expr, $is_enumerable:expr, $is_configurable:expr) => {
            maybe!(define_property_or_throw(
                cx,
                realm.global_object,
                $name,
                PropertyDescriptor::data($value, $is_writable, $is_enumerable, $is_configurable)
            ));
        };
    }

    macro_rules! func_prop {
        ($str_name:expr, $func_name:expr, $length:expr) => {{
            let func_object = BuiltinFunction::create(
                cx,
                $func_name,
                $length,
                $str_name,
                Some(realm),
                None,
                None,
            )
            .into();
            value_prop!($str_name, func_object, true, false, true);
        }};
    }

    macro_rules! intrinsic_prop {
        ($name:expr, $intrinsic:ident) => {
            let value = realm.get_intrinsic(Intrinsic::$intrinsic);
            maybe!(define_property_or_throw(
                cx,
                realm.global_object,
                $name,
                PropertyDescriptor::data(value.into(), true, false, true)
            ));
        };
    }

    // 19.1 Value Properties of the Global Object
    value_prop!(
        "globalThis",
        realm.global_env.global_this_value.into(),
        true,
        false,
        true
    );
    value_prop!(
        "Infinity",
        Value::number(f64::INFINITY),
        false,
        false,
        false
    );
    value_prop!("NaN", Value::nan(), false, false, false);
    value_prop!("undefined", Value::undefined(), false, false, false);

    // 19.2 Function Properties of the Global Object
    func_prop!("isNaN", is_nan, 1);
    func_prop!("isFinite", is_finite, 1);

    // 19.3 Constructor Properties of the Global Object
    intrinsic_prop!("Array", ArrayConstructor);
    intrinsic_prop!("Boolean", BooleanConstructor);
    intrinsic_prop!("Error", ErrorConstructor);
    intrinsic_prop!("EvalError", EvalErrorConstructor);
    intrinsic_prop!("Function", FunctionConstructor);
    intrinsic_prop!("Number", NumberConstructor);
    intrinsic_prop!("Object", ObjectConstructor);
    intrinsic_prop!("RangeError", RangeErrorConstructor);
    intrinsic_prop!("ReferenceError", ReferenceErrorConstructor);
    intrinsic_prop!("String", StringConstructor);
    intrinsic_prop!("SyntaxError", SyntaxErrorConstructor);
    intrinsic_prop!("TypeError", TypeErrorConstructor);
    intrinsic_prop!("URIError", URIErrorConstructor);

    // Non-standard, environment specific properties of global object
    let console_object = ConsoleObject::new(cx, realm).into();
    value_prop!("console", console_object, true, false, true);

    ().into()
}

// 19.2.2 isFinite
fn is_finite(
    cx: &mut Context,
    _: Value,
    arguments: &[Value],
    _: Option<Gc<ObjectValue>>,
) -> EvalResult<Value> {
    let num = maybe!(to_number(cx, get_argument(arguments, 0)));
    (!num.is_nan() && !num.is_infinity()).into()
}

// 19.2.3 isNaN
fn is_nan(
    cx: &mut Context,
    _: Value,
    arguments: &[Value],
    _: Option<Gc<ObjectValue>>,
) -> EvalResult<Value> {
    let num = maybe!(to_number(cx, get_argument(arguments, 0)));
    num.is_nan().into()
}
