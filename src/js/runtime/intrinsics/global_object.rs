use crate::{
    js::runtime::{
        abstract_operations::define_property_or_throw, builtin_function::BuiltinFunction,
        console::ConsoleObject, eval::eval::perform_eval, function::get_argument,
        object_value::ObjectValue, property_descriptor::PropertyDescriptor,
        type_utilities::to_number, Context, EvalResult, Gc, Realm, Value,
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
        &cx.names.global_this(),
        realm.global_env.global_this_value.into(),
        true,
        false,
        true
    );
    value_prop!(&cx.names.infinity(), Value::number(f64::INFINITY), false, false, false);
    value_prop!(&cx.names.nan(), Value::nan(), false, false, false);
    value_prop!(&cx.names.undefined(), Value::undefined(), false, false, false);

    // 19.2 Function Properties of the Global Object
    intrinsic_prop!(&cx.names.eval(), Eval);
    func_prop!(&cx.names.is_nan(), is_nan, 1);
    func_prop!(&cx.names.is_finite(), is_finite, 1);

    // 19.3 Constructor Properties of the Global Object
    intrinsic_prop!(&cx.names.array(), ArrayConstructor);
    intrinsic_prop!(&cx.names.boolean(), BooleanConstructor);
    intrinsic_prop!(&cx.names.error(), ErrorConstructor);
    intrinsic_prop!(&cx.names.eval_error(), EvalErrorConstructor);
    intrinsic_prop!(&cx.names.function(), FunctionConstructor);
    intrinsic_prop!(&cx.names.number(), NumberConstructor);
    intrinsic_prop!(&cx.names.object(), ObjectConstructor);
    intrinsic_prop!(&cx.names.range_error(), RangeErrorConstructor);
    intrinsic_prop!(&cx.names.reference_error(), ReferenceErrorConstructor);
    intrinsic_prop!(&cx.names.string(), StringConstructor);
    intrinsic_prop!(&cx.names.symbol(), SymbolConstructor);
    intrinsic_prop!(&cx.names.syntax_error(), SyntaxErrorConstructor);
    intrinsic_prop!(&cx.names.type_error(), TypeErrorConstructor);
    intrinsic_prop!(&cx.names.uri_error(), URIErrorConstructor);

    // Non-standard, environment specific properties of global object
    let console_object = ConsoleObject::new(cx, realm).into();
    value_prop!(&cx.names.console(), console_object, true, false, true);

    ().into()
}

pub fn create_eval(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
    BuiltinFunction::create(cx, eval, 1, &cx.names.eval(), Some(realm), None, None)
}

// 19.2.1 eval
fn eval(
    cx: &mut Context,
    _: Value,
    arguments: &[Value],
    _: Option<Gc<ObjectValue>>,
) -> EvalResult<Value> {
    perform_eval(cx, get_argument(arguments, 0), false, false)
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
