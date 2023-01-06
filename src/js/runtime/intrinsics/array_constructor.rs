use crate::js::runtime::{
    builtin_function::BuiltinFunction, function::get_argument, object_value::ObjectValue,
    type_utilities::is_array, Context, EvalResult, Gc, Realm, Value,
};

pub struct ArrayConstructor;

impl ArrayConstructor {
    // 23.1.2 Properties of the Array Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func =
            BuiltinFunction::create(cx, Self::construct, 0, "Array", Some(realm), None, None);

        func.set_is_constructor();

        func.intrinsic_func(cx, "isArray", Self::is_array, 1, realm);

        func
    }

    // 23.1.1.1 Array
    fn construct(
        cx: &mut Context,
        _: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        unimplemented!("array constructor")
    }

    // 23.1.2.2 Array.isArray
    fn is_array(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        is_array(get_argument(arguments, 0)).into()
    }
}
