use crate::js::runtime::{
    builtin_function::BuiltinFunction, function::get_argument, object_value::ObjectValue,
    property::Property, type_utilities::is_array, Context, EvalResult, Gc, Realm, Value,
};

use super::intrinsics::Intrinsic;

pub struct ArrayConstructor;

impl ArrayConstructor {
    // 23.1.2 Properties of the Array Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            0,
            &cx.names.array(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            &cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::ArrayPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func.intrinsic_func(cx, &cx.names.is_array(), Self::is_array, 1, realm);

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
