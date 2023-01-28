use crate::{
    js::runtime::{
        builtin_function::BuiltinFunction, completion::EvalResult, function::get_argument, gc::Gc,
        object_value::ObjectValue, ordinary_object::ordinary_create_from_constructor,
        property::Property, realm::Realm, string_object::StringObject, type_utilities::to_string,
        value::Value, Context,
    },
    maybe,
};

use super::{intrinsics::Intrinsic, symbol_prototype::symbol_descriptive_string};

pub struct StringConstructor;

impl StringConstructor {
    // 22.1.2 Properties of the String Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            &cx.names.string(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            &cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::StringPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func
    }

    // 22.1.1.1 String
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let string_value = if arguments.is_empty() {
            cx.heap.alloc_string(String::new()).into()
        } else {
            let value = get_argument(arguments, 0);
            if new_target.is_none() && value.is_symbol() {
                return symbol_descriptive_string(cx, value.as_symbol()).into();
            }

            maybe!(to_string(cx, value))
        };

        match new_target {
            None => string_value.into(),
            Some(new_target) => {
                let object = maybe!(ordinary_create_from_constructor(
                    cx,
                    new_target,
                    Intrinsic::StringPrototype
                ));

                let string_object = StringObject::new(cx, object, string_value);
                cx.heap.alloc(string_object).into()
            }
        }
    }
}
