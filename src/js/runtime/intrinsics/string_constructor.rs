use crate::{
    js::runtime::{
        builtin_function::BuiltinFunction, completion::AbstractResult, function::get_argument,
        gc::Gc, object_value::ObjectValue, realm::Realm, type_utilities::to_string, value::Value,
        Context,
    },
    maybe_,
};

pub struct StringConstructor;

impl StringConstructor {
    // 22.1.2 Properties of the String Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func =
            BuiltinFunction::create(cx, Self::construct, 1, "String", Some(realm), None, None);

        func.set_is_constructor();

        func
    }

    // 22.1.1.1 String
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> AbstractResult<Value> {
        let string_value = if arguments.is_empty() {
            cx.heap.alloc_string(String::new()).into()
        } else {
            let value = get_argument(arguments, 0);
            if new_target.is_none() && value.is_symbol() {
                unimplemented!("Symbols")
            }

            maybe_!(to_string(cx, value))
        };

        match new_target {
            None => string_value.into(),
            Some(_) => unimplemented!("string objects"),
        }
    }
}
