use crate::js::runtime::{
    completion::EvalResult, function::get_argument, generator_object::generator_resume,
    object_value::ObjectValue, property::Property, realm::Realm, value::Value, Context, Handle,
};

use super::intrinsics::Intrinsic;

pub struct GeneratorPrototype;

impl GeneratorPrototype {
    // 27.5.1 The %GeneratorPrototype% Object
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::IteratorPrototype)), true);

        // Constructor property is added once GeneratorFunctionPrototype is created

        object.intrinsic_func(cx, cx.names.next(), Self::next, 1, realm);

        // 27.5.1.5 %GeneratorPrototype% [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.generator().as_string().into(), false, false, true),
        );

        object
    }

    // 27.5.1.2 %GeneratorPrototype%.next
    pub fn next(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        generator_resume(cx, this_value, value)
    }
}
