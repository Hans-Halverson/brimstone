use crate::{
    js::runtime::{
        builtin_function::BuiltinFunction,
        completion::AbstractResult,
        function::get_argument,
        gc::Gc,
        object_value::ObjectValue,
        ordinary_object::{ordinary_create_from_constructor, ordinary_object_create},
        property::Property,
        realm::Realm,
        type_utilities::to_object,
        value::Value,
        Context,
    },
    maybe_, must_,
};

use super::intrinsics::Intrinsic;

pub struct ObjectConstructor;

impl ObjectConstructor {
    // 20.1.2 Properties of the Object Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func =
            BuiltinFunction::create(cx, Self::construct, 1, "Object", Some(realm), None, None);

        func.set_is_constructor();
        func.set_property(
            "prototype".to_owned(),
            Property::data(
                realm.get_intrinsic(Intrinsic::ObjectPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func
    }

    // 20.1.1.1 Object
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: Vec<Value>,
        new_target: Option<Gc<ObjectValue>>,
    ) -> AbstractResult<Value> {
        if let Some(new_target) = new_target {
            if cx.current_execution_context().function.unwrap() != new_target {
                let new_object = maybe_!(ordinary_create_from_constructor(
                    cx,
                    new_target,
                    Intrinsic::ObjectConstructor
                ));
                let new_value: Value = cx.heap.alloc(new_object).into();
                return new_value.into();
            }
        }

        let value = get_argument(&arguments, 0);
        if value.is_nullish() {
            let new_object = ordinary_object_create(
                cx.current_realm().get_intrinsic(Intrinsic::ObjectPrototype),
            );
            let new_value: Value = cx.heap.alloc(new_object).into();
            return new_value.into();
        }

        must_!(to_object(cx, value)).into()
    }
}
