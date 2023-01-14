use crate::{
    js::runtime::{
        abstract_operations::define_property_or_throw,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        gc::Gc,
        object_value::ObjectValue,
        ordinary_object::{ordinary_create_from_constructor, ordinary_object_create},
        property::Property,
        property_descriptor::{from_property_descriptor, to_property_descriptor},
        realm::Realm,
        type_utilities::{to_object, to_property_key},
        value::Value,
        Context,
    },
    maybe, must,
};

use super::intrinsics::Intrinsic;

pub struct ObjectConstructor;

impl ObjectConstructor {
    // 20.1.2 Properties of the Object Constructor
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<BuiltinFunction> {
        let mut func = BuiltinFunction::create(
            cx,
            Self::construct,
            1,
            cx.names.object,
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            cx.names.prototype,
            Property::data(
                realm.get_intrinsic(Intrinsic::ObjectPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func.intrinsic_func(
            cx,
            cx.names.define_property,
            Self::define_property,
            3,
            realm,
        );
        func.intrinsic_func(
            cx,
            cx.names.get_own_property_descriptor,
            Self::get_own_property_descriptor,
            2,
            realm,
        );

        func
    }

    // 20.1.1.1 Object
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        new_target: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if let Some(new_target) = new_target {
            if cx
                .current_execution_context()
                .function
                .unwrap()
                .ptr_eq(&new_target)
            {
                let new_object = maybe!(ordinary_create_from_constructor(
                    cx,
                    new_target,
                    Intrinsic::ObjectConstructor
                ));
                let new_value: Value = cx.heap.alloc(new_object).into();
                return new_value.into();
            }
        }

        let value = get_argument(arguments, 0);
        if value.is_nullish() {
            let new_object = ordinary_object_create(
                cx.current_realm().get_intrinsic(Intrinsic::ObjectPrototype),
            );
            let new_value: Value = cx.heap.alloc(new_object).into();
            return new_value.into();
        }

        must!(to_object(cx, value)).into()
    }

    // 20.1.2.4 Object.defineProperty
    fn define_property(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = get_argument(arguments, 0);
        if !object.is_object() {
            return type_error_(cx, "can only define property on an object");
        }

        let property_key = to_property_key(get_argument(arguments, 1));
        let desc = maybe!(to_property_descriptor(cx, get_argument(arguments, 2)));

        maybe!(define_property_or_throw(
            cx,
            object.as_object(),
            property_key,
            desc,
        ));

        object.into()
    }

    // 20.1.2.8 Object.getOwnPropertyDescriptor
    fn get_own_property_descriptor(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, get_argument(arguments, 0)));
        let property_key = to_property_key(get_argument(arguments, 1));

        match maybe!(object.get_own_property(property_key)) {
            None => Value::undefined().into(),
            Some(desc) => from_property_descriptor(cx, desc).into(),
        }
    }
}
