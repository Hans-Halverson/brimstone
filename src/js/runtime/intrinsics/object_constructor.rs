use crate::{
    js::runtime::{
        abstract_operations::{define_property_or_throw, has_own_property, is_extensible},
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
        type_utilities::{require_object_coercible, same_value, to_object, to_property_key},
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
            &cx.names.object(),
            Some(realm),
            None,
            None,
        );

        func.set_is_constructor();
        func.set_property(
            &cx.names.prototype(),
            Property::data(
                realm.get_intrinsic(Intrinsic::ObjectPrototype).into(),
                false,
                false,
                false,
            ),
        );

        func.intrinsic_func(cx, &cx.names.define_property(), Self::define_property, 3, realm);
        func.intrinsic_func(
            cx,
            &cx.names.get_own_property_descriptor(),
            Self::get_own_property_descriptor,
            2,
            realm,
        );
        func.intrinsic_func(cx, &cx.names.get_prototype_of(), Self::get_prototype_of, 1, realm);
        func.intrinsic_func(cx, &cx.names.has_own(), Self::has_own, 2, realm);
        func.intrinsic_func(cx, &cx.names.is(), Self::is, 2, realm);
        func.intrinsic_func(cx, &cx.names.is_extensible(), Self::is_extensible, 1, realm);
        func.intrinsic_func(cx, &cx.names.prevent_extensions(), Self::prevent_extensions, 1, realm);
        func.intrinsic_func(cx, &cx.names.set_prototype_of(), Self::set_prototype_of, 2, realm);

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

        let property_key = maybe!(to_property_key(cx, get_argument(arguments, 1)));
        let desc = maybe!(to_property_descriptor(cx, get_argument(arguments, 2)));

        maybe!(define_property_or_throw(cx, object.as_object(), &property_key, desc,));

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
        let property_key = maybe!(to_property_key(cx, get_argument(arguments, 1)));

        match maybe!(object.get_own_property(&property_key)) {
            None => Value::undefined().into(),
            Some(desc) => from_property_descriptor(cx, desc).into(),
        }
    }

    // 20.1.2.12 Object.getPrototypeOf
    fn get_prototype_of(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, get_argument(arguments, 0)));
        let prototype = maybe!(object.get_prototype_of());

        match prototype {
            None => Value::null().into(),
            Some(prototype) => prototype.into(),
        }
    }

    // 20.1.2.13 Object.hasOwn
    fn has_own(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, get_argument(arguments, 0)));
        let key = maybe!(to_property_key(cx, get_argument(arguments, 1)));

        maybe!(has_own_property(object, &key)).into()
    }

    // 20.1.2.14 Object.is
    fn is(
        _: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        same_value(get_argument(arguments, 0), get_argument(arguments, 1)).into()
    }

    // 20.1.2.15 Object.isExtensible
    fn is_extensible(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let value = get_argument(arguments, 0);
        if !value.is_object() {
            return type_error_(cx, "expected object");
        }

        maybe!(is_extensible(value.as_object())).into()
    }

    // 20.1.2.19 Object.preventExtensions
    fn prevent_extensions(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let value = get_argument(arguments, 0);
        if !value.is_object() {
            return type_error_(cx, "expected object");
        }

        if !maybe!(value.as_object().prevent_extensions()) {
            return type_error_(cx, "failed to prevent extensions on object");
        }

        value.into()
    }

    // 20.1.2.22 Object.setPrototypeOf
    fn set_prototype_of(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(require_object_coercible(cx, get_argument(arguments, 0)));

        let proto = get_argument(arguments, 1);
        let proto = if proto.is_object() {
            Some(proto.as_object())
        } else if proto.is_null() {
            None
        } else {
            return type_error_(cx, "prototype must be an object or null");
        };

        if !object.is_object() {
            return object.into();
        }
        let mut object = object.as_object();

        if !maybe!(object.set_prototype_of(proto)) {
            return type_error_(cx, "failed to set object prototype");
        }

        object.into()
    }
}
