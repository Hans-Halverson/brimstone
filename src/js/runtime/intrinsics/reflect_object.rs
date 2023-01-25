use crate::{
    js::runtime::{
        abstract_operations::{call_object, construct, create_list_from_array_like},
        array_object::create_array_from_list,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        gc::Gc,
        object_value::{Object, ObjectValue},
        ordinary_object::OrdinaryObject,
        property::Property,
        property_descriptor::{from_property_descriptor, to_property_descriptor},
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::{is_callable, is_constructor, to_property_key},
        value::Value,
        Context,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 28.1 The Reflect Object
pub struct ReflectObject;

impl ReflectObject {
    pub fn new(cx: &mut Context, realm: Gc<Realm>) -> Gc<ObjectValue> {
        let mut object =
            OrdinaryObject::new(Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        object.intrinsic_func(cx, &cx.names.apply(), Self::apply, 3, realm);
        object.intrinsic_func(cx, &cx.names.construct(), Self::construct, 2, realm);
        object.intrinsic_func(cx, &cx.names.define_property(), Self::define_property, 3, realm);
        object.intrinsic_func(cx, &cx.names.delete_property(), Self::delete_property, 2, realm);
        object.intrinsic_func(cx, &cx.names.get(), Self::get, 2, realm);
        object.intrinsic_func(
            cx,
            &cx.names.get_own_property_descriptor(),
            Self::get_own_property_descriptor,
            2,
            realm,
        );
        object.intrinsic_func(cx, &cx.names.get_prototype_of(), Self::get_prototype_of, 1, realm);
        object.intrinsic_func(cx, &cx.names.has(), Self::has, 2, realm);
        object.intrinsic_func(cx, &cx.names.is_extensible(), Self::is_extensible, 1, realm);
        object.intrinsic_func(cx, &cx.names.own_keys(), Self::own_keys, 1, realm);
        object.intrinsic_func(
            cx,
            &cx.names.prevent_extensions(),
            Self::prevent_extensions,
            1,
            realm,
        );
        object.intrinsic_func(cx, &cx.names.set(), Self::set, 3, realm);
        object.intrinsic_func(cx, &cx.names.set_prototype_of(), Self::set_prototype_of, 2, realm);

        // 28.1.14 Reflect [ @@toStringTag ]
        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        let reflect_name_value = cx.names.reflect().as_string().into();
        object.set_property(
            &to_string_tag_key,
            Property::data(reflect_name_value, false, false, true),
        );

        cx.heap.alloc(object).into()
    }

    // 28.1.1 Reflect.apply
    fn apply(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !is_callable(target) {
            return type_error_(cx, "value is not a function");
        }

        let this_argument = get_argument(arguments, 1);
        let arguments_list = maybe!(create_list_from_array_like(cx, get_argument(arguments, 2)));

        call_object(cx, target.as_object(), this_argument, &arguments_list)
    }

    // 28.1.2 Reflect.construct
    fn construct(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !is_constructor(target) {
            return type_error_(cx, "value is not a constructor");
        }

        let target = target.as_object();

        let new_target = if arguments.len() >= 3 {
            let new_target = get_argument(arguments, 2);
            if !is_constructor(new_target) {
                return type_error_(cx, "value is not a constructor");
            }

            new_target.as_object()
        } else {
            target
        };

        let arguments_list = maybe!(create_list_from_array_like(cx, get_argument(arguments, 1)));

        maybe!(construct(cx, target, &arguments_list, Some(new_target))).into()
    }

    // 28.1.3 Reflect.defineProperty
    fn define_property(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let mut target = target.as_object();
        let key = maybe!(to_property_key(cx, get_argument(arguments, 1)));
        let desc = maybe!(to_property_descriptor(cx, get_argument(arguments, 2)));

        maybe!(target.define_own_property(cx, &key, desc)).into()
    }

    // 28.1.4 Reflect.deleteProperty
    fn delete_property(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let mut target = target.as_object();
        let key = maybe!(to_property_key(cx, get_argument(arguments, 1)));

        maybe!(target.delete(cx, &key)).into()
    }

    // 28.1.5 Reflect.get
    fn get(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let key = maybe!(to_property_key(cx, get_argument(arguments, 1)));

        let receiver = if arguments.len() >= 3 {
            get_argument(arguments, 2)
        } else {
            target
        };

        maybe!(target.as_object().get(cx, &key, receiver)).into()
    }

    // 28.1.6 Reflect.getOwnPropertyDescriptor
    fn get_own_property_descriptor(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let target = target.as_object();
        let key = maybe!(to_property_key(cx, get_argument(arguments, 1)));

        let desc = maybe!(target.get_own_property(cx, &key));

        desc.map(|desc| from_property_descriptor(cx, desc).into())
            .unwrap_or(Value::undefined())
            .into()
    }

    // 28.1.7 Reflect.getPrototypeOf
    fn get_prototype_of(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let prototype = maybe!(target.as_object().get_prototype_of());

        prototype
            .map(|proto| proto.into())
            .unwrap_or(Value::null())
            .into()
    }

    // 28.1.8 Reflect.has
    fn has(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let target = target.as_object();
        let key = maybe!(to_property_key(cx, get_argument(arguments, 1)));

        maybe!(target.has_property(cx, &key)).into()
    }

    // 28.1.9 Reflect.isExtensible
    fn is_extensible(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        maybe!(target.as_object().is_extensible()).into()
    }

    // 28.1.10 Reflect.ownKeys
    fn own_keys(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let own_keys = target.as_object().own_property_keys(cx);

        create_array_from_list(cx, &own_keys).into()
    }

    // 28.1.11 Reflect.preventExtensions
    fn prevent_extensions(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        maybe!(target.as_object().prevent_extensions()).into()
    }

    // 28.1.12 Reflect.set
    fn set(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let key = maybe!(to_property_key(cx, get_argument(arguments, 1)));
        let value = get_argument(arguments, 2);

        let receiver = if arguments.len() >= 4 {
            get_argument(arguments, 3)
        } else {
            target
        };

        maybe!(target.as_object().set(cx, &key, value, receiver)).into()
    }

    // 28.1.13 Reflect.setPrototypeOf
    fn set_prototype_of(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let target = get_argument(arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let proto = get_argument(arguments, 1);
        let proto = if proto.is_object() {
            Some(proto.as_object())
        } else if proto.is_null() {
            None
        } else {
            return type_error_(cx, "prototype must be an object or null");
        };

        maybe!(target.as_object().set_prototype_of(proto)).into()
    }
}
