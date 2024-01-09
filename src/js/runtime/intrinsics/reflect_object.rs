use crate::{
    js::runtime::{
        abstract_operations::{call_object, construct, create_list_from_array_like},
        array_object::create_array_from_list,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        object_value::ObjectValue,
        property::Property,
        property_descriptor::{from_property_descriptor, to_property_descriptor},
        realm::Realm,
        type_utilities::{is_callable, is_constructor, to_property_key},
        Context, Handle, Value,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

// 28.1 The Reflect Object
pub struct ReflectObject;

impl ReflectObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true);

        object.intrinsic_func(cx, cx.names.apply(), Self::apply, 3, realm);
        object.intrinsic_func(cx, cx.names.construct(), Self::construct, 2, realm);
        object.intrinsic_func(cx, cx.names.define_property(), Self::define_property, 3, realm);
        object.intrinsic_func(cx, cx.names.delete_property(), Self::delete_property, 2, realm);
        object.intrinsic_func(cx, cx.names.get(), Self::get, 2, realm);
        object.intrinsic_func(
            cx,
            cx.names.get_own_property_descriptor(),
            Self::get_own_property_descriptor,
            2,
            realm,
        );
        object.intrinsic_func(cx, cx.names.get_prototype_of(), Self::get_prototype_of, 1, realm);
        object.intrinsic_func(cx, cx.names.has(), Self::has, 2, realm);
        object.intrinsic_func(cx, cx.names.is_extensible(), Self::is_extensible, 1, realm);
        object.intrinsic_func(cx, cx.names.own_keys(), Self::own_keys, 1, realm);
        object.intrinsic_func(
            cx,
            cx.names.prevent_extensions(),
            Self::prevent_extensions,
            1,
            realm,
        );
        object.intrinsic_func(cx, cx.names.set_(), Self::set, 3, realm);
        object.intrinsic_func(cx, cx.names.set_prototype_of(), Self::set_prototype_of, 2, realm);

        // 28.1.14 Reflect [ @@toStringTag ]
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let reflect_name_value = cx.names.reflect().as_string().into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(reflect_name_value, false, false, true),
        );

        object
    }

    // 28.1.1 Reflect.apply
    pub fn apply(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !is_callable(target) {
            return type_error_(cx, "value is not a function");
        }

        let this_argument = get_argument(cx, arguments, 1);
        let arguments_arg = get_argument(cx, arguments, 2);
        let arguments_list = maybe!(create_list_from_array_like(cx, arguments_arg));

        call_object(cx, target.as_object(), this_argument, &arguments_list)
    }

    // 28.1.2 Reflect.construct
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !is_constructor(target) {
            return type_error_(cx, "value is not a constructor");
        }

        let target = target.as_object();

        let new_target = if arguments.len() >= 3 {
            let new_target = get_argument(cx, arguments, 2);
            if !is_constructor(new_target) {
                return type_error_(cx, "value is not a constructor");
            }

            new_target.as_object()
        } else {
            target
        };

        let arguments_arg = get_argument(cx, arguments, 1);
        let arguments_list = maybe!(create_list_from_array_like(cx, arguments_arg));

        maybe!(construct(cx, target, &arguments_list, Some(new_target))).into()
    }

    // 28.1.3 Reflect.defineProperty
    pub fn define_property(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let mut target = target.as_object();

        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));

        let desc_arg = get_argument(cx, arguments, 2);
        let desc = maybe!(to_property_descriptor(cx, desc_arg));

        let result = maybe!(target.define_own_property(cx, key, desc));
        cx.bool(result).into()
    }

    // 28.1.4 Reflect.deleteProperty
    pub fn delete_property(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let mut target = target.as_object();
        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));

        let result = maybe!(target.delete(cx, key));
        cx.bool(result).into()
    }

    // 28.1.5 Reflect.get
    pub fn get(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));

        let receiver = if arguments.len() >= 3 {
            get_argument(cx, arguments, 2)
        } else {
            target
        };

        maybe!(target.as_object().get(cx, key, receiver)).into()
    }

    // 28.1.6 Reflect.getOwnPropertyDescriptor
    pub fn get_own_property_descriptor(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let target = target.as_object();
        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));

        let desc = maybe!(target.get_own_property(cx, key));

        desc.map(|desc| from_property_descriptor(cx, desc).into())
            .unwrap_or(cx.undefined())
            .into()
    }

    // 28.1.7 Reflect.getPrototypeOf
    pub fn get_prototype_of(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let prototype = maybe!(target.as_object().get_prototype_of(cx));

        prototype
            .map(|proto| proto.into())
            .unwrap_or(cx.null())
            .into()
    }

    // 28.1.8 Reflect.has
    pub fn has(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let target = target.as_object();
        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));

        let has_property = maybe!(target.has_property(cx, key));
        cx.bool(has_property).into()
    }

    // 28.1.9 Reflect.isExtensible
    pub fn is_extensible(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let is_extensible = maybe!(target.as_object().is_extensible(cx));
        cx.bool(is_extensible).into()
    }

    // 28.1.10 Reflect.ownKeys
    pub fn own_keys(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let own_keys = maybe!(target.as_object().own_property_keys(cx));

        create_array_from_list(cx, &own_keys).into()
    }

    // 28.1.11 Reflect.preventExtensions
    pub fn prevent_extensions(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let result = maybe!(target.as_object().prevent_extensions(cx));
        cx.bool(result).into()
    }

    // 28.1.12 Reflect.set
    pub fn set(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));
        let value = get_argument(cx, arguments, 2);

        let receiver = if arguments.len() >= 4 {
            get_argument(cx, arguments, 3)
        } else {
            target
        };

        let result = maybe!(target.as_object().set(cx, key, value, receiver));
        cx.bool(result).into()
    }

    // 28.1.13 Reflect.setPrototypeOf
    pub fn set_prototype_of(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error_(cx, "value is not an object");
        }

        let proto = get_argument(cx, arguments, 1);
        let proto = if proto.is_object() {
            Some(proto.as_object())
        } else if proto.is_null() {
            None
        } else {
            return type_error_(cx, "prototype must be an object or null");
        };

        let result = maybe!(target.as_object().set_prototype_of(cx, proto));
        cx.bool(result).into()
    }
}
