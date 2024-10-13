use crate::{
    js::runtime::{
        abstract_operations::{call_object, construct, create_list_from_array_like},
        array_object::create_array_from_list,
        completion::EvalResult,
        error::type_error,
        function::get_argument,
        object_value::ObjectValue,
        property::Property,
        property_descriptor::{from_property_descriptor, to_property_descriptor},
        realm::Realm,
        type_utilities::{is_callable, is_constructor_value, to_property_key},
        Context, Handle, Value,
    },
    maybe,
};

use super::intrinsics::Intrinsic;

/// The Reflect Object (https://tc39.es/ecma262/#sec-reflect-object)
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

        // Reflect [ @@toStringTag ] (https://tc39.es/ecma262/#sec-reflect-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let reflect_name_value = cx.names.reflect().as_string().into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(reflect_name_value, false, false, true),
        );

        object
    }

    /// Reflect.apply (https://tc39.es/ecma262/#sec-reflect.apply)
    pub fn apply(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !is_callable(target) {
            return type_error(cx, "value is not a function");
        }

        let this_argument = get_argument(cx, arguments, 1);
        let arguments_arg = get_argument(cx, arguments, 2);
        let arguments_list = maybe!(create_list_from_array_like(cx, arguments_arg));

        call_object(cx, target.as_object(), this_argument, &arguments_list)
    }

    /// Reflect.construct (https://tc39.es/ecma262/#sec-reflect.construct)
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !is_constructor_value(target) {
            return type_error(cx, "value is not a constructor");
        }

        let target = target.as_object();

        let new_target = if arguments.len() >= 3 {
            let new_target = get_argument(cx, arguments, 2);
            if !is_constructor_value(new_target) {
                return type_error(cx, "value is not a constructor");
            }

            new_target.as_object()
        } else {
            target
        };

        let arguments_arg = get_argument(cx, arguments, 1);
        let arguments_list = maybe!(create_list_from_array_like(cx, arguments_arg));

        Ok(maybe!(construct(cx, target, &arguments_list, Some(new_target))).as_value())
    }

    /// Reflect.defineProperty (https://tc39.es/ecma262/#sec-reflect.defineproperty)
    pub fn define_property(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "value is not an object");
        }

        let mut target = target.as_object();

        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));

        let desc_arg = get_argument(cx, arguments, 2);
        let desc = maybe!(to_property_descriptor(cx, desc_arg));

        let result = maybe!(target.define_own_property(cx, key, desc));
        Ok(cx.bool(result))
    }

    /// Reflect.deleteProperty (https://tc39.es/ecma262/#sec-reflect.deleteproperty)
    pub fn delete_property(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "value is not an object");
        }

        let mut target = target.as_object();
        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));

        let result = maybe!(target.delete(cx, key));
        Ok(cx.bool(result))
    }

    /// Reflect.get (https://tc39.es/ecma262/#sec-reflect.get)
    pub fn get(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "value is not an object");
        }

        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));

        let receiver = if arguments.len() >= 3 {
            get_argument(cx, arguments, 2)
        } else {
            target
        };

        target.as_object().get(cx, key, receiver)
    }

    /// Reflect.getOwnPropertyDescriptor (https://tc39.es/ecma262/#sec-reflect.getownpropertydescriptor)
    pub fn get_own_property_descriptor(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "value is not an object");
        }

        let target = target.as_object();
        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));

        let desc = maybe!(target.get_own_property(cx, key));

        Ok(desc
            .map(|desc| from_property_descriptor(cx, desc).into())
            .unwrap_or(cx.undefined()))
    }

    /// Reflect.getPrototypeOf (https://tc39.es/ecma262/#sec-reflect.getprototypeof)
    pub fn get_prototype_of(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "value is not an object");
        }

        let prototype = maybe!(target.as_object().get_prototype_of(cx));

        Ok(prototype.map(|proto| proto.into()).unwrap_or(cx.null()))
    }

    /// Reflect.has (https://tc39.es/ecma262/#sec-reflect.has)
    pub fn has(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "value is not an object");
        }

        let target = target.as_object();
        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));

        let has_property = maybe!(target.has_property(cx, key));
        Ok(cx.bool(has_property))
    }

    /// Reflect.isExtensible (https://tc39.es/ecma262/#sec-reflect.isextensible)
    pub fn is_extensible(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "value is not an object");
        }

        let is_extensible = maybe!(target.as_object().is_extensible(cx));
        Ok(cx.bool(is_extensible))
    }

    /// Reflect.ownKeys (https://tc39.es/ecma262/#sec-reflect.ownkeys)
    pub fn own_keys(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "value is not an object");
        }

        let own_keys = maybe!(target.as_object().own_property_keys(cx));

        Ok(create_array_from_list(cx, &own_keys).as_value())
    }

    /// Reflect.preventExtensions (https://tc39.es/ecma262/#sec-reflect.preventextensions)
    pub fn prevent_extensions(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "value is not an object");
        }

        let result = maybe!(target.as_object().prevent_extensions(cx));
        Ok(cx.bool(result))
    }

    /// Reflect.set (https://tc39.es/ecma262/#sec-reflect.set)
    pub fn set(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "value is not an object");
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
        Ok(cx.bool(result))
    }

    /// Reflect.setPrototypeOf (https://tc39.es/ecma262/#sec-reflect.setprototypeof)
    pub fn set_prototype_of(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "value is not an object");
        }

        let proto = get_argument(cx, arguments, 1);
        let proto = if proto.is_object() {
            Some(proto.as_object())
        } else if proto.is_null() {
            None
        } else {
            return type_error(cx, "prototype must be an object or null");
        };

        let result = maybe!(target.as_object().set_prototype_of(cx, proto));
        Ok(cx.bool(result))
    }
}
