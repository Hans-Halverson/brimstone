use crate::runtime::{
    abstract_operations::{call_object, construct, create_list_from_array_like_arguments},
    alloc_error::AllocResult,
    array_object::create_array_from_list,
    error::type_error,
    eval_result::EvalResult,
    function::get_argument,
    intrinsics::rust_runtime::RuntimeFunction,
    object_value::ObjectValue,
    property::Property,
    property_descriptor::{from_property_descriptor, to_property_descriptor},
    realm::Realm,
    type_utilities::{is_callable, is_constructor_value, to_property_key},
    Context, Handle, Value,
};

use super::intrinsics::Intrinsic;

/// The Reflect Object (https://tc39.es/ecma262/#sec-reflect-object)
pub struct ReflectObject;

impl ReflectObject {
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut object =
            ObjectValue::new(cx, Some(realm.get_intrinsic(Intrinsic::ObjectPrototype)), true)?;

        object.intrinsic_func(
            cx,
            cx.names.apply(),
            RuntimeFunction::ReflectObject_apply,
            3,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.construct(),
            RuntimeFunction::ReflectObject_construct,
            2,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.define_property(),
            RuntimeFunction::ReflectObject_define_property,
            3,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.delete_property(),
            RuntimeFunction::ReflectObject_delete_property,
            2,
            realm,
        )?;
        object.intrinsic_func(cx, cx.names.get(), RuntimeFunction::ReflectObject_get, 2, realm)?;
        object.intrinsic_func(
            cx,
            cx.names.get_own_property_descriptor(),
            RuntimeFunction::ReflectObject_get_own_property_descriptor,
            2,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.get_prototype_of(),
            RuntimeFunction::ReflectObject_get_prototype_of,
            1,
            realm,
        )?;
        object.intrinsic_func(cx, cx.names.has(), RuntimeFunction::ReflectObject_has, 2, realm)?;
        object.intrinsic_func(
            cx,
            cx.names.is_extensible(),
            RuntimeFunction::ReflectObject_is_extensible,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.own_keys(),
            RuntimeFunction::ReflectObject_own_keys,
            1,
            realm,
        )?;
        object.intrinsic_func(
            cx,
            cx.names.prevent_extensions(),
            RuntimeFunction::ReflectObject_prevent_extensions,
            1,
            realm,
        )?;
        object.intrinsic_func(cx, cx.names.set_(), RuntimeFunction::ReflectObject_set, 3, realm)?;
        object.intrinsic_func(
            cx,
            cx.names.set_prototype_of(),
            RuntimeFunction::ReflectObject_set_prototype_of,
            2,
            realm,
        )?;

        // Reflect [ @@toStringTag ] (https://tc39.es/ecma262/#sec-reflect-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let reflect_name_value = cx.names.reflect().as_string().into();
        object.set_property(
            cx,
            to_string_tag_key,
            Property::data(reflect_name_value, false, false, true),
        )?;

        Ok(object)
    }

    /// Reflect.apply (https://tc39.es/ecma262/#sec-reflect.apply)
    pub fn apply(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !is_callable(target) {
            return type_error(cx, "Reflect.apply target must be a function");
        }

        let this_argument = get_argument(cx, arguments, 1);
        let arguments_arg = get_argument(cx, arguments, 2);
        let arguments_list =
            create_list_from_array_like_arguments(cx, arguments_arg, "Reflect.apply")?;

        call_object(cx, target.as_object(), this_argument, &arguments_list)
    }

    /// Reflect.construct (https://tc39.es/ecma262/#sec-reflect.construct)
    pub fn construct(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !is_constructor_value(target) {
            return type_error(cx, "Reflect.construct target must be a constructor");
        }

        let target = target.as_object();

        let new_target = if arguments.len() >= 3 {
            let new_target = get_argument(cx, arguments, 2);
            if !is_constructor_value(new_target) {
                return type_error(cx, "Reflect.construct newTarget must be a constructor");
            }

            new_target.as_object()
        } else {
            target
        };

        let arguments_arg = get_argument(cx, arguments, 1);
        let arguments_list =
            create_list_from_array_like_arguments(cx, arguments_arg, "Reflect.construct")?;

        Ok(construct(cx, target, &arguments_list, Some(new_target))?.as_value())
    }

    /// Reflect.defineProperty (https://tc39.es/ecma262/#sec-reflect.defineproperty)
    pub fn define_property(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "Reflect.defineProperty target must be an object");
        }

        let mut target = target.as_object();

        let key_arg = get_argument(cx, arguments, 1);
        let key = to_property_key(cx, key_arg)?;

        let desc_arg = get_argument(cx, arguments, 2);
        let desc = to_property_descriptor(cx, desc_arg)?;

        let result = target.define_own_property(cx, key, desc)?;
        Ok(cx.bool(result))
    }

    /// Reflect.deleteProperty (https://tc39.es/ecma262/#sec-reflect.deleteproperty)
    pub fn delete_property(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "Reflect.deleteProperty target must be an object");
        }

        let mut target = target.as_object();
        let key_arg = get_argument(cx, arguments, 1);
        let key = to_property_key(cx, key_arg)?;

        let result = target.delete(cx, key)?;
        Ok(cx.bool(result))
    }

    /// Reflect.get (https://tc39.es/ecma262/#sec-reflect.get)
    pub fn get(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "Reflect.get target must be an object");
        }

        let key_arg = get_argument(cx, arguments, 1);
        let key = to_property_key(cx, key_arg)?;

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
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "Reflect.getOwnPropertyDescriptor target must be an object");
        }

        let target = target.as_object();
        let key_arg = get_argument(cx, arguments, 1);
        let key = to_property_key(cx, key_arg)?;

        let desc = target.get_own_property(cx, key)?;

        Ok(desc
            .map(|desc| from_property_descriptor(cx, desc))
            .transpose()?
            .map(|desc_object| desc_object.as_value())
            .unwrap_or(cx.undefined()))
    }

    /// Reflect.getPrototypeOf (https://tc39.es/ecma262/#sec-reflect.getprototypeof)
    pub fn get_prototype_of(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "Reflect.getPrototypeOf target must be an object");
        }

        let prototype = target.as_object().get_prototype_of(cx)?;

        Ok(prototype.map(|proto| proto.into()).unwrap_or(cx.null()))
    }

    /// Reflect.has (https://tc39.es/ecma262/#sec-reflect.has)
    pub fn has(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "Reflect.has target must be an object");
        }

        let target = target.as_object();
        let key_arg = get_argument(cx, arguments, 1);
        let key = to_property_key(cx, key_arg)?;

        let has_property = target.has_property(cx, key)?;
        Ok(cx.bool(has_property))
    }

    /// Reflect.isExtensible (https://tc39.es/ecma262/#sec-reflect.isextensible)
    pub fn is_extensible(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "Reflect.isExtensible target must be an object");
        }

        let is_extensible = target.as_object().is_extensible(cx)?;
        Ok(cx.bool(is_extensible))
    }

    /// Reflect.ownKeys (https://tc39.es/ecma262/#sec-reflect.ownkeys)
    pub fn own_keys(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "Reflect.ownKeys target must be an object");
        }

        let own_keys = target.as_object().own_property_keys(cx)?;

        Ok(create_array_from_list(cx, &own_keys)?.as_value())
    }

    /// Reflect.preventExtensions (https://tc39.es/ecma262/#sec-reflect.preventextensions)
    pub fn prevent_extensions(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "Reflect.preventExtensions target must be an object");
        }

        let result = target.as_object().prevent_extensions(cx)?;
        Ok(cx.bool(result))
    }

    /// Reflect.set (https://tc39.es/ecma262/#sec-reflect.set)
    pub fn set(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "Reflect.set target must be an object");
        }

        let key_arg = get_argument(cx, arguments, 1);
        let key = to_property_key(cx, key_arg)?;
        let value = get_argument(cx, arguments, 2);

        let receiver = if arguments.len() >= 4 {
            get_argument(cx, arguments, 3)
        } else {
            target
        };

        let result = target.as_object().set(cx, key, value, receiver)?;
        Ok(cx.bool(result))
    }

    /// Reflect.setPrototypeOf (https://tc39.es/ecma262/#sec-reflect.setprototypeof)
    pub fn set_prototype_of(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let target = get_argument(cx, arguments, 0);
        if !target.is_object() {
            return type_error(cx, "Reflect.setPrototypeOf target must be an object");
        }

        let proto = get_argument(cx, arguments, 1);
        let proto = if proto.is_object() {
            Some(proto.as_object())
        } else if proto.is_null() {
            None
        } else {
            return type_error(cx, "Reflect.setPrototypeOf prototype must be an object or null");
        };

        let result = target.as_object().set_prototype_of(cx, proto)?;
        Ok(cx.bool(result))
    }
}
