use crate::{
    js::runtime::{
        abstract_operations::{
            create_data_property_or_throw, define_property_or_throw, enumerable_own_property_names,
            get, group_by, has_own_property, is_extensible, set, set_integrity_level,
            test_integrity_level, GroupByKeyCoercion, IntegrityLevel, KeyOrValue,
        },
        array_object::create_array_from_list,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error,
        function::get_argument,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::{
            object_create_from_constructor, object_create_with_optional_proto,
            ordinary_object_create,
        },
        property_descriptor::{from_property_descriptor, to_property_descriptor},
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::{require_object_coercible, same_value, to_object, to_property_key},
        Context, Handle, Value,
    },
    maybe, must,
};

use super::{intrinsics::Intrinsic, map_constructor::add_entries_from_iterable};

pub struct ObjectConstructor;

impl ObjectConstructor {
    /// Properties of the Object Constructor, https://tc39.es/ecma262/#sec-properties-of-the-object-constructor
    pub fn new(cx: Context, realm: Handle<Realm>) -> Handle<ObjectValue> {
        let mut func = BuiltinFunction::intrinsic_constructor(
            cx,
            Self::construct,
            1,
            cx.names.object(),
            realm,
            None,
        );

        func.intrinsic_frozen_property(
            cx,
            cx.names.prototype(),
            realm.get_intrinsic(Intrinsic::ObjectPrototype).into(),
        );

        func.intrinsic_func(cx, cx.names.assign(), Self::assign, 2, realm);
        func.intrinsic_func(cx, cx.names.create(), Self::create, 2, realm);
        func.intrinsic_func(cx, cx.names.define_properties(), Self::define_properties, 2, realm);
        func.intrinsic_func(cx, cx.names.define_property(), Self::define_property, 3, realm);
        func.intrinsic_func(cx, cx.names.from_entries(), Self::from_entries, 1, realm);
        func.intrinsic_func(
            cx,
            cx.names.get_own_property_descriptor(),
            Self::get_own_property_descriptor,
            2,
            realm,
        );
        func.intrinsic_func(
            cx,
            cx.names.get_own_property_descriptors(),
            Self::get_own_property_descriptors,
            1,
            realm,
        );
        func.intrinsic_func(cx, cx.names.entries(), Self::entries, 1, realm);
        func.intrinsic_func(cx, cx.names.freeze(), Self::freeze, 1, realm);
        func.intrinsic_func(
            cx,
            cx.names.get_own_property_names(),
            Self::get_own_property_names,
            1,
            realm,
        );
        func.intrinsic_func(
            cx,
            cx.names.get_own_property_symbols(),
            Self::get_own_property_symbols,
            1,
            realm,
        );
        func.intrinsic_func(cx, cx.names.get_prototype_of(), Self::get_prototype_of, 1, realm);
        func.intrinsic_func(cx, cx.names.group_by(), Self::group_by, 2, realm);
        func.intrinsic_func(cx, cx.names.has_own(), Self::has_own, 2, realm);
        func.intrinsic_func(cx, cx.names.is(), Self::is, 2, realm);
        func.intrinsic_func(cx, cx.names.is_extensible(), Self::is_extensible, 1, realm);
        func.intrinsic_func(cx, cx.names.is_frozen(), Self::is_frozen, 1, realm);
        func.intrinsic_func(cx, cx.names.is_sealed(), Self::is_sealed, 1, realm);
        func.intrinsic_func(cx, cx.names.keys(), Self::keys, 1, realm);
        func.intrinsic_func(cx, cx.names.prevent_extensions(), Self::prevent_extensions, 1, realm);
        func.intrinsic_func(cx, cx.names.seal(), Self::seal, 1, realm);
        func.intrinsic_func(cx, cx.names.set_prototype_of(), Self::set_prototype_of, 2, realm);
        func.intrinsic_func(cx, cx.names.values(), Self::values, 1, realm);

        func
    }

    /// Object, https://tc39.es/ecma262/#sec-object-value
    pub fn construct(
        mut cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        new_target: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        if let Some(new_target) = new_target {
            if !cx.current_function().ptr_eq(&new_target) {
                let new_object = maybe!(object_create_from_constructor::<ObjectValue>(
                    cx,
                    new_target,
                    ObjectKind::OrdinaryObject,
                    Intrinsic::ObjectPrototype,
                ));
                return new_object.to_handle().into();
            }
        }

        let value = get_argument(cx, arguments, 0);
        if value.is_nullish() {
            let new_value: Handle<Value> = ordinary_object_create(cx).into();
            return new_value.into();
        }

        must!(to_object(cx, value)).into()
    }

    /// Object.assign, https://tc39.es/ecma262/#sec-object.assign
    pub fn assign(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let to_arg = get_argument(cx, arguments, 0);
        let to = maybe!(to_object(cx, to_arg));

        if arguments.len() <= 1 {
            return to.into();
        }

        // Shared between iterations
        let mut property_key = PropertyKey::uninit().to_handle(cx);

        for argument in &arguments[1..] {
            if !argument.is_nullish() {
                let from = must!(to_object(cx, *argument));
                let keys = maybe!(from.own_property_keys(cx));

                for next_key in keys {
                    property_key.replace(must!(PropertyKey::from_value(cx, next_key)));
                    let desc = maybe!(from.get_own_property(cx, property_key));
                    if let Some(desc) = desc {
                        if let Some(true) = desc.is_enumerable {
                            let value = maybe!(get(cx, from, property_key));
                            maybe!(set(cx, to, property_key, value, true));
                        }
                    }
                }
            }
        }

        to.into()
    }

    /// Object.create, https://tc39.es/ecma262/#sec-object.create
    pub fn create(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let proto = get_argument(cx, arguments, 0);
        let proto = if proto.is_object() {
            Some(proto.as_object())
        } else if proto.is_null() {
            None
        } else {
            return type_error(cx, "prototype must be an object or null");
        };

        let object =
            object_create_with_optional_proto::<ObjectValue>(cx, ObjectKind::OrdinaryObject, proto)
                .to_handle();

        let properties = get_argument(cx, arguments, 1);
        if properties.is_undefined() {
            object.into()
        } else {
            Self::object_define_properties(cx, object, properties)
        }
    }

    /// Object.defineProperties, https://tc39.es/ecma262/#sec-object.defineproperties
    pub fn define_properties(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = get_argument(cx, arguments, 0);
        if !object.is_object() {
            return type_error(cx, "value is not an object");
        }

        let properties_arg = get_argument(cx, arguments, 1);
        Self::object_define_properties(cx, object.as_object(), properties_arg)
    }

    /// ObjectDefineProperties, https://tc39.es/ecma262/#sec-objectdefineproperties
    pub fn object_define_properties(
        cx: Context,
        object: Handle<ObjectValue>,
        properties: Handle<Value>,
    ) -> EvalResult<Handle<Value>> {
        let properties = maybe!(to_object(cx, properties));

        let keys = maybe!(properties.own_property_keys(cx));

        let mut descriptors = vec![];

        for key_value in keys {
            let key = must!(PropertyKey::from_value(cx, key_value)).to_handle(cx);
            let prop_desc = maybe!(properties.get_own_property(cx, key));
            if let Some(prop_desc) = prop_desc {
                if let Some(true) = prop_desc.is_enumerable {
                    let desc_object = maybe!(get(cx, properties, key));
                    let desc = maybe!(to_property_descriptor(cx, desc_object));

                    descriptors.push((key, desc));
                }
            }
        }

        for (key, desc) in descriptors {
            maybe!(define_property_or_throw(cx, object, key, desc));
        }

        object.into()
    }

    /// Object.defineProperty, https://tc39.es/ecma262/#sec-object.defineproperty
    pub fn define_property(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = get_argument(cx, arguments, 0);
        if !object.is_object() {
            return type_error(cx, "can only define property on an object");
        }

        let property_arg = get_argument(cx, arguments, 1);
        let property_key = maybe!(to_property_key(cx, property_arg));

        let desc_arg = get_argument(cx, arguments, 2);
        let desc = maybe!(to_property_descriptor(cx, desc_arg));

        maybe!(define_property_or_throw(cx, object.as_object(), property_key, desc,));

        object.into()
    }

    /// Object.entries, https://tc39.es/ecma262/#sec-object.defineproperty
    pub fn entries(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object_arg = get_argument(cx, arguments, 0);
        let object = maybe!(to_object(cx, object_arg));
        let name_list = maybe!(enumerable_own_property_names(cx, object, KeyOrValue::KeyAndValue));
        create_array_from_list(cx, &name_list).into()
    }

    /// Object.freeze, https://tc39.es/ecma262/#sec-object.freeze
    pub fn freeze(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = get_argument(cx, arguments, 0);
        if !object.is_object() {
            return object.into();
        }

        if !maybe!(set_integrity_level(cx, object.as_object(), IntegrityLevel::Frozen)) {
            return type_error(cx, "failed to freeze object");
        }

        object.into()
    }

    /// Object.fromEntries, https://tc39.es/ecma262/#sec-object.fromentries
    pub fn from_entries(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let iterable_arg = get_argument(cx, arguments, 0);
        let iterable = maybe!(require_object_coercible(cx, iterable_arg));

        let object = ordinary_object_create(cx);

        add_entries_from_iterable(cx, object.into(), iterable, |cx, key, value| {
            let property_key = maybe!(to_property_key(cx, key));
            must!(create_data_property_or_throw(cx, object, property_key, value));
            ().into()
        })
    }

    /// Object.getOwnPropertyDescriptor, https://tc39.es/ecma262/#sec-object.getownpropertydescriptor
    pub fn get_own_property_descriptor(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object_arg = get_argument(cx, arguments, 0);
        let object = maybe!(to_object(cx, object_arg));

        let property_arg = get_argument(cx, arguments, 1);
        let property_key = maybe!(to_property_key(cx, property_arg));

        match maybe!(object.get_own_property(cx, property_key)) {
            None => cx.undefined().into(),
            Some(desc) => from_property_descriptor(cx, desc).into(),
        }
    }

    /// Object.getOwnPropertyDescriptors, https://tc39.es/ecma262/#sec-object.getownpropertydescriptors
    pub fn get_own_property_descriptors(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object_arg = get_argument(cx, arguments, 0);
        let object = maybe!(to_object(cx, object_arg));

        let keys = maybe!(object.own_property_keys(cx));

        let descriptors = ordinary_object_create(cx);

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for key_value in keys {
            key.replace(must!(PropertyKey::from_value(cx, key_value)));
            let desc = maybe!(object.get_own_property(cx, key));
            if let Some(desc) = desc {
                let desc_object = from_property_descriptor(cx, desc);
                must!(create_data_property_or_throw(cx, descriptors, key, desc_object.into()));
            }
        }

        descriptors.into()
    }

    /// Object.getOwnPropertyNames, https://tc39.es/ecma262/#sec-object.getownpropertynames
    pub fn get_own_property_names(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object_arg = get_argument(cx, arguments, 0);
        let symbol_keys = maybe!(Self::get_own_property_keys(cx, object_arg, true));
        create_array_from_list(cx, &symbol_keys).into()
    }

    /// Object.getOwnPropertySymbols, https://tc39.es/ecma262/#sec-object.getownpropertysymbols
    pub fn get_own_property_symbols(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object_arg = get_argument(cx, arguments, 0);
        let symbol_keys = maybe!(Self::get_own_property_keys(cx, object_arg, false));
        create_array_from_list(cx, &symbol_keys).into()
    }

    /// GetOwnPropertyKeys, https://tc39.es/ecma262/#sec-getownpropertykeys
    pub fn get_own_property_keys(
        cx: Context,
        object: Handle<Value>,
        string_keys: bool,
    ) -> EvalResult<Vec<Handle<Value>>> {
        let object = maybe!(to_object(cx, object));
        let keys = maybe!(object.own_property_keys(cx));

        let keys_of_type: Vec<Handle<Value>> = keys
            .into_iter()
            .filter(|key| {
                if string_keys {
                    key.is_string()
                } else {
                    key.is_symbol()
                }
            })
            .collect();

        keys_of_type.into()
    }

    /// Object.getPrototypeOf, https://tc39.es/ecma262/#sec-object.getprototypeof
    pub fn get_prototype_of(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object_arg = get_argument(cx, arguments, 0);
        let object = maybe!(to_object(cx, object_arg));
        let prototype = maybe!(object.get_prototype_of(cx));

        match prototype {
            None => cx.null().into(),
            Some(prototype) => prototype.into(),
        }
    }

    /// Object.groupBy, https://tc39.es/ecma262/#sec-object.groupby
    pub fn group_by(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let items = get_argument(cx, arguments, 0);
        let callback = get_argument(cx, arguments, 1);

        let groups = maybe!(group_by(cx, items, callback, GroupByKeyCoercion::Property));

        let object =
            object_create_with_optional_proto::<ObjectValue>(cx, ObjectKind::OrdinaryObject, None)
                .to_handle();

        for group in groups {
            let property_key = group.key.cast::<PropertyKey>();
            let items = create_array_from_list(cx, &group.items);
            must!(create_data_property_or_throw(cx, object, property_key, items.into()));
        }

        object.into()
    }

    /// Object.hasOwn, https://tc39.es/ecma262/#sec-object.hasown
    pub fn has_own(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object_arg = get_argument(cx, arguments, 0);
        let object = maybe!(to_object(cx, object_arg));

        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));

        let has_own = maybe!(has_own_property(cx, object, key));
        cx.bool(has_own).into()
    }

    /// Object.is, https://tc39.es/ecma262/#sec-object.is
    pub fn is(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let is_same = same_value(get_argument(cx, arguments, 0), get_argument(cx, arguments, 1));
        cx.bool(is_same).into()
    }

    /// Object.isExtensible, https://tc39.es/ecma262/#sec-object.isextensible
    pub fn is_extensible(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        if !value.is_object() {
            return cx.bool(false).into();
        }

        let is_extensible = maybe!(is_extensible(cx, value.as_object()));
        cx.bool(is_extensible).into()
    }

    /// Object.isFrozen, https://tc39.es/ecma262/#sec-object.isfrozen
    pub fn is_frozen(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        if !value.is_object() {
            return cx.bool(true).into();
        }

        let is_frozen = maybe!(test_integrity_level(cx, value.as_object(), IntegrityLevel::Frozen));
        cx.bool(is_frozen).into()
    }

    /// Object.isSealed, https://tc39.es/ecma262/#sec-object.issealed
    pub fn is_sealed(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        if !value.is_object() {
            return cx.bool(true).into();
        }

        let is_sealed = maybe!(test_integrity_level(cx, value.as_object(), IntegrityLevel::Sealed));
        cx.bool(is_sealed).into()
    }

    /// Object.keys, https://tc39.es/ecma262/#sec-object.keys
    pub fn keys(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object_arg = get_argument(cx, arguments, 0);
        let object = maybe!(to_object(cx, object_arg));
        let name_list = maybe!(enumerable_own_property_names(cx, object, KeyOrValue::Key));
        create_array_from_list(cx, &name_list).into()
    }

    /// Object.preventExtensions, https://tc39.es/ecma262/#sec-object.preventextensions
    pub fn prevent_extensions(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
        if !value.is_object() {
            return value.into();
        }

        if !maybe!(value.as_object().prevent_extensions(cx)) {
            return type_error(cx, "failed to prevent extensions on object");
        }

        value.into()
    }

    /// Object.seal, https://tc39.es/ecma262/#sec-object.seal
    pub fn seal(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object = get_argument(cx, arguments, 0);
        if !object.is_object() {
            return object.into();
        }

        if !maybe!(set_integrity_level(cx, object.as_object(), IntegrityLevel::Sealed)) {
            return type_error(cx, "failed to seal object");
        }

        object.into()
    }

    /// Object.setPrototypeOf, https://tc39.es/ecma262/#sec-object.setprototypeof
    pub fn set_prototype_of(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object_arg = get_argument(cx, arguments, 0);
        let object = maybe!(require_object_coercible(cx, object_arg));

        let proto = get_argument(cx, arguments, 1);
        let proto = if proto.is_object() {
            Some(proto.as_object())
        } else if proto.is_null() {
            None
        } else {
            return type_error(cx, "prototype must be an object or null");
        };

        if !object.is_object() {
            return object.into();
        }
        let mut object = object.as_object();

        if !maybe!(object.set_prototype_of(cx, proto)) {
            return type_error(cx, "failed to set object prototype");
        }

        object.into()
    }

    /// Object.values, https://tc39.es/ecma262/#sec-object.values
    pub fn values(
        cx: Context,
        _: Handle<Value>,
        arguments: &[Handle<Value>],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<Handle<Value>> {
        let object_arg = get_argument(cx, arguments, 0);
        let object = maybe!(to_object(cx, object_arg));
        let name_list = maybe!(enumerable_own_property_names(cx, object, KeyOrValue::Value));
        create_array_from_list(cx, &name_list).into()
    }
}
