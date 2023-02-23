use crate::{
    js::runtime::{
        abstract_operations::{
            create_data_property_or_throw, define_property_or_throw, enumerable_own_property_names,
            get, has_own_property, is_extensible, set, set_integrity_level, test_integrity_level,
            IntegrityLevel, KeyOrValue,
        },
        array_object::create_array_from_list,
        builtin_function::BuiltinFunction,
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        gc::Gc,
        object_value::ObjectValue,
        ordinary_object::{
            ordinary_create_from_constructor, ordinary_object_create,
            ordinary_object_create_optional_proto,
        },
        property::Property,
        property_descriptor::{from_property_descriptor, to_property_descriptor},
        property_key::PropertyKey,
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

        func.intrinsic_func(cx, &cx.names.assign(), Self::assign, 2, realm);
        func.intrinsic_func(cx, &cx.names.create(), Self::create, 2, realm);
        func.intrinsic_func(cx, &cx.names.define_properties(), Self::define_properties, 2, realm);
        func.intrinsic_func(cx, &cx.names.define_property(), Self::define_property, 3, realm);
        func.intrinsic_func(
            cx,
            &cx.names.get_own_property_descriptor(),
            Self::get_own_property_descriptor,
            2,
            realm,
        );
        func.intrinsic_func(
            cx,
            &cx.names.get_own_property_descriptors(),
            Self::get_own_property_descriptors,
            1,
            realm,
        );
        func.intrinsic_func(cx, &cx.names.entries(), Self::entries, 1, realm);
        func.intrinsic_func(cx, &cx.names.freeze(), Self::freeze, 1, realm);
        func.intrinsic_func(
            cx,
            &cx.names.get_own_property_names(),
            Self::get_own_property_names,
            1,
            realm,
        );
        func.intrinsic_func(
            cx,
            &cx.names.get_own_property_symbols(),
            Self::get_own_property_symbols,
            1,
            realm,
        );
        func.intrinsic_func(cx, &cx.names.get_prototype_of(), Self::get_prototype_of, 1, realm);
        func.intrinsic_func(cx, &cx.names.has_own(), Self::has_own, 2, realm);
        func.intrinsic_func(cx, &cx.names.is(), Self::is, 2, realm);
        func.intrinsic_func(cx, &cx.names.is_extensible(), Self::is_extensible, 1, realm);
        func.intrinsic_func(cx, &cx.names.is_frozen(), Self::is_frozen, 1, realm);
        func.intrinsic_func(cx, &cx.names.is_sealed(), Self::is_sealed, 1, realm);
        func.intrinsic_func(cx, &cx.names.keys(), Self::keys, 1, realm);
        func.intrinsic_func(cx, &cx.names.prevent_extensions(), Self::prevent_extensions, 1, realm);
        func.intrinsic_func(cx, &cx.names.seal(), Self::seal, 1, realm);
        func.intrinsic_func(cx, &cx.names.set_prototype_of(), Self::set_prototype_of, 2, realm);
        func.intrinsic_func(cx, &cx.names.values(), Self::values, 1, realm);

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
            if !cx
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

    // 20.1.2.1 Object.assign
    fn assign(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let to = maybe!(to_object(cx, get_argument(arguments, 0)));

        if arguments.len() <= 1 {
            return to.into();
        }

        for argument in &arguments[1..] {
            if !argument.is_nullish() {
                let from = must!(to_object(cx, *argument));
                let keys = maybe!(from.own_property_keys(cx));

                for next_key in keys {
                    let property_key = must!(PropertyKey::from_value(cx, next_key));
                    let desc = maybe!(from.get_own_property(cx, &property_key));
                    if let Some(desc) = desc {
                        if let Some(true) = desc.is_enumerable {
                            let value = maybe!(get(cx, from, &property_key));
                            maybe!(set(cx, to, &property_key, value, true));
                        }
                    }
                }
            }
        }

        to.into()
    }

    // 20.1.2.2 Object.create
    fn create(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let proto = get_argument(arguments, 0);
        let proto = if proto.is_object() {
            Some(proto.as_object())
        } else if proto.is_null() {
            None
        } else {
            return type_error_(cx, "prototype must be an object or null");
        };

        let object: Gc<ObjectValue> = cx
            .heap
            .alloc(ordinary_object_create_optional_proto(proto))
            .into();

        let properties = get_argument(arguments, 1);
        if properties.is_undefined() {
            object.into()
        } else {
            Self::object_define_properties(cx, object, properties)
        }
    }

    // 20.1.2.3 Object.defineProperties
    fn define_properties(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = get_argument(arguments, 0);
        if !object.is_object() {
            return type_error_(cx, "value is not an object");
        }

        Self::object_define_properties(cx, object.as_object(), get_argument(arguments, 1))
    }

    // 20.1.2.3.1 ObjectDefineProperties
    fn object_define_properties(
        cx: &mut Context,
        object: Gc<ObjectValue>,
        properties: Value,
    ) -> EvalResult<Value> {
        let properties = maybe!(to_object(cx, properties));

        let keys = maybe!(properties.own_property_keys(cx));

        let mut descriptors = vec![];

        for key in keys {
            let key = must!(PropertyKey::from_value(cx, key));
            let prop_desc = maybe!(properties.get_own_property(cx, &key));
            if let Some(prop_desc) = prop_desc {
                if let Some(true) = prop_desc.is_enumerable {
                    let desc_object = maybe!(get(cx, properties, &key));
                    let desc = maybe!(to_property_descriptor(cx, desc_object));

                    descriptors.push((key, desc));
                }
            }
        }

        for (key, desc) in descriptors {
            maybe!(define_property_or_throw(cx, object, &key, desc));
        }

        object.into()
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

    // 20.1.2.5 Object.entries
    fn entries(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, get_argument(arguments, 0)));
        let name_list = maybe!(enumerable_own_property_names(cx, object, KeyOrValue::KeyAndValue));
        create_array_from_list(cx, &name_list).into()
    }

    // 20.1.2.6 Object.freeze
    fn freeze(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = get_argument(arguments, 0);
        if !object.is_object() {
            return object.into();
        }

        if !maybe!(set_integrity_level(cx, object.as_object(), IntegrityLevel::Frozen)) {
            return type_error_(cx, "failed to freeze object");
        }

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

        match maybe!(object.get_own_property(cx, &property_key)) {
            None => Value::undefined().into(),
            Some(desc) => from_property_descriptor(cx, desc).into(),
        }
    }

    // 20.1.2.9 Object.getOwnPropertyDescriptors
    fn get_own_property_descriptors(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, get_argument(arguments, 0)));

        let keys = maybe!(object.own_property_keys(cx));

        let proto = cx.current_realm().get_intrinsic(Intrinsic::ObjectPrototype);
        let descriptors: Gc<ObjectValue> = cx.heap.alloc(ordinary_object_create(proto)).into();

        for key in keys {
            let key = must!(PropertyKey::from_value(cx, key));
            let desc = maybe!(object.get_own_property(cx, &key));
            if let Some(desc) = desc {
                let desc_object = from_property_descriptor(cx, desc);
                must!(create_data_property_or_throw(cx, descriptors, &key, desc_object.into()));
            }
        }

        descriptors.into()
    }

    // 20.1.2.10 Object.getOwnPropertyNames
    fn get_own_property_names(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let symbol_keys = maybe!(Self::get_own_property_keys(cx, get_argument(arguments, 0), true));
        create_array_from_list(cx, &symbol_keys).into()
    }

    // 20.1.2.11 Object.getOwnPropertySymbols
    fn get_own_property_symbols(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let symbol_keys =
            maybe!(Self::get_own_property_keys(cx, get_argument(arguments, 0), false));
        create_array_from_list(cx, &symbol_keys).into()
    }

    // 20.1.2.11.1 GetOwnPropertyKeys
    fn get_own_property_keys(
        cx: &mut Context,
        object: Value,
        string_keys: bool,
    ) -> EvalResult<Vec<Value>> {
        let object = maybe!(to_object(cx, object));
        let keys = maybe!(object.own_property_keys(cx));

        let keys_of_type: Vec<Value> = keys
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

    // 20.1.2.12 Object.getPrototypeOf
    fn get_prototype_of(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, get_argument(arguments, 0)));
        let prototype = maybe!(object.get_prototype_of(cx));

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

        maybe!(has_own_property(cx, object, &key)).into()
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

        maybe!(is_extensible(cx, value.as_object())).into()
    }

    // 20.1.2.16 Object.isFrozen
    fn is_frozen(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let value = get_argument(arguments, 0);
        if !value.is_object() {
            return type_error_(cx, "expected object");
        }

        maybe!(test_integrity_level(cx, value.as_object(), IntegrityLevel::Frozen)).into()
    }

    // 20.1.2.17 Object.isSealed
    fn is_sealed(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let value = get_argument(arguments, 0);
        if !value.is_object() {
            return type_error_(cx, "expected object");
        }

        maybe!(test_integrity_level(cx, value.as_object(), IntegrityLevel::Sealed)).into()
    }

    // 20.1.2.18 Object.keys
    fn keys(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, get_argument(arguments, 0)));
        let name_list = maybe!(enumerable_own_property_names(cx, object, KeyOrValue::Key));
        create_array_from_list(cx, &name_list).into()
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

        if !maybe!(value.as_object().prevent_extensions(cx)) {
            return type_error_(cx, "failed to prevent extensions on object");
        }

        value.into()
    }

    // 20.1.2.21 Object.seal
    fn seal(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = get_argument(arguments, 0);
        if !object.is_object() {
            return object.into();
        }

        if !maybe!(set_integrity_level(cx, object.as_object(), IntegrityLevel::Sealed)) {
            return type_error_(cx, "failed to seal object");
        }

        object.into()
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

        if !maybe!(object.set_prototype_of(cx, proto)) {
            return type_error_(cx, "failed to set object prototype");
        }

        object.into()
    }

    // 20.1.2.23 Object.values
    fn values(
        cx: &mut Context,
        _: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, get_argument(arguments, 0)));
        let name_list = maybe!(enumerable_own_property_names(cx, object, KeyOrValue::Value));
        create_array_from_list(cx, &name_list).into()
    }
}
