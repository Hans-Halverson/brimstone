use crate::{
    intrinsic_methods, must,
    runtime::{
        Context, Handle, HeapItemKind, Value,
        abstract_operations::{
            GroupByKeyCoercion, IntegrityLevel, KeyOrValue, create_data_property_or_throw,
            define_property_or_throw, enumerable_own_property_names, get, group_by,
            has_own_property, is_extensible, set, set_integrity_level, test_integrity_level,
        },
        alloc_error::AllocResult,
        array_object::create_array_from_list,
        error::type_error,
        eval_result::EvalResult,
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            intrinsics::Intrinsic, map_constructor::add_entries_from_iterable,
            rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        ordinary_object::{
            object_create_from_constructor, object_create_with_optional_proto,
            ordinary_object_create, ordinary_object_create_without_proto,
        },
        property_descriptor::{from_property_descriptor, to_property_descriptor},
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::{require_object_coercible, same_value, to_object, to_property_key},
    },
    runtime_fn,
};

pub struct ObjectConstructor;

impl ObjectConstructor {
    /// Properties of the Object Constructor (https://tc39.es/ecma262/#sec-properties-of-the-object-constructor)
    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<Handle<ObjectValue>> {
        let mut builder = IntrinsicBuilder::constructor(
            cx,
            realm,
            RuntimeFunction::ObjectConstructor_construct,
            1,
            cx.names.object(),
            Intrinsic::FunctionPrototype,
        )?;

        builder.prototype(Intrinsic::ObjectPrototype)?;

        intrinsic_methods!(cx, builder, {
            assign                       ObjectConstructor_assign                       (2),
            create                       ObjectConstructor_create                       (2),
            define_properties            ObjectConstructor_define_properties            (2),
            define_property              ObjectConstructor_define_property              (3),
            from_entries                 ObjectConstructor_from_entries                 (1),
            get_own_property_descriptor  ObjectConstructor_get_own_property_descriptor  (2),
            get_own_property_descriptors ObjectConstructor_get_own_property_descriptors (1),
            entries                      ObjectConstructor_entries                      (1),
            freeze                       ObjectConstructor_freeze                       (1),
            get_own_property_names       ObjectConstructor_get_own_property_names       (1),
            get_own_property_symbols     ObjectConstructor_get_own_property_symbols     (1),
            get_prototype_of             ObjectConstructor_get_prototype_of             (1),
            group_by                     ObjectConstructor_group_by                     (2),
            has_own                      ObjectConstructor_has_own                      (2),
            is                           ObjectConstructor_is                           (2),
            is_extensible                ObjectConstructor_is_extensible                (1),
            is_frozen                    ObjectConstructor_is_frozen                    (1),
            is_sealed                    ObjectConstructor_is_sealed                    (1),
            keys                         ObjectConstructor_keys                         (1),
            prevent_extensions           ObjectConstructor_prevent_extensions           (1),
            seal                         ObjectConstructor_seal                         (1),
            set_prototype_of             ObjectConstructor_set_prototype_of             (2),
            values                       ObjectConstructor_values                       (1),
        });

        builder.build()
    }

    runtime_fn! {
    /// Object (https://tc39.es/ecma262/#sec-object-value)
    fn construct(cx, _, arguments) {
        if let Some(new_target) = cx.current_new_target() {
            if !cx.current_function().ptr_eq(&new_target) {
                let new_object = object_create_from_constructor::<ObjectValue>(
                    cx,
                    new_target,
                    HeapItemKind::OrdinaryObject,
                    Intrinsic::ObjectPrototype,
                )?;
                return Ok(new_object.to_handle().as_value());
            }
        }

        let value = arguments.get(cx, 0);
        if value.is_nullish() {
            let new_value: Handle<Value> = ordinary_object_create(cx)?.into();
            return Ok(new_value);
        }

        Ok(must!(to_object(cx, value)).as_value())
    }}

    runtime_fn! {
    /// Object.assign (https://tc39.es/ecma262/#sec-object.assign)
    fn assign(cx, _, arguments) {
        let to_arg = arguments.get(cx, 0);
        let to = to_object(cx, to_arg)?;

        if arguments.len() <= 1 {
            return Ok(to.as_value());
        }

        // Shared between iterations
        let mut property_key = PropertyKey::uninit().to_handle(cx);

        for argument in &arguments[1..] {
            if !argument.is_nullish() {
                let from = must!(to_object(cx, *argument));
                let keys = from.own_property_keys(cx)?;

                for next_key in keys {
                    property_key.replace(must!(PropertyKey::from_value(cx, next_key)));
                    let desc = from.get_own_property(cx, property_key)?;
                    if let Some(desc) = desc {
                        if let Some(true) = desc.is_enumerable {
                            let value = get(cx, from, property_key)?;
                            set(cx, to, property_key, value, true)?;
                        }
                    }
                }
            }
        }

        Ok(to.as_value())
    }}

    runtime_fn! {
    /// Object.create (https://tc39.es/ecma262/#sec-object.create)
    fn create(cx, _, arguments) {
        let proto = arguments.get(cx, 0);
        let proto = if proto.is_object() {
            Some(proto.as_object())
        } else if proto.is_null() {
            None
        } else {
            return type_error(cx, "Object.create prototype must be an object or null");
        };

        let object = object_create_with_optional_proto::<ObjectValue>(
            cx,
            HeapItemKind::OrdinaryObject,
            proto,
        )?
        .to_handle();

        let properties = arguments.get(cx, 1);
        if properties.is_undefined() {
            Ok(object.as_value())
        } else {
            Self::object_define_properties(cx, object, properties)
        }
    }}

    runtime_fn! {
    /// Object.defineProperties (https://tc39.es/ecma262/#sec-object.defineproperties)
    fn define_properties(cx, _, arguments) {
        let object = arguments.get(cx, 0);
        if !object.is_object() {
            return type_error(cx, "Object.defineProperties target must be an object");
        }

        let properties_arg = arguments.get(cx, 1);
        Self::object_define_properties(cx, object.as_object(), properties_arg)
    }}

    /// ObjectDefineProperties (https://tc39.es/ecma262/#sec-objectdefineproperties)
    pub fn object_define_properties(
        cx: Context,
        object: Handle<ObjectValue>,
        properties: Handle<Value>,
    ) -> EvalResult<Handle<Value>> {
        let properties = to_object(cx, properties)?;

        let keys = properties.own_property_keys(cx)?;

        let mut descriptors = vec![];

        for key_value in keys {
            let key = must!(PropertyKey::from_value(cx, key_value)).to_handle(cx);
            let prop_desc = properties.get_own_property(cx, key)?;
            if let Some(prop_desc) = prop_desc {
                if let Some(true) = prop_desc.is_enumerable {
                    let desc_object = get(cx, properties, key)?;
                    let desc = to_property_descriptor(cx, desc_object)?;

                    descriptors.push((key, desc));
                }
            }
        }

        for (key, desc) in descriptors {
            define_property_or_throw(cx, object, key, desc)?;
        }

        Ok(object.as_value())
    }

    runtime_fn! {
    /// Object.defineProperty (https://tc39.es/ecma262/#sec-object.defineproperty)
    fn define_property(cx, _, arguments) {
        let object = arguments.get(cx, 0);
        if !object.is_object() {
            return type_error(cx, "Object.defineProperty target must be an object");
        }

        let property_arg = arguments.get(cx, 1);
        let property_key = to_property_key(cx, property_arg)?;

        let desc_arg = arguments.get(cx, 2);
        let desc = to_property_descriptor(cx, desc_arg)?;

        define_property_or_throw(cx, object.as_object(), property_key, desc)?;

        Ok(object)
    }}

    runtime_fn! {
    /// Object.entries (https://tc39.es/ecma262/#sec-object.defineproperty)
    fn entries(cx, _, arguments) {
        let object_arg = arguments.get(cx, 0);
        let object = to_object(cx, object_arg)?;
        let name_list = enumerable_own_property_names(cx, object, KeyOrValue::KeyAndValue)?;
        Ok(create_array_from_list(cx, &name_list)?.as_value())
    }}

    runtime_fn! {
    /// Object.freeze (https://tc39.es/ecma262/#sec-object.freeze)
    fn freeze(cx, _, arguments) {
        let object = arguments.get(cx, 0);
        if !object.is_object() {
            return Ok(object);
        }

        if !set_integrity_level(cx, object.as_object(), IntegrityLevel::Frozen)? {
            return type_error(cx, "Object.freeze failed to freeze object");
        }

        Ok(object)
    }}

    runtime_fn! {
    /// Object.fromEntries (https://tc39.es/ecma262/#sec-object.fromentries)
    fn from_entries(cx, _, arguments) {
        let iterable_arg = arguments.get(cx, 0);
        let iterable = require_object_coercible(cx, iterable_arg)?;

        let object = ordinary_object_create(cx)?;

        add_entries_from_iterable(cx, object.into(), iterable, |cx, key, value| {
            let property_key = to_property_key(cx, key)?;
            must!(create_data_property_or_throw(cx, object, property_key, value));
            Ok(())
        })
    }}

    runtime_fn! {
    /// Object.getOwnPropertyDescriptor (https://tc39.es/ecma262/#sec-object.getownpropertydescriptor)
    fn get_own_property_descriptor(cx, _, arguments) {
        let object_arg = arguments.get(cx, 0);
        let object = to_object(cx, object_arg)?;

        let property_arg = arguments.get(cx, 1);
        let property_key = to_property_key(cx, property_arg)?;

        match object.get_own_property(cx, property_key)? {
            None => Ok(cx.undefined()),
            Some(desc) => Ok(from_property_descriptor(cx, desc)?.as_value()),
        }
    }}

    runtime_fn! {
    /// Object.getOwnPropertyDescriptors (https://tc39.es/ecma262/#sec-object.getownpropertydescriptors)
    fn get_own_property_descriptors(cx, _, arguments) {
        let object_arg = arguments.get(cx, 0);
        let object = to_object(cx, object_arg)?;

        let keys = object.own_property_keys(cx)?;

        let descriptors = ordinary_object_create(cx)?;

        // Shared between iterations
        let mut key = PropertyKey::uninit().to_handle(cx);

        for key_value in keys {
            key.replace(must!(PropertyKey::from_value(cx, key_value)));
            let desc = object.get_own_property(cx, key)?;
            if let Some(desc) = desc {
                let desc_object = from_property_descriptor(cx, desc)?;
                must!(create_data_property_or_throw(cx, descriptors, key, desc_object.into()));
            }
        }

        Ok(descriptors.as_value())
    }}

    runtime_fn! {
    /// Object.getOwnPropertyNames (https://tc39.es/ecma262/#sec-object.getownpropertynames)
    fn get_own_property_names(cx, _, arguments) {
        let object_arg = arguments.get(cx, 0);
        let symbol_keys = Self::get_own_property_keys(cx, object_arg, true)?;
        Ok(create_array_from_list(cx, &symbol_keys)?.as_value())
    }}

    runtime_fn! {
    /// Object.getOwnPropertySymbols (https://tc39.es/ecma262/#sec-object.getownpropertysymbols)
    fn get_own_property_symbols(cx, _, arguments) {
        let object_arg = arguments.get(cx, 0);
        let symbol_keys = Self::get_own_property_keys(cx, object_arg, false)?;
        Ok(create_array_from_list(cx, &symbol_keys)?.as_value())
    }}

    /// GetOwnPropertyKeys (https://tc39.es/ecma262/#sec-getownpropertykeys)
    pub fn get_own_property_keys(
        cx: Context,
        object: Handle<Value>,
        string_keys: bool,
    ) -> EvalResult<Vec<Handle<Value>>> {
        let object = to_object(cx, object)?;
        let keys = object.own_property_keys(cx)?;

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

        Ok(keys_of_type)
    }

    runtime_fn! {
    /// Object.getPrototypeOf (https://tc39.es/ecma262/#sec-object.getprototypeof)
    fn get_prototype_of(cx, _, arguments) {
        let object_arg = arguments.get(cx, 0);
        let object = to_object(cx, object_arg)?;
        let prototype = object.get_prototype_of(cx)?;

        match prototype {
            None => Ok(cx.null()),
            Some(prototype) => Ok(prototype.as_value()),
        }
    }}

    runtime_fn! {
    /// Object.groupBy (https://tc39.es/ecma262/#sec-object.groupby)
    fn group_by(cx, _, arguments) {
        let items = arguments.get(cx, 0);
        let callback = arguments.get(cx, 1);

        let groups = group_by(cx, items, callback, GroupByKeyCoercion::Property)?;

        let object = ordinary_object_create_without_proto(cx)?;

        for group in groups {
            let property_key = group.key.cast::<PropertyKey>();
            let items = create_array_from_list(cx, &group.items)?;
            must!(create_data_property_or_throw(cx, object, property_key, items.into()));
        }

        Ok(object.as_value())
    }}

    runtime_fn! {
    /// Object.hasOwn (https://tc39.es/ecma262/#sec-object.hasown)
    fn has_own(cx, _, arguments) {
        let object_arg = arguments.get(cx, 0);
        let object = to_object(cx, object_arg)?;

        let key_arg = arguments.get(cx, 1);
        let key = to_property_key(cx, key_arg)?;

        let has_own = has_own_property(cx, object, key)?;
        Ok(cx.bool(has_own))
    }}

    runtime_fn! {
    /// Object.is (https://tc39.es/ecma262/#sec-object.is)
    fn is(cx, _, arguments) {
        let is_same = same_value(arguments.get(cx, 0), arguments.get(cx, 1))?;
        Ok(cx.bool(is_same))
    }}

    runtime_fn! {
    /// Object.isExtensible (https://tc39.es/ecma262/#sec-object.isextensible)
    fn is_extensible(cx, _, arguments) {
        let value = arguments.get(cx, 0);
        if !value.is_object() {
            return Ok(cx.bool(false));
        }

        let is_extensible = is_extensible(cx, value.as_object())?;
        Ok(cx.bool(is_extensible))
    }}

    runtime_fn! {
    /// Object.isFrozen (https://tc39.es/ecma262/#sec-object.isfrozen)
    fn is_frozen(cx, _, arguments) {
        let value = arguments.get(cx, 0);
        if !value.is_object() {
            return Ok(cx.bool(true));
        }

        let is_frozen = test_integrity_level(cx, value.as_object(), IntegrityLevel::Frozen)?;
        Ok(cx.bool(is_frozen))
    }}

    runtime_fn! {
    /// Object.isSealed (https://tc39.es/ecma262/#sec-object.issealed)
    fn is_sealed(cx, _, arguments) {
        let value = arguments.get(cx, 0);
        if !value.is_object() {
            return Ok(cx.bool(true));
        }

        let is_sealed = test_integrity_level(cx, value.as_object(), IntegrityLevel::Sealed)?;
        Ok(cx.bool(is_sealed))
    }}

    runtime_fn! {
    /// Object.keys (https://tc39.es/ecma262/#sec-object.keys)
    fn keys(cx, _, arguments) {
        let object_arg = arguments.get(cx, 0);
        let object = to_object(cx, object_arg)?;
        let name_list = enumerable_own_property_names(cx, object, KeyOrValue::Key)?;
        Ok(create_array_from_list(cx, &name_list)?.as_value())
    }}

    runtime_fn! {
    /// Object.preventExtensions (https://tc39.es/ecma262/#sec-object.preventextensions)
    fn prevent_extensions(cx, _, arguments) {
        let value = arguments.get(cx, 0);
        if !value.is_object() {
            return Ok(value);
        }

        if !value.as_object().prevent_extensions(cx)? {
            return type_error(
                cx,
                "Object.preventExtensions failed to prevent extensions on object",
            );
        }

        Ok(value)
    }}

    runtime_fn! {
    /// Object.seal (https://tc39.es/ecma262/#sec-object.seal)
    fn seal(cx, _, arguments) {
        let object = arguments.get(cx, 0);
        if !object.is_object() {
            return Ok(object);
        }

        if !set_integrity_level(cx, object.as_object(), IntegrityLevel::Sealed)? {
            return type_error(cx, "Object.seal failed to seal object");
        }

        Ok(object)
    }}

    runtime_fn! {
    /// Object.setPrototypeOf (https://tc39.es/ecma262/#sec-object.setprototypeof)
    fn set_prototype_of(cx, _, arguments) {
        let object_arg = arguments.get(cx, 0);
        let object = require_object_coercible(cx, object_arg)?;

        let proto = arguments.get(cx, 1);
        let proto = if proto.is_object() {
            Some(proto.as_object())
        } else if proto.is_null() {
            None
        } else {
            return type_error(cx, "Object.setPrototypeOf prototype must be an object or null");
        };

        if !object.is_object() {
            return Ok(object);
        }
        let mut object = object.as_object();

        if !object.set_prototype_of(cx, proto)? {
            return type_error(cx, "Object.setPrototypeOf failed to set object prototype");
        }

        Ok(object.as_value())
    }}

    runtime_fn! {
    /// Object.values (https://tc39.es/ecma262/#sec-object.values)
    fn values(cx, _, arguments) {
        let object_arg = arguments.get(cx, 0);
        let object = to_object(cx, object_arg)?;
        let name_list = enumerable_own_property_names(cx, object, KeyOrValue::Value)?;
        Ok(create_array_from_list(cx, &name_list)?.as_value())
    }}
}
