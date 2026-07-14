use std::mem::size_of;

use crate::{
    extend_object, intrinsic_methods,
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr,
        abstract_operations::{define_property_or_throw, get, has_own_property, invoke},
        accessor::Accessor,
        alloc_error::AllocResult,
        error::type_error,
        gc::{HeapItem, HeapVisitor},
        intrinsic_builder::IntrinsicBuilder,
        intrinsics::{
            boolean_object::BooleanObject, date_object::DateObject, error_object::ErrorObject,
            number_object::NumberObject, regexp_object::RegExpObject,
            rust_runtime::RuntimeFunction,
        },
        ordinary_object::{init_object_fields, ordinary_object_create_without_proto},
        property::DEFAULT_ACCESSOR_PROPERTY_FLAGS,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        shape_registry::ShapeRegistry,
        string_object::StringObject,
        string_value::StringValue,
        type_utilities::{
            is_array, is_callable, require_object_coercible, same_object_value_handles, to_object,
            to_property_key,
        },
    },
    runtime_fn,
};

extend_object! {
    pub struct ObjectPrototypeObject {}
}

impl ObjectPrototypeObject {
    // Start out uninitialized and then initialize later to break dependency cycles.
    pub fn new_uninit(cx: Context) -> AllocResult<Handle<ObjectPrototypeObject>> {
        // Initialized with correct values in initialize method, but set to default value
        // at first to be GC-safe until initialize method is called.
        Ok(ordinary_object_create_without_proto(cx)?.cast())
    }

    /// Properties of the Object Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-object-prototype-object)
    pub fn initialize(
        cx: Context,
        object: Handle<ObjectPrototypeObject>,
        realm: Handle<Realm>,
    ) -> AllocResult<()> {
        let object = object.as_object();

        let shape =
            ShapeRegistry::get_root_object_shape(cx, HeapItemKind::ObjectPrototypeObject, None)?;
        init_object_fields(cx, *object, *shape);

        let mut builder = IntrinsicBuilder::ordinary(cx, realm, object);

        // Constructor property is added once ObjectConstructor has been created
        intrinsic_methods!(cx, builder, {
            has_own_property       ObjectPrototype_has_own_property       (1),
            is_prototype_of        ObjectPrototype_is_prototype_of        (1),
            property_is_enumerable ObjectPrototype_property_is_enumerable (1),
            value_of               ObjectPrototype_value_of               (0),
            to_locale_string       ObjectPrototype_to_locale_string       (0),
            to_string              ObjectPrototype_to_string              (0),
            __define_getter__      ObjectPrototype_define_getter          (2),
            __define_setter__      ObjectPrototype_define_setter          (2),
            __lookup_getter__      ObjectPrototype_lookup_getter          (1),
            __lookup_setter__      ObjectPrototype_lookup_setter          (1),
        });

        // Object.prototype.__proto__ (https://tc39.es/ecma262/#sec-object.prototype.__proto__)
        builder.getter_setter(
            cx.names.__proto__(),
            RuntimeFunction::ObjectPrototype_get_proto,
            RuntimeFunction::ObjectPrototype_set_proto,
        )?;

        builder.build()?;

        Ok(())
    }

    runtime_fn! {
    /// Object.prototype.hasOwnProperty (https://tc39.es/ecma262/#sec-object.prototype.hasownproperty)
    fn has_own_property(cx, this_value, arguments) {
        let property_arg = arguments.get(cx, 0);
        let property_key = to_property_key(cx, property_arg)?;
        let this_object = to_object(cx, this_value)?;

        let has_own_property = has_own_property(cx, this_object, property_key)?;
        Ok(cx.bool(has_own_property))
    }}

    runtime_fn! {
    /// Object.prototype.isPrototypeOf (https://tc39.es/ecma262/#sec-object.prototype.isprototypeof)
    fn is_prototype_of(cx, this_value, arguments) {
        let value = arguments.get(cx, 0);
        if !value.is_object() {
            return Ok(cx.bool(false));
        }

        let this_object = to_object(cx, this_value)?;

        // Walk prototype chain, seeing if this_object is in the prototype chain
        let mut current_value = value.as_object();
        loop {
            match current_value.get_prototype_of(cx)? {
                None => return Ok(cx.bool(false)),
                Some(prototype) => {
                    if same_object_value_handles(this_object, prototype) {
                        return Ok(cx.bool(true));
                    }

                    current_value = prototype;
                }
            }
        }
    }}

    runtime_fn! {
    /// Object.prototype.propertyIsEnumerable (https://tc39.es/ecma262/#sec-object.prototype.propertyisenumerable)
    fn property_is_enumerable(cx, this_value, arguments) {
        let property_arg = arguments.get(cx, 0);
        let property_key = to_property_key(cx, property_arg)?;
        let this_object = to_object(cx, this_value)?;

        match this_object.get_own_property(cx, property_key)? {
            None => Ok(cx.bool(false)),
            Some(property) => Ok(cx.bool(property.is_enumerable())),
        }
    }}

    runtime_fn! {
    /// Object.prototype.toLocaleString (https://tc39.es/ecma262/#sec-object.prototype.tolocalestring)
    fn to_locale_string(cx, this_value, _) {
        invoke(cx, this_value, cx.names.to_string(), &[])
    }}

    runtime_fn! {
    /// Object.prototype.toString (https://tc39.es/ecma262/#sec-object.prototype.tostring)
    fn to_string(cx, this_value, _) {
        if this_value.is_undefined() {
            return Ok(cx.alloc_static_string("[object Undefined]")?.as_value());
        } else if this_value.is_null() {
            return Ok(cx.alloc_static_string("[object Null]")?.as_value());
        }

        let object = to_object(cx, this_value)?;

        let is_array = is_array(cx, object.into())?;

        let to_string_tag_key = cx.symbols.to_string_tag();
        let tag = get(cx, object, to_string_tag_key)?;

        let tag_string = if tag.is_string() {
            let string_prefix = cx.alloc_static_string("[object ")?;
            let string_suffix = cx.alloc_static_string("]")?;

            let full_string =
                StringValue::concat_all(cx, &[string_prefix, tag.as_string(), string_suffix])?;

            return Ok(full_string.as_value());
        } else if is_array {
            "Array"
        } else if object.is_arguments_object() {
            "Arguments"
        } else if object.is_callable() {
            "Function"
        } else if object.is::<ErrorObject>() {
            "Error"
        } else if object.is::<BooleanObject>() {
            "Boolean"
        } else if object.is::<NumberObject>() {
            "Number"
        } else if object.is::<StringObject>() {
            "String"
        } else if object.is::<DateObject>() {
            "Date"
        } else if object.is::<RegExpObject>() {
            "RegExp"
        } else {
            "Object"
        };

        Ok(cx
            .alloc_string(&format!("[object {tag_string}]"))?
            .as_value())
    }}

    runtime_fn! {
    /// Object.prototype.valueOf (https://tc39.es/ecma262/#sec-object.prototype.valueof)
    fn value_of(cx, this_value, _) {
        Ok(to_object(cx, this_value)?.as_value())
    }}

    runtime_fn! {
    /// get Object.prototype.__proto__ (https://tc39.es/ecma262/#sec-get-object.prototype.__proto__)
    fn get_proto(cx, this_value, _) {
        let object = to_object(cx, this_value)?;
        match object.get_prototype_of(cx)? {
            None => Ok(cx.null()),
            Some(prototype) => Ok(prototype.as_value()),
        }
    }}

    runtime_fn! {
    /// set Object.prototype.__proto__ (https://tc39.es/ecma262/#sec-set-object.prototype.__proto__)
    fn set_proto(cx, this_value, arguments) {
        let object = require_object_coercible(cx, this_value)?;

        let proto = arguments.get(cx, 0);
        let proto = if proto.is_object() {
            Some(proto.as_object())
        } else if proto.is_null() {
            None
        } else {
            return Ok(cx.undefined());
        };

        if !object.is_object() {
            return Ok(cx.undefined());
        }

        if !object.as_object().set_prototype_of(cx, proto)? {
            return type_error(cx, "Object.prototype.__proto__ failed to set object prototype");
        }

        Ok(cx.undefined())
    }}

    runtime_fn! {
    /// Object.prototype.__defineGetter__ (https://tc39.es/ecma262/#sec-object.prototype.__defineGetter__)
    fn define_getter(cx, this_value, arguments) {
        let object = to_object(cx, this_value)?;

        let getter = arguments.get(cx, 1);
        if !is_callable(getter) {
            return type_error(cx, "Object.prototype.__defineGetter__ getter must be a function");
        }

        let key_arg = arguments.get(cx, 0);
        let key = to_property_key(cx, key_arg)?;
        let desc =
            PropertyDescriptor::getter(Some(getter.as_object()), DEFAULT_ACCESSOR_PROPERTY_FLAGS);

        define_property_or_throw(cx, object, key, desc)?;

        Ok(cx.undefined())
    }}

    runtime_fn! {
    /// Object.prototype.__defineSetter__ (https://tc39.es/ecma262/#sec-object.prototype.__defineSetter__)
    fn define_setter(cx, this_value, arguments) {
        let object = to_object(cx, this_value)?;

        let setter = arguments.get(cx, 1);
        if !is_callable(setter) {
            return type_error(cx, "Object.prototype.__defineSetter__ setter must be a function");
        }

        let key_arg = arguments.get(cx, 0);
        let key = to_property_key(cx, key_arg)?;
        let desc =
            PropertyDescriptor::setter(Some(setter.as_object()), DEFAULT_ACCESSOR_PROPERTY_FLAGS);

        define_property_or_throw(cx, object, key, desc)?;

        Ok(cx.undefined())
    }}

    runtime_fn! {
    /// Object.prototype.__lookupGetter__ (https://tc39.es/ecma262/#sec-object.prototype.__lookupGetter__)
    fn lookup_getter(cx, this_value, arguments) {
        let object = to_object(cx, this_value)?;
        let key_arg = arguments.get(cx, 0);
        let key = to_property_key(cx, key_arg)?;

        let mut current_object = object;
        loop {
            let property = current_object.get_own_property(cx, key)?;
            match property {
                Some(property) => {
                    return if property.is_accessor() {
                        let accessor = Accessor::from_value_handle(property.value());
                        match accessor.get {
                            Some(get) => Ok(get.to_handle().as_value()),
                            None => Ok(cx.undefined()),
                        }
                    } else {
                        Ok(cx.undefined())
                    };
                }
                None => match current_object.get_prototype_of(cx)? {
                    Some(proto) => current_object = proto,
                    None => return Ok(cx.undefined()),
                },
            }
        }
    }}

    runtime_fn! {
    /// Object.prototype.__lookupSetter__ (https://tc39.es/ecma262/#sec-object.prototype.__lookupSetter__)
    fn lookup_setter(cx, this_value, arguments) {
        let object = to_object(cx, this_value)?;
        let key_arg = arguments.get(cx, 0);
        let key = to_property_key(cx, key_arg)?;

        let mut current_object = object;
        loop {
            let property = current_object.get_own_property(cx, key)?;
            match property {
                Some(property) => {
                    return if property.is_accessor() {
                        let accessor = Accessor::from_value_handle(property.value());
                        match accessor.set {
                            Some(set) => Ok(set.to_handle().as_value()),
                            None => Ok(cx.undefined()),
                        }
                    } else {
                        Ok(cx.undefined())
                    };
                }
                None => match current_object.get_prototype_of(cx)? {
                    Some(proto) => current_object = proto,
                    None => return Ok(cx.undefined()),
                },
            }
        }
    }}
}

impl HeapItem for ObjectPrototypeObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<ObjectPrototypeObject>()
    }

    fn visit_pointers(mut object_prototype: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        object_prototype.visit_object_pointers(visitor);
    }
}
