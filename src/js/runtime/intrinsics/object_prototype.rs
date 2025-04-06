use std::mem::size_of;

use crate::{
    extend_object,
    runtime::{
        abstract_operations::{define_property_or_throw, get, has_own_property, invoke},
        error::type_error,
        eval_result::EvalResult,
        function::get_argument,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_ordinary_init,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        string_value::StringValue,
        type_utilities::{
            is_array, is_callable, require_object_coercible, same_object_value_handles, to_object,
            to_property_key,
        },
        Context, Handle, HeapPtr, Value,
    },
};

extend_object! {
    pub struct ObjectPrototype {}
}

impl ObjectPrototype {
    // Start out uninitialized and then initialize later to break dependency cycles.
    pub fn new_uninit(cx: Context) -> Handle<ObjectPrototype> {
        // Initialized with correct values in initialize method, but set to default value
        // at first to be GC safe until initialize method is called.
        ObjectValue::new(cx, None, false).cast()
    }

    /// Properties of the Object Prototype Object (https://tc39.es/ecma262/#sec-properties-of-the-object-prototype-object)
    pub fn initialize(cx: Context, object: Handle<ObjectPrototype>, realm: Handle<Realm>) {
        let mut object = object.as_object();

        let descriptor = cx.base_descriptors.get(ObjectKind::ObjectPrototype);
        object_ordinary_init(cx, *object, descriptor, None);

        // Constructor property is added once ObjectConstructor has been created
        object.intrinsic_func(cx, cx.names.has_own_property(), Self::has_own_property, 1, realm);
        object.intrinsic_func(cx, cx.names.is_prototype_of(), Self::is_prototype_of, 1, realm);
        object.intrinsic_func(
            cx,
            cx.names.property_is_enumerable(),
            Self::property_is_enumerable,
            1,
            realm,
        );
        object.intrinsic_func(cx, cx.names.value_of(), Self::value_of, 0, realm);
        object.intrinsic_func(cx, cx.names.to_locale_string(), Self::to_locale_string, 0, realm);
        object.intrinsic_func(cx, cx.names.to_string(), Self::to_string, 0, realm);

        object.intrinsic_getter_and_setter(
            cx,
            cx.names.__proto__(),
            Self::get_proto,
            Self::set_proto,
            realm,
        );

        object.intrinsic_func(cx, cx.names.__define_getter__(), Self::define_getter, 2, realm);
        object.intrinsic_func(cx, cx.names.__define_setter__(), Self::define_setter, 2, realm);
        object.intrinsic_func(cx, cx.names.__lookup_getter__(), Self::lookup_getter, 1, realm);
        object.intrinsic_func(cx, cx.names.__lookup_setter__(), Self::lookup_setter, 1, realm);
    }

    /// Object.prototype.hasOwnProperty (https://tc39.es/ecma262/#sec-object.prototype.hasownproperty)
    pub fn has_own_property(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let property_arg = get_argument(cx, arguments, 0);
        let property_key = to_property_key(cx, property_arg)?;
        let this_object = to_object(cx, this_value)?;

        let has_own_property = has_own_property(cx, this_object, property_key)?;
        Ok(cx.bool(has_own_property))
    }

    /// Object.prototype.isPrototypeOf (https://tc39.es/ecma262/#sec-object.prototype.isprototypeof)
    pub fn is_prototype_of(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let value = get_argument(cx, arguments, 0);
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
    }

    /// Object.prototype.propertyIsEnumerable (https://tc39.es/ecma262/#sec-object.prototype.propertyisenumerable)
    pub fn property_is_enumerable(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let property_arg = get_argument(cx, arguments, 0);
        let property_key = to_property_key(cx, property_arg)?;
        let this_object = to_object(cx, this_value)?;

        match this_object.get_own_property(cx, property_key)? {
            None => Ok(cx.bool(false)),
            Some(desc) => Ok(cx.bool(desc.is_enumerable())),
        }
    }

    /// Object.prototype.toLocaleString (https://tc39.es/ecma262/#sec-object.prototype.tolocalestring)
    pub fn to_locale_string(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        invoke(cx, this_value, cx.names.to_string(), &[])
    }

    /// Object.prototype.toString (https://tc39.es/ecma262/#sec-object.prototype.tostring)
    pub fn to_string(
        mut cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        if this_value.is_undefined() {
            return Ok(cx.alloc_string("[object Undefined]").as_value());
        } else if this_value.is_null() {
            return Ok(cx.alloc_string("[object Null]").as_value());
        }

        let object = to_object(cx, this_value)?;

        let is_array = is_array(cx, object.into())?;

        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let tag = get(cx, object, to_string_tag_key)?;

        let tag_string = if tag.is_string() {
            let string_prefix = cx.alloc_string("[object ").as_string();
            let string_suffix = cx.alloc_string("]").as_string();

            let full_string =
                StringValue::concat_all(cx, &[string_prefix, tag.as_string(), string_suffix]);

            return Ok(full_string.as_value());
        } else if is_array {
            "Array"
        } else if object.is_arguments_object() {
            "Arguments"
        } else if object.is_callable() {
            "Function"
        } else if object.is_error() {
            "Error"
        } else if object.is_boolean_object() {
            "Boolean"
        } else if object.is_number_object() {
            "Number"
        } else if object.is_string_object() {
            "String"
        } else if object.is_date_object() {
            "Date"
        } else if object.is_regexp_object() {
            "RegExp"
        } else {
            "Object"
        };

        Ok(cx
            .alloc_string(&format!("[object {}]", tag_string))
            .as_value())
    }

    /// Object.prototype.valueOf (https://tc39.es/ecma262/#sec-object.prototype.valueof)
    pub fn value_of(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        Ok(to_object(cx, this_value)?.as_value())
    }

    /// get Object.prototype.__proto__ (https://tc39.es/ecma262/#sec-get-object.prototype.__proto__)
    pub fn get_proto(
        cx: Context,
        this_value: Handle<Value>,
        _: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        match object.get_prototype_of(cx)? {
            None => Ok(cx.null()),
            Some(prototype) => Ok(prototype.as_value()),
        }
    }

    /// set Object.prototype.__proto__ (https://tc39.es/ecma262/#sec-set-object.prototype.__proto__)
    pub fn set_proto(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = require_object_coercible(cx, this_value)?;

        let proto = get_argument(cx, arguments, 0);
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
            return type_error(cx, "failed to set object prototype");
        }

        Ok(cx.undefined())
    }

    /// Object.prototype.__defineGetter__ (https://tc39.es/ecma262/#sec-object.prototype.__defineGetter__)
    pub fn define_getter(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;

        let getter = get_argument(cx, arguments, 1);
        if !is_callable(getter) {
            return type_error(cx, "getter must be a function");
        }

        let key_arg = get_argument(cx, arguments, 0);
        let key = to_property_key(cx, key_arg)?;
        let desc = PropertyDescriptor::get_only(Some(getter.as_object()), true, true);

        define_property_or_throw(cx, object, key, desc)?;

        Ok(cx.undefined())
    }

    /// Object.prototype.__defineSetter__ (https://tc39.es/ecma262/#sec-object.prototype.__defineSetter__)
    pub fn define_setter(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;

        let setter = get_argument(cx, arguments, 1);
        if !is_callable(setter) {
            return type_error(cx, "setter must be a function");
        }

        let key_arg = get_argument(cx, arguments, 0);
        let key = to_property_key(cx, key_arg)?;
        let desc = PropertyDescriptor::set_only(Some(setter.as_object()), true, true);

        define_property_or_throw(cx, object, key, desc)?;

        Ok(cx.undefined())
    }

    /// Object.prototype.__lookupGetter__ (https://tc39.es/ecma262/#sec-object.prototype.__lookupGetter__)
    pub fn lookup_getter(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let key_arg = get_argument(cx, arguments, 0);
        let key = to_property_key(cx, key_arg)?;

        let mut current_object = object;
        loop {
            let desc = current_object.get_own_property(cx, key)?;
            match desc {
                Some(desc) => {
                    return if desc.is_accessor_descriptor() {
                        match desc.get {
                            Some(get) => Ok(get.as_value()),
                            None => Ok(cx.undefined()),
                        }
                    } else {
                        Ok(cx.undefined())
                    }
                }
                None => match current_object.get_prototype_of(cx)? {
                    Some(proto) => current_object = proto,
                    None => return Ok(cx.undefined()),
                },
            }
        }
    }

    /// Object.prototype.__lookupSetter__ (https://tc39.es/ecma262/#sec-object.prototype.__lookupSetter__)
    pub fn lookup_setter(
        cx: Context,
        this_value: Handle<Value>,
        arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        let object = to_object(cx, this_value)?;
        let key_arg = get_argument(cx, arguments, 0);
        let key = to_property_key(cx, key_arg)?;

        let mut current_object = object;
        loop {
            let desc = current_object.get_own_property(cx, key)?;
            match desc {
                Some(desc) => {
                    return if desc.is_accessor_descriptor() {
                        match desc.set {
                            Some(set) => Ok(set.as_value()),
                            None => Ok(cx.undefined()),
                        }
                    } else {
                        Ok(cx.undefined())
                    }
                }
                None => match current_object.get_prototype_of(cx)? {
                    Some(proto) => current_object = proto,
                    None => return Ok(cx.undefined()),
                },
            }
        }
    }
}

impl HeapObject for HeapPtr<ObjectPrototype> {
    fn byte_size(&self) -> usize {
        size_of::<ObjectPrototype>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
    }
}
