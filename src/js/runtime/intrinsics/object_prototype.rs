use crate::{
    js::runtime::{
        abstract_operations::{define_property_or_throw, get, has_own_property, invoke},
        completion::EvalResult,
        error::type_error_,
        function::get_argument,
        gc::HandleValue,
        object_descriptor::ObjectKind,
        object_value::ObjectValue,
        ordinary_object::object_ordinary_init,
        property_descriptor::PropertyDescriptor,
        realm::Realm,
        string_value::StringValue,
        type_utilities::{
            is_array, is_callable, require_object_coercible, same_object_value, to_object,
            to_property_key,
        },
        Context, Handle,
    },
    maybe,
};

pub struct ObjectPrototype;

impl ObjectPrototype {
    // Start out uninitialized and then initialize later to break dependency cycles.
    pub fn new_uninit(cx: &mut Context) -> Handle<ObjectValue> {
        // Initialized with correct values in initialize method, but set to default value
        // at first to be GC safe until initialize method is called.
        ObjectValue::new(cx, None, false)
    }

    // 20.1.3 Properties of the Object Prototype Object
    pub fn initialize(cx: &mut Context, mut object: Handle<ObjectValue>, realm: Handle<Realm>) {
        let descriptor = cx.base_descriptors.get(ObjectKind::ObjectPrototype);
        object_ordinary_init(cx, object.object(), descriptor, None);

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

    // 20.1.3.2 Object.prototype.hasOwnProperty
    fn has_own_property(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let property_arg = get_argument(cx, arguments, 0);
        let property_key = maybe!(to_property_key(cx, property_arg));
        let this_object = maybe!(to_object(cx, this_value));

        let has_own_property = maybe!(has_own_property(cx, this_object, property_key));
        cx.bool(has_own_property).into()
    }

    // 20.1.3.3 Object.prototype.isPrototypeOf
    fn is_prototype_of(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let value = get_argument(cx, arguments, 0);
        if !value.is_object() {
            return cx.bool(false).into();
        }

        let this_object = maybe!(to_object(cx, this_value));

        // Walk prototype chain, seeing if this_object is in the prototype chain
        let mut current_value = value.as_object();
        loop {
            match maybe!(current_value.get_prototype_of(cx)) {
                None => return cx.bool(false).into(),
                Some(prototype) => {
                    if same_object_value(this_object, prototype) {
                        return cx.bool(true).into();
                    }

                    current_value = prototype;
                }
            }
        }
    }

    // 20.1.3.4 Object.prototype.propertyIsEnumerable
    fn property_is_enumerable(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let property_arg = get_argument(cx, arguments, 0);
        let property_key = maybe!(to_property_key(cx, property_arg));
        let this_object = maybe!(to_object(cx, this_value));

        match maybe!(this_object.get_own_property(cx, property_key)) {
            None => cx.bool(false).into(),
            Some(desc) => cx.bool(desc.is_enumerable()).into(),
        }
    }

    // 20.1.3.5 Object.prototype.toLocaleString
    fn to_locale_string(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        invoke(cx, this_value, cx.names.to_string(), &[])
    }

    // 20.1.3.6 Object.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        if this_value.is_undefined() {
            return cx.alloc_string("[object Undefined]".to_owned()).into();
        } else if this_value.is_null() {
            return cx.alloc_string("[object Null]".to_owned()).into();
        }

        let object = maybe!(to_object(cx, this_value));

        let is_array = maybe!(is_array(cx, object.into()));

        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        let tag = maybe!(get(cx, object, to_string_tag_key));

        let tag_string = if tag.is_string() {
            let string_prefix = cx.alloc_string(String::from("[object "));
            let string_suffix = cx.alloc_string(String::from("]"));

            return StringValue::concat_all(cx, &[string_prefix, tag.as_string(), string_suffix])
                .into();
        } else if is_array {
            "Array"
        } else if object.is_arguments_object() {
            "Arguments"
        } else if object.is_callable() {
            "Function"
        } else if object.is_error() {
            "Error"
        } else if object.is_bool_object() {
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

        cx.alloc_string(format!("[object {}]", tag_string)).into()
    }

    // 20.1.3.7 Object.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        maybe!(to_object(cx, this_value)).into()
    }

    // 20.1.3.8.1 get Object.prototype.__proto__
    fn get_proto(
        cx: &mut Context,
        this_value: HandleValue,
        _: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        match maybe!(object.get_prototype_of(cx)) {
            None => cx.null().into(),
            Some(prototype) => prototype.into(),
        }
    }

    // 20.1.3.8.2 set Object.prototype.__proto__
    fn set_proto(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(require_object_coercible(cx, this_value));

        let proto = get_argument(cx, arguments, 0);
        let proto = if proto.is_object() {
            Some(proto.as_object())
        } else if proto.is_null() {
            None
        } else {
            return cx.undefined().into();
        };

        if !object.is_object() {
            return cx.undefined().into();
        }

        if !maybe!(object.as_object().set_prototype_of(cx, proto)) {
            return type_error_(cx, "failed to set object prototype");
        }

        cx.undefined().into()
    }

    // 20.1.3.9.1 Object.prototype.__defineGetter__
    fn define_getter(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));

        let getter = get_argument(cx, arguments, 0);
        if !is_callable(getter) {
            return type_error_(cx, "getter must be a function");
        }

        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));
        let desc = PropertyDescriptor::accessor(Some(getter.as_object()), None, true, true);

        maybe!(define_property_or_throw(cx, object, key, desc));

        cx.undefined().into()
    }

    // 20.1.3.9.2 Object.prototype.__defineSetter__
    fn define_setter(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));

        let setter = get_argument(cx, arguments, 0);
        if !is_callable(setter) {
            return type_error_(cx, "setter must be a function");
        }

        let key_arg = get_argument(cx, arguments, 1);
        let key = maybe!(to_property_key(cx, key_arg));
        let desc = PropertyDescriptor::accessor(None, Some(setter.as_object()), true, true);

        maybe!(define_property_or_throw(cx, object, key, desc));

        cx.undefined().into()
    }

    // 20.1.3.9.3 Object.prototype.__lookupGetter__
    fn lookup_getter(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let key_arg = get_argument(cx, arguments, 0);
        let key = maybe!(to_property_key(cx, key_arg));

        let mut current_object = object;
        loop {
            let desc = maybe!(current_object.get_own_property(cx, key));
            match desc {
                Some(desc) => {
                    return if desc.is_accessor_descriptor() {
                        match desc.get {
                            Some(get) => get.into(),
                            None => cx.undefined().into(),
                        }
                    } else {
                        cx.undefined().into()
                    }
                }
                None => match maybe!(current_object.get_prototype_of(cx)) {
                    Some(proto) => current_object = proto,
                    None => return cx.undefined().into(),
                },
            }
        }
    }

    // 20.1.3.9.4 Object.prototype.__lookupSetter__
    fn lookup_setter(
        cx: &mut Context,
        this_value: HandleValue,
        arguments: &[HandleValue],
        _: Option<Handle<ObjectValue>>,
    ) -> EvalResult<HandleValue> {
        let object = maybe!(to_object(cx, this_value));
        let key_arg = get_argument(cx, arguments, 0);
        let key = maybe!(to_property_key(cx, key_arg));

        let mut current_object = object;
        loop {
            let desc = maybe!(current_object.get_own_property(cx, key));
            match desc {
                Some(desc) => {
                    return if desc.is_accessor_descriptor() {
                        match desc.set {
                            Some(set) => set.into(),
                            None => cx.undefined().into(),
                        }
                    } else {
                        cx.undefined().into()
                    }
                }
                None => match maybe!(current_object.get_prototype_of(cx)) {
                    Some(proto) => current_object = proto,
                    None => return cx.undefined().into(),
                },
            }
        }
    }
}
