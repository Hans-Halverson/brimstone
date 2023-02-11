use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    impl_gc_into,
    js::runtime::{
        abstract_operations::{define_property_or_throw, get, has_own_property, invoke},
        completion::EvalResult,
        environment::private_environment::PrivateNameId,
        error::type_error_,
        function::get_argument,
        gc::{Gc, GcDeref},
        object_value::{
            extract_object_vtable, set_immutable_prototype, Object, ObjectValue, ObjectValueVtable,
        },
        ordinary_object::OrdinaryObject,
        property::{PrivateProperty, Property},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        realm::Realm,
        type_utilities::{
            is_array, is_callable, require_object_coercible, same_object_value, to_object,
            to_property_key,
        },
        value::{Value, NULL_TAG, OBJECT_TAG},
        Context,
    },
    maybe,
};

#[repr(C)]
pub struct ObjectPrototype {
    _vtable: ObjectValueVtable,
    object: OrdinaryObject,
}

impl GcDeref for ObjectPrototype {}

impl ObjectPrototype {
    const VTABLE: *const () = extract_object_vtable::<ObjectPrototype>();

    // Start out uninitialized and then initialize later to break dependency cycles.
    pub fn new_uninit(cx: &mut Context) -> Gc<ObjectPrototype> {
        let object_prototype =
            ObjectPrototype { _vtable: Self::VTABLE, object: OrdinaryObject::new_uninit() };
        cx.heap.alloc(object_prototype)
    }

    // 20.1.3 Properties of the Object Prototype Object
    pub fn initialize(&mut self, cx: &mut Context, realm: Gc<Realm>) {
        self.object = OrdinaryObject::new(None, true);

        // Constructor property is added once ObjectConstructor has been created
        self.object.intrinsic_func(
            cx,
            &cx.names.has_own_property(),
            Self::has_own_property,
            1,
            realm,
        );
        self.object.intrinsic_func(
            cx,
            &cx.names.is_prototype_of(),
            Self::is_prototype_of,
            1,
            realm,
        );
        self.object.intrinsic_func(
            cx,
            &cx.names.property_is_enumerable(),
            Self::property_is_enumerable,
            1,
            realm,
        );
        self.object
            .intrinsic_func(cx, &cx.names.value_of(), Self::value_of, 0, realm);
        self.object.intrinsic_func(
            cx,
            &cx.names.to_locale_string(),
            Self::to_locale_string,
            0,
            realm,
        );
        self.object
            .intrinsic_func(cx, &cx.names.to_string(), Self::to_string, 0, realm);

        self.object.intrinsic_getter_and_setter(
            cx,
            &cx.names.__proto__(),
            Self::get_proto,
            Self::set_proto,
            realm,
        );

        self.object.intrinsic_func(
            cx,
            &cx.names.__define_getter__(),
            Self::define_getter,
            2,
            realm,
        );
        self.object.intrinsic_func(
            cx,
            &cx.names.__define_setter__(),
            Self::define_setter,
            2,
            realm,
        );
        self.object.intrinsic_func(
            cx,
            &cx.names.__lookup_getter__(),
            Self::lookup_getter,
            1,
            realm,
        );
        self.object.intrinsic_func(
            cx,
            &cx.names.__lookup_setter__(),
            Self::lookup_setter,
            1,
            realm,
        );
    }

    #[inline]
    fn object(&self) -> &OrdinaryObject {
        &self.object
    }

    #[inline]
    fn object_mut(&mut self) -> &mut OrdinaryObject {
        &mut self.object
    }

    // 20.1.3.2 Object.prototype.hasOwnProperty
    fn has_own_property(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let property_key = maybe!(to_property_key(cx, get_argument(arguments, 0)));
        let this_object = maybe!(to_object(cx, this_value));

        maybe!(has_own_property(cx, this_object, &property_key)).into()
    }

    // 20.1.3.3 Object.prototype.isPrototypeOf
    fn is_prototype_of(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let value = get_argument(arguments, 0);
        if !value.is_object() {
            return false.into();
        }

        let this_object = maybe!(to_object(cx, this_value));

        // Walk prototype chain, seeing if this_object is in the prototype chain
        let mut current_value = value.as_object();
        loop {
            match maybe!(current_value.get_prototype_of(cx)) {
                None => return false.into(),
                Some(prototype) => {
                    if same_object_value(this_object, prototype) {
                        return true.into();
                    }

                    current_value = prototype;
                }
            }
        }
    }

    // 20.1.3.4 Object.prototype.propertyIsEnumerable
    fn property_is_enumerable(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let property_key = maybe!(to_property_key(cx, get_argument(arguments, 0)));
        let this_object = maybe!(to_object(cx, this_value));

        match maybe!(this_object.get_own_property(cx, &property_key)) {
            None => false.into(),
            Some(desc) => desc.is_enumerable().into(),
        }
    }

    // 20.1.3.5 Object.prototype.toLocaleString
    fn to_locale_string(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        invoke(cx, this_value, &cx.names.to_string(), &[])
    }

    // 20.1.3.6 Object.prototype.toString
    fn to_string(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        if this_value.is_undefined() {
            return cx.heap.alloc_string("[object Undefined]".to_owned()).into();
        } else if this_value.is_null() {
            return cx.heap.alloc_string("[object Null]".to_owned()).into();
        }

        let object = maybe!(to_object(cx, this_value));

        let is_array = maybe!(is_array(cx, object.into()));

        let to_string_tag_key = PropertyKey::symbol(cx.well_known_symbols.to_string_tag);
        let tag = maybe!(get(cx, object, &to_string_tag_key));

        let tag_string = if tag.is_string() {
            return cx
                .heap
                .alloc_string(format!("[object {}]", tag.as_string().str()))
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

        cx.heap
            .alloc_string(format!("[object {}]", tag_string))
            .into()
    }

    // 20.1.3.7 Object.prototype.valueOf
    fn value_of(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        maybe!(to_object(cx, this_value)).into()
    }

    // 20.1.3.8.1 get Object.prototype.__proto__
    fn get_proto(
        cx: &mut Context,
        this_value: Value,
        _: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        match maybe!(object.get_prototype_of(cx)) {
            None => Value::null().into(),
            Some(prototype) => prototype.into(),
        }
    }

    // 20.1.3.8.2 set Object.prototype.__proto__
    fn set_proto(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(require_object_coercible(cx, this_value));

        let proto = get_argument(arguments, 0);
        let proto = match proto.get_tag() {
            OBJECT_TAG => Some(proto.as_object()),
            NULL_TAG => None,
            _ => return Value::undefined().into(),
        };

        if !object.is_object() {
            return Value::undefined().into();
        }

        if !maybe!(object.as_object().set_prototype_of(cx, proto)) {
            return type_error_(cx, "failed to set object prototype");
        }

        Value::undefined().into()
    }

    // 20.1.3.9.1 Object.prototype.__defineGetter__
    fn define_getter(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));

        let getter = get_argument(arguments, 0);
        if !is_callable(getter) {
            return type_error_(cx, "getter must be a function");
        }

        let key = maybe!(to_property_key(cx, get_argument(arguments, 1)));
        let desc = PropertyDescriptor::accessor(Some(getter.as_object()), None, true, true);

        maybe!(define_property_or_throw(cx, object, &key, desc));

        Value::undefined().into()
    }

    // 20.1.3.9.2 Object.prototype.__defineSetter__
    fn define_setter(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));

        let setter = get_argument(arguments, 0);
        if !is_callable(setter) {
            return type_error_(cx, "setter must be a function");
        }

        let key = maybe!(to_property_key(cx, get_argument(arguments, 1)));
        let desc = PropertyDescriptor::accessor(None, Some(setter.as_object()), true, true);

        maybe!(define_property_or_throw(cx, object, &key, desc));

        Value::undefined().into()
    }

    // 20.1.3.9.3 Object.prototype.__lookupGetter__
    fn lookup_getter(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let key = maybe!(to_property_key(cx, get_argument(arguments, 0)));

        let mut current_object = object;
        loop {
            let desc = maybe!(current_object.get_own_property(cx, &key));
            match desc {
                Some(desc) => {
                    return if desc.is_accessor_descriptor() {
                        match desc.get {
                            Some(get) => get.into(),
                            None => Value::undefined().into(),
                        }
                    } else {
                        Value::undefined().into()
                    }
                }
                None => match maybe!(current_object.get_prototype_of(cx)) {
                    Some(proto) => current_object = proto,
                    None => return Value::undefined().into(),
                },
            }
        }
    }

    // 20.1.3.9.4 Object.prototype.__lookupSetter__
    fn lookup_setter(
        cx: &mut Context,
        this_value: Value,
        arguments: &[Value],
        _: Option<Gc<ObjectValue>>,
    ) -> EvalResult<Value> {
        let object = maybe!(to_object(cx, this_value));
        let key = maybe!(to_property_key(cx, get_argument(arguments, 0)));

        let mut current_object = object;
        loop {
            let desc = maybe!(current_object.get_own_property(cx, &key));
            match desc {
                Some(desc) => {
                    return if desc.is_accessor_descriptor() {
                        match desc.set {
                            Some(set) => set.into(),
                            None => Value::undefined().into(),
                        }
                    } else {
                        Value::undefined().into()
                    }
                }
                None => match maybe!(current_object.get_prototype_of(cx)) {
                    Some(proto) => current_object = proto,
                    None => return Value::undefined().into(),
                },
            }
        }
    }
}

#[wrap_ordinary_object]
impl Object for ObjectPrototype {
    fn set_prototype_of(
        &mut self,
        cx: &mut Context,
        proto: Option<Gc<ObjectValue>>,
    ) -> EvalResult<bool> {
        set_immutable_prototype(cx, self.into(), proto)
    }
}

impl_gc_into!(ObjectPrototype, ObjectValue);
