use std::collections::HashMap;

use crate::{impl_gc_into, maybe, must};

use super::{
    abstract_operations::{call_object, create_data_property, get, get_function_realm},
    builtin_function::{BuiltinFunction, BuiltinFunctionPtr},
    completion::EvalResult,
    gc::{Gc, GcDeref},
    intrinsics::intrinsics::Intrinsic,
    object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
    property::Property,
    property_descriptor::PropertyDescriptor,
    realm::Realm,
    type_utilities::{same_object_value, same_opt_object_value, same_value},
    value::{AccessorValue, Value},
    Context,
};

// An ordinary object
#[repr(C)]
pub struct OrdinaryObject {
    _vtable: ObjectValueVtable,
    // None represents the null value
    prototype: Option<Gc<ObjectValue>>,
    // Properties with string keys. Does not yet support symbol or number keys.
    properties: HashMap<String, Property>,
    is_extensible: bool,
}

impl GcDeref for OrdinaryObject {}

impl_gc_into!(OrdinaryObject, ObjectValue);

const VTABLE: *const () = extract_object_vtable::<OrdinaryObject>();

impl OrdinaryObject {
    pub fn new(prototype: Option<Gc<ObjectValue>>, is_extensible: bool) -> OrdinaryObject {
        OrdinaryObject {
            _vtable: VTABLE,
            prototype,
            properties: HashMap::new(),
            is_extensible,
        }
    }

    pub fn new_uninit() -> OrdinaryObject {
        OrdinaryObject {
            _vtable: VTABLE,
            prototype: None,
            properties: HashMap::new(),
            is_extensible: false,
        }
    }

    pub fn set_property(&mut self, key: String, value: Property) {
        self.properties.insert(key, value);
    }

    pub fn intrinsic_data_prop(&mut self, key: &str, value: Value) {
        self.set_property(key.to_owned(), Property::data(value, true, false, true))
    }

    pub fn instrinsic_length_prop(&mut self, length: f64) {
        self.set_property(
            "length".to_owned(),
            Property::data(Value::number(length), false, false, true),
        )
    }

    pub fn intrinsic_name_prop(&mut self, cx: &mut Context, name: &str) {
        self.set_property(
            "name".to_owned(),
            Property::data(
                Value::string(cx.heap.alloc_string(name.to_owned())),
                true,
                false,
                true,
            ),
        )
    }

    pub fn intrinsic_func(
        &mut self,
        cx: &mut Context,
        name: &str,
        func: BuiltinFunctionPtr,
        length: u32,
        realm: Gc<Realm>,
    ) {
        self.intrinsic_data_prop(
            name,
            BuiltinFunction::create(cx, func, 0, name, Some(realm), None, None).into(),
        );
    }
}

impl Object for OrdinaryObject {
    // 10.1.1 [[GetPrototypeOf]]
    // 10.1.1.1 OrdinaryGetPrototypeOf
    fn get_prototype_of(&self) -> EvalResult<Option<Gc<ObjectValue>>> {
        self.prototype.into()
    }

    // 10.1.2 [[SetPrototypeOf]]
    // 10.1.2.1 OrdinarySetPrototypeOf
    fn set_prototype_of(&mut self, new_prototype: Option<Gc<ObjectValue>>) -> EvalResult<bool> {
        if same_opt_object_value(self.prototype, new_prototype) {
            return true.into();
        }

        if !self.is_extensible {
            return false.into();
        }

        let mut current_prototype = new_prototype;
        loop {
            match current_prototype {
                None => break,
                Some(current_proto) => {
                    if same_object_value(current_proto, self.into()) {
                        return false.into();
                    }

                    // TODO: Check if p is a Proxy object
                    current_prototype = must!(current_proto.get_prototype_of());
                }
            }
        }

        self.prototype = new_prototype;

        return true.into();
    }

    // 10.1.3 [[IsExtensible]]
    // 10.1.3.1 OrdinaryIsExtensible
    fn is_extensible(&self) -> EvalResult<bool> {
        self.is_extensible.into()
    }

    // 10.1.4 [[PreventExtensions]]
    // 10.1.4.1 OrdinaryPreventExtensions
    fn prevent_extensions(&mut self) -> EvalResult<bool> {
        self.is_extensible = false;
        true.into()
    }

    // 10.1.5 [[GetOwnProperty]]
    fn get_own_property(&self, key: &str) -> EvalResult<Option<PropertyDescriptor>> {
        ordinary_get_own_property(self, key).into()
    }

    // 10.1.6 [[DefineOwnProperty]]
    fn define_own_property(
        &mut self,
        cx: &mut Context,
        key: &str,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        ordinary_define_own_property(cx, self, key, desc)
    }

    // 10.1.7 [[HasProperty]]
    fn has_property(&self, key: &str) -> EvalResult<bool> {
        ordinary_has_property(self, key)
    }

    // 10.1.8 [[Get]]
    fn get(&self, cx: &mut Context, key: &str, receiver: Value) -> EvalResult<Value> {
        ordinary_get(cx, self, key, receiver)
    }

    // 10.1.9 [[Set]]
    fn set(
        &mut self,
        cx: &mut Context,
        key: &str,
        value: Value,
        receiver: Value,
    ) -> EvalResult<bool> {
        ordinary_set(cx, self, key, value, receiver)
    }

    // 10.1.10 [[Delete]]
    fn delete(&mut self, key: &str) -> EvalResult<bool> {
        ordinary_delete(self, key)
    }

    // 10.1.11 [[OwnPropertyKeys]]
    fn own_property_keys(&self, cx: &mut Context) -> Vec<Value> {
        ordinary_own_property_keys(cx, self)
    }
}

// 10.1.5.1 OrdinaryGetOwnProperty
pub fn ordinary_get_own_property(object: &OrdinaryObject, key: &str) -> Option<PropertyDescriptor> {
    match object.properties.get(key) {
        None => None,
        Some(property) => {
            if property.value().is_accessor() {
                let accessor_value = property.value().as_accessor();
                Some(PropertyDescriptor::accessor(
                    accessor_value.as_ref().get,
                    accessor_value.as_ref().set,
                    property.is_enumerable(),
                    property.is_configurable(),
                ))
            } else {
                Some(PropertyDescriptor::data(
                    property.value(),
                    property.is_writable(),
                    property.is_enumerable(),
                    property.is_configurable(),
                ))
            }
        }
    }
}

// 10.1.6.1 OrdinaryDefineOwnProperty
pub fn ordinary_define_own_property(
    cx: &mut Context,
    object: &mut OrdinaryObject,
    key: &str,
    desc: PropertyDescriptor,
) -> EvalResult<bool> {
    let current_desc = maybe!(object.get_own_property(key));
    let is_extensible = maybe!(object.is_extensible());

    validate_and_apply_property_descriptor(cx, Some(object), key, is_extensible, desc, current_desc)
        .into()
}

// 10.1.6.2 IsCompatiblePropertyDescriptor
pub fn is_compatible_property_descriptor(
    cx: &mut Context,
    is_extensible: bool,
    desc: PropertyDescriptor,
    current_desc: Option<PropertyDescriptor>,
) -> bool {
    validate_and_apply_property_descriptor(cx, None, "", is_extensible, desc, current_desc)
}

// 10.1.6.3 ValidateAndApplyPropertyDescriptor
pub fn validate_and_apply_property_descriptor(
    cx: &mut Context,
    mut object: Option<&mut OrdinaryObject>,
    key: &str,
    is_extensible: bool,
    desc: PropertyDescriptor,
    current_desc: Option<PropertyDescriptor>,
) -> bool {
    if current_desc.is_none() {
        if !is_extensible {
            return false;
        }

        if object.is_none() {
            return true;
        }
        let object = object.unwrap();

        // Create new property with fields in descriptor, using default if field is not set
        let is_enumerable = desc.is_enumerable.unwrap_or(false);
        let is_configurable = desc.is_configurable.unwrap_or(false);

        let property = if desc.is_accessor_descriptor() {
            let accessor_value = cx.heap.alloc(AccessorValue {
                get: desc.get,
                set: desc.set,
            });

            Property::accessor(accessor_value.into(), is_enumerable, is_configurable)
        } else {
            let is_writable = desc.is_writable.unwrap_or(false);
            let value = desc.value.unwrap_or_else(|| Value::undefined());

            Property::data(value, is_enumerable, is_configurable, is_writable)
        };

        object.properties.insert(key.to_string(), property);

        return true;
    }

    let current_desc = current_desc.unwrap();
    if desc.has_no_fields() {
        return true;
    }

    if !current_desc.is_configurable() {
        if let Some(true) = desc.is_configurable {
            return false;
        }

        match desc.is_enumerable {
            Some(is_enumerable) if is_enumerable != current_desc.is_enumerable() => return false,
            _ => {}
        }
    }

    if desc.is_generic_descriptor() {
        // No validation necessary
    } else if current_desc.is_data_descriptor() != desc.is_data_descriptor() {
        if !current_desc.is_configurable() {
            return false;
        }

        match &mut object {
            None => return true,
            Some(object) => {
                // Converting between data and accessor. Preserve shared fields and set others to
                // their defaults.
                let property = object.properties.get_mut(key).unwrap();
                if desc.is_data_descriptor() {
                    property.set_value(Value::undefined());
                    property.set_is_writable(false);
                } else {
                    let accessor_value = cx.heap.alloc(AccessorValue {
                        get: None,
                        set: None,
                    });
                    property.set_value(accessor_value.into());
                }
            }
        }
    } else if current_desc.is_data_descriptor() && desc.is_data_descriptor() {
        if !current_desc.is_configurable() && !current_desc.is_writable() {
            if let Some(true) = desc.is_writable {
                return false;
            }

            match desc.value {
                Some(value) if !same_value(value, current_desc.value.unwrap()) => return false,
                _ => {}
            }

            return true;
        }
    } else {
        if !current_desc.is_configurable() {
            match desc.get {
                Some(get) if !same_object_value(get, current_desc.get.unwrap()) => return false,
                _ => {}
            }

            match desc.set {
                Some(set) if !same_object_value(set, current_desc.set.unwrap()) => return false,
                _ => {}
            }

            return true;
        }
    }

    match &mut object {
        Some(object) => {
            // For every field in new descriptor that is present, set the corresponding attribute in
            // the existing descriptor.
            let property = object.properties.get_mut(key).unwrap();

            if let Some(is_enumerable) = desc.is_enumerable {
                property.set_is_enumerable(is_enumerable);
            }

            if let Some(is_configurable) = desc.is_configurable {
                property.set_is_configurable(is_configurable);
            }

            if desc.is_data_descriptor() {
                if let Some(is_writable) = desc.is_writable {
                    property.set_is_writable(is_writable);
                }

                if let Some(value) = desc.value {
                    property.set_value(value);
                }
            } else {
                let mut accessor_value = property.value().as_accessor();

                if let Some(get) = desc.get {
                    accessor_value.as_mut().get = Some(get);
                }

                if let Some(set) = desc.set {
                    accessor_value.as_mut().set = Some(set);
                }
            }
        }
        None => {}
    }

    true
}

// 10.1.7.1 OrdinaryHasProperty
pub fn ordinary_has_property(object: &OrdinaryObject, key: &str) -> EvalResult<bool> {
    let own_property = maybe!(object.get_own_property(key));
    if own_property.is_some() {
        return true.into();
    }

    let parent = maybe!(object.get_prototype_of());
    match parent {
        Some(parent) => parent.has_property(key),
        None => false.into(),
    }
}

// 10.1.8.1 OrdinaryGet
pub fn ordinary_get(
    cx: &mut Context,
    object: &OrdinaryObject,
    key: &str,
    receiver: Value,
) -> EvalResult<Value> {
    let desc = maybe!(object.get_own_property(key));
    match desc {
        None => {
            let parent = maybe!(object.get_prototype_of());
            match parent {
                None => Value::undefined().into(),
                Some(parent) => parent.get(cx, key, receiver),
            }
        }
        Some(desc) if desc.is_data_descriptor() => desc.value.unwrap().into(),
        Some(PropertyDescriptor { get: None, .. }) => Value::undefined().into(),
        Some(PropertyDescriptor {
            get: Some(getter), ..
        }) => call_object(cx, getter, receiver, &[]),
    }
}

// 10.1.9.1 OrdinarySet
// 10.1.9.2 OrdinarySetWithOwnDescriptor
pub fn ordinary_set(
    cx: &mut Context,
    object: &mut OrdinaryObject,
    key: &str,
    value: Value,
    receiver: Value,
) -> EvalResult<bool> {
    let own_desc = maybe!(object.get_own_property(key));
    let own_desc = match own_desc {
        None => {
            let parent = maybe!(object.get_prototype_of());
            match parent {
                None => PropertyDescriptor::data(Value::undefined(), true, true, true),
                Some(mut parent) => return parent.set(cx, key, value, receiver),
            }
        }
        Some(own_desc) => own_desc,
    };

    if own_desc.is_data_descriptor() {
        match own_desc.is_writable {
            Some(false) => return false.into(),
            _ => {}
        }

        if !receiver.is_object() {
            return false.into();
        }

        let mut receiver = receiver.as_object();
        let existing_descriptor = maybe!(receiver.get_own_property(key));
        match existing_descriptor {
            None => create_data_property(cx, receiver, key, value),
            Some(existing_descriptor) if existing_descriptor.is_accessor_descriptor() => {
                false.into()
            }
            Some(PropertyDescriptor {
                is_writable: Some(false),
                ..
            }) => false.into(),
            Some(_) => {
                let value_desc = PropertyDescriptor::data_value_only(value);
                receiver.define_own_property(cx, key, value_desc)
            }
        }
    } else {
        match own_desc.set {
            None => false.into(),
            Some(setter) => {
                maybe!(call_object(cx, setter, receiver, &[value]));
                true.into()
            }
        }
    }
}

// 10.1.10.1 OrdinaryDelete
pub fn ordinary_delete(object: &mut OrdinaryObject, key: &str) -> EvalResult<bool> {
    let desc = maybe!(object.get_own_property(key));
    match desc {
        None => true.into(),
        Some(desc) => {
            if desc.is_configurable() {
                object.properties.remove(key);
                true.into()
            } else {
                false.into()
            }
        }
    }
}

// 10.1.11.1 OrdinaryOwnPropertyKeys
pub fn ordinary_own_property_keys(cx: &mut Context, object: &OrdinaryObject) -> Vec<Value> {
    // TODO: Return keys in order of property creation
    // TODO: Add array index and symbol keys
    object
        .properties
        .keys()
        .map(|str| Value::string(cx.heap.alloc_string(str.into())))
        .collect()
}

// 10.1.12 OrdinaryObjectCreate
pub fn ordinary_object_create(proto: Gc<ObjectValue>) -> OrdinaryObject {
    OrdinaryObject {
        _vtable: VTABLE,
        prototype: Some(proto),
        properties: HashMap::new(),
        is_extensible: true,
    }
}

// 10.1.13 OrdinaryCreateFromConstructor
pub fn ordinary_create_from_constructor(
    cx: &mut Context,
    constructor: Gc<ObjectValue>,
    intrinsic_default_proto: Intrinsic,
) -> EvalResult<OrdinaryObject> {
    let proto = maybe!(get_prototype_from_constructor(
        cx,
        constructor,
        intrinsic_default_proto
    ));

    ordinary_object_create(proto).into()
}

// 10.1.14 GetPrototypeFromConstructor
pub fn get_prototype_from_constructor(
    cx: &mut Context,
    constructor: Gc<ObjectValue>,
    intrinsic_default_proto: Intrinsic,
) -> EvalResult<Gc<ObjectValue>> {
    let proto = maybe!(get(cx, constructor.into(), "prototype"));
    if proto.is_object() {
        proto.as_object().into()
    } else {
        let realm = maybe!(get_function_realm(constructor));
        realm.get_intrinsic(intrinsic_default_proto).into()
    }
}
