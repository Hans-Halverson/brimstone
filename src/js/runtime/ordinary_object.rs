use indexmap::IndexMap;

use std::ops::DerefMut;

use crate::{extend_object, maybe, must};

use super::{
    abstract_operations::{call_object, create_data_property, get, get_function_realm},
    array_properties::ArrayProperties,
    completion::EvalResult,
    gc::{Gc, Handle, HandleValue, HeapPtr},
    intrinsics::intrinsics::Intrinsic,
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::{ObjectValue, VirtualObject},
    property::Property,
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    type_utilities::{same_object_value, same_opt_object_value, same_value},
    value::{AccessorValue, Value},
    Context,
};

// An ordinary object is used to create the vtable for a generic object. Must be a separate type
// from ObjectValue so that the same methods can appear on ObjectValue but perform dynamic dispatch.
//
// Should never be created. Only used to reference its vtable.
extend_object! {
    pub struct OrdinaryObject {}
}

impl Into<ObjectValue> for OrdinaryObject {
    fn into(self) -> ObjectValue {
        unsafe { std::mem::transmute::<OrdinaryObject, ObjectValue>(self) }
    }
}

impl ObjectValue {
    // 10.1.1 [[GetPrototypeOf]]
    // 10.1.1.1 OrdinaryGetPrototypeOf
    pub fn ordinary_get_prototype_of(&self) -> EvalResult<Option<Handle<ObjectValue>>> {
        self.prototype().map(Handle::from_heap).into()
    }

    // 10.1.3 [[IsExtensible]]
    // 10.1.3.1 OrdinaryIsExtensible
    pub fn ordinary_is_extensible(&self) -> EvalResult<bool> {
        self.is_extensible_field().into()
    }

    // 10.1.4 [[PreventExtensions]]
    // 10.1.4.1 OrdinaryPreventExtensions
    pub fn ordinary_prevent_extensions(&mut self) -> EvalResult<bool> {
        self.set_is_extensible_field(false);
        true.into()
    }
}

impl Handle<ObjectValue> {
    // 10.1.2 [[SetPrototypeOf]]
    // 10.1.2.1 OrdinarySetPrototypeOf
    pub fn ordinary_set_prototype_of(
        &mut self,
        cx: &mut Context,
        new_prototype: Option<Handle<ObjectValue>>,
    ) -> EvalResult<bool> {
        if same_opt_object_value(self.prototype(), new_prototype) {
            return true.into();
        }

        // Inlined 10.4.7.2 SetImmutablePrototype, currently only applies to object prototypes.
        // If the prototypes differ, then a set immutable prototype always fails.
        if self.is_object_prototype() {
            return false.into();
        }

        if !self.is_extensible_field() {
            return false.into();
        }

        let mut current_prototype = new_prototype;
        loop {
            match current_prototype {
                None => break,
                Some(current_proto) => {
                    if same_object_value(current_proto, *self) {
                        return false.into();
                    }

                    if current_proto.is_proxy() {
                        break;
                    } else {
                        current_prototype = must!(current_proto.get_prototype_of(cx));
                    }
                }
            }
        }

        self.set_prototype(new_prototype);

        return true.into();
    }
}

impl VirtualObject for Handle<OrdinaryObject> {
    // 10.1.5 [[GetOwnProperty]]
    fn get_own_property(
        &self,
        _: &mut Context,
        key: PropertyKey,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        ordinary_get_own_property(self.object(), key).into()
    }

    // 10.1.6 [[DefineOwnProperty]]
    fn define_own_property(
        &mut self,
        cx: &mut Context,
        key: PropertyKey,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        ordinary_define_own_property(cx, self.object(), key, desc)
    }

    // 10.1.7 [[HasProperty]]
    fn has_property(&self, cx: &mut Context, key: PropertyKey) -> EvalResult<bool> {
        ordinary_has_property(cx, self.object(), key)
    }

    // 10.1.8 [[Get]]
    fn get(
        &self,
        cx: &mut Context,
        key: PropertyKey,
        receiver: HandleValue,
    ) -> EvalResult<HandleValue> {
        ordinary_get(cx, self.object(), key, receiver)
    }

    // 10.1.9 [[Set]]
    fn set(
        &mut self,
        cx: &mut Context,
        key: PropertyKey,
        value: HandleValue,
        receiver: HandleValue,
    ) -> EvalResult<bool> {
        ordinary_set(cx, self.object(), key, value, receiver)
    }

    // 10.1.10 [[Delete]]
    fn delete(&mut self, cx: &mut Context, key: PropertyKey) -> EvalResult<bool> {
        ordinary_delete(cx, self.object(), key)
    }

    // 10.1.11 [[OwnPropertyKeys]]
    fn own_property_keys(&self, cx: &mut Context) -> EvalResult<Vec<HandleValue>> {
        ordinary_own_property_keys(cx, self.object()).into()
    }
}

// 10.1.5.1 OrdinaryGetOwnProperty
pub fn ordinary_get_own_property(
    object: Handle<ObjectValue>,
    key: PropertyKey,
) -> Option<PropertyDescriptor> {
    match object.get_property(key) {
        None => None,
        Some(property) => {
            if property.value().is_accessor() {
                let accessor_value = property.value().as_accessor();
                Some(PropertyDescriptor::accessor(
                    accessor_value.get,
                    accessor_value.set,
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
    object: Handle<ObjectValue>,
    key: PropertyKey,
    desc: PropertyDescriptor,
) -> EvalResult<bool> {
    let current_desc = maybe!(object.get_own_property(cx, key));
    let is_extensible = maybe!(object.is_extensible(cx));

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
    validate_and_apply_property_descriptor(
        cx,
        None,
        cx.names.empty_string(),
        is_extensible,
        desc,
        current_desc,
    )
}

// 10.1.6.3 ValidateAndApplyPropertyDescriptor
pub fn validate_and_apply_property_descriptor(
    cx: &mut Context,
    mut object: Option<Handle<ObjectValue>>,
    key: PropertyKey,
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
        let mut object = object.unwrap();

        // Create new property with fields in descriptor, using default if field is not set
        let is_enumerable = desc.is_enumerable.unwrap_or(false);
        let is_configurable = desc.is_configurable.unwrap_or(false);

        let property = if desc.is_accessor_descriptor() {
            let accessor_value = AccessorValue::new(cx, desc.get, desc.set);

            Property::accessor(accessor_value.into(), is_enumerable, is_configurable)
        } else {
            let is_writable = desc.is_writable.unwrap_or(false);
            let value = desc.value.unwrap_or_else(|| Value::undefined());

            Property::data(value, is_writable, is_enumerable, is_configurable)
        };

        object.set_property(cx, key, property);

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
                let mut property = object.get_property(key).unwrap();
                if desc.is_data_descriptor() {
                    property.set_value(Value::undefined());
                    property.set_is_writable(false);
                } else {
                    let accessor_value = AccessorValue::new(cx, None, None);
                    property.set_value(accessor_value.into());
                }

                // Set modified property on object
                object.set_property(cx, key, property)
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
            let mut property = object.get_property(key).unwrap();

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
                    accessor_value.get = Some(get);
                }

                if let Some(set) = desc.set {
                    accessor_value.set = Some(set);
                }
            }

            // Set modified property on object
            object.set_property(cx, key, property);
        }
        None => {}
    }

    true
}

// 10.1.7.1 OrdinaryHasProperty
pub fn ordinary_has_property(
    cx: &mut Context,
    object: Handle<ObjectValue>,
    key: PropertyKey,
) -> EvalResult<bool> {
    let own_property = maybe!(object.get_own_property(cx, key));
    if own_property.is_some() {
        return true.into();
    }

    let parent = maybe!(object.get_prototype_of(cx));
    match parent {
        Some(parent) => parent.has_property(cx, key),
        None => false.into(),
    }
}

// 10.1.8.1 OrdinaryGet
pub fn ordinary_get(
    cx: &mut Context,
    object: Handle<ObjectValue>,
    key: PropertyKey,
    receiver: HandleValue,
) -> EvalResult<HandleValue> {
    let desc = maybe!(object.get_own_property(cx, key));
    match desc {
        None => {
            let parent = maybe!(object.get_prototype_of(cx));
            match parent {
                None => Value::undefined().into(),
                Some(parent) => parent.get(cx, key, receiver),
            }
        }
        Some(desc) if desc.is_data_descriptor() => desc.value.unwrap().into(),
        Some(PropertyDescriptor { get: None, .. }) => Value::undefined().into(),
        Some(PropertyDescriptor { get: Some(getter), .. }) => {
            call_object(cx, getter, receiver, &[])
        }
    }
}

// 10.1.9.1 OrdinarySet
// 10.1.9.2 OrdinarySetWithOwnDescriptor
pub fn ordinary_set(
    cx: &mut Context,
    object: Handle<ObjectValue>,
    key: PropertyKey,
    value: HandleValue,
    receiver: HandleValue,
) -> EvalResult<bool> {
    let own_desc = maybe!(object.get_own_property(cx, key));
    let own_desc = match own_desc {
        None => {
            let parent = maybe!(object.get_prototype_of(cx));
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
        let existing_descriptor = maybe!(receiver.get_own_property(cx, key));
        match existing_descriptor {
            None => create_data_property(cx, receiver, key, value),
            Some(existing_descriptor) if existing_descriptor.is_accessor_descriptor() => {
                false.into()
            }
            Some(PropertyDescriptor { is_writable: Some(false), .. }) => false.into(),
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
pub fn ordinary_delete(
    cx: &mut Context,
    mut object: Handle<ObjectValue>,
    key: PropertyKey,
) -> EvalResult<bool> {
    let desc = maybe!(object.get_own_property(cx, key));
    match desc {
        None => true.into(),
        Some(desc) => {
            if desc.is_configurable() {
                object.remove_property(key);
                true.into()
            } else {
                false.into()
            }
        }
    }
}

// 10.1.11.1 OrdinaryOwnPropertyKeys
pub fn ordinary_own_property_keys(
    cx: &mut Context,
    object: Handle<ObjectValue>,
) -> Vec<HandleValue> {
    let mut keys: Vec<HandleValue> = vec![];

    ordinary_filtered_own_indexed_property_keys(cx, object, &mut keys, |_| true);
    ordinary_own_string_symbol_property_keys(object, &mut keys);

    keys
}

#[inline]
pub fn ordinary_filtered_own_indexed_property_keys<F: Fn(usize) -> bool>(
    cx: &mut Context,
    object: Handle<ObjectValue>,
    keys: &mut Vec<HandleValue>,
    filter: F,
) {
    // Return array index properties in numerical order
    let array_properties = object.array_properties();
    if let Some(dense_properties) = array_properties.as_dense_opt() {
        let dense_properties = Handle::from_heap(dense_properties);
        for (index, value) in dense_properties.iter().enumerate() {
            if filter(index) {
                if !value.is_empty() {
                    let index_string = cx.alloc_string(index.to_string());
                    keys.push(Value::string(index_string));
                }
            }
        }
    } else {
        let sparse_properties = array_properties.as_sparse();

        for index in sparse_properties.ordered_keys() {
            if filter(index as usize) {
                let index_string = cx.alloc_string(index.to_string());
                keys.push(Value::string(index_string));
            }
        }
    }
}

#[inline]
pub fn ordinary_own_string_symbol_property_keys(
    object: Handle<ObjectValue>,
    keys: &mut Vec<HandleValue>,
) {
    // Safe since we do not allocate on managed heap during iteration
    object.iter_named_property_keys_gc_unsafe(|property_key| {
        if let Some(string_key) = property_key.as_string_opt() {
            keys.push(string_key.into());
        }
    });

    // Safe since we do not allocate on managed heap during iteration
    object.iter_named_property_keys_gc_unsafe(|property_key| {
        if let Some(symbol_key) = property_key.as_symbol_opt() {
            keys.push(symbol_key.into());
        }
    });
}

pub fn ordinary_object_create(cx: &mut Context) -> Handle<ObjectValue> {
    let object = cx.heap.alloc_uninit::<ObjectValue>();

    let descriptor = cx.base_descriptors.get(ObjectKind::OrdinaryObject);
    let proto = cx.get_intrinsic_ptr(Intrinsic::ObjectPrototype);
    object_ordinary_init(cx, object, descriptor, Some(proto));

    object
}

pub fn object_create<T>(
    cx: &mut Context,
    descriptor_kind: ObjectKind,
    intrinsic_proto: Intrinsic,
) -> HeapPtr<T>
where
    HeapPtr<T>: Into<HeapPtr<ObjectValue>>,
{
    let object = cx.heap.alloc_uninit::<T>();

    let descriptor = cx.base_descriptors.get(descriptor_kind);
    let proto = cx.get_intrinsic_ptr(intrinsic_proto);
    object_ordinary_init(cx, object.into(), descriptor, Some(proto));

    object
}

pub fn object_create_with_proto<T>(
    cx: &mut Context,
    descriptor_kind: ObjectKind,
    proto: Handle<ObjectValue>,
) -> HeapPtr<T>
where
    HeapPtr<T>: Into<HeapPtr<ObjectValue>>,
{
    let object = cx.heap.alloc_uninit::<T>();

    let descriptor = cx.base_descriptors.get(descriptor_kind);
    object_ordinary_init(cx, object.into(), descriptor, Some(proto.get_()));

    object
}

pub fn object_create_with_optional_proto<T>(
    cx: &mut Context,
    descriptor_kind: ObjectKind,
    proto: Option<Handle<ObjectValue>>,
) -> HeapPtr<T>
where
    HeapPtr<T>: Into<HeapPtr<ObjectValue>>,
{
    let object = cx.heap.alloc_uninit::<T>();

    let descriptor = cx.base_descriptors.get(descriptor_kind);
    let proto = proto.map(|p| p.get_());
    object_ordinary_init(cx, object.into(), descriptor, proto);

    object
}

pub fn object_ordinary_init(
    cx: &mut Context,
    mut object: HeapPtr<ObjectValue>,
    descriptor: HeapPtr<ObjectDescriptor>,
    proto: Option<HeapPtr<ObjectValue>>,
) {
    // Object initialization does not currentlly allocate so a GC cannot occur. This means it is
    // safe to use a raw reference.
    let object = object.deref_mut();

    object.set_descriptor(descriptor);
    object.set_prototype(proto);
    object.set_named_properties(IndexMap::new());
    object.set_array_properties(ArrayProperties::initial(cx));
    object.set_is_extensible_field(true);
    object.set_uninit_hash_code();
}

// 10.1.13 OrdinaryCreateFromConstructor
// Creates an object of type T, and initializes the standard object fields.
pub fn object_create_from_constructor<T>(
    cx: &mut Context,
    constructor: Handle<ObjectValue>,
    descriptor_kind: ObjectKind,
    intrinsic_default_proto: Intrinsic,
) -> EvalResult<HeapPtr<T>>
where
    HeapPtr<T>: Into<HeapPtr<ObjectValue>>,
{
    // May allocate, so call before allocating object
    let proto = maybe!(get_prototype_from_constructor(cx, constructor, intrinsic_default_proto));

    let object = cx.heap.alloc_uninit::<T>();

    let descriptor = cx.base_descriptors.get(descriptor_kind);
    object_ordinary_init(cx, object.into(), descriptor, Some(proto.get_()));

    EvalResult::Ok(object)
}

// 10.1.14 GetPrototypeFromConstructor
// May allocate.
pub fn get_prototype_from_constructor(
    cx: &mut Context,
    constructor: Handle<ObjectValue>,
    intrinsic_default_proto: Intrinsic,
) -> EvalResult<Handle<ObjectValue>> {
    let proto = maybe!(get(cx, constructor, cx.names.prototype()));
    if proto.is_object() {
        proto.as_object().into()
    } else {
        let realm = maybe!(get_function_realm(cx, constructor));
        realm.get_intrinsic(intrinsic_default_proto).into()
    }
}
