use crate::{
    extend_object, must, must_a,
    runtime::{
        Context, HeapItemKind,
        abstract_operations::{call_object, create_data_property, get, get_function_realm},
        accessor::Accessor,
        alloc_error::AllocResult,
        eval_result::EvalResult,
        gc::{Handle, HeapItem, HeapPtr, HeapVisitor},
        intrinsics::{intrinsics::Intrinsic, object_prototype_object::ObjectPrototypeObject},
        object_value::{ObjectValue, VirtualObject},
        property::Property,
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        proxy_object::ProxyObject,
        rust_vtables::extract_virtual_object_vtable,
        shape::{Shape, TransitionResult},
        shape_registry::ShapeRegistry,
        type_utilities::{same_object_value_handles, same_opt_object_value, same_value},
        value::Value,
    },
};

extend_object! {
    /// An ordinary object is used to create the vtable for a generic object. Must be a separate
    /// type from ObjectValue so that the same methods can appear on ObjectValue but perform dynamic
    /// dispatch.
    pub struct OrdinaryObject {}
}

impl Handle<ObjectValue> {
    /// OrdinarySetPrototypeOf (https://tc39.es/ecma262/#sec-ordinarysetprototypeof)
    pub fn ordinary_set_prototype_of(
        &mut self,
        cx: Context,
        new_prototype: Option<Handle<ObjectValue>>,
    ) -> EvalResult<bool> {
        if same_opt_object_value(self.prototype(), new_prototype.map(|p| *p)) {
            return Ok(true);
        }

        // Inlined SetImmutablePrototype (https://tc39.es/ecma262/#sec-set-immutable-prototype,)
        // currently only applies to object prototypes. If the prototypes differ, then a set
        // immutable prototype always fails.
        if self.is::<ObjectPrototypeObject>() {
            return Ok(false);
        }

        if !self.shape_ptr().is_extensible() {
            return Ok(false);
        }

        let mut current_prototype = new_prototype;
        loop {
            match current_prototype {
                None => break,
                Some(current_proto) => {
                    if same_object_value_handles(current_proto, *self) {
                        return Ok(false);
                    }

                    if current_proto.is::<ProxyObject>() {
                        break;
                    } else {
                        current_prototype = must!(current_proto.get_prototype_of(cx));
                    }
                }
            }
        }

        // Prototype is stored on shape. Transition to a new shape with the updated prototype.
        match self.shape().set_prototype(cx, new_prototype)? {
            TransitionResult::Transitioned(new_shape) => {
                self.set_shape(new_shape);
            }
            TransitionResult::EnterMapMode => {
                self.enter_map_mode(cx)?;
                self.shape().set_prototype(cx, new_prototype)?;
            }
        }

        Ok(true)
    }
}

impl OrdinaryObject {
    pub const VIRTUAL_OBJECT_VTABLE: *const () = extract_virtual_object_vtable::<Self>();
}

impl VirtualObject for Handle<OrdinaryObject> {
    /// [[GetOwnProperty]] (https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-getownproperty-p)
    fn get_own_property(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        Ok(ordinary_get_own_property(cx, self.as_object(), key))
    }

    /// [[DefineOwnProperty]] (https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-defineownproperty-p-desc)
    fn define_own_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        ordinary_define_own_property(cx, self.as_object(), key, desc)
    }

    /// [[HasProperty]] (https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-hasproperty-p)
    fn has_property(&self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
        ordinary_has_property(cx, self.as_object(), key)
    }

    /// [[Get]] (https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-get-p-receiver)
    fn get(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
        receiver: Handle<Value>,
    ) -> EvalResult<Handle<Value>> {
        ordinary_get(cx, self.as_object(), key, receiver)
    }

    /// [[Set]] (https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-set-p-v-receiver)
    fn set(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        value: Handle<Value>,
        receiver: Handle<Value>,
    ) -> EvalResult<bool> {
        ordinary_set(cx, self.as_object(), key, value, receiver)
    }

    /// [[Delete]] (https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-delete-p)
    fn delete(&mut self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
        ordinary_delete(cx, self.as_object(), key)
    }

    /// [[OwnPropertyKeys]] (https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-ownpropertykeys)
    fn own_property_keys(&self, cx: Context) -> EvalResult<Vec<Handle<Value>>> {
        Ok(ordinary_own_property_keys(cx, self.as_object())?)
    }
}

/// OrdinaryGetOwnProperty (https://tc39.es/ecma262/#sec-ordinarygetownproperty)
pub fn ordinary_get_own_property(
    cx: Context,
    object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
) -> Option<PropertyDescriptor> {
    match object.get_property(cx, key) {
        None => None,
        Some(property) => {
            let value = property.value();
            if property.is_accessor() {
                let accessor_value = Accessor::from_value(value);
                Some(PropertyDescriptor::accessor(
                    accessor_value.get.map(|f| f.to_handle()),
                    accessor_value.set.map(|f| f.to_handle()),
                    property.is_enumerable(),
                    property.is_configurable(),
                ))
            } else {
                Some(PropertyDescriptor::data(
                    value,
                    property.is_writable(),
                    property.is_enumerable(),
                    property.is_configurable(),
                ))
            }
        }
    }
}

/// OrdinaryDefineOwnProperty (https://tc39.es/ecma262/#sec-ordinarydefineownproperty)
pub fn ordinary_define_own_property(
    cx: Context,
    object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
    desc: PropertyDescriptor,
) -> EvalResult<bool> {
    let current_desc = object.get_own_property(cx, key)?;
    let is_extensible = object.is_extensible(cx)?;

    Ok(validate_and_apply_property_descriptor(
        cx,
        Some(object),
        key,
        is_extensible,
        desc,
        current_desc,
    )?)
}

/// IsCompatiblePropertyDescriptor (https://tc39.es/ecma262/#sec-iscompatiblepropertydescriptor)
pub fn is_compatible_property_descriptor(
    cx: Context,
    is_extensible: bool,
    desc: PropertyDescriptor,
    current_desc: Option<PropertyDescriptor>,
) -> AllocResult<bool> {
    validate_and_apply_property_descriptor(
        cx,
        None,
        cx.names.empty_string(),
        is_extensible,
        desc,
        current_desc,
    )
}

/// ValidateAndApplyPropertyDescriptor (https://tc39.es/ecma262/#sec-validateandapplypropertydescriptor)
pub fn validate_and_apply_property_descriptor(
    cx: Context,
    mut object: Option<Handle<ObjectValue>>,
    key: Handle<PropertyKey>,
    is_extensible: bool,
    desc: PropertyDescriptor,
    current_desc: Option<PropertyDescriptor>,
) -> AllocResult<bool> {
    if current_desc.is_none() {
        if !is_extensible {
            return Ok(false);
        }

        if object.is_none() {
            return Ok(true);
        }
        let mut object = object.unwrap();

        // Create new property with fields in descriptor, using default if field is not set
        let is_enumerable = desc.is_enumerable.unwrap_or(false);
        let is_configurable = desc.is_configurable.unwrap_or(false);

        let property = if desc.is_accessor_descriptor() {
            let accessor_value = Accessor::new(cx, desc.get, desc.set)?;

            Property::accessor(accessor_value.into(), is_enumerable, is_configurable)
        } else {
            let is_writable = desc.is_writable.unwrap_or(false);
            let value = desc.value.unwrap_or_else(|| cx.undefined());

            Property::data(value, is_writable, is_enumerable, is_configurable)
        };

        object.set_property(cx, key, property)?;

        return Ok(true);
    }

    let current_desc = current_desc.unwrap();
    if desc.has_no_fields() {
        return Ok(true);
    }

    if !current_desc.is_configurable() {
        if let Some(true) = desc.is_configurable {
            return Ok(false);
        }

        match desc.is_enumerable {
            Some(is_enumerable) if is_enumerable != current_desc.is_enumerable() => {
                return Ok(false);
            }
            _ => {}
        }
    }

    if desc.is_generic_descriptor() {
        // No validation necessary
    } else if current_desc.is_data_descriptor() != desc.is_data_descriptor() {
        if !current_desc.is_configurable() {
            return Ok(false);
        }

        match &mut object {
            None => return Ok(true),
            Some(object) => {
                // Converting between data and accessor. Preserve shared fields and set others to
                // their defaults.
                let mut property = object.get_property(cx, key).unwrap();
                if desc.is_data_descriptor() {
                    property.set_value(cx.undefined());
                    property.set_is_writable(false);
                    property.set_is_accessor(false);
                } else {
                    let accessor_value = Accessor::new(cx, None, None)?;
                    property.set_value(accessor_value.into());
                    property.set_is_writable(false);
                    property.set_is_accessor(true);
                }

                // Set modified property on object
                object.set_property(cx, key, property)?
            }
        }
    } else if current_desc.is_data_descriptor() && desc.is_data_descriptor() {
        if !current_desc.is_configurable() && !current_desc.is_writable() {
            if let Some(true) = desc.is_writable {
                return Ok(false);
            }

            match desc.value {
                Some(value) if !same_value(value, current_desc.value.unwrap())? => {
                    return Ok(false);
                }
                _ => {}
            }

            return Ok(true);
        }
    } else {
        if !current_desc.is_configurable() {
            // Error if a [[Get]] was provided but does not match the existing [[Get]]
            if desc.has_get {
                match (desc.get, current_desc.get) {
                    (Some(get), Some(current_get))
                        if !same_object_value_handles(get, current_get) =>
                    {
                        return Ok(false);
                    }
                    (Some(_), None) | (None, Some(_)) => return Ok(false),
                    _ => {}
                }
            }

            // Error if a [[Set]] was provided but does not match the existing [[Set]]
            if desc.has_set {
                match (desc.set, current_desc.set) {
                    (Some(set), Some(current_set))
                        if !same_object_value_handles(set, current_set) =>
                    {
                        return Ok(false);
                    }
                    (Some(_), None) | (None, Some(_)) => return Ok(false),
                    _ => {}
                }
            }

            return Ok(true);
        }
    }

    if let Some(object) = &mut object {
        // For every field in new descriptor that is present, set the corresponding attribute in
        // the existing descriptor.
        let mut property = object.get_property(cx, key).unwrap();

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
            if desc.has_get {
                let mut accessor_value = Accessor::from_value(property.value());
                accessor_value.get = desc.get.map(|x| *x);
            }

            if desc.has_set {
                let mut accessor_value = Accessor::from_value(property.value());
                accessor_value.set = desc.set.map(|x| *x);
            }
        }

        // Set modified property on object
        object.set_property(cx, key, property)?;
    }

    Ok(true)
}

/// OrdinaryHasProperty (https://tc39.es/ecma262/#sec-ordinaryhasproperty)
pub fn ordinary_has_property(
    cx: Context,
    object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
) -> EvalResult<bool> {
    let own_property = object.get_own_property(cx, key)?;
    if own_property.is_some() {
        return Ok(true);
    }

    let parent = object.get_prototype_of(cx)?;
    match parent {
        Some(parent) => parent.has_property(cx, key),
        None => Ok(false),
    }
}

/// OrdinaryGet (https://tc39.es/ecma262/#sec-ordinaryget)
pub fn ordinary_get(
    cx: Context,
    object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
    receiver: Handle<Value>,
) -> EvalResult<Handle<Value>> {
    let desc = object.get_own_property(cx, key)?;
    match desc {
        None => {
            let parent = object.get_prototype_of(cx)?;
            match parent {
                None => Ok(cx.undefined()),
                Some(parent) => parent.get(cx, key, receiver),
            }
        }
        Some(desc) if desc.is_data_descriptor() => Ok(desc.value.unwrap()),
        Some(PropertyDescriptor { get: None, .. }) => Ok(cx.undefined()),
        Some(PropertyDescriptor { get: Some(getter), .. }) => {
            call_object(cx, getter, receiver, &[])
        }
    }
}

/// OrdinarySet (https://tc39.es/ecma262/#sec-ordinaryset)
/// OrdinarySetWithOwnDescriptor (https://tc39.es/ecma262/#sec-ordinarysetwithowndescriptor)
pub fn ordinary_set(
    cx: Context,
    object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
    value: Handle<Value>,
    receiver: Handle<Value>,
) -> EvalResult<bool> {
    let own_desc = object.get_own_property(cx, key)?;
    let own_desc = match own_desc {
        None => {
            let parent = object.get_prototype_of(cx)?;
            match parent {
                None => PropertyDescriptor::data(cx.undefined(), true, true, true),
                Some(mut parent) => return parent.set(cx, key, value, receiver),
            }
        }
        Some(own_desc) => own_desc,
    };

    if own_desc.is_data_descriptor() {
        if let Some(false) = own_desc.is_writable {
            return Ok(false);
        }

        if !receiver.is_object() {
            return Ok(false);
        }

        let mut receiver = receiver.as_object();
        let existing_descriptor = receiver.get_own_property(cx, key)?;
        match existing_descriptor {
            None => create_data_property(cx, receiver, key, value),
            Some(existing_descriptor) if existing_descriptor.is_accessor_descriptor() => Ok(false),
            Some(PropertyDescriptor { is_writable: Some(false), .. }) => Ok(false),
            Some(_) => {
                let value_desc = PropertyDescriptor::data_value_only(value);
                receiver.define_own_property(cx, key, value_desc)
            }
        }
    } else {
        match own_desc.set {
            None => Ok(false),
            Some(setter) => {
                call_object(cx, setter, receiver, &[value])?;
                Ok(true)
            }
        }
    }
}

/// OrdinaryDelete (https://tc39.es/ecma262/#sec-ordinarydelete)
pub fn ordinary_delete(
    cx: Context,
    mut object: Handle<ObjectValue>,
    key: Handle<PropertyKey>,
) -> EvalResult<bool> {
    let desc = object.get_own_property(cx, key)?;
    match desc {
        None => Ok(true),
        Some(desc) => {
            if desc.is_configurable() {
                object.remove_property(cx, key)?;
                Ok(true)
            } else {
                Ok(false)
            }
        }
    }
}

/// OrdinaryOwnPropertyKeys (https://tc39.es/ecma262/#sec-ordinaryownpropertykeys)
pub fn ordinary_own_property_keys(
    cx: Context,
    object: Handle<ObjectValue>,
) -> AllocResult<Vec<Handle<Value>>> {
    let mut keys: Vec<Handle<Value>> = vec![];

    ordinary_filtered_own_indexed_property_keys(cx, object, &mut keys, |_| true)?;
    ordinary_own_string_symbol_property_keys(object, &mut keys);

    Ok(keys)
}

#[inline]
pub fn ordinary_filtered_own_indexed_property_keys<F: Fn(usize) -> bool>(
    mut cx: Context,
    object: Handle<ObjectValue>,
    keys: &mut Vec<Handle<Value>>,
    filter: F,
) -> AllocResult<()> {
    // Return array index properties in numerical order
    let array_properties = object.array_properties();
    if let Some(dense_properties) = array_properties.as_dense_opt() {
        let dense_properties = dense_properties.to_handle();
        for (index, value) in dense_properties.iter().enumerate() {
            if filter(index) && !value.is_empty() {
                // Safe since string length is guaranteed to be valid
                let index_string = must_a!(cx.alloc_string(&index.to_string()));
                keys.push(index_string.into());
            }
        }
    } else {
        let sparse_properties = array_properties.as_sparse();

        for index in sparse_properties.ordered_keys() {
            if filter(index as usize) {
                // Safe since string length is guaranteed to be valid
                let index_string = must_a!(cx.alloc_string(&index.to_string()));
                keys.push(index_string.into());
            }
        }
    }

    Ok(())
}

#[inline]
pub fn ordinary_own_string_symbol_property_keys(
    object: Handle<ObjectValue>,
    keys: &mut Vec<Handle<Value>>,
) {
    // Collect symbol keys to be added after string keys
    let mut symbol_keys: Vec<Handle<Value>> = vec![];

    object.iter_named_properties_gc_unsafe(|property_key, flags| {
        if property_key.is_string() {
            keys.push(property_key.as_string().to_handle().into());
        } else if property_key.is_symbol() && !flags.is_private() {
            // Make sure not to include private properties
            symbol_keys.push(property_key.as_symbol().to_handle().into());
        }
    });

    keys.extend(symbol_keys);
}

pub fn ordinary_object_create(cx: Context) -> AllocResult<Handle<ObjectValue>> {
    let proto = cx.get_intrinsic(Intrinsic::ObjectPrototype);
    let shape =
        ShapeRegistry::get_root_object_shape(cx, HeapItemKind::OrdinaryObject, Some(proto))?;

    let object = cx.alloc_uninit::<ObjectValue>()?;
    init_object_fields(cx, object, *shape);

    Ok(object.to_handle())
}

pub fn ordinary_object_create_without_proto(cx: Context) -> AllocResult<Handle<ObjectValue>> {
    let shape = ShapeRegistry::get_root_object_shape(cx, HeapItemKind::OrdinaryObject, None)?;

    let object = cx.alloc_uninit::<ObjectValue>()?;
    init_object_fields(cx, object, *shape);

    Ok(object.to_handle())
}

pub fn object_create<T>(
    cx: Context,
    shape_kind: HeapItemKind,
    intrinsic_proto: Intrinsic,
) -> AllocResult<HeapPtr<T>>
where
    HeapPtr<T>: Into<HeapPtr<ObjectValue>>,
{
    let proto = cx.get_intrinsic(intrinsic_proto);
    let shape = ShapeRegistry::get_root_object_shape(cx, shape_kind, Some(proto))?;

    let object = cx.alloc_uninit::<T>()?;
    init_object_fields(cx, object.into(), *shape);

    Ok(object)
}

pub fn object_create_with_size<T>(
    cx: Context,
    size: usize,
    shape_kind: HeapItemKind,
    intrinsic_proto: Intrinsic,
) -> AllocResult<HeapPtr<T>>
where
    HeapPtr<T>: Into<HeapPtr<ObjectValue>>,
{
    let proto = cx.get_intrinsic(intrinsic_proto);
    let shape = ShapeRegistry::get_root_object_shape(cx, shape_kind, Some(proto))?;

    let object = cx.alloc_uninit_with_size::<T>(size)?;
    init_object_fields(cx, object.into(), *shape);

    Ok(object)
}

pub fn object_create_with_proto<T>(
    cx: Context,
    shape_kind: HeapItemKind,
    proto: Handle<ObjectValue>,
) -> AllocResult<HeapPtr<T>>
where
    HeapPtr<T>: Into<HeapPtr<ObjectValue>>,
{
    let shape = ShapeRegistry::get_root_object_shape(cx, shape_kind, Some(proto))?;

    let object = cx.alloc_uninit::<T>()?;
    init_object_fields(cx, object.into(), *shape);

    Ok(object)
}

pub fn object_create_with_optional_proto<T>(
    cx: Context,
    shape_kind: HeapItemKind,
    proto: Option<Handle<ObjectValue>>,
) -> AllocResult<HeapPtr<T>>
where
    HeapPtr<T>: Into<HeapPtr<ObjectValue>>,
{
    let shape = ShapeRegistry::get_root_object_shape(cx, shape_kind, proto)?;

    let object = cx.alloc_uninit::<T>()?;
    init_object_fields(cx, object.into(), *shape);

    Ok(object)
}

pub fn init_object_fields(cx: Context, mut object: HeapPtr<ObjectValue>, shape: HeapPtr<Shape>) {
    object.set_shape(shape);

    if shape.is_map_mode() {
        object.set_named_properties_map(cx.default_named_properties_map);
    } else {
        object.set_named_properties_array(cx.default_named_properties_array);
    }

    object.set_array_properties(cx.default_array_properties);
    object.set_uninit_hash_code();
}

/// OrdinaryCreateFromConstructor (https://tc39.es/ecma262/#sec-ordinarycreatefromconstructor)
/// Creates an object of type T, and initializes the standard object fields.
pub fn object_create_from_constructor<T>(
    cx: Context,
    constructor: Handle<ObjectValue>,
    shape_kind: HeapItemKind,
    intrinsic_default_proto: Intrinsic,
) -> EvalResult<HeapPtr<T>>
where
    HeapPtr<T>: Into<HeapPtr<ObjectValue>>,
{
    // May allocate, so call before allocating object
    let proto = get_prototype_from_constructor(cx, constructor, intrinsic_default_proto)?;
    let shape = ShapeRegistry::get_root_object_shape(cx, shape_kind, Some(proto))?;

    let object = cx.alloc_uninit::<T>()?;
    init_object_fields(cx, object.into(), *shape);

    Ok(object)
}

/// GetPrototypeFromConstructor (https://tc39.es/ecma262/#sec-getprototypefromconstructor)
/// May allocate.
pub fn get_prototype_from_constructor(
    cx: Context,
    constructor: Handle<ObjectValue>,
    intrinsic_default_proto: Intrinsic,
) -> EvalResult<Handle<ObjectValue>> {
    let proto = get(cx, constructor, cx.names.prototype())?;
    if proto.is_object() {
        Ok(proto.as_object())
    } else {
        let realm = get_function_realm(cx, constructor)?;
        Ok(realm.get_intrinsic(intrinsic_default_proto))
    }
}

impl HeapItem for OrdinaryObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<OrdinaryObject>()
    }

    fn visit_pointers(mut object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        object.visit_object_pointers(visitor);
    }
}
