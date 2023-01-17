use std::collections::HashMap;

use crate::{impl_gc_into, maybe, must};

use super::{
    abstract_operations::{call_object, create_data_property, get, get_function_realm},
    builtin_function::{BuiltinFunction, BuiltinFunctionPtr},
    completion::EvalResult,
    environment::private_environment::PrivateNameId,
    error::type_error_,
    gc::{Gc, GcDeref},
    intrinsics::intrinsics::Intrinsic,
    object_value::{extract_object_vtable, Object, ObjectValue, ObjectValueVtable},
    property::{PrivateProperty, Property},
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
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
    // String and symbol properties by their property key
    properties: HashMap<PropertyKey, Property>,
    // Array index properties by their property key
    array_properties: ArrayProperties,
    // Private properties with string keys
    private_properties: HashMap<PrivateNameId, PrivateProperty>,
    is_extensible: bool,
}

impl GcDeref for OrdinaryObject {}

impl_gc_into!(OrdinaryObject, ObjectValue);

// Number of indices past the end of an array an access can occur before dense array is converted
// to a sparse array.
const SPARSE_ARRAY_THRESHOLD: usize = 100;

impl OrdinaryObject {
    const VTABLE: *const () = extract_object_vtable::<OrdinaryObject>();

    pub fn new(prototype: Option<Gc<ObjectValue>>, is_extensible: bool) -> OrdinaryObject {
        OrdinaryObject {
            _vtable: Self::VTABLE,
            prototype,
            properties: HashMap::new(),
            array_properties: ArrayProperties::new(),
            private_properties: HashMap::new(),
            is_extensible,
        }
    }

    pub fn new_uninit() -> OrdinaryObject {
        OrdinaryObject {
            _vtable: Self::VTABLE,
            prototype: None,
            properties: HashMap::new(),
            array_properties: ArrayProperties::new(),
            private_properties: HashMap::new(),
            is_extensible: false,
        }
    }

    #[inline]
    fn expand_dense_properties(&mut self, new_length: u32) {
        if let ArrayProperties::Dense(array) = &mut self.array_properties {
            let default_value = Property::data(Value::empty(), true, true, true);
            array.resize(new_length as usize, default_value);
        } else {
            unreachable!("expected dense properties");
        }
    }

    fn fall_back_to_sparse_properties(&mut self) {
        let array = if let ArrayProperties::Dense(array) = &self.array_properties {
            array
        } else {
            unreachable!("expected dense properties");
        };

        let mut sparse_map = HashMap::new();

        for (index, value) in array.iter().enumerate() {
            if !value.value().is_empty() {
                sparse_map.insert(index as u32, (*value).clone());
            }
        }

        self.array_properties = ArrayProperties::Sparse { sparse_map, length: array.len() as u32 }
    }

    pub fn array_properties_length(&self) -> u32 {
        match &self.array_properties {
            ArrayProperties::Dense(array) => array.len() as u32,
            ArrayProperties::Sparse { length, .. } => *length,
        }
    }

    // Resize array properties to match a new array length, potentially expanding and adding empty
    // values, or shrinking and removing existing values.
    //
    // Properties are removed in descending numerical order, and removal can fail if the property
    // is not configurable. In this case set the new length to have that property at the end of the
    // array, stop deleting other properties, and return false.
    //
    // Returns return true on success.
    pub fn set_array_properties_length(&mut self, new_length: u32) -> bool {
        // First try falling back to sparse properties if this is an expanded dense array
        match &self.array_properties {
            ArrayProperties::Dense(array)
                if new_length as usize >= array.len() + SPARSE_ARRAY_THRESHOLD =>
            {
                self.fall_back_to_sparse_properties();
            }
            _ => {}
        }

        match &mut self.array_properties {
            ArrayProperties::Dense(array) => {
                // In dense expansion case we resize array with new empty elements
                if new_length >= array.len() as u32 {
                    self.expand_dense_properties(new_length);
                    return true;
                }

                // In dense truncating case we must first find the last non-configurable property
                let mut last_non_configurable_index = None;
                for (index, value) in array.iter().enumerate().rev() {
                    if (index as u32) < new_length {
                        break;
                    }

                    if !value.is_configurable() {
                        last_non_configurable_index = Some(index);
                        break;
                    }
                }

                // And only truncate to one past the last non-configurable property, keeping it
                if let Some(last_index) = last_non_configurable_index {
                    array.truncate(last_index + 1);
                    false
                } else {
                    array.truncate(new_length as usize);
                    true
                }
            }
            ArrayProperties::Sparse { sparse_map, length } => {
                // Sparse expand case is easy, we simply set the new length
                if new_length >= *length {
                    *length = new_length;
                    return true;
                }

                // In sparse removal case we must first find the last non-configurable property
                let mut last_non_configurable_index = None;
                for (index, value) in sparse_map.iter() {
                    if (*index as u32) < new_length {
                        continue;
                    }

                    if !value.is_configurable() {
                        if *index <= last_non_configurable_index.unwrap_or(*index) {
                            last_non_configurable_index = Some(*index);
                        }
                    }
                }

                let new_length = if let Some(last_index) = last_non_configurable_index {
                    last_index + 1
                } else {
                    new_length
                };

                // Create a new map with non-truncated values
                let mut new_sparse_map = HashMap::new();
                for (index, value) in sparse_map.iter() {
                    if *index < new_length {
                        new_sparse_map.insert(*index, value.clone());
                    }
                }

                *sparse_map = new_sparse_map;
                *length = new_length;

                last_non_configurable_index.is_none()
            }
        }
    }

    // Intrinsic creation utilities
    pub fn intrinsic_data_prop(&mut self, key: &PropertyKey, value: Value) {
        self.set_property(key, Property::data(value, true, false, true))
    }

    pub fn instrinsic_length_prop(&mut self, cx: &mut Context, length: i32) {
        self.set_property(
            &cx.names.length(),
            Property::data(Value::smi(length), false, false, true),
        )
    }

    pub fn intrinsic_name_prop(&mut self, cx: &mut Context, name: &str) {
        self.set_property(
            &cx.names.name(),
            Property::data(
                Value::string(cx.heap.alloc_string(name.to_owned())),
                false,
                false,
                true,
            ),
        )
    }

    pub fn intrinsic_getter(
        &mut self,
        cx: &mut Context,
        name: &PropertyKey,
        func: BuiltinFunctionPtr,
        realm: Gc<Realm>,
    ) {
        let getter = BuiltinFunction::create(cx, func, 0, name, Some(realm), None, Some("get"));
        let accessor_value = cx
            .heap
            .alloc(AccessorValue { get: Some(getter.into()), set: None });
        self.set_property(name, Property::accessor(accessor_value.into(), false, true));
    }

    pub fn intrinsic_func(
        &mut self,
        cx: &mut Context,
        name: &PropertyKey,
        func: BuiltinFunctionPtr,
        length: i32,
        realm: Gc<Realm>,
    ) {
        self.intrinsic_data_prop(
            name,
            BuiltinFunction::create(cx, func, length, name, Some(realm), None, None).into(),
        );
    }
}

// Properties keyed by array index. Keep dense and backed by a true array if possible, otherwise
// switch to sparse array represented by map.
enum ArrayProperties {
    Dense(Vec<Property>),
    Sparse { sparse_map: HashMap<u32, Property>, length: u32 },
}

impl ArrayProperties {
    pub const fn new() -> ArrayProperties {
        Self::Dense(vec![])
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
    fn get_own_property(&self, key: &PropertyKey) -> EvalResult<Option<PropertyDescriptor>> {
        ordinary_get_own_property(self, key).into()
    }

    // 10.1.6 [[DefineOwnProperty]]
    fn define_own_property(
        &mut self,
        cx: &mut Context,
        key: &PropertyKey,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        ordinary_define_own_property(cx, self.into(), key, desc)
    }

    // 10.1.7 [[HasProperty]]
    fn has_property(&self, key: &PropertyKey) -> EvalResult<bool> {
        ordinary_has_property(self.into(), key)
    }

    // 10.1.8 [[Get]]
    fn get(&self, cx: &mut Context, key: &PropertyKey, receiver: Value) -> EvalResult<Value> {
        ordinary_get(cx, self.into(), key, receiver)
    }

    // 10.1.9 [[Set]]
    fn set(
        &mut self,
        cx: &mut Context,
        key: &PropertyKey,
        value: Value,
        receiver: Value,
    ) -> EvalResult<bool> {
        ordinary_set(cx, self.into(), key, value, receiver)
    }

    // 10.1.10 [[Delete]]
    fn delete(&mut self, key: &PropertyKey) -> EvalResult<bool> {
        ordinary_delete(self.into(), key)
    }

    // 10.1.11 [[OwnPropertyKeys]]
    fn own_property_keys(&self, cx: &mut Context) -> Vec<Value> {
        ordinary_own_property_keys(cx, self)
    }

    // 7.3.27 PrivateElementFind
    fn private_element_find(&mut self, private_id: PrivateNameId) -> Option<&mut PrivateProperty> {
        self.private_properties.get_mut(&private_id)
    }

    // 7.3.28 PrivateFieldAdd
    fn private_field_add(
        &mut self,
        cx: &mut Context,
        private_id: PrivateNameId,
        value: Value,
    ) -> EvalResult<()> {
        match self.private_element_find(private_id) {
            Some(_) => type_error_(cx, "private property already defined"),
            None => {
                let property = PrivateProperty::field(value);
                self.private_properties.insert(private_id, property);
                ().into()
            }
        }
    }

    // 7.3.29 PrivateMethodOrAccessorAdd
    fn private_method_or_accessor_add(
        &mut self,
        cx: &mut Context,
        private_id: PrivateNameId,
        private_method: PrivateProperty,
    ) -> EvalResult<()> {
        match self.private_element_find(private_id) {
            Some(_) => type_error_(cx, "private property already defined"),
            None => {
                self.private_properties.insert(private_id, private_method);
                ().into()
            }
        }
    }

    // Property accessors and mutators
    fn set_property(&mut self, key: &PropertyKey, value: Property) {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            match &mut self.array_properties {
                ArrayProperties::Dense(array) => {
                    if array_index as usize >= array.len() {
                        if array_index as usize >= array.len() + SPARSE_ARRAY_THRESHOLD {
                            self.fall_back_to_sparse_properties();
                        } else {
                            self.expand_dense_properties(array_index + 1);
                        }

                        self.set_property(key, value);
                    } else {
                        array[array_index as usize] = value;
                    }
                }
                ArrayProperties::Sparse { sparse_map, length } => {
                    sparse_map.insert(array_index, value);
                    if array_index >= *length {
                        *length = array_index + 1;
                    }
                }
            }

            return;
        }

        self.properties.insert(key.clone(), value);
    }

    fn get_property(&self, key: &PropertyKey) -> Option<&Property> {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            return match &self.array_properties {
                ArrayProperties::Dense(array) => {
                    if array_index as usize >= array.len() {
                        return None;
                    }

                    let value = &array[array_index as usize];
                    if value.value().is_empty() {
                        return None;
                    }

                    Some(value)
                }
                ArrayProperties::Sparse { sparse_map, .. } => sparse_map.get(&array_index),
            };
        }

        self.properties.get(key)
    }

    fn get_property_mut(&mut self, key: &PropertyKey) -> Option<&mut Property> {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            return match &mut self.array_properties {
                ArrayProperties::Dense(array) => {
                    if array_index as usize >= array.len() {
                        return None;
                    }

                    let value = &mut array[array_index as usize];
                    if value.value().is_empty() {
                        return None;
                    }

                    Some(value)
                }
                ArrayProperties::Sparse { sparse_map, .. } => sparse_map.get_mut(&array_index),
            };
        }

        self.properties.get_mut(key)
    }

    fn remove_property(&mut self, key: &PropertyKey) {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            match &mut self.array_properties {
                ArrayProperties::Dense(array) => {
                    array[array_index as usize] = Property::data(Value::empty(), true, true, true);
                }
                ArrayProperties::Sparse { sparse_map, .. } => {
                    sparse_map.remove(&array_index);
                }
            }

            return;
        }

        self.properties.remove(key);
    }
}

// 10.1.5.1 OrdinaryGetOwnProperty
pub fn ordinary_get_own_property(
    object: &OrdinaryObject,
    key: &PropertyKey,
) -> Option<PropertyDescriptor> {
    match object.get_property(key) {
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
    object: Gc<ObjectValue>,
    key: &PropertyKey,
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
    validate_and_apply_property_descriptor(
        cx,
        None,
        &cx.names.empty_string(),
        is_extensible,
        desc,
        current_desc,
    )
}

// 10.1.6.3 ValidateAndApplyPropertyDescriptor
pub fn validate_and_apply_property_descriptor(
    cx: &mut Context,
    mut object: Option<Gc<ObjectValue>>,
    key: &PropertyKey,
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
            let accessor_value = cx
                .heap
                .alloc(AccessorValue { get: desc.get, set: desc.set });

            Property::accessor(accessor_value.into(), is_enumerable, is_configurable)
        } else {
            let is_writable = desc.is_writable.unwrap_or(false);
            let value = desc.value.unwrap_or_else(|| Value::undefined());

            Property::data(value, is_writable, is_enumerable, is_configurable)
        };

        object.set_property(key, property);

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
                let property = object.get_property_mut(key).unwrap();
                if desc.is_data_descriptor() {
                    property.set_value(Value::undefined());
                    property.set_is_writable(false);
                } else {
                    let accessor_value = cx.heap.alloc(AccessorValue { get: None, set: None });
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
            let property = object.get_property_mut(key).unwrap();

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
pub fn ordinary_has_property(object: Gc<ObjectValue>, key: &PropertyKey) -> EvalResult<bool> {
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
    object: Gc<ObjectValue>,
    key: &PropertyKey,
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
        Some(PropertyDescriptor { get: Some(getter), .. }) => {
            call_object(cx, getter, receiver, &[])
        }
    }
}

// 10.1.9.1 OrdinarySet
// 10.1.9.2 OrdinarySetWithOwnDescriptor
pub fn ordinary_set(
    cx: &mut Context,
    object: Gc<ObjectValue>,
    key: &PropertyKey,
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
pub fn ordinary_delete(mut object: Gc<ObjectValue>, key: &PropertyKey) -> EvalResult<bool> {
    let desc = maybe!(object.get_own_property(key));
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
pub fn ordinary_own_property_keys(cx: &mut Context, object: &OrdinaryObject) -> Vec<Value> {
    let mut keys: Vec<Value> = vec![];

    // Return array index properties in numerical order
    match &object.array_properties {
        ArrayProperties::Dense(array) => {
            for (index, value) in array.iter().enumerate() {
                if !value.value().is_empty() {
                    let index_string = cx.heap.alloc_string(index.to_string());
                    keys.push(Value::string(index_string));
                }
            }
        }
        ArrayProperties::Sparse { sparse_map, .. } => {
            // Sparse map is unordered, so first extract and order keys
            let mut indexes_array = sparse_map.keys().map(|key| *key).collect::<Vec<_>>();
            indexes_array.sort();

            for index in indexes_array {
                let index_string = cx.heap.alloc_string(index.to_string());
                keys.push(Value::string(index_string));
            }
        }
    }

    // TODO: Return string and symbol keys in order of property creation

    for property_key in object.properties.keys() {
        if property_key.as_symbol().is_none() {
            keys.push(property_key.non_symbol_to_string(cx).into());
        }
    }

    for property_key in object.properties.keys() {
        if let Some(sym) = property_key.as_symbol() {
            keys.push(sym.into());
        }
    }

    keys
}

// 10.1.12 OrdinaryObjectCreate
pub fn ordinary_object_create(proto: Gc<ObjectValue>) -> OrdinaryObject {
    OrdinaryObject {
        _vtable: OrdinaryObject::VTABLE,
        prototype: Some(proto),
        properties: HashMap::new(),
        array_properties: ArrayProperties::new(),
        private_properties: HashMap::new(),
        is_extensible: true,
    }
}

pub fn ordinary_object_create_optional_proto(prototype: Option<Gc<ObjectValue>>) -> OrdinaryObject {
    OrdinaryObject {
        _vtable: OrdinaryObject::VTABLE,
        prototype,
        properties: HashMap::new(),
        array_properties: ArrayProperties::new(),
        private_properties: HashMap::new(),
        is_extensible: true,
    }
}

// 10.1.13 OrdinaryCreateFromConstructor
pub fn ordinary_create_from_constructor(
    cx: &mut Context,
    constructor: Gc<ObjectValue>,
    intrinsic_default_proto: Intrinsic,
) -> EvalResult<OrdinaryObject> {
    let proto = maybe!(get_prototype_from_constructor(cx, constructor, intrinsic_default_proto));

    ordinary_object_create(proto).into()
}

// 10.1.14 GetPrototypeFromConstructor
pub fn get_prototype_from_constructor(
    cx: &mut Context,
    constructor: Gc<ObjectValue>,
    intrinsic_default_proto: Intrinsic,
) -> EvalResult<Gc<ObjectValue>> {
    let proto = maybe!(get(cx, constructor.into(), &cx.names.prototype()));
    if proto.is_object() {
        proto.as_object().into()
    } else {
        let realm = maybe!(get_function_realm(constructor));
        realm.get_intrinsic(intrinsic_default_proto).into()
    }
}
