use std::{mem::transmute_copy, num::NonZeroU32};

use rand::Rng;

use crate::{
    impl_index_map_instance,
    runtime::{
        Context, HeapItemKind, Realm, SymbolValue,
        alloc_error::AllocResult,
        array_properties::ArrayProperties,
        collections::{
            BsIndexMapField, BsVecField, VecInstance, index_map::IndexMapInstance, vec::ValueVec,
        },
        error::type_error,
        eval_result::EvalResult,
        gc::{Handle, HeapInfo, HeapItem, HeapPtr, HeapVisitor, IsHeapItem, WithHeapItemKind},
        intrinsics::typed_array::DynTypedArray,
        property::{HeapProperty, Property, PropertyFlags},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        proxy_object::ProxyObject,
        shape::{DefinePropertyLocation, TransitionResult},
        transitions::PropertyLocation,
        type_utilities::is_callable_object,
        value::Value,
    },
    set_uninit,
};

// Macro that wraps a struct that optionally contains fields. Makes that struct inherit from object
// with the given additional fields.
//
// Safe to apply to root ObjectValue, since it avoids self-referencing conversions.
#[macro_export]
macro_rules! extend_object_without_conversions {
    ($(#[$struct_meta:meta])* $vis:vis struct $name:ident $(<$($generics:tt),*>)? {
        $($(#[$field_meta:meta])* $field_vis:vis $field_name:ident: $field_type:ty,)*
    }) => {
        #[repr(C)]
        $(#[$struct_meta])*
        $vis struct $name $(<$($generics),*>)? {
            // All objects start with object vtable
            shape: $crate::runtime::HeapPtr<$crate::runtime::shape::Shape>,

            // Inherited object fields

            // Storage of string and symbol properties, including private properties.
            named_properties: $crate::runtime::HeapPtr<$crate::runtime::AnyHeapItem>,

            // Array index properties by their property key
            array_properties: $crate::runtime::HeapPtr<$crate::runtime::array_properties::ArrayProperties>,

            // Stable hash code for this object, since object can be moved by GC. Lazily initialized.
            hash_code: Option<std::num::NonZeroU32>,

            // Child fields
            $($(#[$field_meta])* $field_vis $field_name: $field_type,)*
        }

        impl $(<$($generics),*>)? $name $(<$($generics),*>)? {
            #[allow(dead_code)]
            #[inline]
            pub fn shape_ptr(&self) -> $crate::runtime::HeapPtr<$crate::runtime::shape::Shape> {
                self.shape
            }

            #[allow(dead_code)]
            #[inline]
            pub fn shape(&self) -> $crate::runtime::Handle<$crate::runtime::shape::Shape> {
                self.shape.to_handle()
            }

            #[allow(dead_code)]
            #[inline]
            pub fn set_shape(&mut self, shape: $crate::runtime::HeapPtr<$crate::runtime::shape::Shape>)  {
                self.shape = shape
            }
        }

        impl $(<$($generics),*>)? $crate::runtime::HeapPtr<$name $(<$($generics),*>)?> {
            #[allow(dead_code)]
            #[inline]
            pub fn as_object(&self) -> $crate::runtime::HeapPtr<$crate::runtime::object_value::ObjectValue> {
                self.cast()
            }

            #[allow(dead_code)]
            #[inline]
            pub fn as_value(&self) -> $crate::runtime::Value {
                self.as_object().into()
            }
        }

        impl $(<$($generics),*>)? $crate::runtime::Handle<$name $(<$($generics),*>)?> {
            #[allow(dead_code)]
            #[inline]
            pub fn as_object(&self) -> $crate::runtime::Handle<$crate::runtime::object_value::ObjectValue> {
                self.cast()
            }

            #[allow(dead_code)]
            #[inline]
            pub fn as_value(&self) -> $crate::runtime::Handle<$crate::runtime::Value> {
                self.cast()
            }

            /// Cast to an ordinary object so that ordinary object's methods can be called. Only
            /// used when we know we want the default ordinary methods to be called.
            #[allow(dead_code)]
            #[inline]
            pub fn ordinary_object(&self) -> $crate::runtime::Handle<$crate::runtime::ordinary_object::OrdinaryObject> {
                self.cast()
            }
        }
    }
}

// Extend from object for all subclasses. Use this instead of extend_object_without_conversions for
// all objects except the root object.
#[macro_export]
macro_rules! extend_object {
    ($(#[$struct_meta:meta])* $vis:vis struct $name:ident $(<$($generics:tt),*>)? {
        $($(#[$field_meta:meta])* $field_vis:vis $field_name:ident: $field_type:ty,)*
    }) => {
        $crate::extend_object_without_conversions! {
            $(#[$struct_meta])*
            $vis struct $name $(<$($generics),*>)? {
                $($(#[$field_meta])* $field_vis $field_name: $field_type,)*
            }
        }

        impl $(<$($generics),*>)? From<$crate::runtime::HeapPtr<$name $(<$($generics),*>)?>> for $crate::runtime::HeapPtr<$crate::runtime::object_value::ObjectValue> {
            fn from(value: $crate::runtime::HeapPtr<$name $(<$($generics),*>)?>) -> Self {
                value.cast()
            }
        }

        impl $(<$($generics),*>)? From<$crate::runtime::Handle<$name $(<$($generics),*>)?>> for $crate::runtime::Handle<$crate::runtime::object_value::ObjectValue> {
            fn from(value: $crate::runtime::Handle<$name $(<$($generics),*>)?>) -> Self {
                value.cast::<$crate::runtime::object_value::ObjectValue>()
            }
        }

        impl $(<$($generics),*>)? $crate::runtime::HeapPtr<$name $(<$($generics),*>)?> {
            #[inline]
            pub fn visit_object_pointers(&mut self, visitor: &mut impl $crate::runtime::gc::HeapVisitor) {
                visitor.visit_pointer(&mut self.shape);
                visitor.visit_pointer(&mut self.named_properties);
                visitor.visit_pointer(&mut self.array_properties);
            }
        }
    }
}

// Generic object type encompassing ordinary objects and all forms of exotic objects.
extend_object_without_conversions! {
    pub struct ObjectValue {}
}

impl ObjectValue {
    // Array properties methods
    pub fn array_properties_length(&self) -> u32 {
        self.array_properties.array_length()
    }

    /// Fetch a named property value from the object given its property location.
    ///
    /// Assumes the object is in array mode and the property location is valid.
    fn lookup_location_unchecked(&self, location: PropertyLocation) -> Value {
        match location {
            PropertyLocation::PropertyArray { index } => *self
                .named_properties
                .cast::<ValueVec>()
                .get_unchecked(index as usize),
        }
    }

    fn set_location_unchecked(&mut self, location: PropertyLocation, value: Value) {
        match location {
            PropertyLocation::PropertyArray { index } => {
                self.named_properties
                    .cast::<ValueVec>()
                    .set_unchecked(index as usize, value);
            }
        }
    }

    fn get_named_property(&self, cx: Context, key: Handle<PropertyKey>) -> Option<Property> {
        match self.named_properties() {
            NamedProperties::Array => {
                if let Some(def) = self.shape.lookup_own_property(*key) {
                    let value = self.lookup_location_unchecked(def.location);
                    Some(Property::from_heap(cx, &HeapProperty::new(value, def.attributes)))
                } else {
                    None
                }
            }
            NamedProperties::Map(map) => map
                .get(&key)
                .map(|property| Property::from_heap(cx, property)),
        }
    }

    fn has_named_property(&self, key: Handle<PropertyKey>) -> bool {
        match self.named_properties() {
            NamedProperties::Array => self.shape.has_own_property(*key),
            NamedProperties::Map(map) => map.contains_key(&key),
        }
    }

    // Property accessors and mutators
    pub fn get_property(&self, cx: Context, key: Handle<PropertyKey>) -> Option<Property> {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            return self.array_properties.get_property(cx, array_index);
        }

        self.get_named_property(cx, key)
    }

    /// PrivateElementFind (https://tc39.es/ecma262/#sec-privateelementfind)
    pub fn private_element_find(
        &self,
        cx: Context,
        private_name: Handle<SymbolValue>,
    ) -> Option<Property> {
        let property_key = PropertyKey::symbol(private_name);
        self.get_named_property(cx, property_key)
    }

    pub fn has_private_element(&self, private_name: Handle<SymbolValue>) -> bool {
        let property_key = PropertyKey::symbol(private_name);
        self.has_named_property(property_key)
    }

    /// An iterator over the named properties and their keys of this object that is not GC safe.
    /// Caller must ensure that a GC cannot occur while the iterator is in use.
    #[inline]
    pub fn iter_named_properties_gc_unsafe<F: FnMut(PropertyKey, PropertyFlags)>(&self, mut f: F) {
        match self.named_properties() {
            NamedProperties::Array => {
                self.shape
                    .iter_own_properties_gc_unsafe(|property_key, flags| {
                        f(property_key, flags);
                    });
            }
            NamedProperties::Map(map) => {
                for (property_key, property) in map.iter_gc_unsafe() {
                    f(property_key, property.flags());
                }
            }
        }
    }
}

// Object field accessors
impl ObjectValue {
    #[inline]
    pub fn prototype(&self) -> Option<HeapPtr<ObjectValue>> {
        self.shape.prototype_ptr()
    }

    #[inline]
    fn named_properties(&self) -> NamedProperties {
        if self.named_properties.is::<ValueVec>() {
            NamedProperties::Array
        } else {
            NamedProperties::Map(self.named_properties.cast::<NamedPropertiesMap>())
        }
    }

    #[inline]
    pub fn set_named_properties_array(&mut self, properties: HeapPtr<ValueVec>) {
        self.named_properties = properties.as_any();
    }

    #[inline]
    pub fn set_named_properties_map(&mut self, properties: HeapPtr<NamedPropertiesMap>) {
        self.named_properties = properties.as_any();
    }

    #[inline]
    pub fn array_properties(&self) -> HeapPtr<ArrayProperties> {
        self.array_properties
    }

    #[inline]
    pub fn set_array_properties(&mut self, array_properties: HeapPtr<ArrayProperties>) {
        self.array_properties = array_properties;
    }

    #[inline]
    pub fn set_uninit_hash_code(&mut self) {
        set_uninit!(self.hash_code, None);
    }

    /// Return the Context for this heap item. Only use when absolutely necessary - prefer to
    /// instead explicitly pass in the Context.
    #[inline]
    fn cx(&self) -> Context {
        HeapInfo::from_raw_heap_ptr(self as *const _).cx()
    }
}

impl ObjectValue {
    #[inline]
    pub fn is_shared_array_buffer(&self) -> bool {
        false
    }

    #[inline]
    pub fn is_typed_array(&self) -> bool {
        let kind = self.shape_ptr().kind() as u8;
        (kind >= HeapItemKind::Int8ArrayObject as u8)
            && (kind <= HeapItemKind::Float64ArrayObject as u8)
    }

    #[inline]
    pub fn is_arguments_object(&self) -> bool {
        matches!(
            self.shape_ptr().kind(),
            HeapItemKind::MappedArgumentsObject | HeapItemKind::UnmappedArgumentsObject
        )
    }
}

impl HeapPtr<ObjectValue> {
    #[inline]
    pub fn hash_code(&mut self) -> NonZeroU32 {
        match self.hash_code {
            Some(hash_code) => hash_code,
            None => {
                // May be called from std::hash::Hash so cannot pass in Context
                let hash_code = self.cx().rand.r#gen::<NonZeroU32>();
                self.hash_code = Some(hash_code);
                hash_code
            }
        }
    }
}

impl Handle<ObjectValue> {
    /// Cast as a virtual object, allowing virtual methods to be called. Manually constructs a
    /// trait object using the vtable stored in the object shape.
    #[inline]
    fn as_trait_object(&self) -> ObjectTraitObject {
        let data = self as *const Handle<ObjectValue>;
        let vtable = self.shape_ptr().vtable();

        ObjectTraitObject { data, vtable }
    }

    #[inline]
    fn virtual_object(&self) -> &dyn VirtualObject {
        let trait_object = self.as_trait_object();
        unsafe { transmute_copy::<ObjectTraitObject, &dyn VirtualObject>(&trait_object) }
    }

    #[inline]
    fn virtual_object_mut(&mut self) -> &mut dyn VirtualObject {
        let trait_object = self.as_trait_object();
        unsafe { transmute_copy::<ObjectTraitObject, &mut dyn VirtualObject>(&trait_object) }
    }

    fn set_named_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        property: Property,
    ) -> AllocResult<()> {
        match self.named_properties() {
            NamedProperties::Array => {
                // Add property to the shape which may transition to a new shape
                let (new_shape, location) =
                    self.shape()
                        .define_own_property(cx, key, property.flags())?;

                match location {
                    DefinePropertyLocation::Location(location) => {
                        self.set_shape(new_shape);
                        self.set_location_unchecked(location, *property.value());
                    }
                    DefinePropertyLocation::NewArrayProperty => {
                        self.set_shape(new_shape);
                        NamedPropertiesVecField(*self)
                            .maybe_grow_for_push(cx)?
                            .push_without_growing(*property.value());
                    }
                    DefinePropertyLocation::EnterMapMode => {
                        self.enter_map_mode(cx)?;
                        self.set_named_property_map_mode(cx, key, property)?;
                    }
                }
            }
            NamedProperties::Map(_) => {
                self.set_named_property_map_mode(cx, key, property)?;
            }
        }

        Ok(())
    }

    fn set_named_property_map_mode(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        property: Property,
    ) -> AllocResult<()> {
        NamedPropertiesMapField(*self)
            .maybe_grow_for_insertion(cx)?
            .insert_without_growing(*key, property.to_heap());

        // Always invalidate. Technically we could check if the property already existed and
        // only invalidate if it was a new property or had different attributes, but lets
        // optimize to avoid checks in the fast path and instead pessimistically invalidate
        // all prototype object mutations.
        self.shape.invalidate_if_prototype_object();

        Ok(())
    }

    fn remove_named_property(&mut self, cx: Context, key: Handle<PropertyKey>) -> AllocResult<()> {
        // Array mode does not support removing properties so convert to map mode first
        let mut map = match self.named_properties() {
            NamedProperties::Array => *self.enter_map_mode(cx)?,
            NamedProperties::Map(map) => map,
        };

        map.remove(&key);

        self.shape.invalidate_if_prototype_object();

        Ok(())
    }

    // Property accessors and mutators
    pub fn set_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        property: Property,
    ) -> AllocResult<()> {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            ArrayProperties::set_property(cx, *self, array_index, property)?;

            return Ok(());
        }

        self.set_named_property(cx, key, property)
    }

    pub fn remove_property(&mut self, cx: Context, key: Handle<PropertyKey>) -> AllocResult<()> {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            self.array_properties.remove_property(array_index);

            return Ok(());
        }

        self.remove_named_property(cx, key)
    }

    pub fn private_element_set(
        &mut self,
        cx: Context,
        private_name: Handle<SymbolValue>,
        value: Handle<Value>,
    ) -> AllocResult<()> {
        let property_key = PropertyKey::symbol(private_name);
        let property = Property::private_field(value);

        self.set_named_property(cx, property_key, property)
    }

    /// PrivateFieldAdd (https://tc39.es/ecma262/#sec-privatefieldadd)
    /// PrivateMethodOrAccessorAdd (https://tc39.es/ecma262/#sec-privatemethodoraccessoradd)
    pub fn private_property_add(
        &mut self,
        cx: Context,
        private_name: Handle<SymbolValue>,
        private_property: Property,
    ) -> EvalResult<()> {
        if self.has_private_element(private_name) {
            type_error(cx, "private property already defined")
        } else {
            let property_key = PropertyKey::symbol(private_name);
            self.set_named_property(cx, property_key, private_property)?;

            Ok(())
        }
    }

    pub fn set_array_properties_length(
        &mut self,
        cx: Context,
        new_length: u32,
    ) -> AllocResult<bool> {
        ArrayProperties::set_len(cx, *self, new_length)
    }

    /// Convert this object into map mode, which replaces it with a hash map from property keys to
    /// property values.
    ///
    /// Return the new named properties map.
    pub fn enter_map_mode(&mut self, cx: Context) -> AllocResult<Handle<NamedPropertiesMap>> {
        debug_assert!(!self.shape.is_map_mode());

        // Replace properties array with a map built from the shape's property definitions
        let num_properties = self.shape.num_properties();
        let map_capacity = NamedPropertiesMap::min_capacity_needed(num_properties as usize);
        let mut map = NamedPropertiesMap::new(cx, map_capacity)?.to_handle();

        for property_definition in self.shape.property_definitions_slice() {
            let value = self.lookup_location_unchecked(property_definition.location);
            let property = HeapProperty::new(value, property_definition.attributes);
            map.insert_without_growing(property_definition.key, property);
        }

        self.set_named_properties_map(*map);

        // Convert shape to map mode, possibly allocating a new one
        let mut old_shape = self.shape();
        let new_shape = old_shape.convert_to_map_mode(cx)?;
        self.set_shape(new_shape);

        Ok(map)
    }
}

// Non-virtual object internal methods from spec
impl Handle<ObjectValue> {
    /// The [[GetPrototypeOf]] internal method for all objects. Dispatches to type-specific
    /// implementations as necessary.
    pub fn get_prototype_of(&self, cx: Context) -> EvalResult<Option<Handle<ObjectValue>>> {
        if let Some(proxy_object) = self.as_opt::<ProxyObject>() {
            proxy_object.get_prototype_of(cx)
        } else {
            Ok(self.prototype().map(|p| p.to_handle()))
        }
    }

    /// The [[SetPrototypeOf]] internal method for all objects. Dispatches to type-specific
    /// implementations as necessary.
    pub fn set_prototype_of(
        &mut self,
        cx: Context,
        new_prototype: Option<Handle<ObjectValue>>,
    ) -> EvalResult<bool> {
        if let Some(mut proxy_object) = self.as_opt::<ProxyObject>() {
            proxy_object.set_prototype_of(cx, new_prototype)
        } else {
            self.ordinary_set_prototype_of(cx, new_prototype)
        }
    }

    /// The [[IsExtensible]] internal method for all objects. Dispatches to type-specific
    /// implementations as necessary.
    pub fn is_extensible(&self, cx: Context) -> EvalResult<bool> {
        if let Some(proxy_object) = self.as_opt::<ProxyObject>() {
            proxy_object.is_extensible(cx)
        } else {
            Ok(self.shape.is_extensible())
        }
    }

    /// The [[PreventExtensions]] internal method for all objects. Dispatches to type-specific
    /// implementations as necessary.
    pub fn prevent_extensions(&mut self, cx: Context) -> EvalResult<bool> {
        if let Some(mut proxy_object) = self.as_opt::<ProxyObject>() {
            return proxy_object.prevent_extensions(cx);
        }

        if self.is_typed_array() {
            if !is_typed_array_fixed_length(self.as_typed_array()) {
                return Ok(false);
            }
        }

        // OrdinaryPreventExtensions (https://tc39.es/ecma262/#sec-ordinarypreventextensions)
        match self.shape().prevent_extensions(cx)? {
            TransitionResult::Transitioned(new_shape) => {
                self.set_shape(new_shape);
            }
            TransitionResult::EnterMapMode => {
                self.enter_map_mode(cx)?;
                self.shape().prevent_extensions(cx)?;
            }
        }

        Ok(true)
    }
}

/// IsTypedArrayFixedLength (https://tc39.es/ecma262/#sec-istypedarrayfixedlength)
fn is_typed_array_fixed_length(typed_array: DynTypedArray) -> bool {
    if typed_array.array_length().is_none() {
        return false;
    }

    typed_array.viewed_array_buffer().is_fixed_length()
}

// Wrap all virtual methods for easy access
impl Handle<ObjectValue> {
    #[inline]
    pub fn get_own_property(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        self.virtual_object().get_own_property(cx, key)
    }

    #[inline]
    pub fn define_own_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        self.virtual_object_mut().define_own_property(cx, key, desc)
    }

    #[inline]
    pub fn has_property(&self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
        self.virtual_object().has_property(cx, key)
    }

    #[inline]
    pub fn get(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
        receiver: Handle<Value>,
    ) -> EvalResult<Handle<Value>> {
        self.virtual_object().get(cx, key, receiver)
    }

    #[inline]
    pub fn set(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        value: Handle<Value>,
        receiver: Handle<Value>,
    ) -> EvalResult<bool> {
        self.virtual_object_mut().set(cx, key, value, receiver)
    }

    #[inline]
    pub fn delete(&mut self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
        self.virtual_object_mut().delete(cx, key)
    }

    #[inline]
    pub fn own_property_keys(&self, cx: Context) -> EvalResult<Vec<Handle<Value>>> {
        self.virtual_object().own_property_keys(cx)
    }

    // Type utilities
    #[inline]
    pub fn is_callable(&self) -> bool {
        is_callable_object(*self)
    }

    #[inline]
    pub fn get_realm(&self, cx: Context) -> EvalResult<HeapPtr<Realm>> {
        self.virtual_object().get_realm(cx)
    }

    #[inline]
    pub fn as_typed_array(&self) -> DynTypedArray {
        self.virtual_object().as_typed_array()
    }
}

impl Handle<ObjectValue> {
    /// Whether this is a heap item of a particular type.
    #[inline]
    pub fn is<T: WithHeapItemKind>(&self) -> bool {
        self.shape_ptr().kind() == T::KIND
    }

    /// Return this value as a heap item of a particular type, or None if it is not of that type.
    #[inline]
    pub fn as_opt<T: WithHeapItemKind>(&self) -> Option<Handle<T>> {
        if self.is::<T>() {
            Some(self.cast())
        } else {
            None
        }
    }
}

pub type VirtualObjectVtable = *const ();

/// Virtual methods for an object. Creates vtable which is stored in object shape.
pub trait VirtualObject {
    fn get_own_property(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
    ) -> EvalResult<Option<PropertyDescriptor>>;

    fn define_own_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool>;

    fn has_property(&self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool>;

    fn get(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
        receiver: Handle<Value>,
    ) -> EvalResult<Handle<Value>>;

    fn set(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        value: Handle<Value>,
        receiver: Handle<Value>,
    ) -> EvalResult<bool>;

    fn delete(&mut self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool>;

    fn own_property_keys(&self, cx: Context) -> EvalResult<Vec<Handle<Value>>>;

    fn call(
        &self,
        _: Context,
        _this_argument: Handle<Value>,
        _arguments: &[Handle<Value>],
    ) -> EvalResult<Handle<Value>> {
        panic!("[[Call]] not implemented for this object")
    }

    fn construct(
        &self,
        _: Context,
        _arguments: &[Handle<Value>],
        _new_target: Handle<ObjectValue>,
    ) -> EvalResult<Handle<ObjectValue>> {
        panic!("[[Construct]] not implemented for this object")
    }

    fn get_realm(&self, cx: Context) -> EvalResult<HeapPtr<Realm>> {
        Ok(cx.current_realm_ptr())
    }

    fn as_typed_array(&self) -> DynTypedArray {
        unreachable!("as_typed_array can only be called on typed arrays")
    }
}

enum NamedProperties {
    Array,
    Map(HeapPtr<NamedPropertiesMap>),
}

impl_index_map_instance!(NamedPropertiesMap, PropertyKey, HeapProperty);

impl HeapItem for NamedPropertiesMap {
    fn byte_size(map: HeapPtr<NamedPropertiesMap>) -> usize {
        NamedPropertiesMap::calculate_size_in_bytes(map.capacity())
    }

    fn visit_pointers(map: HeapPtr<NamedPropertiesMap>, visitor: &mut impl HeapVisitor) {
        NamedPropertiesMap::visit_pointers_impl(map, visitor, |mut map, visitor| {
            for (property_key, property) in map.iter_mut_gc_unsafe() {
                visitor.visit_property_key(property_key);
                property.visit_pointers(visitor);
            }
        });
    }
}

pub struct NamedPropertiesMapField(Handle<ObjectValue>);

impl BsIndexMapField<NamedPropertiesMap> for NamedPropertiesMapField {
    fn get(&self) -> HeapPtr<NamedPropertiesMap> {
        self.0.named_properties.cast::<NamedPropertiesMap>()
    }

    fn set_new(
        &mut self,
        cx: Context,
        capacity: usize,
    ) -> AllocResult<HeapPtr<NamedPropertiesMap>> {
        let map = NamedPropertiesMap::new(cx, capacity)?;
        self.0.set_named_properties_map(map);
        Ok(map)
    }
}

struct NamedPropertiesVecField(Handle<ObjectValue>);

impl BsVecField<ValueVec> for NamedPropertiesVecField {
    fn get(&self) -> HeapPtr<ValueVec> {
        self.0.named_properties.cast::<ValueVec>()
    }

    fn set_new(&mut self, cx: Context, capacity: usize) -> AllocResult<HeapPtr<ValueVec>> {
        let new_vec = ValueVec::new(cx, capacity)?;
        self.0.set_named_properties_array(new_vec);
        Ok(new_vec)
    }
}

// Same layout as in std::raw, which is not exposed in stable. This definition is only used
// to properly type our custom trait object creation.
#[repr(C)]
struct ObjectTraitObject {
    data: *const Handle<ObjectValue>,
    vtable: *const (),
}

// Only necessary so we get deref for HeapPtrs.
impl IsHeapItem for ObjectValue {}
