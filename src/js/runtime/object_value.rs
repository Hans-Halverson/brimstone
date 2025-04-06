use rand::Rng;

use std::{
    mem::{size_of, transmute, transmute_copy},
    num::NonZeroU32,
};

use crate::{handle_scope_guard, set_uninit};

use super::{
    accessor::Accessor,
    array_object::ArrayObject,
    array_properties::ArrayProperties,
    async_generator_object::AsyncGeneratorObject,
    builtin_function::BuiltinFunction,
    bytecode::function::Closure,
    collections::{BsIndexMap, BsIndexMapField},
    error::type_error,
    eval_result::EvalResult,
    gc::{Handle, HeapObject, HeapPtr, HeapVisitor},
    generator_object::GeneratorObject,
    intrinsics::{
        array_buffer_constructor::ArrayBufferObject, bigint_constructor::BigIntObject,
        boolean_constructor::BooleanObject, data_view_constructor::DataViewObject,
        date_object::DateObject, error_constructor::ErrorObject,
        finalization_registry_object::FinalizationRegistryObject,
        iterator_constructor::WrappedValidIterator, iterator_helper_object::IteratorHelperObject,
        map_object::MapObject, number_constructor::NumberObject, object_prototype::ObjectPrototype,
        regexp_constructor::RegExpObject, rust_runtime::RustRuntimeFunction, set_object::SetObject,
        symbol_constructor::SymbolObject, typed_array::DynTypedArray,
        weak_map_object::WeakMapObject, weak_ref_constructor::WeakRefObject,
        weak_set_object::WeakSetObject,
    },
    object_descriptor::ObjectKind,
    promise_object::PromiseObject,
    property::{HeapProperty, Property},
    property_descriptor::PropertyDescriptor,
    property_key::PropertyKey,
    proxy_object::ProxyObject,
    string_object::StringObject,
    type_utilities::is_callable_object,
    value::{SymbolValue, Value},
    Context, Realm,
};

// Macro that wraps a struct that optionally contains fields. Makes that struct inherit from object
// with the given additional fields.
//
// Safe to apply to root ObjectValue, since it avoids self-referencing conversions.
#[macro_export]
macro_rules! extend_object_without_conversions {
    ($vis:vis struct $name:ident $(<$($generics:tt),*>)? {
        $($field_vis:vis $field_name:ident: $field_type:ty,)*
    }) => {
        #[repr(C)]
        $vis struct $name $(<$($generics),*>)? {
            // All objects start with object vtable
            descriptor: $crate::runtime::HeapPtr<$crate::runtime::object_descriptor::ObjectDescriptor>,

            // Inherited object fields

            // None represents the null value
            prototype: Option<$crate::runtime::HeapPtr<$crate::runtime::object_value::ObjectValue>>,

            // String and symbol properties by their property key. Includes private properties.
            named_properties: $crate::runtime::HeapPtr<$crate::runtime::collections::BsIndexMap<$crate::runtime::property_key::PropertyKey, $crate::runtime::property::HeapProperty>>,

            // Array index properties by their property key
            array_properties: $crate::runtime::HeapPtr<$crate::runtime::array_properties::ArrayProperties>,

            // Whether this object can be extended with new properties
            is_extensible_field: bool,

            // Stable hash code for this object, since object can be moved by GC. Lazily initialized.
            hash_code: Option<std::num::NonZeroU32>,

            // Child fields
            $($field_vis $field_name: $field_type,)*
        }

        impl $(<$($generics),*>)? $name $(<$($generics),*>)? {
            #[allow(dead_code)]
            #[inline]
            pub fn descriptor(&self) -> $crate::runtime::HeapPtr<$crate::runtime::object_descriptor::ObjectDescriptor> {
                self.descriptor
            }

            #[allow(dead_code)]
            #[inline]
            pub fn set_descriptor(&mut self, descriptor: $crate::runtime::HeapPtr<$crate::runtime::object_descriptor::ObjectDescriptor>)  {
                self.descriptor = descriptor
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
    ($vis:vis struct $name:ident $(<$($generics:tt),*>)? {
        $($field_vis:vis $field_name:ident: $field_type:ty,)*
    }) => {
        $crate::extend_object_without_conversions! {
            $vis struct $name $(<$($generics),*>)? {
                $($field_vis $field_name: $field_type,)*
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
            pub fn visit_object_pointers(&self, visitor: &mut impl $crate::runtime::gc::HeapVisitor) {
                use $crate::runtime::gc::HeapObject;
                self.as_object().visit_pointers(visitor);
            }
        }
    }
}

// Generic object type encompassing ordinary objects and all forms of exotic objects.
extend_object_without_conversions! {
    pub struct ObjectValue {}
}

impl ObjectValue {
    pub fn new(
        cx: Context,
        prototype: Option<Handle<ObjectValue>>,
        is_extensible: bool,
    ) -> Handle<ObjectValue> {
        let mut object = cx.alloc_uninit::<ObjectValue>();

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::OrdinaryObject));
        set_uninit!(object.prototype, prototype.map(|p| *p));
        set_uninit!(object.named_properties, cx.default_named_properties);
        set_uninit!(object.array_properties, cx.default_array_properties);
        set_uninit!(object.is_extensible_field, is_extensible);
        object.set_uninit_hash_code();

        object.to_handle()
    }

    // Array properties methods
    pub fn array_properties_length(&self) -> u32 {
        self.array_properties.array_length()
    }

    /// PrivateElementFind (https://tc39.es/ecma262/#sec-privateelementfind)
    pub fn private_element_find(
        &self,
        cx: Context,
        private_name: Handle<SymbolValue>,
    ) -> Option<Property> {
        let property_key = PropertyKey::symbol(private_name);
        // Safe since get_mut does not allocate on managed heap, and property reference is
        // immediately cloned.
        self.named_properties
            .get(&property_key)
            .map(|property| Property::from_heap(cx, property))
    }

    pub fn has_private_element(&self, private_name: Handle<SymbolValue>) -> bool {
        let property_key = PropertyKey::symbol(private_name);
        // Safe since contains_key does not allocate on managed heap
        self.named_properties.contains_key(&property_key)
    }

    // Property accessors and mutators
    pub fn get_property(&self, cx: Context, key: Handle<PropertyKey>) -> Option<Property> {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            return self.array_properties.get_property(cx, array_index);
        }

        // Safe since get does not allocate on managed heap
        self.named_properties
            .get(&key)
            .map(|property| Property::from_heap(cx, property))
    }

    /// An iterator over the named property keys of this object that is not GC safe. Caller must
    /// ensure that a GC cannot occur while the iterator is in use.
    #[inline]
    pub fn iter_named_property_keys_gc_unsafe<F: FnMut(PropertyKey)>(&self, mut f: F) {
        for property_key in self.named_properties.keys_gc_unsafe() {
            f(property_key);
        }
    }

    /// An iterator over the named properties and their keys of this object that is not GC safe.
    /// Caller must ensure that a GC cannot occur while the iterator is in use.
    #[inline]
    pub fn iter_named_properties_gc_unsafe<F: FnMut(PropertyKey, HeapProperty)>(&self, mut f: F) {
        for (property_key, property) in self.named_properties.iter_gc_unsafe() {
            f(property_key, property);
        }
    }
}

// Object field accessors
impl ObjectValue {
    #[inline]
    pub fn prototype(&self) -> Option<HeapPtr<ObjectValue>> {
        self.prototype
    }

    #[inline]
    pub fn set_prototype(&mut self, prototype: Option<HeapPtr<ObjectValue>>) {
        self.prototype = prototype
    }

    #[inline]
    pub fn set_named_properties(&mut self, properties: HeapPtr<NamedPropertiesMap>) {
        self.named_properties = properties;
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
    pub fn is_extensible_field(&self) -> bool {
        self.is_extensible_field
    }

    #[inline]
    pub fn set_is_extensible_field(&mut self, is_extensible_field: bool) {
        self.is_extensible_field = is_extensible_field;
    }

    #[inline]
    pub fn set_uninit_hash_code(&mut self) {
        set_uninit!(self.hash_code, None);
    }

    #[inline]
    pub fn hash_code(&mut self) -> NonZeroU32 {
        match self.hash_code {
            Some(hash_code) => hash_code,
            None => {
                let hash_code = rand::thread_rng().gen::<NonZeroU32>();
                self.hash_code = Some(hash_code);
                hash_code
            }
        }
    }
}

impl ObjectValue {
    #[inline]
    pub fn is_shared_array_buffer(&self) -> bool {
        false
    }

    #[inline]
    pub fn is_typed_array(&self) -> bool {
        let kind = self.descriptor().kind() as u8;
        (kind >= ObjectKind::Int8Array as u8) && (kind <= ObjectKind::Float64Array as u8)
    }

    #[inline]
    pub fn is_arguments_object(&self) -> bool {
        matches!(
            self.descriptor().kind(),
            ObjectKind::MappedArgumentsObject | ObjectKind::UnmappedArgumentsObject
        )
    }
}

impl Handle<ObjectValue> {
    /// Cast as a virtual object, allowing virtual methods to be called. Manually constructs a
    /// trait object using the vtable stored in the object descriptor.
    #[inline]
    fn as_trait_object(&self) -> ObjectTraitObject {
        let data = self as *const Handle<ObjectValue>;
        let vtable = self.descriptor().vtable();

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

    fn named_properties_field(&self) -> NamedPropertiesMapField {
        NamedPropertiesMapField(*self)
    }

    // Property accessors and mutators
    pub fn set_property(&mut self, cx: Context, key: Handle<PropertyKey>, property: Property) {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            ArrayProperties::set_property(cx, *self, array_index, property);

            return;
        }

        // Safe since insert does allocate on managed heap
        self.named_properties_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(*key, property.to_heap());
    }

    pub fn remove_property(&mut self, key: Handle<PropertyKey>) {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            self.array_properties.remove_property(array_index);

            return;
        }

        // Removal is O(1) but leaves permanent tombstone
        self.named_properties.remove(&key);
    }

    pub fn private_element_set(
        &mut self,
        cx: Context,
        private_name: Handle<SymbolValue>,
        value: Handle<Value>,
    ) {
        let property_key = PropertyKey::symbol(private_name);
        let property = Property::private_field(value);
        // Safe since insert does not allocate on managed heap
        self.named_properties_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(*property_key, property.to_heap());
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
            // Safe since insert does not allocate on managed heap
            let property_key = PropertyKey::symbol(private_name);
            self.named_properties_field()
                .maybe_grow_for_insertion(cx)
                .insert_without_growing(*property_key, private_property.to_heap());

            Ok(())
        }
    }

    pub fn set_array_properties_length(&mut self, cx: Context, new_length: u32) -> bool {
        ArrayProperties::set_len(cx, *self, new_length)
    }

    // Intrinsic creation utilities
    pub fn intrinsic_data_prop(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        value: Handle<Value>,
    ) {
        handle_scope_guard!(cx);

        self.set_property(cx, key, Property::data(value, true, false, true))
    }

    pub fn instrinsic_length_prop(&mut self, cx: Context, length: i32) {
        handle_scope_guard!(cx);

        let length_value = cx.smi(length);
        self.set_property(cx, cx.names.length(), Property::data(length_value, false, false, true))
    }

    pub fn intrinsic_name_prop(&mut self, mut cx: Context, name: &str) {
        handle_scope_guard!(cx);

        let name_value = cx.alloc_string(name).into();
        self.set_property(cx, cx.names.name(), Property::data(name_value, false, false, true))
    }

    pub fn intrinsic_getter(
        &mut self,
        cx: Context,
        name: Handle<PropertyKey>,
        func: RustRuntimeFunction,
        realm: Handle<Realm>,
    ) {
        handle_scope_guard!(cx);

        let getter = BuiltinFunction::create(cx, func, 0, name, realm, None, Some("get"));
        let accessor_value = Accessor::new(cx, Some(getter), None);
        self.set_property(cx, name, Property::accessor(accessor_value.into(), false, true));
    }

    pub fn intrinsic_getter_and_setter(
        &mut self,
        cx: Context,
        name: Handle<PropertyKey>,
        getter: RustRuntimeFunction,
        setter: RustRuntimeFunction,
        realm: Handle<Realm>,
    ) {
        handle_scope_guard!(cx);

        let getter = BuiltinFunction::create(cx, getter, 0, name, realm, None, Some("get"));
        let setter = BuiltinFunction::create(cx, setter, 1, name, realm, None, Some("set"));
        let accessor_value = Accessor::new(cx, Some(getter), Some(setter));
        self.set_property(cx, name, Property::accessor(accessor_value.into(), false, true));
    }

    pub fn intrinsic_func(
        &mut self,
        cx: Context,
        name: Handle<PropertyKey>,
        func: RustRuntimeFunction,
        length: u32,
        realm: Handle<Realm>,
    ) {
        handle_scope_guard!(cx);

        let func = BuiltinFunction::create(cx, func, length, name, realm, None, None).into();
        self.intrinsic_data_prop(cx, name, func);
    }

    pub fn intrinsic_frozen_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        value: Handle<Value>,
    ) {
        handle_scope_guard!(cx);

        self.set_property(cx, key, Property::data(value, false, false, false));
    }
}

// Non-virtual object internal methods from spec
impl Handle<ObjectValue> {
    /// The [[GetPrototypeOf]] internal method for all objects. Dispatches to type-specific
    /// implementations as necessary.
    pub fn get_prototype_of(&self, cx: Context) -> EvalResult<Option<Handle<ObjectValue>>> {
        if let Some(proxy_object) = self.as_proxy() {
            proxy_object.get_prototype_of(cx)
        } else {
            self.ordinary_get_prototype_of()
        }
    }

    /// The [[SetPrototypeOf]] internal method for all objects. Dispatches to type-specific
    /// implementations as necessary.
    pub fn set_prototype_of(
        &mut self,
        cx: Context,
        new_prototype: Option<Handle<ObjectValue>>,
    ) -> EvalResult<bool> {
        if let Some(mut proxy_object) = self.as_proxy() {
            proxy_object.set_prototype_of(cx, new_prototype)
        } else {
            self.ordinary_set_prototype_of(cx, new_prototype)
        }
    }

    /// The [[IsExtensible]] internal method for all objects. Dispatches to type-specific
    /// implementations as necessary.
    pub fn is_extensible(&self, cx: Context) -> EvalResult<bool> {
        if let Some(proxy_object) = self.as_proxy() {
            proxy_object.is_extensible(cx)
        } else {
            self.ordinary_is_extensible()
        }
    }

    /// The [[PreventExtensions]] internal method for all objects. Dispatches to type-specific
    /// implementations as necessary.
    pub fn prevent_extensions(&mut self, cx: Context) -> EvalResult<bool> {
        if let Some(mut proxy_object) = self.as_proxy() {
            return proxy_object.prevent_extensions(cx);
        }

        if self.is_typed_array() {
            if !is_typed_array_fixed_length(self.as_typed_array()) {
                return Ok(false);
            }
        }

        self.ordinary_prevent_extensions()
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

pub type VirtualObjectVtable = *const ();

/// Virtual methods for an object. Creates vtable which is stored in object descriptor.
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

pub type NamedPropertiesMap = BsIndexMap<PropertyKey, HeapProperty>;

pub struct NamedPropertiesMapField(Handle<ObjectValue>);

impl BsIndexMapField<PropertyKey, HeapProperty> for NamedPropertiesMapField {
    fn new_map(&self, cx: Context, capacity: usize) -> HeapPtr<NamedPropertiesMap> {
        NamedPropertiesMap::new(cx, ObjectKind::ObjectNamedPropertiesMap, capacity)
    }

    fn get(&self) -> HeapPtr<NamedPropertiesMap> {
        self.0.named_properties
    }

    fn set(&mut self, map: HeapPtr<NamedPropertiesMap>) {
        self.0.set_named_properties(map);
    }
}

// Same layout as in std::raw, which is not exposed in stable. This definition is only used
// to properly type our custom trait object creation.
#[repr(C)]
struct ObjectTraitObject {
    data: *const Handle<ObjectValue>,
    vtable: *const (),
}

/// Compile time shenanigans to extract the trait object vtable for a particular type that
/// implements Object so that we can construct our own trait objects manually.
pub const fn extract_object_vtable<T: VirtualObject>() -> *const () {
    unsafe {
        let example_ptr: *const T = std::ptr::null();
        let example_trait_object: *const dyn VirtualObject = example_ptr;
        let object_trait_object =
            transmute::<*const dyn VirtualObject, ObjectTraitObject>(example_trait_object);

        object_trait_object.vtable
    }
}

impl HeapObject for HeapPtr<ObjectValue> {
    fn byte_size(&self) -> usize {
        size_of::<ObjectValue>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer_opt(&mut self.prototype);
        visitor.visit_pointer(&mut self.named_properties);
        visitor.visit_pointer(&mut self.array_properties);
    }
}

impl NamedPropertiesMapField {
    pub fn byte_size(map: &HeapPtr<NamedPropertiesMap>) -> usize {
        NamedPropertiesMap::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(map: &mut HeapPtr<NamedPropertiesMap>, visitor: &mut impl HeapVisitor) {
        map.visit_pointers_impl(visitor, |map, visitor| {
            for (property_key, property) in map.iter_mut_gc_unsafe() {
                visitor.visit_property_key(property_key);
                property.visit_pointers(visitor);
            }
        });
    }
}

/// Implement subtype checks and downcasts for an object subtype.
///
/// - `is_subtype` returns true if the object is of the given subtype.
/// - `as_subtype` returns the object as the given subtype if it is of that subtype, otherwise None.
macro_rules! impl_subtype_casts {
    ($subtype:ident, $desc:expr, $is_func:ident, $as_func:ident) => {
        impl ObjectValue {
            #[inline]
            pub fn $is_func(&self) -> bool {
                self.descriptor().kind() == $desc
            }
        }

        impl HeapPtr<ObjectValue> {
            #[inline]
            pub fn $as_func(&self) -> Option<HeapPtr<$subtype>> {
                if self.$is_func() {
                    Some(self.cast())
                } else {
                    None
                }
            }
        }

        impl Handle<ObjectValue> {
            #[inline]
            pub fn $as_func(&self) -> Option<Handle<$subtype>> {
                if self.$is_func() {
                    Some(self.cast())
                } else {
                    None
                }
            }
        }
    };
}

impl_subtype_casts!(ArrayObject, ObjectKind::ArrayObject, is_array, as_array);
impl_subtype_casts!(ErrorObject, ObjectKind::ErrorObject, is_error, as_error);
impl_subtype_casts!(BooleanObject, ObjectKind::BooleanObject, is_boolean_object, as_boolean_object);
impl_subtype_casts!(NumberObject, ObjectKind::NumberObject, is_number_object, as_number_object);
impl_subtype_casts!(StringObject, ObjectKind::StringObject, is_string_object, as_string_object);
impl_subtype_casts!(SymbolObject, ObjectKind::SymbolObject, is_symbol_object, as_symbol_object);
impl_subtype_casts!(BigIntObject, ObjectKind::BigIntObject, is_bigint_object, as_bigint_object);
impl_subtype_casts!(DateObject, ObjectKind::DateObject, is_date_object, as_date_object);
impl_subtype_casts!(RegExpObject, ObjectKind::RegExpObject, is_regexp_object, as_regexp_object);
impl_subtype_casts!(Closure, ObjectKind::Closure, is_closure, as_closure);
impl_subtype_casts!(MapObject, ObjectKind::MapObject, is_map_object, as_map_object);
impl_subtype_casts!(SetObject, ObjectKind::SetObject, is_set_object, as_set_object);
impl_subtype_casts!(
    ArrayBufferObject,
    ObjectKind::ArrayBufferObject,
    is_array_buffer,
    as_array_buffer
);
impl_subtype_casts!(DataViewObject, ObjectKind::DataViewObject, is_data_view, as_data_view);
impl_subtype_casts!(ProxyObject, ObjectKind::Proxy, is_proxy, as_proxy);
impl_subtype_casts!(GeneratorObject, ObjectKind::Generator, is_generator, as_generator);
impl_subtype_casts!(
    AsyncGeneratorObject,
    ObjectKind::AsyncGenerator,
    is_async_generator,
    as_async_generator
);
impl_subtype_casts!(PromiseObject, ObjectKind::Promise, is_promise, as_promise);
impl_subtype_casts!(
    ObjectPrototype,
    ObjectKind::ObjectPrototype,
    is_object_prototype,
    as_object_prototype
);
impl_subtype_casts!(
    WeakRefObject,
    ObjectKind::WeakRefObject,
    is_weak_ref_object,
    as_weak_ref_object
);
impl_subtype_casts!(
    WeakSetObject,
    ObjectKind::WeakSetObject,
    is_weak_set_object,
    as_weak_set_object
);
impl_subtype_casts!(
    WeakMapObject,
    ObjectKind::WeakMapObject,
    is_weak_map_object,
    as_weak_map_object
);
impl_subtype_casts!(
    FinalizationRegistryObject,
    ObjectKind::FinalizationRegistryObject,
    is_finalization_registry_object,
    as_finalization_registry_object
);
impl_subtype_casts!(
    WrappedValidIterator,
    ObjectKind::WrappedValidIterator,
    is_wrapped_valid_iterator_object,
    as_wrapped_valid_iterator_object
);
impl_subtype_casts!(
    IteratorHelperObject,
    ObjectKind::IteratorHelperObject,
    is_iterator_helper_object,
    as_iterator_helper_object
);
