use indexmap::IndexMap;
use rand::Rng;

use std::{
    mem::{transmute, transmute_copy},
    num::NonZeroU32,
};

use crate::set_uninit;

use super::{
    array_properties::ArrayProperties,
    builtin_function::{BuiltinFunction, BuiltinFunctionPtr},
    completion::EvalResult,
    environment::private_environment::PrivateName,
    error::type_error_,
    gc::{Handle, HandleValue, HeapPtr},
    intrinsics::typed_array::DynTypedArray,
    object_descriptor::ObjectKind,
    property::{HeapProperty, Property},
    property_descriptor::PropertyDescriptor,
    property_key::{HeapPropertyKey, PropertyKey},
    proxy_object::ProxyObject,
    value::{AccessorValue, Value},
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
            descriptor: $crate::js::runtime::HeapPtr<$crate::js::runtime::object_descriptor::ObjectDescriptor>,

            // Inherited object fields

            // None represents the null value
            prototype: Option<$crate::js::runtime::HeapPtr<$crate::js::runtime::object_value::ObjectValue>>,

            // String and symbol properties by their property key. Includes private properties.
            named_properties: indexmap::IndexMap<$crate::js::runtime::property_key::HeapPropertyKey, $crate::js::runtime::property::HeapProperty>,

            // Array index properties by their property key
            array_properties: $crate::js::runtime::HeapPtr<$crate::js::runtime::array_properties::ArrayProperties>,

            // Whether this object can be extended with new properties
            is_extensible_field: bool,

            // Stable hash code for this object, since object can be moved by GC. Lazily initialized.
            hash_code: Option<std::num::NonZeroU32>,

            // Child fields
            $($field_vis $field_name: $field_type,)*
        }

        impl $(<$($generics),*>)? $crate::js::runtime::gc::GcDeref for $name $(<$($generics),*>)? {}

        impl $(<$($generics),*>)? $name $(<$($generics),*>)? {
            #[inline]
            pub fn descriptor(&self) -> $crate::js::runtime::HeapPtr<$crate::js::runtime::object_descriptor::ObjectDescriptor> {
                self.descriptor
            }

            #[inline]
            pub fn set_descriptor(&mut self, descriptor: $crate::js::runtime::HeapPtr<$crate::js::runtime::object_descriptor::ObjectDescriptor>)  {
                self.descriptor = descriptor
            }
        }

        impl $(<$($generics),*>)? $crate::js::runtime::Handle<$name $(<$($generics),*>)?> {
            #[inline]
            pub fn object(&self) -> $crate::js::runtime::Handle<$crate::js::runtime::object_value::ObjectValue> {
                self.cast()
            }

            /// Cast to an ordinary object so that ordinary object's methods can be called. Only
            /// used when we know we want the default ordinary methods to be called.
            #[inline]
            pub fn ordinary_object(&self) -> $crate::js::runtime::Handle<$crate::js::runtime::ordinary_object::OrdinaryObject> {
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

        impl $(<$($generics),*>)? Into<Gc<$crate::js::runtime::object_value::ObjectValue>> for Gc<$name $(<$($generics),*>)?> {
            fn into(self) -> Gc<$crate::js::runtime::object_value::ObjectValue> {
                self.cast()
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
        cx: &mut Context,
        prototype: Option<Handle<ObjectValue>>,
        is_extensible: bool,
    ) -> Handle<ObjectValue> {
        let mut object = cx.heap.alloc_uninit::<ObjectValue>();

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::OrdinaryObject));
        set_uninit!(object.prototype, prototype.map(|p| p.get_()));
        set_uninit!(object.named_properties, IndexMap::new());
        set_uninit!(object.array_properties, ArrayProperties::initial(cx));
        set_uninit!(object.is_extensible_field, is_extensible);
        object.set_uninit_hash_code();

        object
    }

    // Array properties methods
    pub fn array_properties_length(&self) -> u32 {
        self.array_properties.len()
    }

    // 7.3.27 PrivateElementFind
    pub fn private_element_find(&mut self, private_name: PrivateName) -> Option<Property> {
        let property_key = PropertyKey::symbol(private_name);
        // Safe since get_mut does not allocate on managed heap, and property reference is
        // immediately cloned.
        self.named_properties
            .get(&property_key.to_heap())
            .map(Property::from_heap)
    }

    pub fn has_private_element(&self, private_name: PrivateName) -> bool {
        let property_key = PropertyKey::symbol(private_name);
        // Safe since contains_key does not allocate on managed heap
        self.named_properties.contains_key(&property_key.to_heap())
    }

    pub fn private_element_set(&mut self, private_name: PrivateName, value: Value) {
        let property_key = PropertyKey::symbol(private_name);
        let property = Property::private_field(value);
        // Safe since insert does not allocate on managed heap
        self.named_properties
            .insert(property_key.to_heap(), property.to_heap());
    }

    // 7.3.28 PrivateFieldAdd
    pub fn private_field_add(
        &mut self,
        cx: &mut Context,
        private_name: PrivateName,
        value: Value,
    ) -> EvalResult<()> {
        if self.has_private_element(private_name) {
            type_error_(cx, "private property already defined")
        } else {
            let property = Property::private_field(value);
            // Safe since insert does not allocate on managed heap
            self.named_properties
                .insert(PropertyKey::symbol(private_name).to_heap(), property.to_heap());
            ().into()
        }
    }

    // 7.3.29 PrivateMethodOrAccessorAdd
    pub fn private_method_or_accessor_add(
        &mut self,
        cx: &mut Context,
        private_name: PrivateName,
        private_method: Property,
    ) -> EvalResult<()> {
        if self.has_private_element(private_name) {
            type_error_(cx, "private property already defined")
        } else {
            // Safe since insert does not allocate on managed heap
            self.named_properties
                .insert(PropertyKey::symbol(private_name).to_heap(), private_method.to_heap());
            ().into()
        }
    }

    // Property accessors and mutators
    pub fn get_property(&self, key: PropertyKey) -> Option<Property> {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            return self.array_properties.get_property(array_index);
        }

        // Safe since get does not allocate on managed heap
        self.named_properties
            .get(&key.to_heap())
            .map(Property::from_heap)
    }

    /// An iterator over the named property keys of this object is not GC safe. Caller must ensure
    /// that a GC cannot occur while the iterator is in use.
    #[inline]
    pub fn iter_named_property_keys_gc_unsafe<F: FnMut(HeapPropertyKey)>(&self, mut f: F) {
        for property_key in self.named_properties.keys() {
            f(property_key.clone());
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
    pub fn set_named_properties(&mut self, properties: IndexMap<HeapPropertyKey, HeapProperty>) {
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
    fn descriptor_kind(&self) -> ObjectKind {
        self.descriptor().kind()
    }

    pub fn is_array(&self) -> bool {
        self.descriptor_kind() == ObjectKind::ArrayObject
    }

    pub fn is_error(&self) -> bool {
        self.descriptor_kind() == ObjectKind::ErrorObject
    }

    pub fn is_bool_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::BooleanObject
    }

    pub fn is_number_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::NumberObject
    }

    pub fn is_string_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::StringObject
    }

    pub fn is_symbol_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::SymbolObject
    }

    pub fn is_bigint_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::BigIntObject
    }

    pub fn is_date_object(&self) -> bool {
        false
    }

    pub fn is_regexp_object(&self) -> bool {
        false
    }

    pub fn is_map_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::MapObject
    }

    pub fn is_set_object(&self) -> bool {
        self.descriptor_kind() == ObjectKind::SetObject
    }

    pub fn is_array_buffer(&self) -> bool {
        self.descriptor_kind() == ObjectKind::ArrayBufferObject
    }

    pub fn is_shared_array_buffer(&self) -> bool {
        false
    }

    pub fn is_typed_array(&self) -> bool {
        let kind = self.descriptor_kind() as u8;
        (kind >= ObjectKind::Int8Array as u8) && (kind <= ObjectKind::Float64Array as u8)
    }

    pub fn is_data_view(&self) -> bool {
        self.descriptor_kind() == ObjectKind::DataViewObject
    }

    pub fn is_arguments_object(&self) -> bool {
        match self.descriptor_kind() {
            ObjectKind::MappedArgumentsObject | ObjectKind::UnmappedArgumentsObject => true,
            _ => false,
        }
    }

    pub fn is_proxy(&self) -> bool {
        self.descriptor_kind() == ObjectKind::Proxy
    }

    pub fn is_bound_function(&self) -> bool {
        self.descriptor_kind() == ObjectKind::BoundFunctionObject
    }

    pub fn is_object_prototype(&self) -> bool {
        self.descriptor_kind() == ObjectKind::ObjectPrototype
    }
}

impl Handle<ObjectValue> {
    /// Cast as a virtual object, allowing virtual methods to be called. Manually constructs a
    /// trait object using the vtable stored in the object descriptor.
    #[inline]
    fn virtual_object(&self) -> &mut dyn VirtualObject {
        unsafe {
            let data = self as *const Handle<ObjectValue>;
            let vtable = self.descriptor().vtable();

            let trait_object = ObjectTraitObject { data, vtable };

            transmute_copy::<ObjectTraitObject, &mut dyn VirtualObject>(&trait_object)
        }
    }

    // Type refinement functions
    pub fn as_builtin_function_opt(&self) -> Option<Handle<BuiltinFunction>> {
        if self.descriptor_kind() == ObjectKind::BuiltinFunction {
            Some(self.cast())
        } else {
            None
        }
    }

    // Property accessors and mutators
    pub fn set_property(&mut self, cx: &mut Context, key: PropertyKey, property: Property) {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            ArrayProperties::set_property(cx, *self, array_index, property);

            return;
        }

        // Safe since insert does allocate on managed heap
        self.named_properties
            .insert(key.to_heap(), property.to_heap());
    }

    pub fn remove_property(&mut self, key: PropertyKey) {
        if key.is_array_index() {
            let array_index = key.as_array_index();
            self.array_properties.remove_property(array_index);

            return;
        }

        // TODO: Removal is currently O(n) to maintain order, improve if possible
        // Safe since shift_remove does allocate on managed heap
        self.named_properties.shift_remove(&key.to_heap());
    }

    pub fn set_array_properties_length(&mut self, cx: &mut Context, new_length: u32) -> bool {
        ArrayProperties::set_len(cx, *self, new_length)
    }

    // Intrinsic creation utilities
    pub fn intrinsic_data_prop(&mut self, cx: &mut Context, key: PropertyKey, value: HandleValue) {
        self.set_property(cx, key, Property::data(value, true, false, true))
    }

    pub fn instrinsic_length_prop(&mut self, cx: &mut Context, length: i32) {
        self.set_property(
            cx,
            cx.names.length(),
            Property::data(Value::smi(length), false, false, true),
        )
    }

    pub fn intrinsic_name_prop(&mut self, cx: &mut Context, name: &str) {
        let name_value = Value::string(cx.alloc_string(name.to_owned()));
        self.set_property(cx, cx.names.name(), Property::data(name_value, false, false, true))
    }

    pub fn intrinsic_getter(
        &mut self,
        cx: &mut Context,
        name: PropertyKey,
        func: BuiltinFunctionPtr,
        realm: Handle<Realm>,
    ) {
        let getter = BuiltinFunction::create(cx, func, 0, name, Some(realm), None, Some("get"));
        let accessor_value = AccessorValue::new(cx, Some(getter.into()), None);
        self.set_property(cx, name, Property::accessor(accessor_value.into(), false, true));
    }

    pub fn intrinsic_getter_and_setter(
        &mut self,
        cx: &mut Context,
        name: PropertyKey,
        getter: BuiltinFunctionPtr,
        setter: BuiltinFunctionPtr,
        realm: Handle<Realm>,
    ) {
        let getter = BuiltinFunction::create(cx, getter, 0, name, Some(realm), None, Some("get"));
        let setter = BuiltinFunction::create(cx, setter, 1, name, Some(realm), None, Some("set"));
        let accessor_value = AccessorValue::new(cx, Some(getter.into()), Some(setter.into()));
        self.set_property(cx, name, Property::accessor(accessor_value.into(), false, true));
    }

    pub fn intrinsic_func(
        &mut self,
        cx: &mut Context,
        name: PropertyKey,
        func: BuiltinFunctionPtr,
        length: i32,
        realm: Handle<Realm>,
    ) {
        let func = BuiltinFunction::create(cx, func, length, name, Some(realm), None, None).into();
        self.intrinsic_data_prop(cx, name, func);
    }

    pub fn intrinsic_frozen_property(
        &mut self,
        cx: &mut Context,
        key: PropertyKey,
        value: HandleValue,
    ) {
        self.set_property(cx, key, Property::data(value, false, false, false));
    }
}

// Non-virtual object internal methods from spec
impl Handle<ObjectValue> {
    /// The [[GetPrototypeOf]] internal method for all objects. Dispatches to type-specific
    /// implementations as necessary.
    pub fn get_prototype_of(&self, cx: &mut Context) -> EvalResult<Option<Handle<ObjectValue>>> {
        if self.is_proxy() {
            self.cast::<ProxyObject>().get_prototype_of(cx)
        } else {
            self.ordinary_get_prototype_of()
        }
    }

    /// The [[SetPrototypeOf]] internal method for all objects. Dispatches to type-specific
    /// implementations as necessary.
    pub fn set_prototype_of(
        &mut self,
        cx: &mut Context,
        new_prototype: Option<Handle<ObjectValue>>,
    ) -> EvalResult<bool> {
        if self.is_proxy() {
            self.cast::<ProxyObject>()
                .set_prototype_of(cx, new_prototype)
        } else {
            self.ordinary_set_prototype_of(cx, new_prototype)
        }
    }

    /// The [[IsExtensible]] internal method for all objects. Dispatches to type-specific
    /// implementations as necessary.
    pub fn is_extensible(&self, cx: &mut Context) -> EvalResult<bool> {
        if self.is_proxy() {
            self.cast::<ProxyObject>().is_extensible(cx)
        } else {
            self.ordinary_is_extensible()
        }
    }

    /// The [[PreventExtensions]] internal method for all objects. Dispatches to type-specific
    /// implementations as necessary.
    pub fn prevent_extensions(&mut self, cx: &mut Context) -> EvalResult<bool> {
        if self.is_proxy() {
            self.cast::<ProxyObject>().prevent_extensions(cx)
        } else {
            self.ordinary_prevent_extensions()
        }
    }
}

// Wrap all virtual methods for easy access
impl Handle<ObjectValue> {
    #[inline]
    pub fn get_own_property(
        &self,
        cx: &mut Context,
        key: PropertyKey,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        self.virtual_object().get_own_property(cx, key)
    }

    #[inline]
    pub fn define_own_property(
        &mut self,
        cx: &mut Context,
        key: PropertyKey,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        self.virtual_object().define_own_property(cx, key, desc)
    }

    #[inline]
    pub fn has_property(&self, cx: &mut Context, key: PropertyKey) -> EvalResult<bool> {
        self.virtual_object().has_property(cx, key)
    }

    #[inline]
    pub fn get(
        &self,
        cx: &mut Context,
        key: PropertyKey,
        receiver: HandleValue,
    ) -> EvalResult<HandleValue> {
        self.virtual_object().get(cx, key, receiver)
    }

    #[inline]
    pub fn set(
        &mut self,
        cx: &mut Context,
        key: PropertyKey,
        value: HandleValue,
        receiver: HandleValue,
    ) -> EvalResult<bool> {
        self.virtual_object().set(cx, key, value, receiver)
    }

    #[inline]
    pub fn delete(&mut self, cx: &mut Context, key: PropertyKey) -> EvalResult<bool> {
        self.virtual_object().delete(cx, key)
    }

    #[inline]
    pub fn own_property_keys(&self, cx: &mut Context) -> EvalResult<Vec<HandleValue>> {
        self.virtual_object().own_property_keys(cx)
    }

    #[inline]
    pub fn call(
        &self,
        cx: &mut Context,
        this_argument: HandleValue,
        arguments: &[HandleValue],
    ) -> EvalResult<HandleValue> {
        self.virtual_object().call(cx, this_argument, arguments)
    }

    #[inline]
    pub fn construct(
        &self,
        cx: &mut Context,
        arguments: &[HandleValue],
        new_target: Handle<ObjectValue>,
    ) -> EvalResult<Handle<ObjectValue>> {
        self.virtual_object().construct(cx, arguments, new_target)
    }

    // Type utilities
    #[inline]
    pub fn is_callable(&self) -> bool {
        self.virtual_object().is_callable()
    }

    #[inline]
    pub fn is_constructor(&self) -> bool {
        self.virtual_object().is_constructor()
    }

    #[inline]
    pub fn get_realm(&self, cx: &mut Context) -> EvalResult<HeapPtr<Realm>> {
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
        cx: &mut Context,
        key: PropertyKey,
    ) -> EvalResult<Option<PropertyDescriptor>>;

    fn define_own_property(
        &mut self,
        cx: &mut Context,
        key: PropertyKey,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool>;

    fn has_property(&self, cx: &mut Context, key: PropertyKey) -> EvalResult<bool>;

    fn get(
        &self,
        cx: &mut Context,
        key: PropertyKey,
        receiver: HandleValue,
    ) -> EvalResult<HandleValue>;

    fn set(
        &mut self,
        cx: &mut Context,
        key: PropertyKey,
        value: HandleValue,
        receiver: HandleValue,
    ) -> EvalResult<bool>;

    fn delete(&mut self, cx: &mut Context, key: PropertyKey) -> EvalResult<bool>;

    fn own_property_keys(&self, cx: &mut Context) -> EvalResult<Vec<HandleValue>>;

    fn call(
        &self,
        _: &mut Context,
        _this_argument: HandleValue,
        _arguments: &[HandleValue],
    ) -> EvalResult<HandleValue> {
        panic!("[[Call]] not implemented for this object")
    }

    fn construct(
        &self,
        _: &mut Context,
        _arguments: &[HandleValue],
        _new_target: Handle<ObjectValue>,
    ) -> EvalResult<Handle<ObjectValue>> {
        panic!("[[Construct]] not implemented for this object")
    }

    // Type utilities
    fn is_callable(&self) -> bool {
        false
    }

    fn is_constructor(&self) -> bool {
        false
    }

    fn get_realm(&self, cx: &mut Context) -> EvalResult<HeapPtr<Realm>> {
        cx.current_realm().into()
    }

    fn as_typed_array(&self) -> DynTypedArray {
        unreachable!("as_typed_array can only be called on typed arrays")
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
