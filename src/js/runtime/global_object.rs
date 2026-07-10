use brimstone_macros::wrap_ordinary_object;

use crate::{
    extend_object, impl_index_map_instance,
    runtime::{
        Context, HeapItemKind, Realm,
        alloc_error::AllocResult,
        collections::{BsIndexMapField, index_map::IndexMapInstance},
        eval_result::EvalResult,
        gc::{Handle, HeapItem, HeapPtr, HeapVisitor},
        intrinsics::intrinsics::Intrinsic,
        object_value::VirtualObject,
        ordinary_object::{
            PropertyStorage, object_create_with_proto, ordinary_define_own_property,
            ordinary_delete, ordinary_filtered_own_indexed_property_keys,
            ordinary_get_own_property, validate_and_apply_property_descriptor,
        },
        property::{DEFAULT_DATA_PROPERTY_FLAGS, HeapProperty, Property, PropertyFlags},
        property_descriptor::PropertyDescriptor,
        property_key::PropertyKey,
        rust_vtables::extract_virtual_object_vtable,
        shape::Shape,
        value::Value,
    },
    set_uninit,
};

extend_object! {
    /// Global object of a realm. Spec treats it as an ordinary object but our implementation is
    /// actually an exotic object under the hood to enable various optimizations.
    ///
    /// All named property access is intercepted by the exotic object handlers.
    pub struct GlobalObject {
        /// Map from each named property key to its property definition.
        properties: HeapPtr<GlobalPropertiesMap>,
    }
}

impl GlobalObject {
    pub const VIRTUAL_OBJECT_VTABLE: *const () = extract_virtual_object_vtable::<Self>();

    pub fn new(cx: Context, realm: Handle<Realm>) -> AllocResult<HeapPtr<GlobalObject>> {
        let object_prototype = realm.get_intrinsic(Intrinsic::ObjectPrototype);

        let properties =
            GlobalPropertiesMap::new(cx, GlobalPropertiesMap::MIN_CAPACITY)?.to_handle();

        let mut object = object_create_with_proto::<GlobalObject>(
            cx,
            HeapItemKind::GlobalObject,
            object_prototype,
        )?;

        set_uninit!(object.properties, *properties);

        Ok(object)
    }

    /// Look up a named property of the global object, if one exists.
    #[inline]
    pub fn lookup_named_property(&self, key: PropertyKey) -> Option<HeapPtr<GlobalProperty>> {
        self.properties.get(&key).copied()
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<GlobalObject> {
    fn get_own_property(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
    ) -> EvalResult<Option<Property>> {
        if key.is_array_index() {
            return Ok(ordinary_get_own_property(cx, self.as_object(), key));
        }

        // All named properties are intercepted by the global object
        Ok(self
            .lookup_named_property(*key)
            .map(|property| property.to_property(cx)))
    }

    fn define_own_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        if key.is_array_index() {
            return ordinary_define_own_property(cx, self.as_object(), key, desc);
        }

        // All named properties are intercepted by the global object
        let current_desc = self
            .get_own_property(cx, key)?
            .map(|property| PropertyDescriptor::from_property(&property));
        let is_extensible = self.as_object().is_extensible(cx)?;

        Ok(validate_and_apply_property_descriptor(
            cx,
            Some(*self),
            key,
            is_extensible,
            desc,
            current_desc,
        )?)
    }

    fn delete(&mut self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
        if key.is_array_index() {
            return ordinary_delete(cx, self.as_object(), key);
        }

        // All named properties are intercepted by the global object
        match self.lookup_named_property(*key) {
            None => Ok(true),
            Some(mut property) => {
                if property.flags().is_configurable() {
                    // Invalidate any caches holding this deleted global property
                    property.invalidate_deleted();
                    self.properties.remove(&key);

                    Ok(true)
                } else {
                    Ok(false)
                }
            }
        }
    }

    fn own_property_keys(&self, cx: Context) -> EvalResult<Vec<Handle<Value>>> {
        let mut keys: Vec<Handle<Value>> = vec![];

        ordinary_filtered_own_indexed_property_keys(cx, self.as_object(), &mut keys, |_| true)?;

        // Collect symbol keys to be added after string keys
        let mut symbol_keys: Vec<Handle<Value>> = vec![];

        for (property_key, _) in self.properties.iter_gc_unsafe() {
            if property_key.is_string() {
                keys.push(property_key.as_string().to_handle().into());
            } else if property_key.is_symbol() {
                // No need to exclude private properties here since they do not appear in the
                // GlobalPropertiesMap.
                symbol_keys.push(property_key.as_symbol().to_handle().into());
            }
        }

        keys.extend(symbol_keys);

        Ok(keys)
    }
}

impl PropertyStorage for Handle<GlobalObject> {
    fn get_property(&self, cx: Context, key: Handle<PropertyKey>) -> Option<Property> {
        // Only named properties are intercepted
        if key.is_array_index() {
            return PropertyStorage::get_property(&self.as_object(), cx, key);
        }

        self.lookup_named_property(*key)
            .map(|property| property.to_property(cx))
    }

    fn set_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        property: Property,
    ) -> AllocResult<()> {
        // Only named properties are intercepted
        if key.is_array_index() {
            return PropertyStorage::set_property(&mut self.as_object(), cx, key, property);
        }

        if let Some(mut global_property) = self.lookup_named_property(*key) {
            // Update the existing global property in place. Caches remain valid.
            global_property.set_from_property(property);
        } else {
            let global_property = GlobalProperty::new(cx, property)?;

            GlobalPropertiesMapField(*self)
                .maybe_grow_for_insertion(cx)?
                .insert_without_growing(*key, *global_property);
        }

        Ok(())
    }
}

impl HeapItem for GlobalObject {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<GlobalObject>()
    }

    fn visit_pointers(mut object: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        object.visit_object_pointers(visitor);
        visitor.visit_pointer(&mut object.properties);
    }
}

/// A property definition of a named property of the global object, including the property's
/// value and attributes.
#[repr(C)]
pub struct GlobalProperty {
    shape: HeapPtr<Shape>,
    value: Value,
    flags: PropertyFlags,
    /// Whether this property is still valid. Set to false when the property is deleted or shadowed
    /// by a global lexical binding.
    is_valid: bool,
}

impl GlobalProperty {
    pub fn new(cx: Context, property: Property) -> AllocResult<Handle<GlobalProperty>> {
        let mut global_property = cx.alloc_uninit::<GlobalProperty>()?;

        set_uninit!(global_property.shape, cx.shapes.get(HeapItemKind::GlobalProperty));
        set_uninit!(global_property.value, *property.value());
        set_uninit!(global_property.flags, property.flags());
        set_uninit!(global_property.is_valid, true);

        Ok(global_property.to_handle())
    }

    #[inline]
    pub fn value(&self) -> Value {
        self.value
    }

    #[inline]
    pub fn flags(&self) -> PropertyFlags {
        self.flags
    }

    #[inline]
    pub fn is_valid(&self) -> bool {
        self.is_valid
    }

    /// Invalidate due to the property being deleted.
    pub fn invalidate_deleted(&mut self) {
        self.is_valid = false;
        // Clear the value so it can be GC'd
        self.value = Value::undefined();
        self.flags = DEFAULT_DATA_PROPERTY_FLAGS;
    }

    /// Invalidate due to the property being shadowed by a global lexical binding. Still can be
    /// accessed via `globalThis` so the value is not cleared.
    pub fn invalidate_shadowed(&mut self) {
        self.is_valid = false;
    }

    #[inline]
    pub fn set_value(&mut self, value: Value) {
        self.value = value;
    }

    pub fn to_property(&self, cx: Context) -> Property {
        Property::from_heap(cx, &HeapProperty::new(self.value, self.flags))
    }

    pub fn set_from_property(&mut self, property: Property) {
        self.value = *property.value();
        self.flags = property.flags();
    }
}

impl HeapItem for GlobalProperty {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<GlobalProperty>()
    }

    fn visit_pointers(mut property: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut property.shape);
        visitor.visit_value(&mut property.value);
    }
}

impl_index_map_instance!(GlobalPropertiesMap, PropertyKey, HeapPtr<GlobalProperty>);

impl HeapItem for GlobalPropertiesMap {
    fn byte_size(map: HeapPtr<GlobalPropertiesMap>) -> usize {
        GlobalPropertiesMap::calculate_size_in_bytes(map.capacity())
    }

    fn visit_pointers(map: HeapPtr<GlobalPropertiesMap>, visitor: &mut impl HeapVisitor) {
        GlobalPropertiesMap::visit_pointers_impl(map, visitor, |mut map, visitor| {
            for (property_key, property) in map.iter_mut_gc_unsafe() {
                visitor.visit_property_key(property_key);
                visitor.visit_pointer(property);
            }
        });
    }
}

pub struct GlobalPropertiesMapField(Handle<GlobalObject>);

impl BsIndexMapField<GlobalPropertiesMap> for GlobalPropertiesMapField {
    fn get(&self) -> HeapPtr<GlobalPropertiesMap> {
        self.0.properties
    }

    fn set_new(
        &mut self,
        cx: Context,
        capacity: usize,
    ) -> AllocResult<HeapPtr<GlobalPropertiesMap>> {
        let map = GlobalPropertiesMap::new(cx, capacity)?;
        self.0.properties = map;
        Ok(map)
    }
}
