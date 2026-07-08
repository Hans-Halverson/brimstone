use crate::runtime::{
    Context, Handle, HeapItemKind, HeapPtr, PropertyKey, Value,
    accessor::Accessor,
    alloc_error::AllocResult,
    gc::HeapVisitor,
    object_value::ObjectValue,
    shape::{Shape, ValidityGuard},
    transitions::PropertyLocation,
};

/// A generic cache with multiple specific cache types.
///
/// Note that caches currently hold onto cached heap items strongly.
#[repr(C)]
#[derive(Clone, Copy)]
pub enum Cache {
    /// A cache that has not yet been used. This is the initial value when a cache is allocated.
    Uninitialized,
    /// Caching has failed and is no longer occurring (e.g. due to too many types).
    Failed,
    GetNamedProperty(GetNamedPropertyCache),
}

impl Cache {
    #[inline]
    pub fn get_named_property(cache: Option<GetNamedPropertyCache>) -> Self {
        match cache {
            Some(cache) => Self::GetNamedProperty(cache),
            None => Self::Failed,
        }
    }
}

#[derive(Clone, Copy)]
pub enum GetNamedPropertyCache {
    /// Property is found at this location on the receiver object itself.
    Own {
        shape: HeapPtr<Shape>,
        location: PropertyLocation,
        is_accessor: bool,
    },
    /// Property is found at this location on a prototype object in the receiver's prototype chain.
    Proto {
        shape: HeapPtr<Shape>,
        guard: ValidityGuard,
        proto: HeapPtr<ObjectValue>,
        location: PropertyLocation,
        is_accessor: bool,
    },
    /// Property was not found on the receiver or anywhere in its prototype chain. The guard is only
    /// set if the receiver has a prototype.
    NotFound { shape: HeapPtr<Shape>, guard: Option<ValidityGuard> },
}

pub enum GetNamedPropertyCacheResult {
    /// Data property was found with this value.
    Data(Value),
    /// Accessor property was found with this getter function.
    Accessor(HeapPtr<ObjectValue>),
    /// Shape was the same but the validity guard was invalid.
    InvalidGuard,
    /// A different shape was encountered than the one cached.
    DifferentShape,
}

impl GetNamedPropertyCache {
    /// Match the cache against a receiver object and return the cached property if it is still
    /// valid. If the cache is invalid, returns a result indicating why it is invalid.
    pub fn try_match(&self, receiver: HeapPtr<ObjectValue>) -> GetNamedPropertyCacheResult {
        match *self {
            Self::Own { shape, location, is_accessor } if shape.ptr_eq(&receiver.shape_ptr()) => {
                let value = receiver.lookup_location_unchecked(location);
                if is_accessor {
                    let accessor = Accessor::from_value(value);
                    if let Some(getter) = accessor.get {
                        GetNamedPropertyCacheResult::Accessor(getter)
                    } else {
                        // Setter only property
                        GetNamedPropertyCacheResult::Data(Value::undefined())
                    }
                } else {
                    GetNamedPropertyCacheResult::Data(value)
                }
            }
            Self::Proto { shape, guard, proto, location, is_accessor }
                if shape.ptr_eq(&receiver.shape_ptr()) =>
            {
                if guard.is_valid() {
                    let value = proto.lookup_location_unchecked(location);
                    if is_accessor {
                        let accessor = Accessor::from_value(value);
                        if let Some(getter) = accessor.get {
                            GetNamedPropertyCacheResult::Accessor(getter)
                        } else {
                            // Setter only property
                            GetNamedPropertyCacheResult::Data(Value::undefined())
                        }
                    } else {
                        GetNamedPropertyCacheResult::Data(value)
                    }
                } else {
                    GetNamedPropertyCacheResult::InvalidGuard
                }
            }
            Self::NotFound { shape, guard } if shape.ptr_eq(&receiver.shape_ptr()) => {
                if guard.is_none() || matches!(guard, Some(guard) if guard.is_valid()) {
                    GetNamedPropertyCacheResult::Data(Value::undefined())
                } else {
                    GetNamedPropertyCacheResult::InvalidGuard
                }
            }
            _ => GetNamedPropertyCacheResult::DifferentShape,
        }
    }

    /// Fill the cache if possible for accessing a property key on a receiver object.
    ///
    /// Returns None if the property access is not cacheable.
    pub fn fill(
        cx: Context,
        mut receiver: Handle<ObjectValue>,
        key: Handle<PropertyKey>,
    ) -> AllocResult<Option<GetNamedPropertyCache>> {
        if !Self::is_cacheable_property(cx, *receiver, *key) {
            return Ok(None);
        }

        // First check for an own property
        let shape = receiver.shape_ptr();
        if let Some(def) = shape.lookup_own_property(*key) {
            return Ok(Some(Self::Own {
                shape,
                location: def.location,
                is_accessor: def.attributes.is_accessor(),
            }));
        }

        // Walk the prototype chain looking for the property. Does not allocate.
        let mut next_proto = shape.prototype_ptr();
        while let Some(proto) = next_proto {
            let proto_shape = proto.shape_ptr();

            // Cannot cache exotic object behavior
            if Self::has_exotic_property_access(cx, proto, *key) {
                return Ok(None);
            }

            if proto_shape.is_map_mode() {
                // Properties stored on map mode objects cannot be cached
                let map = proto.named_properties_map_opt().unwrap();
                if map.contains_key(&key) {
                    return Ok(None);
                }
            } else if let Some(def) = proto_shape.lookup_own_property(*key) {
                // Property is stored in the prototype object's array
                let proto = proto.to_handle();
                let location = def.location;
                let is_accessor = def.attributes.is_accessor();

                // May allocate
                let guard = receiver.request_validity_guard(cx)?.unwrap();

                return Ok(Some(Self::Proto {
                    shape: receiver.shape_ptr(),
                    guard,
                    proto: *proto,
                    location,
                    is_accessor,
                }));
            }

            next_proto = proto_shape.prototype_ptr();
        }

        // May allocate
        let guard = receiver.request_validity_guard(cx)?;

        Ok(Some(Self::NotFound { shape: receiver.shape_ptr(), guard }))
    }

    /// Whether a named property access on this object can be cached at all.
    fn is_cacheable_property(cx: Context, object: HeapPtr<ObjectValue>, key: PropertyKey) -> bool {
        if key.is_array_index() {
            return false;
        }

        // Shapes that are not part of the transition tree
        let shape = object.shape_ptr();
        if shape.is_map_mode() || shape.is_prototype_object() {
            return false;
        }

        !Self::has_exotic_property_access(cx, object, key)
    }

    /// Whether named property access on this object has exotic vs ordinary behavior.
    fn has_exotic_property_access(
        cx: Context,
        object: HeapPtr<ObjectValue>,
        key: PropertyKey,
    ) -> bool {
        match object.shape_ptr().kind() {
            // Exotic objects with special behavior for named property access
            HeapItemKind::ProxyObject
            | HeapItemKind::StringObject
            | HeapItemKind::ModuleNamespaceObject
            | HeapItemKind::MappedArgumentsObject => true,
            // Arrays only intercept named access to their length property
            HeapItemKind::ArrayObject => key == *cx.names.length(),
            // Typed arrays intercept canonical numeric keys which we don't detect here, so reject
            _ if object.is_typed_array() => true,
            _ => false,
        }
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        match self {
            Self::Own { shape, .. } => {
                visitor.visit_pointer(shape);
            }
            Self::Proto { shape, guard, proto, .. } => {
                visitor.visit_pointer(shape);
                guard.visit_pointers(visitor);
                visitor.visit_pointer(proto);
            }
            Self::NotFound { shape, guard } => {
                visitor.visit_pointer(shape);

                if let Some(guard) = guard {
                    guard.visit_pointers(visitor);
                }
            }
        }
    }
}

impl Cache {
    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        match self {
            Self::Uninitialized | Self::Failed => {}
            Self::GetNamedProperty(cache) => cache.visit_pointers(visitor),
        }
    }
}
