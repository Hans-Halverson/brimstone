use crate::runtime::{
    Context, Handle, HeapItemKind, HeapPtr, PropertyKey, Value,
    accessor::Accessor,
    alloc_error::AllocResult,
    gc::HeapVisitor,
    object_value::ObjectValue,
    property::DEFAULT_DATA_PROPERTY_FLAGS,
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
    SetNamedProperty(SetNamedPropertyCache),
}

impl Cache {
    #[inline]
    pub fn get_named_property(cache: Option<GetNamedPropertyCache>) -> Self {
        match cache {
            Some(cache) => Self::GetNamedProperty(cache),
            None => Self::Failed,
        }
    }

    #[inline]
    pub fn set_named_property(cache: Option<SetNamedPropertyCache>) -> Self {
        match cache {
            Some(cache) => Self::SetNamedProperty(cache),
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
    ) -> AllocResult<Option<Self>> {
        if !is_cacheable_named_property(cx, *receiver, *key) {
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
            if has_exotic_named_property_access(cx, proto, *key) {
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
                let guard = receiver.request_prototype_validity_guard(cx)?.unwrap();

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
        let guard = receiver.request_prototype_validity_guard(cx)?;

        Ok(Some(Self::NotFound { shape: receiver.shape_ptr(), guard }))
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

#[derive(Clone, Copy)]
pub enum SetNamedPropertyCache {
    /// Property is found at this location on the receiver object itself.
    Own {
        shape: HeapPtr<Shape>,
        location: PropertyLocation,
        is_accessor: bool,
    },
    /// Accessor property is found at this location on a prototype object in the receiver's
    /// prototype chain.
    ProtoAccessor {
        shape: HeapPtr<Shape>,
        guard: ValidityGuard,
        proto: HeapPtr<ObjectValue>,
        location: PropertyLocation,
    },
    /// Store a new property with default attributes, transitioning the receiver to a new shape and
    /// appending the new property. The guard is only set if the receiver has a prototype.
    TransitionStore {
        shape: HeapPtr<Shape>,
        guard: Option<ValidityGuard>,
        new_shape: HeapPtr<Shape>,
    },
}

pub enum SetNamedPropertyCacheResult {
    /// Property was successfully stored.
    Success,
    /// Property can be successfully stored after the receiver transitions to the new shape.
    Transition { new_shape: HeapPtr<Shape> },
    /// Accessor property was found. Not guaranteed to contain a setter.
    Accessor(HeapPtr<Accessor>),
    /// Shape was the same but the validity guard was invalid.
    InvalidGuard,
    /// A different shape was encountered than the one cached.
    DifferentShape,
}

impl SetNamedPropertyCache {
    /// Match the cache against a receiver object and return the cached property location if it is
    /// still valid. If the cache is invalid, returns a result indicating why it is invalid.
    pub fn try_match(
        &self,
        mut receiver: HeapPtr<ObjectValue>,
        value: Value,
    ) -> SetNamedPropertyCacheResult {
        match *self {
            Self::Own { shape, location, is_accessor } if shape.ptr_eq(&receiver.shape_ptr()) => {
                if is_accessor {
                    let value = receiver.lookup_location_unchecked(location);
                    let accessor = Accessor::from_value(value);
                    SetNamedPropertyCacheResult::Accessor(accessor)
                } else {
                    receiver.set_location_unchecked(location, value);
                    SetNamedPropertyCacheResult::Success
                }
            }
            Self::ProtoAccessor { shape, guard, proto, location }
                if shape.ptr_eq(&receiver.shape_ptr()) =>
            {
                if guard.is_valid() {
                    let value = proto.lookup_location_unchecked(location);
                    let accessor = Accessor::from_value(value);
                    SetNamedPropertyCacheResult::Accessor(accessor)
                } else {
                    SetNamedPropertyCacheResult::InvalidGuard
                }
            }
            Self::TransitionStore { shape, guard, new_shape }
                if shape.ptr_eq(&receiver.shape_ptr()) =>
            {
                if guard.is_none() || matches!(guard, Some(guard) if guard.is_valid()) {
                    SetNamedPropertyCacheResult::Transition { new_shape }
                } else {
                    SetNamedPropertyCacheResult::InvalidGuard
                }
            }
            _ => SetNamedPropertyCacheResult::DifferentShape,
        }
    }

    /// Fill the cache if possible for storing a property key on a receiver object. Takes the
    /// receiver's old shape (before the set) since the receiver may have a new shape.
    ///
    /// Returns None if the property access is not cacheable.
    pub fn fill(
        cx: Context,
        mut receiver: Handle<ObjectValue>,
        key: Handle<PropertyKey>,
        old_shape: Handle<Shape>,
    ) -> AllocResult<Option<Self>> {
        if !is_cacheable_named_property(cx, *receiver, *key) {
            return Ok(None);
        }

        // First check for an own property
        if let Some(property_definition) = old_shape.lookup_own_property(*key) {
            // Property is an accessor property
            if property_definition.attributes.is_accessor() {
                return Ok(Some(Self::Own {
                    shape: *old_shape,
                    location: property_definition.location,
                    is_accessor: true,
                }));
            }

            // Otherwise property is a data property, which is only cacheable if it is writable
            if !property_definition.attributes.is_writable() {
                return Ok(None);
            }

            return Ok(Some(Self::Own {
                shape: *old_shape,
                location: property_definition.location,
                is_accessor: false,
            }));
        }

        // Walk the old prototype chain looking for the property. Note that we walk the old shape's
        // shape's prototype since that was the shape actually used by the store.
        //
        // Does not allocate.
        let mut next_proto = old_shape.prototype_ptr();
        let mut has_found_property = false;

        while let Some(proto) = next_proto {
            let proto_shape = proto.shape_ptr();

            // Cannot cache if exotic object behavior exists anywhere on the prototype chain
            if has_exotic_named_property_access(cx, proto, *key) {
                return Ok(None);
            }

            // Keep checking rest of prototype chain for exotic behavior even if a matching property
            // on the prototype chain has already been found.
            if has_found_property {
                next_proto = proto_shape.prototype_ptr();
                continue;
            }

            if proto_shape.is_map_mode() {
                // Properties stored on map mode objects cannot be cached
                let map = proto.named_properties_map_opt().unwrap();
                if map.contains_key(&key) {
                    return Ok(None);
                }
            } else if let Some(property_definition) = proto_shape.lookup_own_property(*key) {
                if property_definition.attributes.is_accessor() {
                    // Matched an accessor property on the prototype chain. Be sure to return the
                    // pre-store shape, guard, and prototype object that were actually used by the
                    // store, since the store may have arbitrarily mutated the receiver.
                    let proto = proto.to_handle();
                    let location = property_definition.location;
                    let mut old_prototype = old_shape.prototype_ptr().unwrap().to_handle();

                    // May allocate
                    let guard = old_prototype.request_own_validity_guard(cx)?;

                    return Ok(Some(Self::ProtoAccessor {
                        shape: *old_shape,
                        guard,
                        proto: *proto,
                        location,
                    }));
                } else if !property_definition.attributes.is_writable() {
                    // Stores stop at the first matching data property on the prototype chain, even
                    // if it is non-writable. Do not cache this situation.
                    return Ok(None);
                }

                // A writable data property on the prototype chain means that a shadowed own
                // property will be created on the receiver. The prototype data property is ignored.
                has_found_property = true;
            }

            next_proto = proto_shape.prototype_ptr();
        }

        // Otherwise we try to return a TransitionStore cache.
        //
        // Cache is filled after the store has executed, which could have been a setter that mutated
        // the receiver. We only want to add a TransitionStore cache if the new shape is the direct
        // result of a transition from adding the property to the old shape (i.e. no other
        // property modifications occurred in between).
        //
        // Verify this by ensuring there is a direct transition from the old shape to the new shape.
        let new_shape = receiver.shape_ptr();
        match new_shape.parent_shape_ptr() {
            Some(parent) if parent.ptr_eq(&old_shape) => {}
            _ => return Ok(None),
        }

        // Verify that the new shape comes from a transition that appends a property, since the
        // property is found on the new shape but not the old shape.
        let Some(new_property_definition) = new_shape.lookup_own_property(*key) else {
            return Ok(None);
        };

        // TransitionStore must be for a standard property add with default attributes.
        //
        // Check that property matches. This distinguishes against self-deleting setters on the
        // prototype chain that may have defined this property with non-default attributes.
        if new_property_definition.attributes != DEFAULT_DATA_PROPERTY_FLAGS {
            return Ok(None);
        }

        let new_shape = new_shape.to_handle();

        // May allocate
        let guard = receiver.request_prototype_validity_guard(cx)?;

        Ok(Some(Self::TransitionStore { shape: *old_shape, guard, new_shape: *new_shape }))
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        match self {
            Self::Own { shape, .. } => {
                visitor.visit_pointer(shape);
            }
            Self::ProtoAccessor { shape, guard, proto, .. } => {
                visitor.visit_pointer(shape);
                guard.visit_pointers(visitor);
                visitor.visit_pointer(proto);
            }
            Self::TransitionStore { shape, guard, new_shape } => {
                visitor.visit_pointer(shape);
                if let Some(guard) = guard {
                    guard.visit_pointers(visitor);
                }
                visitor.visit_pointer(new_shape);
            }
        }
    }
}

/// Whether a named property access on this object can be cached at all.
fn is_cacheable_named_property(
    cx: Context,
    object: HeapPtr<ObjectValue>,
    key: PropertyKey,
) -> bool {
    if key.is_array_index() {
        return false;
    }

    // Shapes that are not part of the transition tree
    let shape = object.shape_ptr();
    if shape.is_map_mode() || shape.is_prototype_object() {
        return false;
    }

    !has_exotic_named_property_access(cx, object, key)
}

/// Whether named property access on this object has exotic vs ordinary behavior.
fn has_exotic_named_property_access(
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

impl Cache {
    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        match self {
            Self::Uninitialized | Self::Failed => {}
            Self::GetNamedProperty(cache) => cache.visit_pointers(visitor),
            Self::SetNamedProperty(cache) => cache.visit_pointers(visitor),
        }
    }
}
