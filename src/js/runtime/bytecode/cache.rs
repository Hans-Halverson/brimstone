use crate::runtime::{
    Context, Handle, HeapItemKind, HeapPtr, PropertyKey, Realm, Value,
    accessor::Accessor,
    alloc_error::AllocResult,
    array_object::ArrayObject,
    bytecode::function::CacheArray,
    gc::HeapVisitor,
    global_object::GlobalProperty,
    object_value::{MapModeCachedLocation, ObjectValue},
    property::DEFAULT_DATA_PROPERTY_FLAGS,
    shape::{Shape, ValidityGuard},
    string_value::FlatString,
    transitions::PropertyLocation,
};

/// The maximum number of entries in a polymorphic cache.
pub const POLYMORPHIC_CACHE_SIZE: usize = 4;

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
    GlobalProperty(GlobalPropertyCache),
    Polymorphic(HeapPtr<CacheArray>),
}

impl Cache {
    /// Insert a new cache entry into the cache at the given index. Replaces an existing entry or
    /// promotes to a polymorphic cache when possible.
    ///
    /// An entry of `None` means caching has failed and we stop caching at this site entirely.
    ///
    /// Only supported for GetNamedProperty and SetNamedProperty caches.
    pub fn insert(
        mut caches: HeapPtr<CacheArray>,
        cache_index: usize,
        new_entry: Option<Cache>,
        new_polymorphic_cache: Option<Handle<CacheArray>>,
    ) {
        // An uncacheable result:
        // - If monomorphic, stop caching at this site entirely
        // - If polymorphic, keep the existing cache
        let Some(new_entry) = new_entry else {
            if !(matches!(caches.get(cache_index), Cache::Polymorphic(_))) {
                caches.set(cache_index, Cache::Failed);
            }
            return;
        };

        match caches.get(cache_index) {
            // Once caching has failed at this site it is never resumed
            Cache::Failed => {}
            // First time cache is filled, so just insert the (monomorphic) entry
            Cache::Uninitialized => caches.set(cache_index, new_entry),
            // Monomorphic cache already exists, so either replace or promote to polymorphic
            existing @ (Cache::GetNamedProperty(_) | Cache::SetNamedProperty(_)) => {
                if Self::same_receiver_keys(existing, new_entry) || existing.is_stale() {
                    caches.set(cache_index, new_entry);
                } else {
                    // A second shape was seen, promote to a polymorphic cache with both entries
                    let mut new_polymorphic_cache = new_polymorphic_cache.unwrap();
                    new_polymorphic_cache.set(0, existing);
                    new_polymorphic_cache.set(1, new_entry);
                    caches.set(cache_index, Cache::Polymorphic(*new_polymorphic_cache));
                }
            }
            Cache::Polymorphic(mut entries) => {
                let mut first_stale_index = None;
                let mut first_uninitialized_index = None;

                for i in 0..entries.len() {
                    match entries.get(i) {
                        Cache::Uninitialized => {
                            first_uninitialized_index = Some(i);
                            break;
                        }
                        // Replace entries with the same receiver key
                        entry if Self::same_receiver_keys(entry, new_entry) => {
                            entries.set(i, new_entry);
                            return;
                        }
                        entry if first_stale_index.is_none() && entry.is_stale() => {
                            first_stale_index = Some(i);
                        }
                        _ => {}
                    }
                }

                // No matching entry was found, try to replace a stale entry first
                if let Some(i) = first_stale_index {
                    entries.set(i, new_entry);
                } else if let Some(i) = first_uninitialized_index {
                    entries.set(i, new_entry);
                } else {
                    // Polymorphic cache is full, stop caching at this site
                    caches.set(cache_index, Cache::Failed);
                }
            }
            Cache::GlobalProperty(_) => unreachable!("wrong cache kind for insert"),
        }
    }

    /// Whether this entry is keyed on a stale shape, meaning it can never be matched again.
    pub fn is_stale(&self) -> bool {
        match self.receiver_key() {
            CacheReceiverKey::Shape(shape) => shape.is_stale(),
            CacheReceiverKey::ArrayObject | CacheReceiverKey::String => false,
        }
    }

    fn same_receiver_keys(cache_a: Cache, cache_b: Cache) -> bool {
        cache_a.receiver_key().matches(cache_b.receiver_key())
    }

    fn receiver_key(&self) -> CacheReceiverKey {
        match self {
            Self::GetNamedProperty(cache) => cache.receiver_key(),
            Self::SetNamedProperty(cache) => CacheReceiverKey::Shape(cache.receiver_shape()),
            _ => unreachable!("cache does not have a receiver key"),
        }
    }
}

/// A generic receiver key for comparing whether two cache entries depend on the same shape or kind
/// of the receiver.
#[derive(Clone, Copy)]
enum CacheReceiverKey {
    Shape(HeapPtr<Shape>),
    ArrayObject,
    String,
}

impl CacheReceiverKey {
    fn matches(&self, other: CacheReceiverKey) -> bool {
        match (self, other) {
            (Self::Shape(shape), Self::Shape(other_shape)) => shape.ptr_eq(&other_shape),
            (Self::ArrayObject, Self::ArrayObject) | (Self::String, Self::String) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy)]
pub enum GetNamedPropertyCache {
    /// Property is found at this location on the receiver object itself.
    Own {
        shape: HeapPtr<Shape>,
        location: CachedPropertyLocation,
        is_accessor: bool,
    },
    /// Property is found at this location on a prototype object in the receiver's prototype chain.
    ///
    /// Can be a primitive receiver, in which case the shape is the primitive's singleton shape.
    Proto {
        shape: HeapPtr<Shape>,
        guard: ValidityGuard,
        proto: HeapPtr<ObjectValue>,
        location: CachedPropertyLocation,
        is_accessor: bool,
    },
    /// Property was not found on the receiver or anywhere in its prototype chain. The guard is only
    /// set if the receiver has a prototype.
    NotFound { shape: HeapPtr<Shape>, guard: Option<ValidityGuard> },
    /// Receiver is an array object and the property is `length`.
    ArrayLength,
    /// Receiver is a string primitive and the property is `length`.
    StringLength,
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
    /// Match the cache against a receiver value and return the cached property if it is still
    /// valid. If the cache is invalid, returns a result indicating why it is invalid.
    #[inline(always)]
    pub fn try_match(&self, receiver: Value) -> GetNamedPropertyCacheResult {
        debug_assert!(receiver.is_pointer());
        let receiver_shape = receiver.as_pointer().shape();

        match *self {
            Self::Own { shape, location, is_accessor } if shape.ptr_eq(&receiver_shape) => {
                let value = receiver
                    .as_object()
                    .lookup_cached_location_unchecked(location);
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
                if shape.ptr_eq(&receiver_shape) =>
            {
                if guard.is_valid() {
                    let value = proto.lookup_cached_location_unchecked(location);
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
            Self::NotFound { shape, guard } if shape.ptr_eq(&receiver_shape) => {
                if guard.is_none() || matches!(guard, Some(guard) if guard.is_valid()) {
                    GetNamedPropertyCacheResult::Data(Value::undefined())
                } else {
                    GetNamedPropertyCacheResult::InvalidGuard
                }
            }
            Self::ArrayLength if receiver_shape.kind() == HeapItemKind::ArrayObject => {
                GetNamedPropertyCacheResult::Data(Value::number(
                    receiver.as_object().array_properties_length(),
                ))
            }
            Self::StringLength if receiver_shape.kind() == HeapItemKind::StringValue => {
                GetNamedPropertyCacheResult::Data(Value::number(receiver.as_string().len()))
            }
            _ => GetNamedPropertyCacheResult::DifferentShape,
        }
    }

    /// Fill the cache if possible for accessing a property key on a receiver.
    ///
    /// Takes both the original receiver and the receiver coerced to an object since some caches
    /// depend on the original receiver.
    ///
    /// Returns None if the property access is not cacheable.
    pub fn fill(
        cx: Context,
        original_receiver: Handle<Value>,
        mut receiver: Handle<ObjectValue>,
        key: Handle<PropertyKey>,
    ) -> AllocResult<Option<Self>> {
        // Neither string length nor array length is stored as a regular property, so as long as the
        // receiver has the right kind they can be cached as special cases.
        if original_receiver.is_string() && *key == *cx.names.length() {
            return Ok(Some(Self::StringLength));
        }

        if receiver.is::<ArrayObject>() && *key == *cx.names.length() {
            return Ok(Some(Self::ArrayLength));
        }

        if !original_receiver.is_object() {
            return Self::fill_primitive(cx, original_receiver, receiver, key);
        }

        if !is_cacheable_named_property_receiver(cx, *receiver, *key) {
            return Ok(None);
        }

        // First check for an own property
        let shape = receiver.shape_ptr();
        if shape.is_map_mode() {
            match receiver.map_mode_cached_location(*key) {
                MapModeCachedLocation::Found(location, flags) => {
                    return Ok(Some(Self::Own {
                        shape,
                        location,
                        is_accessor: flags.is_accessor(),
                    }));
                }
                MapModeCachedLocation::Uncacheable => return Ok(None),
                MapModeCachedLocation::NotFound => {}
            }
        } else if let Some(def) = shape.lookup_own_property(*key) {
            return Ok(Some(Self::Own {
                shape,
                location: ObjectValue::cached_location(def.location),
                is_accessor: def.attributes.is_accessor(),
            }));
        }

        match Self::lookup_prototype_chain(cx, shape.prototype_ptr(), key) {
            PrototypeLookup::Uncacheable => Ok(None),
            PrototypeLookup::Found { proto, location, is_accessor } => {
                let proto = proto.to_handle();

                // May allocate
                let guard = receiver.request_prototype_validity_guard(cx)?.unwrap();

                Ok(Some(Self::Proto {
                    shape: receiver.shape_ptr(),
                    guard,
                    proto: *proto,
                    location,
                    is_accessor,
                }))
            }
            PrototypeLookup::NotFound => {
                // May allocate
                let guard = receiver.request_prototype_validity_guard(cx)?;

                Ok(Some(Self::NotFound { shape: receiver.shape_ptr(), guard }))
            }
        }
    }

    /// Fill the cache for a property access on a primitive receiver.
    fn fill_primitive(
        cx: Context,
        original_receiver: Handle<Value>,
        mut receiver: Handle<ObjectValue>,
        key: Handle<PropertyKey>,
    ) -> AllocResult<Option<Self>> {
        if !Self::is_cacheable_primitive(*original_receiver) {
            return Ok(None);
        }

        if key.is_array_index() {
            return Ok(None);
        }

        match Self::lookup_prototype_chain(cx, receiver.shape_ptr().prototype_ptr(), key) {
            PrototypeLookup::Uncacheable => Ok(None),
            PrototypeLookup::Found { proto, location, is_accessor } => {
                // Accessors take the slow path since the coerced receiver is visible
                if is_accessor {
                    return Ok(None);
                }

                let proto = proto.to_handle();

                // May allocate
                let guard = receiver.request_prototype_validity_guard(cx)?.unwrap();

                Ok(Some(Self::Proto {
                    shape: original_receiver.as_pointer().shape(),
                    guard,
                    proto: *proto,
                    location,
                    is_accessor: false,
                }))
            }
            PrototypeLookup::NotFound => {
                // May allocate
                let guard = receiver.request_prototype_validity_guard(cx)?;

                Ok(Some(Self::NotFound { shape: original_receiver.as_pointer().shape(), guard }))
            }
        }
    }

    /// The receiver shape used for a fill of this cache.
    pub fn fill_receiver_shape(
        original_receiver: Value,
        receiver: HeapPtr<ObjectValue>,
    ) -> HeapPtr<Shape> {
        if Self::is_cacheable_primitive(original_receiver) {
            original_receiver.as_pointer().shape()
        } else {
            receiver.shape_ptr()
        }
    }

    /// Whether a named property access on a primitive receiver is cacheable.
    #[inline(always)]
    pub fn is_cacheable_primitive(receiver_value: Value) -> bool {
        // Only primitives that are heap items are cacheable since the cache needs a shape
        receiver_value.is_string() || receiver_value.is_symbol() || receiver_value.is_bigint()
    }

    /// Walk a prototype chain looking for a property. Does not allocate.
    fn lookup_prototype_chain(
        cx: Context,
        mut next_proto: Option<HeapPtr<ObjectValue>>,
        key: Handle<PropertyKey>,
    ) -> PrototypeLookup {
        while let Some(proto) = next_proto {
            let proto_shape = proto.shape_ptr();

            // Cannot cache exotic object behavior
            if has_exotic_named_property_access(cx, proto, *key) {
                return PrototypeLookup::Uncacheable;
            }

            if proto_shape.is_map_mode() {
                // Property is stored in the map mode prototype object's properties
                match proto.map_mode_cached_location(*key) {
                    MapModeCachedLocation::Found(location, flags) => {
                        return PrototypeLookup::Found {
                            proto,
                            location,
                            is_accessor: flags.is_accessor(),
                        };
                    }
                    MapModeCachedLocation::Uncacheable => return PrototypeLookup::Uncacheable,
                    MapModeCachedLocation::NotFound => {}
                }
            } else if let Some(def) = proto_shape.lookup_own_property(*key) {
                // Property is stored in the prototype object's own properties
                return PrototypeLookup::Found {
                    proto,
                    location: ObjectValue::cached_location(def.location),
                    is_accessor: def.attributes.is_accessor(),
                };
            }

            next_proto = proto_shape.prototype_ptr();
        }

        PrototypeLookup::NotFound
    }

    /// The receiver shape this cache is keyed on, if any. Returns None if the cache is instead
    /// keyed on the receiver's kind.
    pub fn receiver_shape(&self) -> Option<HeapPtr<Shape>> {
        match self {
            Self::Own { shape, .. } | Self::Proto { shape, .. } | Self::NotFound { shape, .. } => {
                Some(*shape)
            }
            Self::ArrayLength | Self::StringLength => None,
        }
    }

    /// The receiver shape or key this cache is keyed on.
    fn receiver_key(&self) -> CacheReceiverKey {
        match self {
            Self::Own { shape, .. } | Self::Proto { shape, .. } | Self::NotFound { shape, .. } => {
                CacheReceiverKey::Shape(*shape)
            }
            Self::ArrayLength => CacheReceiverKey::ArrayObject,
            Self::StringLength => CacheReceiverKey::String,
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
            Self::ArrayLength | Self::StringLength => {}
        }
    }
}

/// A cache used for storing a named property, either for a SetNamedProperty or DefineNamedProperty
/// instruction.
#[derive(Clone, Copy)]
pub enum SetNamedPropertyCache {
    /// Property is found at this location on the receiver object itself.
    Own {
        shape: HeapPtr<Shape>,
        location: CachedPropertyLocation,
        is_accessor: bool,
    },
    /// Accessor property is found at this location on a prototype object in the receiver's
    /// prototype chain.
    ProtoAccessor {
        shape: HeapPtr<Shape>,
        guard: ValidityGuard,
        proto: HeapPtr<ObjectValue>,
        location: CachedPropertyLocation,
    },
    /// Store a new property with default attributes, transitioning the receiver to a new shape and
    /// appending the new property. The guard is only set if the receiver has a prototype.
    TransitionStore {
        shape: HeapPtr<Shape>,
        guard: Option<ValidityGuard>,
        new_shape: HeapPtr<Shape>,
        location: PropertyLocation,
    },
}

pub enum SetNamedPropertyCacheResult {
    /// Property was successfully stored.
    Success,
    /// Property can be successfully stored after the receiver transitions to the new shape.
    Transition { new_shape: HeapPtr<Shape>, location: PropertyLocation },
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
    #[inline(always)]
    pub fn try_match(
        &self,
        mut receiver: HeapPtr<ObjectValue>,
        value: Value,
    ) -> SetNamedPropertyCacheResult {
        match *self {
            Self::Own { shape, location, is_accessor } if shape.ptr_eq(&receiver.shape_ptr()) => {
                if is_accessor {
                    let value = receiver.lookup_cached_location_unchecked(location);
                    let accessor = Accessor::from_value(value);
                    SetNamedPropertyCacheResult::Accessor(accessor)
                } else {
                    receiver.set_cached_location_unchecked(location, value);
                    SetNamedPropertyCacheResult::Success
                }
            }
            Self::ProtoAccessor { shape, guard, proto, location }
                if shape.ptr_eq(&receiver.shape_ptr()) =>
            {
                if guard.is_valid() {
                    let value = proto.lookup_cached_location_unchecked(location);
                    let accessor = Accessor::from_value(value);
                    SetNamedPropertyCacheResult::Accessor(accessor)
                } else {
                    SetNamedPropertyCacheResult::InvalidGuard
                }
            }
            Self::TransitionStore { shape, guard, new_shape, location }
                if shape.ptr_eq(&receiver.shape_ptr()) =>
            {
                if guard.is_none() || matches!(guard, Some(guard) if guard.is_valid()) {
                    SetNamedPropertyCacheResult::Transition { new_shape, location }
                } else {
                    SetNamedPropertyCacheResult::InvalidGuard
                }
            }
            _ => SetNamedPropertyCacheResult::DifferentShape,
        }
    }

    /// Fill the cache for a SetNamedProperty instruction if possible, storing a property key on a
    /// receiver object. Takes the receiver's old shape (before the set) since the receiver may have
    /// a new shape.
    ///
    /// Returns None if the property access is not cacheable.
    pub fn fill_for_set_named_property(
        cx: Context,
        mut receiver: Handle<ObjectValue>,
        key: Handle<PropertyKey>,
        old_shape: Handle<Shape>,
    ) -> AllocResult<Option<Self>> {
        if !is_cacheable_named_property_receiver(cx, *receiver, *key) {
            return Ok(None);
        }

        // First check for an own property
        let own_property = if old_shape.is_map_mode() {
            // Only cache if the shape is unchanged, meaning the property already existed and was
            // overwritten in place. This also prevents caching if a setter changed the receiver's
            // shape during the store.
            if !receiver.shape_ptr().ptr_eq(&old_shape) {
                return Ok(None);
            }

            match receiver.map_mode_cached_location(*key) {
                MapModeCachedLocation::Found(location, flags) => Some((location, flags)),
                MapModeCachedLocation::NotFound => None,
                MapModeCachedLocation::Uncacheable => return Ok(None),
            }
        } else if let Some(property_definition) = old_shape.lookup_own_property(*key) {
            let location = ObjectValue::cached_location(property_definition.location);
            Some((location, property_definition.attributes))
        } else {
            None
        };

        if let Some((location, flags)) = own_property {
            // Property is an accessor property
            if flags.is_accessor() {
                return Ok(Some(Self::Own { shape: *old_shape, location, is_accessor: true }));
            }

            // Otherwise property is a data property, which is only cacheable if it is writable
            if !flags.is_writable() {
                return Ok(None);
            }

            return Ok(Some(Self::Own { shape: *old_shape, location, is_accessor: false }));
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

            // Property may be stored in either a map mode or array mode prototype object
            let proto_property = if proto_shape.is_map_mode() {
                match proto.map_mode_cached_location(*key) {
                    MapModeCachedLocation::Found(location, flags) => Some((location, flags)),
                    MapModeCachedLocation::NotFound => None,
                    MapModeCachedLocation::Uncacheable => return Ok(None),
                }
            } else {
                proto_shape.lookup_own_property(*key).map(|def| {
                    let location = ObjectValue::cached_location(def.location);
                    (location, def.attributes)
                })
            };

            if let Some((location, flags)) = proto_property {
                if flags.is_accessor() {
                    // Matched an accessor property on the prototype chain. Be sure to return the
                    // pre-store shape, guard, and prototype object that were actually used by the
                    // store, since the store may have arbitrarily mutated the receiver.
                    let proto = proto.to_handle();
                    let mut old_prototype = old_shape.prototype_ptr().unwrap().to_handle();

                    // May allocate
                    let guard = old_prototype.request_own_validity_guard(cx)?;

                    return Ok(Some(Self::ProtoAccessor {
                        shape: *old_shape,
                        guard,
                        proto: *proto,
                        location,
                    }));
                } else if !flags.is_writable() {
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

        let location = new_property_definition.location;
        let new_shape = new_shape.to_handle();

        // May allocate
        let guard = receiver.request_prototype_validity_guard(cx)?;

        Ok(Some(Self::TransitionStore {
            shape: *old_shape,
            guard,
            new_shape: *new_shape,
            location,
        }))
    }

    /// Fill the cache for a DefineNamedProperty instruction if possible, defining a data property
    /// with default attributes on a receiver object. Takes the receiver's old shape (before the
    /// define) since the receiver may have a new shape.
    ///
    /// Returns None if the property define is not cacheable.
    pub fn fill_for_define_named_property(
        cx: Context,
        receiver: Handle<ObjectValue>,
        key: Handle<PropertyKey>,
        old_shape: Handle<Shape>,
    ) -> Option<Self> {
        if !is_cacheable_named_property_receiver(cx, *receiver, *key) {
            return None;
        }

        // First check if an existing own property was overwritten. Only cacheable if a simple store
        // can be performed to overwrite the existing property since it was a data property with
        // default attributes.
        let own_property = if old_shape.is_map_mode() {
            // Only cache if the shape is unchanged, meaning the property already existed and was
            // overwritten in place.
            if !receiver.shape_ptr().ptr_eq(&old_shape) {
                return None;
            }

            let MapModeCachedLocation::Found(location, flags) =
                receiver.map_mode_cached_location(*key)
            else {
                return None;
            };

            Some((location, flags))
        } else {
            old_shape.lookup_own_property(*key).map(|def| {
                let location = ObjectValue::cached_location(def.location);
                (location, def.attributes)
            })
        };

        if let Some((location, flags)) = own_property {
            if flags != DEFAULT_DATA_PROPERTY_FLAGS {
                return None;
            }

            return Some(Self::Own { shape: *old_shape, location, is_accessor: false });
        }

        // Otherwise return a TransitionStore cache.
        //
        // Unlike SetNamedProperty we know that the new shape is the direct result of adding a
        // property with default attributes to the old shape, since no user code may have run.
        let new_shape = receiver.shape_ptr();

        // Adding the property may have moved the receiver into map mode which means it is no longer
        // part of the transition tree.
        if new_shape.is_map_mode() {
            return None;
        }

        debug_assert!(
            matches!(new_shape.parent_shape_ptr(), Some(parent) if parent.ptr_eq(&old_shape)),
        );

        let new_property_definition = new_shape.lookup_own_property(*key).unwrap();
        debug_assert!(new_property_definition.attributes == DEFAULT_DATA_PROPERTY_FLAGS);

        Some(Self::TransitionStore {
            shape: *old_shape,
            guard: None,
            new_shape,
            location: new_property_definition.location,
        })
    }

    pub fn receiver_shape(&self) -> HeapPtr<Shape> {
        match self {
            Self::Own { shape, .. }
            | Self::ProtoAccessor { shape, .. }
            | Self::TransitionStore { shape, .. } => *shape,
        }
    }

    /// Whether the store caused the receiver to update to a new map mode shape.
    #[inline(always)]
    pub fn is_updated_map_mode_shape(
        receiver: HeapPtr<ObjectValue>,
        old_shape: HeapPtr<Shape>,
    ) -> bool {
        let new_shape = receiver.shape_ptr();
        new_shape.is_map_mode() && !new_shape.ptr_eq(&old_shape)
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
            Self::TransitionStore { shape, guard, new_shape, .. } => {
                visitor.visit_pointer(shape);
                if let Some(guard) = guard {
                    guard.visit_pointers(visitor);
                }
                visitor.visit_pointer(new_shape);
            }
        }
    }
}

/// The result of looking for a property on a prototype chain.
enum PrototypeLookup {
    /// Property was found at this location on this prototype object.
    Found {
        proto: HeapPtr<ObjectValue>,
        location: CachedPropertyLocation,
        is_accessor: bool,
    },
    /// Property was not found anywhere on the prototype chain.
    NotFound,
    /// The prototype chain has behavior that cannot be cached.
    Uncacheable,
}

/// Whether a named property access on this receiver can be cached at all.
fn is_cacheable_named_property_receiver(
    cx: Context,
    receiver: HeapPtr<ObjectValue>,
    key: PropertyKey,
) -> bool {
    if key.is_array_index() {
        return false;
    }

    // Prototype object shapes are mutated in place, so they cannot be the key of a cache
    if receiver.shape_ptr().is_prototype_object() {
        return false;
    }

    !has_exotic_named_property_access(cx, receiver, key)
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
        | HeapItemKind::ModuleNamespaceObject
        | HeapItemKind::MappedArgumentsObject
        | HeapItemKind::GlobalObject => true,
        // Arrays only intercept named access to their length property
        HeapItemKind::ArrayObject => key == *cx.names.length(),
        // StringObjects and typed arrays only have exotic behavior if the key is a canonical
        // numeric index string. Other named accesses are ordinary and can be cached.
        HeapItemKind::StringObject => is_possible_string_object_canonical_numeric_index_string(key),
        _ if object.is_typed_array() => {
            is_possible_typed_array_canonical_numeric_index_string(cx, key)
        }
        _ => false,
    }
}

/// Whether a key may be a canonical numeric index string for a StringObject. Is conservative and
/// cheap.
///
/// StringObjects only have exotic behavior for canonical numeric index strings that are valid array
/// indices, i.e. disallowing negative numbers and NaN/Infinity.
fn is_possible_string_object_canonical_numeric_index_string(key: PropertyKey) -> bool {
    if key.is_array_index() {
        return true;
    }

    if !key.is_string() {
        return false;
    }

    let key_string = key.as_string();
    if key_string.is_empty() {
        return false;
    }

    // Numeric index strings must start with a digit
    let first_code_unit = key_string.as_flat().code_unit_at(0);
    (b'0' as u16..=b'9' as u16).contains(&first_code_unit)
}

/// Whether a key may be a canonical numeric index string for a typed array. Is conservative and
/// cheap.
///
/// Typed arrays have exotic behavior for all canonical numeric index strings, even if they are not
/// valid array indices, including negative numbers and NaN/Infinity.
fn is_possible_typed_array_canonical_numeric_index_string(cx: Context, key: PropertyKey) -> bool {
    if key.is_array_index() {
        return true;
    }

    if !key.is_string() {
        return false;
    }

    let key_string = key.as_string();
    if key_string.is_empty() {
        return false;
    }

    let first_code_unit = key_string.as_flat().code_unit_at(0);
    if (b'0' as u16..=b'9' as u16).contains(&first_code_unit) || first_code_unit == b'-' as u16 {
        return true;
    }

    key == *cx.names.nan() || key == *cx.names.infinity()
}

#[derive(Clone, Copy)]
pub struct GlobalPropertyCache {
    property: HeapPtr<GlobalProperty>,
}

pub enum GlobalPropertyCacheResult {
    /// Data property was found.
    Data(HeapPtr<GlobalProperty>),
    /// Accessor property was found with this getter/setter function.
    Accessor(HeapPtr<ObjectValue>),
    /// Property was found but cannot take the fast path (e.g. non-writable data property).
    PropertyExistsSlowPath,
    /// Property can no longer be found (was deleted or shadowed by a global lexical binding).
    NotFound,
}

impl GlobalPropertyCache {
    /// Match the cache for a global property load and return the cached property if it is still
    /// valid.
    #[inline(always)]
    pub fn try_match_load(&self) -> GlobalPropertyCacheResult {
        if !self.property.is_valid() {
            GlobalPropertyCacheResult::NotFound
        } else if self.property.flags().is_accessor() {
            let accessor = Accessor::from_value(self.property.value());
            if let Some(getter) = accessor.get {
                GlobalPropertyCacheResult::Accessor(getter)
            } else {
                // Property exists but does not have a getter so take the slow path
                GlobalPropertyCacheResult::PropertyExistsSlowPath
            }
        } else {
            GlobalPropertyCacheResult::Data(self.property)
        }
    }

    /// Match the cache for a global property store and return the cached property if it is still
    /// valid.
    #[inline(always)]
    pub fn try_match_store(&self) -> GlobalPropertyCacheResult {
        if !self.property.is_valid() {
            GlobalPropertyCacheResult::NotFound
        } else if self.property.flags().is_accessor() {
            let accessor = Accessor::from_value(self.property.value());
            if let Some(setter) = accessor.set {
                GlobalPropertyCacheResult::Accessor(setter)
            } else {
                // Property exists but does not have a setter so take the slow path
                GlobalPropertyCacheResult::PropertyExistsSlowPath
            }
        } else {
            if self.property.flags().is_writable() {
                GlobalPropertyCacheResult::Data(self.property)
            } else {
                // Property exists but is not writable so take the slow path
                GlobalPropertyCacheResult::PropertyExistsSlowPath
            }
        }
    }

    /// Fill the cache for a global load or store with the given name.
    pub fn fill(realm: HeapPtr<Realm>, name: HeapPtr<FlatString>, name_key: PropertyKey) -> Cache {
        if let Some(property) = realm.global_object_ptr().lookup_named_property(name_key)
            && property.is_valid()
            && !realm.has_lexical_name(name)
        {
            // No need to perform further validation, since we'll always be re-validating when
            // matching since the property may have been arbitrarily modified in between.
            Cache::GlobalProperty(Self { property })
        } else {
            Cache::Failed
        }
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.property);
    }
}

impl Cache {
    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        match self {
            Self::Uninitialized | Self::Failed => {}
            Self::GetNamedProperty(cache) => cache.visit_pointers(visitor),
            Self::SetNamedProperty(cache) => cache.visit_pointers(visitor),
            Self::Polymorphic(entries) => visitor.visit_pointer(entries),
            Self::GlobalProperty(cache) => cache.visit_pointers(visitor),
        }
    }
}

#[derive(Clone, Copy)]
pub enum CachedPropertyLocation {
    /// Property is stored inline in the object at this byte offset from the start of the object.
    Inline { byte_offset: u16 },
    /// Property is stored at this byte offset from the start of the object's named properties heap
    /// item, which is either a named properties array or a named properties map.
    External { byte_offset: u16 },
}
