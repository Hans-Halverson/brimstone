use std::mem::size_of;

use bitflags::bitflags;

use crate::{
    impl_vec_instance,
    runtime::{
        Context, Handle, HeapItemKind, HeapPtr, PropertyKey, Value,
        alloc_error::AllocResult,
        boxed_value::BoxedValue,
        collections::{BsVecField, VecInstance},
        gc::{AnyHeapItem, HeapItem, HeapVisitor},
        object_value::{ObjectValue, VirtualObject, VirtualObjectVtable},
        ordinary_object::OrdinaryObject,
        property::PropertyFlags,
        rust_vtables::extract_virtual_object_vtable,
        transitions::{PropertyDefinition, PropertyLocation, Transition, TransitionKind},
    },
    set_uninit,
};

/// Shapes are the descriptor for all items on the heap. They describe the type of the heap item,
/// and if the heap item is an object they also describe its properties, prototype, and more.
///
/// For objects shapes describe the full set of fields on that object. They are built up from a
/// sequence of transitions learned at runtime. This design allows for quick comparison of shapes
/// using just pointer equality.
///
/// Shape liveness is a particular concern. The general rule is that liveness flows from live heap
/// items themselves up to their shapes and up the shape tree. In particular:
/// - Each heap item strongly references its shape
/// - Each shape strongly references its parent shape (so heap item keeps entire shape path alive)
/// - Weak references in the transitions from a shape to its child shapes, allowing shape tree to be
///   pruned.
/// - Weak references held within transitions itself, must always be live if child shape is live.
#[repr(C)]
pub struct Shape {
    /// Always the singleton shape shape
    shape: HeapPtr<Shape>,

    /// Rust VirtualObject vtable, used for dynamic dispatch to some object methods
    vtable: VirtualObjectVtable,

    /// Object's type
    kind: HeapItemKind,

    /// Bitflags for objects
    flags: ObjectFlags,

    /// Object fields (only set if this shape is for an object of any kind)

    /// Whether this shape is for a prototype object.
    ///
    /// Prototype object shapes are special. They are 1:1 with their prototype object and are not
    /// part of the transition tree. They are mutated in place.
    is_prototype_object: bool,

    /// Prototype of this object.
    prototype: Option<HeapPtr<ObjectValue>>,

    /// In array mode this is the list of property definitions for this shape. Must be treated as a
    /// view into a larger array where the first `property_count` entries are the properties in the
    /// order they were originally added.
    ///
    /// Not used in map mode.
    property_definitions: HeapPtr<PropertyDefinitionVec>,

    /// In array mode this is the number of properties in this shape. This is the size of the view
    /// into the `property_definitions` array.
    ///
    /// Not used in map mode.
    array_mode_property_count: u32,

    /// The parent shape which links to this shape via a transition, if in the transition tree.
    parent_shape: Option<HeapPtr<Shape>>,

    /// Either:
    /// - None, if there are no transitions
    /// - A Shape if there is a single simple add-property transition. The added
    ///   property can be found as the last property in the next shape.
    /// - A TransitionArray if there are multiple transitions or other types of transitions.
    ///
    /// Shape can distinguish between the Shape and TransitionArray cases via a flag in ObjectFlags.
    transitions_or_next_shape: Option<HeapPtr<AnyHeapItem>>,

    /// Prototype fields (only set if this is a shape for a prototype object)

    /// A guard that can be dynamically checked to determine if the shape or its entire prototype
    /// chain has been changed since the guard was created.
    ///
    /// True if the shape and its entire prototype chain have not been modified since the guard was
    /// created. False if any shape in the chain has been modified.
    ///
    /// Guards are lazily created when requested.
    ///
    /// Invariant: If the guard field is set then the guard's value is true.
    validity_guard: Option<HeapPtr<BoxedValue>>,

    /// A collection of prototype object shapes for prototype objects whose prototype is this
    /// object. Elements in this collection may be cleared to None.
    ///
    /// This collection only stores prototype object shapes which have been explicitly registered
    /// for use in guard invalidation. These shapes are lazily registered when a validity guard is
    /// requested.
    prototype_object_children_shapes: Option<HeapPtr<PrototypeObjectChildrenShapesVec>>,
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct ObjectFlags: u8 {
        /// Whether this heap item is an object value
        const IS_OBJECT = 1 << 0;

        /// Whether this is a heap object in map mode
        const IS_MAP_MODE = 1 << 1;

        /// Whether this is an object shape that can be extended with new properties.
        const IS_EXTENSIBLE = 1 << 2;

        /// Whether this prototype object shape is registered as a child in its prototype's shape.
        ///
        /// Invariant: if this flag is set then every shape in the prototype chain above this shape
        /// has been converted to a prototype object shape and is properly registered in its parent
        /// shape, so registration walks can stop at the first registered shape.
        const IS_REGISTERED_PROTOTYPE_CHILD = 1 << 3;

        /// Whether this shape has a transitions vector in `transitions_or_next_shape`.
        const HAS_TRANSITIONS_VEC = 1 << 4;
    }
}

impl ObjectFlags {
    pub fn new_object() -> Self {
        ObjectFlags::IS_OBJECT | ObjectFlags::IS_EXTENSIBLE
    }

    pub fn new_non_object() -> Self {
        ObjectFlags::empty()
    }

    fn is_map_mode(&self) -> bool {
        self.contains(ObjectFlags::IS_MAP_MODE)
    }

    fn set_is_map_mode(&mut self, is_map_mode: bool) {
        self.set(ObjectFlags::IS_MAP_MODE, is_map_mode);
    }

    fn set_is_extensible(&mut self, is_extensible: bool) {
        self.set(ObjectFlags::IS_EXTENSIBLE, is_extensible);
    }

    fn set_is_registered_prototype_child(&mut self, is_registered: bool) {
        self.set(ObjectFlags::IS_REGISTERED_PROTOTYPE_CHILD, is_registered);
    }

    /// Whether this prototype object shape is registered in its prototype's list of prototype
    /// object children. If set the entire prototype chain above this shape is also registered.
    #[inline]
    pub fn is_registered_prototype_child(&self) -> bool {
        self.contains(ObjectFlags::IS_REGISTERED_PROTOTYPE_CHILD)
    }

    #[inline]
    pub fn has_transitions_vec(&self) -> bool {
        self.contains(ObjectFlags::HAS_TRANSITIONS_VEC)
    }
}

/// The maximum number of properties that can be stored in a shape before switching to map mode.
const MAX_ARRAY_PROPERTIES: usize = 64;

/// The maximum number of transitions that can be stored from a single shape. After this limit no
/// new transitions are added and shapes that would need them enter map mode.
const MAX_TRANSITIONS: usize = 256;

enum StoredTransitions {
    None,
    SimpleAddProperty(Handle<Shape>),
    Transitions(Handle<TransitionVec>),
}

impl Shape {
    pub fn new<T>(
        cx: Context,
        shape: Handle<Shape>,
        kind: HeapItemKind,
        flags: ObjectFlags,
        is_prototype_object: bool,
        property_definitions: Handle<PropertyDefinitionVec>,
    ) -> AllocResult<HeapPtr<Shape>>
    where
        Handle<T>: VirtualObject,
    {
        let mut s = cx.alloc_uninit::<Shape>()?;

        set_uninit!(s.shape, *shape);
        set_uninit!(s.vtable, extract_virtual_object_vtable::<T>());
        set_uninit!(s.kind, kind);
        set_uninit!(s.flags, flags);
        set_uninit!(s.is_prototype_object, is_prototype_object);

        set_uninit!(s.prototype, None);
        set_uninit!(s.property_definitions, *property_definitions);
        set_uninit!(s.array_mode_property_count, 0);
        set_uninit!(s.parent_shape, None);
        set_uninit!(s.transitions_or_next_shape, None);
        set_uninit!(s.validity_guard, None);
        set_uninit!(s.prototype_object_children_shapes, None);

        Ok(s)
    }

    /// Create the singleton shape shape, which all shapes have in their
    /// `shape` field.
    pub fn new_shape_shape(
        cx: Context,
        default_property_definitions: Handle<PropertyDefinitionVec>,
    ) -> AllocResult<HeapPtr<Shape>> {
        // Create fake handle which will be read from, in order to initialize shape shape
        let value = Value::empty();
        let fake_shape_handle = Handle::<Value>::from_fixed_non_heap_ptr(&value).cast();

        // First set up the singleton shape shape, using an arbitrary vtable
        // (e.g. OrdinaryObject). Can only set self pointer after object initially created.
        let mut shape = Shape::new::<OrdinaryObject>(
            cx,
            fake_shape_handle,
            HeapItemKind::Shape,
            ObjectFlags::new_non_object(),
            /* is_prototype_object */ false,
            default_property_definitions,
        )?;

        // Shape shape points to itself, like all other shapes
        shape.shape = shape;

        Ok(shape)
    }

    #[inline]
    pub fn shape_ptr(&self) -> HeapPtr<Shape> {
        self.shape
    }

    #[inline]
    pub const fn kind(&self) -> HeapItemKind {
        self.kind
    }

    #[inline]
    pub const fn vtable(&self) -> VirtualObjectVtable {
        self.vtable
    }

    /// Whether this is a shape for an object.
    #[inline]
    pub fn is_object(&self) -> bool {
        self.flags.contains(ObjectFlags::IS_OBJECT)
    }

    /// Whether this is a shape for an object in map mode.
    #[inline]
    pub fn is_map_mode(&self) -> bool {
        self.flags.is_map_mode()
    }

    /// Whether this is a shape for an extensible object.
    #[inline]
    pub fn is_extensible(&self) -> bool {
        self.flags.contains(ObjectFlags::IS_EXTENSIBLE)
    }

    #[inline]
    pub fn prototype_ptr(&self) -> Option<HeapPtr<ObjectValue>> {
        self.prototype
    }

    /// Whether this shape has a `TransitionVec` in its `transitions_or_next_shape` field.
    #[inline]
    pub fn has_transitions_vec(&self) -> bool {
        self.flags.has_transitions_vec()
    }

    pub fn set_shape(&mut self, shape: HeapPtr<Shape>) {
        self.shape = shape;
    }

    pub fn num_properties(&self) -> u32 {
        self.array_mode_property_count
    }

    pub fn transitions_or_next_shape(&self) -> Option<HeapPtr<AnyHeapItem>> {
        self.transitions_or_next_shape
    }

    pub fn set_transitions_or_next_shape(&mut self, ptr: Option<HeapPtr<AnyHeapItem>>) {
        let is_transitions_vec = ptr.is_some_and(|ptr| ptr.is::<TransitionVec>());

        self.flags
            .set(ObjectFlags::HAS_TRANSITIONS_VEC, is_transitions_vec);
        self.transitions_or_next_shape = ptr;
    }

    pub fn property_definitions_slice(&self) -> &[PropertyDefinition] {
        &self.property_definitions.as_slice()[..self.array_mode_property_count as usize]
    }

    fn get_property_at_index(&self, index: usize) -> &PropertyDefinition {
        debug_assert!(index < self.property_definitions.len());

        &self.property_definitions.as_slice()[index]
    }

    fn get_property_at_index_mut(&mut self, index: usize) -> &mut PropertyDefinition {
        debug_assert!(index < self.property_definitions.len());

        &mut self.property_definitions.as_mut_slice()[index]
    }

    /// Return the last property definition added. This is the most recent property added and
    /// therefore is the property added by all incoming transitions which define new properties.
    fn get_last_property_definition(&self) -> &PropertyDefinition {
        self.get_property_at_index((self.array_mode_property_count - 1) as usize)
    }

    fn lookup_own_property_index(&self, key: PropertyKey) -> Option<usize> {
        self.property_definitions_slice()
            .iter()
            .position(|def| def.key == key)
    }

    pub fn has_own_property(&self, key: PropertyKey) -> bool {
        self.property_definitions_slice()
            .iter()
            .any(|def| def.key == key)
    }

    pub fn lookup_own_property(&self, key: PropertyKey) -> Option<&PropertyDefinition> {
        self.property_definitions_slice()
            .iter()
            .find(|def| def.key == key)
    }

    pub fn iter_own_properties_gc_unsafe<F: FnMut(PropertyKey, PropertyFlags)>(&self, mut f: F) {
        for def in self.property_definitions_slice() {
            f(def.key, def.attributes);
        }
    }

    fn stored_transitions(&self) -> StoredTransitions {
        match self.transitions_or_next_shape {
            None => StoredTransitions::None,
            Some(ptr) if self.flags.has_transitions_vec() => {
                StoredTransitions::Transitions(ptr.cast::<TransitionVec>().to_handle())
            }
            Some(ptr) => StoredTransitions::SimpleAddProperty(ptr.cast::<Shape>().to_handle()),
        }
    }

    /// Remove the prototype object child with the given shape from the list of children.
    fn remove_prototype_object_child_shape(&mut self, shape: HeapPtr<Shape>) {
        debug_assert!(self.is_prototype_object);

        if let Some(mut children) = self.prototype_object_children_shapes {
            for child_opt in children.as_mut_slice() {
                if let Some(child) = child_opt
                    && child.ptr_eq(&shape)
                {
                    *child_opt = None;
                }
            }
        }
    }

    /// Invalidate the current guard for this shape. Note that we do not need to allocate a new
    /// guard since guards are lazily created when requested.
    fn invalidate_own_guard(&mut self) {
        if let Some(mut guard) = self.validity_guard {
            guard.set(Value::bool(false));
            self.validity_guard = None;
        }
    }

    /// Shallowly remove this cloned shape from the transition tree.
    ///
    /// Only safe to call after a shallow clone.
    fn remove_cloned_from_transition_tree(&mut self) {
        self.parent_shape = None;
        self.set_transitions_or_next_shape(None);
    }
}

impl HeapPtr<Shape> {
    /// Invalidate the guard for this shape and all shapes with this shape in their prototype chain.
    ///
    /// Note that guards are lazily created when requested.
    fn invalidate(&mut self) {
        self.invalidate_impl(/* unregister */ false);
    }

    fn invalidate_and_unregister(&mut self) {
        self.invalidate_impl(/* unregister */ true);
    }

    fn invalidate_impl(&mut self, unregister: bool) {
        // Safely visit the entire subtree with DFS
        let mut queue = vec![*self];
        while let Some(mut shape) = queue.pop() {
            shape.invalidate_own_guard();

            // Visit all direct prototype object children of this prototype object shape that have
            // been registered for guard invalidation.
            if let Some(mut children) = shape.prototype_object_children_shapes {
                for child_opt in children.as_mut_slice().iter_mut() {
                    if let Some(mut child) = *child_opt {
                        queue.push(child);

                        if unregister {
                            child.flags.set_is_registered_prototype_child(false);
                            *child_opt = None;
                        }
                    }
                }
            }
        }
    }

    pub fn invalidate_if_prototype_object(&mut self) {
        if self.is_prototype_object {
            self.invalidate();
        }
    }
}

impl Handle<Shape> {
    #[inline]
    fn transition_vec_field(&self) -> TransitionVecField {
        TransitionVecField(*self)
    }

    #[inline]
    fn property_definitions_vec_field(&self) -> PropertyDefinitionVecField {
        PropertyDefinitionVecField(*self)
    }

    #[inline]
    fn prototype_object_children_shapes_vec_field(&self) -> PrototypeObjectChildrenShapesVecField {
        PrototypeObjectChildrenShapesVecField(*self)
    }

    fn shallow_clone(&self, cx: Context) -> AllocResult<HeapPtr<Shape>> {
        let cloned = cx.alloc_uninit::<Shape>()?;
        unsafe { std::ptr::copy_nonoverlapping(self.as_ptr(), cloned.as_ptr(), 1) };

        Ok(cloned)
    }

    /// Create a shallow clone of this object shape and set its prototype.
    pub fn clone_for_transition_tree_root(
        &self,
        cx: Context,
        prototype: Option<Handle<ObjectValue>>,
    ) -> AllocResult<HeapPtr<Shape>> {
        let mut cloned = self.shallow_clone(cx)?;
        cloned.prototype = prototype.map(|p| *p);

        Ok(cloned)
    }

    /// Create a shallow clone of this shape for use in a transition. Shape is memcopied
    /// to a new allocation with all fields preserved except for the transitions.
    fn clone_for_transition(&self, cx: Context) -> AllocResult<HeapPtr<Shape>> {
        let mut cloned = self.shallow_clone(cx)?;

        // The new shape is a child of this shape but starts with no transitions of its own.
        cloned.parent_shape = Some(**self);
        cloned.set_transitions_or_next_shape(None);

        Ok(cloned)
    }

    /// Create a shallow clone of this shape except converted to a prototype object shape.
    fn clone_as_prototype_object_shape(&self, cx: Context) -> AllocResult<Handle<Shape>> {
        let mut cloned = self.shallow_clone(cx)?;

        // Convert to a prototype object shape
        cloned.is_prototype_object = true;
        cloned.remove_cloned_from_transition_tree();

        // Create a new copy of all prototype definitions so that they can be modified in place
        let mut cloned = cloned.to_handle();
        cloned.clone_property_definitions_in_place(cx)?;

        Ok(cloned)
    }

    /// Return the equivalent shape converted to map mode. May allocate a shallow clone or may
    /// mutate this shape in place and return itself.
    pub fn convert_to_map_mode(&mut self, cx: Context) -> AllocResult<HeapPtr<Shape>> {
        fn set_map_mode_fields(cx: Context, mut shape: HeapPtr<Shape>) {
            shape.flags.set_is_map_mode(true);
            shape.property_definitions = cx.shapes.default_property_definitions;
            shape.array_mode_property_count = 0;
        }

        // Prototype shapes are always mutated in place, and are not part of the transition tree
        if self.is_prototype_object {
            set_map_mode_fields(cx, **self);

            // Changing to map mode invalidates all shapes with this shape in their prototype chain
            // since lookups may be changed.
            self.invalidate();

            return Ok(**self);
        }

        // All other shapes need a shallow clone removed from the transition tree
        let mut cloned = self.shallow_clone(cx)?;
        set_map_mode_fields(cx, cloned);
        cloned.remove_cloned_from_transition_tree();

        Ok(cloned)
    }

    /// Replace the property definitions with a cloned copy of the existing definitions.
    fn clone_property_definitions_in_place(&mut self, cx: Context) -> AllocResult<()> {
        let count = self.array_mode_property_count as usize;
        let capacity = self.property_definitions.capacity();

        // Cloned vec has the same capacity but we only need to copy over the first `count`
        // properties that have actually been set.
        let mut new_property_definitions = PropertyDefinitionVec::new(cx, capacity)?;
        new_property_definitions.set_len(count);
        new_property_definitions
            .as_mut_slice()
            .copy_from_slice(&self.property_definitions.as_slice()[..count]);

        self.property_definitions = new_property_definitions;

        Ok(())
    }

    /// Define a property with the given attributes on this shape. Property may already exist
    /// in which case the attributes are modified.
    ///
    /// Return the new shape which includes the property. Add transitions if necessary.
    pub fn define_own_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        attributes: PropertyFlags,
    ) -> AllocResult<(HeapPtr<Shape>, DefinePropertyLocation)> {
        // If this is a prototype object shape then no transitions are needed. Prototype object
        // shapes have their own copy of the property definitions and will modify them in place.
        if self.is_prototype_object {
            let existing_property_index = self.lookup_own_property_index(*key);
            let location = if let Some(index) = existing_property_index {
                // Modifying an existing property.
                let property_definition = self.get_property_at_index_mut(index);
                property_definition.attributes = attributes;

                DefinePropertyLocation::Location(property_definition.location)
            } else {
                // Adding a new property. Ensure we have room for another property in array mode.
                if self.array_mode_property_count as usize >= MAX_ARRAY_PROPERTIES {
                    return Ok((**self, DefinePropertyLocation::EnterMapMode));
                }

                // Property is stored at the next open index
                let index = self.array_mode_property_count;
                let location = PropertyLocation::PropertyArray { index };
                self.array_mode_property_count += 1;

                self.property_definitions_vec_field()
                    .maybe_grow_for_push(cx)?
                    .push_without_growing(PropertyDefinition { key: *key, location, attributes });

                DefinePropertyLocation::NewArrayProperty
            };

            // Any change invalidates all shapes with this shape in their prototype chain
            self.invalidate();

            return Ok((**self, location));
        }

        // Check if this property already exists and is not modified, in which case no transition
        // is needed.
        let existing_property_index = self.lookup_own_property_index(*key);
        if let Some(index) = existing_property_index {
            let existing_property = self.get_property_at_index(index);
            if existing_property.attributes == attributes {
                let location = DefinePropertyLocation::Location(existing_property.location);
                return Ok((**self, location));
            }
        }

        // A transition is needed, so see if one already exists for this property and attributes
        let stored_transitions = self.stored_transitions();

        match stored_transitions {
            StoredTransitions::None => {}
            // A simple add property transition refers to the last property of the next shape, see
            // if it matches.
            StoredTransitions::SimpleAddProperty(next_shape) => {
                let property_definition = next_shape.get_last_property_definition();
                if property_definition.key == *key && property_definition.attributes == attributes {
                    return Ok((*next_shape, DefinePropertyLocation::NewArrayProperty));
                }
            }
            // If there is a transition array then search for a matching DefineProperty transition
            StoredTransitions::Transitions(transitions) => {
                for transition in transitions.as_slice() {
                    if let TransitionKind::DefineProperty {
                        key: transition_key,
                        attributes: transition_attributes,
                    } = transition.kind()
                    {
                        if transition_key == *key && transition_attributes == attributes {
                            let new_shape = transition.next_shape_ptr();

                            // Check if the transition is for a property addition or modification by
                            // comparing the number of properties before and after.
                            let added_property =
                                self.num_properties() + 1 == new_shape.num_properties();

                            let location = if added_property {
                                DefinePropertyLocation::NewArrayProperty
                            } else {
                                let property_definition =
                                    new_shape.lookup_own_property(*key).unwrap();
                                DefinePropertyLocation::Location(property_definition.location)
                            };

                            return Ok((new_shape, location));
                        }
                    }
                }

                // A new transition is needed. Check if there is room for another.
                if transitions.len() >= MAX_TRANSITIONS {
                    return Ok((**self, DefinePropertyLocation::EnterMapMode));
                }
            }
        }

        // Otherwise a new transition is needed so create a clone of this shape to start with
        let mut new_shape = self.clone_for_transition(cx)?.to_handle();

        let location = if let Some(index) = existing_property_index {
            // Modifying an existing property.
            //
            // First create a clone of the property definitions, then modify the attributes for the
            // relevant property.
            new_shape.clone_property_definitions_in_place(cx)?;
            let property_definition = new_shape.get_property_at_index_mut(index);
            property_definition.attributes = attributes;

            DefinePropertyLocation::Location(property_definition.location)
        } else {
            // Adding a new property. Ensure we have room for another property in array mode.
            if self.array_mode_property_count as usize >= MAX_ARRAY_PROPERTIES {
                return Ok((**self, DefinePropertyLocation::EnterMapMode));
            }

            // Property is stored at the next open index
            let index = new_shape.array_mode_property_count;
            let location = PropertyLocation::PropertyArray { index };

            // Property will be appended to shared array if the next slot hasn't already been used
            if new_shape.property_definitions.len() > new_shape.array_mode_property_count as usize {
                new_shape.clone_property_definitions_in_place(cx)?;
            }

            new_shape.array_mode_property_count += 1;

            // Append property definition to new shape's array, which may be shared or new
            new_shape
                .property_definitions_vec_field()
                .maybe_grow_for_push(cx)?
                .push_without_growing(PropertyDefinition { key: *key, location, attributes });

            DefinePropertyLocation::NewArrayProperty
        };

        match stored_transitions {
            // This is the first transition for this shape so create a simple add property
            // transition by linking directly to the new shape if possible
            StoredTransitions::None => {
                if existing_property_index.is_none() {
                    self.set_transitions_or_next_shape(Some(new_shape.as_any()));
                } else {
                    // Otherwise this is modifying an existing property so start a transition array
                    let mut transitions = TransitionVec::new_initial(cx)?;
                    transitions.push_without_growing(Transition::new_define_property(
                        *key, attributes, *new_shape,
                    ));
                    self.set_transitions_or_next_shape(Some(transitions.as_any()));
                }
            }
            // There was a single simple add property transition. Convert to a transition array.
            StoredTransitions::SimpleAddProperty(next_shape) => {
                let mut transitions =
                    self.convert_simple_property_transition_to_array(cx, next_shape)?;
                transitions.push_without_growing(Transition::new_define_property(
                    *key, attributes, *new_shape,
                ));
            }
            // There is already a transition array so append this transition to it
            StoredTransitions::Transitions(_) => self
                .transition_vec_field()
                .maybe_grow_for_push(cx)?
                .push_without_growing(Transition::new_define_property(
                    *key, attributes, *new_shape,
                )),
        }

        Ok((*new_shape, location))
    }

    /// Set the prototype object for this shape.
    ///
    /// Return the new shape which includes the prototype object. Add transitions if necessary.
    pub fn set_prototype(
        &mut self,
        cx: Context,
        prototype: Option<Handle<ObjectValue>>,
    ) -> AllocResult<TransitionResult> {
        // If the prototype is the same then no transition is needed
        if are_prototypes_equal(self.prototype, prototype.map(|p| *p)) {
            return Ok(TransitionResult::Transitioned(**self));
        }

        // Shapes which have been detached from the transition tree
        if self.is_prototype_object {
            // If this shape is for a prototype object then remove its old registration from its
            // old prototype object shape.
            if let Some(old_prototype) = self.prototype {
                let mut old_prototype_shape = old_prototype.shape_ptr();
                if old_prototype_shape.is_prototype_object {
                    old_prototype_shape.remove_prototype_object_child_shape(**self);
                }

                // Mark self as unregistered. It will be lazily re-registed when a validity guard
                // that requires it is requested.
                self.flags.set_is_registered_prototype_child(false);
            }

            // Do not register in the prototype shape's list of children yet, nor ensure that the
            // prototype shape is actually a prototype object shape. Conversion and registration
            // happens lazily when a validity guard that requires it is requested.

            // Finally mutate prototype object shape in place
            self.prototype = prototype.map(|p| *p);

            // Changing the prototype invalidates and unregisters all shapes with this shape in
            // their prototype. The unregistration is necessary to keep the invariant that a
            // registered shape must have its entire prototype chain properly registered.
            self.invalidate_and_unregister();

            return Ok(TransitionResult::Transitioned(**self));
        } else if self.is_map_mode() {
            self.prototype = prototype.map(|p| *p);
            return Ok(TransitionResult::Transitioned(**self));
        }

        // A transition is needed, so see if one already exists for this prototype
        let stored_transitions = self.stored_transitions();

        if let StoredTransitions::Transitions(transitions) = stored_transitions {
            for transition in transitions.as_slice() {
                if let TransitionKind::SetPrototype { prototype: transition_prototype } =
                    transition.kind()
                {
                    if are_prototypes_equal(transition_prototype, prototype.map(|p| *p)) {
                        return Ok(TransitionResult::Transitioned(transition.next_shape_ptr()));
                    }
                }
            }

            // A new transition is needed. Check if there is room for another.
            if transitions.len() >= MAX_TRANSITIONS {
                return Ok(TransitionResult::EnterMapMode);
            }
        }

        // Otherwise a new transition is needed so create a clone of this shape to start with
        let mut new_shape = self.clone_for_transition(cx)?.to_handle();
        new_shape.prototype = prototype.map(|p| *p);

        self.append_generic_transition(cx, stored_transitions, || {
            Transition::new_set_prototype(prototype.map(|p| *p), *new_shape)
        })?;

        Ok(TransitionResult::Transitioned(*new_shape))
    }

    /// Prevent extension on the object described by this shape.
    ///
    /// Return the new shape which includes the non-extensible flag. Add transitions if necessary.
    pub fn prevent_extensions(&mut self, cx: Context) -> AllocResult<TransitionResult> {
        // If this shape is already non-extensible then no transition is needed
        if !self.is_extensible() {
            return Ok(TransitionResult::Transitioned(**self));
        }

        // If this is a prototype object shape then no transitions are needed
        if self.is_prototype_object {
            // Finally mutate prototype object shape in place
            self.flags.set_is_extensible(false);

            // Changing the shape invalidates all shapes with this shape in their prototype chain
            self.invalidate();

            return Ok(TransitionResult::Transitioned(**self));
        } else if self.is_map_mode() {
            self.flags.set_is_extensible(false);
            return Ok(TransitionResult::Transitioned(**self));
        }

        // A transition is needed, so see if one already exists for preventing extensions
        let stored_transitions = self.stored_transitions();

        if let StoredTransitions::Transitions(transitions) = stored_transitions {
            for transition in transitions.as_slice() {
                if let TransitionKind::PreventExtensions = transition.kind() {
                    return Ok(TransitionResult::Transitioned(transition.next_shape_ptr()));
                }
            }

            // A new transition is needed. Check if there is room for another.
            if transitions.len() >= MAX_TRANSITIONS {
                return Ok(TransitionResult::EnterMapMode);
            }
        }

        // Otherwise a new transition is needed so create a clone of this shape to start with
        let mut new_shape = self.clone_for_transition(cx)?.to_handle();
        new_shape.flags.set_is_extensible(false);

        self.append_generic_transition(cx, stored_transitions, || {
            Transition::new_prevent_extensions(*new_shape)
        })?;

        Ok(TransitionResult::Transitioned(*new_shape))
    }

    /// Replace a simple property transition with the equivalent transition array.
    ///
    /// Return the transition array. It is guaranteed to have room for at least one more transition
    /// without needing to grow the array.
    fn convert_simple_property_transition_to_array(
        &mut self,
        cx: Context,
        next_shape: Handle<Shape>,
    ) -> AllocResult<HeapPtr<TransitionVec>> {
        let mut transitions = TransitionVec::new_initial(cx)?;

        let first_property_definition = next_shape.get_last_property_definition();
        transitions.push_without_growing(Transition::new_define_property(
            first_property_definition.key,
            first_property_definition.attributes,
            *next_shape,
        ));

        self.set_transitions_or_next_shape(Some(transitions.as_any()));

        Ok(transitions)
    }

    /// Append a (non-property) transition to the set of transitions in this shape.
    ///
    /// Handles all forms of transition storage.
    fn append_generic_transition(
        &mut self,
        cx: Context,
        stored_transitions: StoredTransitions,
        mut create_transition: impl FnMut() -> Transition,
    ) -> AllocResult<()> {
        match stored_transitions {
            // This is the first transition for this shape so create the transition array.
            StoredTransitions::None => {
                let mut transitions = TransitionVec::new_initial(cx)?;
                transitions.push_without_growing(create_transition());
                self.set_transitions_or_next_shape(Some(transitions.as_any()));
            }
            // There was a single simple property transition. Convert to a transition array.
            StoredTransitions::SimpleAddProperty(next_shape) => {
                let mut transitions =
                    self.convert_simple_property_transition_to_array(cx, next_shape)?;
                transitions.push_without_growing(create_transition());
            }
            // There is already a transition array so append this transition to it
            StoredTransitions::Transitions(_) => {
                self.transition_vec_field()
                    .maybe_grow_for_push(cx)?
                    .push_without_growing(create_transition());
            }
        }

        Ok(())
    }

    /// Request a validity guard for the entire prototype chain starting at this prototype object
    /// shape.
    pub fn request_validity_guard(&mut self, cx: Context) -> AllocResult<HeapPtr<BoxedValue>> {
        debug_assert!(self.is_prototype_object);

        let mut shape = *self;

        // First ensure that the entire prototype chain is set up for guard invalidation. This means
        // the entire prototype chain's shapes are prototype object shapes and correctly registered
        // in their parent shapes. Registered flag allows us to early exit the walk since its
        // invariant ensure the rest of the prototype chain is already registered.
        while !shape.flags.is_registered_prototype_child() {
            if let Some(prototype) = shape.prototype {
                let mut prototype = prototype.to_handle();
                let mut prototype_shape = prototype.shape();

                // Ensure each shape in the prototype chain is a prototype object shape
                if !prototype_shape.is_prototype_object {
                    prototype_shape = prototype_shape.clone_as_prototype_object_shape(cx)?;
                    prototype.set_shape(*prototype_shape);
                }

                prototype_shape.register_prototype_object_child_shape(cx, shape)?;
                shape.flags.set_is_registered_prototype_child(true);

                // Move up the prototype chain
                shape = prototype_shape;
            } else {
                // Registered flag remains unset for the top of the prototype chain
                break;
            }
        }

        // Return the guard for this prototype object shape, lazily creating it if necessary
        if let Some(guard) = self.validity_guard {
            Ok(guard)
        } else {
            let guard = BoxedValue::new(cx, cx.bool(true))?;
            self.validity_guard = Some(guard);
            Ok(guard)
        }
    }

    fn register_prototype_object_child_shape(
        &mut self,
        cx: Context,
        child_shape: Handle<Shape>,
    ) -> AllocResult<()> {
        debug_assert!(self.is_prototype_object);

        if self.prototype_object_children_shapes.is_none() {
            let new_children = PrototypeObjectChildrenShapesVec::new_initial(cx)?;
            self.prototype_object_children_shapes = Some(new_children);
        }

        self.prototype_object_children_shapes_vec_field()
            .maybe_grow_for_push(cx)?
            .push_without_growing(Some(*child_shape));

        Ok(())
    }
}

pub enum DefinePropertyLocation {
    Location(PropertyLocation),
    NewArrayProperty,
    EnterMapMode,
}

pub enum TransitionResult {
    Transitioned(HeapPtr<Shape>),
    EnterMapMode,
}

impl_vec_instance!(TransitionVec, Transition);

struct TransitionVecField(Handle<Shape>);

impl HeapItem for TransitionVec {
    fn byte_size(vec: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(vec.capacity())
    }

    fn visit_pointers(mut vec: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        vec.visit_vec_pointers(visitor);

        for transition in vec.as_mut_slice() {
            transition.visit_pointers(visitor);
        }
    }
}

impl BsVecField<TransitionVec> for TransitionVecField {
    fn get(&self) -> HeapPtr<TransitionVec> {
        self.0
            .transitions_or_next_shape
            .unwrap()
            .cast::<TransitionVec>()
    }
    fn set_new(&mut self, cx: Context, capacity: usize) -> AllocResult<HeapPtr<TransitionVec>> {
        let new_vec = TransitionVec::new(cx, capacity)?;

        // Safe to not call `set_transitions_or_next_shape` since this is still a TransitionVec.
        self.0.transitions_or_next_shape = Some(new_vec.cast::<AnyHeapItem>());

        Ok(new_vec)
    }
}

impl_vec_instance!(PropertyDefinitionVec, PropertyDefinition);

impl HeapItem for PropertyDefinitionVec {
    fn byte_size(vec: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(vec.capacity())
    }

    fn visit_pointers(mut vec: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        vec.visit_vec_pointers(visitor);

        for property_definition in vec.as_mut_slice() {
            property_definition.visit_pointers(visitor);
        }
    }
}

struct PropertyDefinitionVecField(Handle<Shape>);

impl BsVecField<PropertyDefinitionVec> for PropertyDefinitionVecField {
    fn get(&self) -> HeapPtr<PropertyDefinitionVec> {
        self.0.property_definitions
    }

    fn set_new(
        &mut self,
        cx: Context,
        capacity: usize,
    ) -> AllocResult<HeapPtr<PropertyDefinitionVec>> {
        let new_vec = PropertyDefinitionVec::new(cx, capacity)?;
        self.0.property_definitions = new_vec;
        Ok(new_vec)
    }
}

// A growable array of weakly held shapes.
//
// Garbage collector is aware of this type and compresses it in place during collection.
impl_vec_instance!(
    PrototypeObjectChildrenShapesVec,
    Option<HeapPtr<Shape>>,
    PrototypeObjectChildrenShapesVecExtraStorage
);

/// Extra header data for a weak value vec, storing the intrusive list of weak vecs for GC.
pub struct PrototypeObjectChildrenShapesVecExtraStorage {
    /// Holds the address of the next weak vec that has been visited during garbage collection.
    /// Unused outside of garbage collection.
    next_weak_vec: Option<HeapPtr<PrototypeObjectChildrenShapesVec>>,
}

impl PrototypeObjectChildrenShapesVec {
    /// Create a new PrototypeObjectChildrenShapesVec with the given capacity.
    ///
    /// This is the only PrototypeObjectChildrenShapesVec creation function that should be used.
    pub fn new(cx: Context, capacity: usize) -> AllocResult<HeapPtr<Self>> {
        let mut vec = <Self as VecInstance>::new(cx, capacity)?;

        set_uninit!(vec.extra_data_mut().next_weak_vec, None);

        Ok(vec)
    }

    pub fn next_weak_vec(&self) -> Option<HeapPtr<Self>> {
        self.extra_data().next_weak_vec
    }

    pub fn set_next_weak_vec(&mut self, next_weak_vec: Option<HeapPtr<Self>>) {
        self.extra_data_mut().next_weak_vec = next_weak_vec;
    }
}

impl HeapItem for PrototypeObjectChildrenShapesVec {
    fn byte_size(vec: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(vec.capacity())
    }

    fn visit_pointers(mut vec: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        vec.visit_vec_pointers(visitor);

        for weak_shape in vec.as_mut_slice() {
            visitor.visit_weak_pointer_opt(weak_shape);
        }
    }
}

struct PrototypeObjectChildrenShapesVecField(Handle<Shape>);

impl BsVecField<PrototypeObjectChildrenShapesVec> for PrototypeObjectChildrenShapesVecField {
    fn get(&self) -> HeapPtr<PrototypeObjectChildrenShapesVec> {
        self.0.prototype_object_children_shapes.unwrap()
    }

    fn set_new(
        &mut self,
        cx: Context,
        capacity: usize,
    ) -> AllocResult<HeapPtr<PrototypeObjectChildrenShapesVec>> {
        let new_vec = PrototypeObjectChildrenShapesVec::new(cx, capacity)?;
        self.0.prototype_object_children_shapes = Some(new_vec);
        Ok(new_vec)
    }
}

/// Whether two optional prototype objects are equal.
fn are_prototypes_equal(
    p1: Option<HeapPtr<ObjectValue>>,
    p2: Option<HeapPtr<ObjectValue>>,
) -> bool {
    match (p1, p2) {
        (None, None) => true,
        (Some(p1), Some(p2)) => p1.ptr_eq(&p2),
        _ => false,
    }
}

impl HeapItem for Shape {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        size_of::<Shape>()
    }

    fn visit_pointers(mut shape: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut shape.shape);
        visitor.visit_rust_vtable_pointer(&mut shape.vtable);
        visitor.visit_pointer_opt(&mut shape.prototype);
        visitor.visit_pointer(&mut shape.property_definitions);

        // Strong references up path of parent shapes to root
        visitor.visit_pointer_opt(&mut shape.parent_shape);

        // Strong reference to the TransitionVec but a weak reference directly to the next shape.
        // We cannot safely check the heap item's kind to distinguish the two, so we store a flag to
        // indicate which is used.
        let has_transitions_vec = shape.flags.has_transitions_vec();
        if let Some(pointer) = shape.transitions_or_next_shape.as_mut() {
            if has_transitions_vec {
                visitor.visit_pointer(pointer);
            } else {
                visitor.visit_weak_pointer(pointer);
            }
        }

        visitor.visit_pointer_opt(&mut shape.prototype_object_children_shapes);
        visitor.visit_pointer_opt(&mut shape.validity_guard);
    }
}
