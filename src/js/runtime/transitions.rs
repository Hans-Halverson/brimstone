use crate::runtime::{
    HeapPtr, PropertyKey, gc::HeapVisitor, object_value::ObjectValue, property::PropertyFlags,
    shape::Shape,
};

#[derive(Clone, Copy)]
#[repr(C)]
pub struct PropertyDefinition {
    pub key: PropertyKey,
    pub location: PropertyLocation,
    pub attributes: PropertyFlags,
}

impl PropertyDefinition {
    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_property_key(&mut self.key);
    }
}

#[derive(Clone, Copy)]
/// Where a property is stored on an object.
pub enum PropertyLocation {
    /// Property is stored in the named properties array at this index.
    PropertyArray { index: u16 },
}

#[derive(Clone, Copy)]
pub enum TransitionKind {
    DefineProperty { key: PropertyKey, attributes: PropertyFlags },
    SetPrototype { prototype: Option<HeapPtr<ObjectValue>> },
    PreventExtensions,
}

#[derive(Clone, Copy)]
pub struct Transition {
    kind: TransitionKind,
    next_shape: HeapPtr<Shape>,
}

impl Transition {
    fn new(kind: TransitionKind, next_shape: HeapPtr<Shape>) -> Self {
        Transition { kind, next_shape }
    }

    pub fn new_define_property(
        key: PropertyKey,
        attributes: PropertyFlags,
        next_shape: HeapPtr<Shape>,
    ) -> Self {
        Self::new(TransitionKind::DefineProperty { key, attributes }, next_shape)
    }

    pub fn new_set_prototype(
        prototype: Option<HeapPtr<ObjectValue>>,
        next_shape: HeapPtr<Shape>,
    ) -> Self {
        Self::new(TransitionKind::SetPrototype { prototype }, next_shape)
    }

    pub fn new_prevent_extensions(next_shape: HeapPtr<Shape>) -> Self {
        Self::new(TransitionKind::PreventExtensions, next_shape)
    }

    pub fn kind(&self) -> TransitionKind {
        self.kind
    }

    pub fn kind_mut(&mut self) -> &mut TransitionKind {
        &mut self.kind
    }

    pub fn next_shape_ptr(&self) -> HeapPtr<Shape> {
        self.next_shape
    }

    pub fn set_next_shape(&mut self, next_shape: HeapPtr<Shape>) {
        self.next_shape = next_shape;
    }

    pub fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        match &mut self.kind {
            TransitionKind::DefineProperty { key, .. } => {
                visitor.visit_weak_property_key(key);
            }
            TransitionKind::SetPrototype { prototype } => {
                visitor.visit_weak_pointer_opt(prototype);
            }
            TransitionKind::PreventExtensions => {}
        }

        visitor.visit_weak_pointer(&mut self.next_shape);
    }
}
