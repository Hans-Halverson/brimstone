use crate::{field_offset, js::runtime::object_descriptor::ObjectKind, maybe, set_uninit};

use super::{
    abstract_operations::has_property,
    collections::InlineArray,
    gc::{HeapObject, HeapVisitor},
    get,
    object_descriptor::ObjectDescriptor,
    object_value::ObjectValue,
    ordinary_object::ordinary_object_create,
    scope_names::ScopeNames,
    string_value::StringValue,
    type_utilities::to_boolean,
    Context, EvalResult, Handle, HeapPtr, PropertyKey, Value,
};

#[repr(C)]
pub struct Scope {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// The type of this scope
    kind: ScopeKind,
    /// Parent scope, forming a chain of scopes up to the global scope.
    parent: Option<HeapPtr<Scope>>,
    /// Names of the slots in this scope.
    scope_names: HeapPtr<ScopeNames>,
    /// Object containing scope bindings if this is a global or with scope.
    object: Option<HeapPtr<ObjectValue>>,
    /// Inline array of slots for variables in this scope.
    slots: InlineArray<Value>,
}

#[derive(Copy, Clone, PartialEq)]
pub enum ScopeKind {
    Global,
    Lexical,
    /// A function scope is treated as a var scope when looking up the containing var scope.
    Function,
    With,
}

impl Scope {
    #[inline]
    fn new(
        cx: Context,
        kind: ScopeKind,
        parent: Option<Handle<Scope>>,
        scope_names: Handle<ScopeNames>,
        object: Option<Handle<ObjectValue>>,
    ) -> Handle<Scope> {
        let num_slots = scope_names.len();
        let size = Self::calculate_size_in_bytes(num_slots);
        let mut scope = cx.alloc_uninit_with_size::<Scope>(size);

        set_uninit!(scope.descriptor, cx.base_descriptors.get(ObjectKind::Scope));
        set_uninit!(scope.kind, kind);
        set_uninit!(scope.parent, parent.map(|p| p.get_()));
        set_uninit!(scope.scope_names, scope_names.get_());
        set_uninit!(scope.object, object.map(|o| o.get_()));

        scope.slots.init_with(num_slots, Value::undefined());

        scope.to_handle()
    }

    pub fn new_global(cx: Context, global_object: Handle<ObjectValue>) -> Handle<Scope> {
        // Use an empty set of scope names
        let scope_names = ScopeNames::new(cx, &[]);
        Self::new(cx, ScopeKind::Global, None, scope_names, Some(global_object))
    }

    pub fn new_lexical(
        cx: Context,
        parent: Handle<Scope>,
        scope_names: Handle<ScopeNames>,
    ) -> Handle<Scope> {
        Self::new(cx, ScopeKind::Lexical, Some(parent), scope_names, None)
    }

    pub fn new_function(
        cx: Context,
        parent: Handle<Scope>,
        scope_names: Handle<ScopeNames>,
    ) -> Handle<Scope> {
        Self::new(cx, ScopeKind::Function, Some(parent), scope_names, None)
    }

    pub fn new_with(
        cx: Context,
        parent: Handle<Scope>,
        scope_names: Handle<ScopeNames>,
        object: Handle<ObjectValue>,
    ) -> Handle<Scope> {
        Self::new(cx, ScopeKind::With, Some(parent), scope_names, Some(object))
    }

    const SLOTS_OFFSET: usize = field_offset!(Scope, slots);

    fn calculate_size_in_bytes(num_slots: usize) -> usize {
        Self::SLOTS_OFFSET + InlineArray::<Value>::calculate_size_in_bytes(num_slots)
    }

    #[inline]
    pub fn kind(&self) -> ScopeKind {
        self.kind
    }

    #[inline]
    pub fn object_ptr(&self) -> HeapPtr<ObjectValue> {
        self.object.unwrap()
    }

    #[inline]
    pub fn object(&self) -> Handle<ObjectValue> {
        self.object_ptr().to_handle()
    }

    #[inline]
    pub fn parent_ptr(&self) -> HeapPtr<Scope> {
        self.parent.unwrap()
    }

    #[inline]
    pub fn scope_names_ptr(&self) -> HeapPtr<ScopeNames> {
        self.scope_names
    }

    #[inline]
    pub fn get_slot(&self, index: usize) -> Value {
        self.slots.as_slice()[index]
    }

    #[inline]
    pub fn set_slot(&mut self, index: usize, value: Value) {
        self.slots.as_mut_slice()[index] = value;
    }
}

impl Handle<Scope> {
    /// Create a new scope that is an exact duplicate of this scope.
    pub fn duplicate(&self, cx: Context) -> HeapPtr<Scope> {
        let size = Scope::calculate_size_in_bytes(self.slots.len());
        let new_scope = cx.alloc_uninit_with_size::<Scope>(size);

        // Can copy the memory directly
        unsafe {
            std::ptr::copy_nonoverlapping(
                self.as_ptr() as *const u8,
                new_scope.as_ptr() as *mut u8,
                size,
            )
        }

        new_scope
    }

    /// Dynamically look up a name in this scope, walking the scope chain until found. The name must
    /// be an interned string.
    #[inline]
    pub fn lookup(
        &self,
        cx: Context,
        name: Handle<StringValue>,
    ) -> EvalResult<Option<Handle<Value>>> {
        // Reuse handles while walking scope chain
        let mut object_handle = Handle::<ObjectValue>::empty(cx);
        let mut scope = Handle::<Scope>::empty(cx);
        scope.replace(self.get_());

        loop {
            // First check inline slots using scope names table
            let scope_names = scope.scope_names_ptr();
            if let Some(index) = scope_names.lookup_name(name.as_flat().get_()) {
                let value = scope.get_slot(index).to_handle(cx);
                return Some(value).into();
            }

            // Then check scope object if one exists
            if let Some(object) = scope.object {
                object_handle.replace(object);

                // Name is an interned string (and cannot be a number) so is already a property key
                let key = name.cast::<PropertyKey>();

                if maybe!(self.has_object_binding(cx, object_handle, key)) {
                    let value = maybe!(get(cx, object_handle, key));
                    return Some(value).into();
                }
            }

            // Otherwise move to parent scope
            if let Some(parent) = scope.parent.as_ref() {
                scope.replace(*parent);
            } else {
                return None.into();
            }
        }
    }

    /// Dynamically store a name in this scope, walking the scope chain until found. Return whether
    /// the name was found. The name must be an interned string.
    #[inline]
    pub fn lookup_store(
        &mut self,
        cx: Context,
        name: Handle<StringValue>,
        value: Handle<Value>,
    ) -> EvalResult<bool> {
        // Reuse handles while walking scope chain
        let mut object_handle = Handle::<ObjectValue>::empty(cx);
        let mut scope = Handle::<Scope>::empty(cx);
        scope.replace(self.get_());

        loop {
            // First check inline slots using scope names table
            let scope_names = scope.scope_names_ptr();
            if let Some(index) = scope_names.lookup_name(name.as_flat().get_()) {
                scope.set_slot(index, value.get());
                return true.into();
            }

            // Then check scope object if one exists
            if let Some(object) = scope.object {
                object_handle.replace(object);

                // Name is an interned string (and cannot be a number) so is already a property key
                let key = name.cast::<PropertyKey>();

                if maybe!(self.has_object_binding(cx, object_handle, key)) {
                    return object_handle.set(cx, key, value, object_handle.into());
                }
            }

            // Otherwise move to parent scope
            if let Some(parent) = scope.parent.as_ref() {
                scope.replace(*parent);
            } else {
                return false.into();
            }
        }
    }

    /// For scopes with an object, return whether the key is a binding in the object.
    ///
    /// For with statements, we must ignore keys that are in object[Symbol.unscopables].
    fn has_object_binding(
        &self,
        cx: Context,
        object: Handle<ObjectValue>,
        key: Handle<PropertyKey>,
    ) -> EvalResult<bool> {
        // Check if key appears in object
        if !maybe!(has_property(cx, object, key)) {
            return false.into();
        }

        if self.kind != ScopeKind::With {
            return true.into();
        }

        // With statements must also ignore properties in @@unscopables
        let unscopables_key = cx.well_known_symbols.unscopables();
        let unscopables = maybe!(get(cx, object, unscopables_key));
        if unscopables.is_object() {
            let unscopables = unscopables.as_object();

            let value = maybe!(get(cx, unscopables, key));
            let blocked = to_boolean(value.get());
            if blocked {
                return false.into();
            }
        }

        true.into()
    }

    /// Return the object for this scope, creating it if necessary.
    pub fn ensure_scope_object(&mut self, cx: Context) -> Handle<ObjectValue> {
        if let Some(object) = self.object {
            return object.to_handle();
        }

        let object = ordinary_object_create(cx);
        self.object = Some(object.get_());
        object
    }
}

impl HeapObject for HeapPtr<Scope> {
    fn byte_size(&self) -> usize {
        Scope::calculate_size_in_bytes(self.slots.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer_opt(&mut self.parent);
        visitor.visit_pointer(&mut self.scope_names);
        visitor.visit_pointer_opt(&mut self.object);

        for slot in self.slots.as_mut_slice() {
            visitor.visit_value(slot);
        }
    }
}
