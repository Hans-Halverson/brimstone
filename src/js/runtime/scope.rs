use crate::{field_offset, js::runtime::object_descriptor::ObjectKind, maybe, set_uninit};

use super::{
    abstract_operations::has_property,
    collections::InlineArray,
    gc::{HeapObject, HeapVisitor},
    get,
    object_descriptor::ObjectDescriptor,
    object_value::ObjectValue,
    scope_names::ScopeNames,
    string_value::StringValue,
    Context, EvalResult, Handle, HeapPtr, PropertyKey, Value,
};

#[repr(C)]
pub struct Scope {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Parent scope, forming a chain of scopes up to the global scope.
    parent: Option<HeapPtr<Scope>>,
    /// Names of the slots in this scope.
    scope_names: HeapPtr<ScopeNames>,
    /// Object containing scope bindings if this is a global or with scope.
    object: Option<HeapPtr<ObjectValue>>,
    /// Inline array of slots for variables in this scope.
    slots: InlineArray<Value>,
}

impl Scope {
    pub fn new_global(cx: Context, global_object: Handle<ObjectValue>) -> Handle<Scope> {
        // Use an empty set of scope names
        let scope_names = ScopeNames::new(cx, &[]);

        let num_slots = scope_names.len();
        let size = Self::calculate_size_in_bytes(num_slots);
        let mut scope = cx.alloc_uninit_with_size::<Scope>(size);

        set_uninit!(scope.descriptor, cx.base_descriptors.get(ObjectKind::Scope));
        set_uninit!(scope.parent, None);
        set_uninit!(scope.scope_names, scope_names.get_());
        set_uninit!(scope.object, Some(global_object.get_()));

        scope.slots.init_with(num_slots, Value::undefined());

        scope.to_handle()
    }

    pub fn new_lexical(
        cx: Context,
        parent: Handle<Scope>,
        scope_names: Handle<ScopeNames>,
    ) -> Handle<Scope> {
        let num_slots = scope_names.len();
        let size = Self::calculate_size_in_bytes(num_slots);
        let mut scope = cx.alloc_uninit_with_size::<Scope>(size);

        set_uninit!(scope.descriptor, cx.base_descriptors.get(ObjectKind::Scope));
        set_uninit!(scope.parent, Some(parent.get_()));
        set_uninit!(scope.scope_names, scope_names.get_());
        set_uninit!(scope.object, None);

        scope.slots.init_with(num_slots, Value::undefined());

        scope.to_handle()
    }

    const SLOTS_OFFSET: usize = field_offset!(Scope, slots);

    fn calculate_size_in_bytes(num_slots: usize) -> usize {
        Self::SLOTS_OFFSET + InlineArray::<Value>::calculate_size_in_bytes(num_slots)
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

                if maybe!(has_property(cx, object_handle, key)) {
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

                if maybe!(has_property(cx, object_handle, key)) {
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
