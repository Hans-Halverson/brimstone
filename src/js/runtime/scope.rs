use crate::{field_offset, runtime::object_descriptor::ObjectKind, set_uninit};

use super::{
    abstract_operations::has_property,
    boxed_value::BoxedValue,
    collections::InlineArray,
    error::{err_assign_constant, err_cannot_set_property, err_not_defined},
    gc::{HeapItem, HeapObject, HeapVisitor},
    get,
    module::source_text_module::SourceTextModule,
    object_descriptor::ObjectDescriptor,
    object_value::ObjectValue,
    ordinary_object::ordinary_object_create,
    scope_names::ScopeNames,
    string_value::StringValue,
    type_utilities::to_boolean,
    Context, EvalResult, Handle, HeapPtr, PropertyKey, Realm, Value,
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ScopeKind {
    Global,
    Module,
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
        set_uninit!(scope.parent, parent.map(|p| *p));
        set_uninit!(scope.scope_names, *scope_names);
        set_uninit!(scope.object, object.map(|o| *o));

        scope.slots.init_with(num_slots, Value::undefined());

        scope.to_handle()
    }

    pub fn new_global(
        cx: Context,
        scope_names: Handle<ScopeNames>,
        global_object: Handle<ObjectValue>,
    ) -> Handle<Scope> {
        Self::new(cx, ScopeKind::Global, None, scope_names, Some(global_object))
    }

    pub fn new_module(
        cx: Context,
        scope_names: Handle<ScopeNames>,
        global_object: Handle<ObjectValue>,
    ) -> Handle<Scope> {
        Self::new(cx, ScopeKind::Module, None, scope_names, Some(global_object))
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
    pub fn parent(&self) -> Option<HeapPtr<Scope>> {
        self.parent
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

    /// Return the boxed value at the given slot index of a module scope.
    #[inline]
    pub fn get_module_slot(&self, index: usize) -> HeapPtr<BoxedValue> {
        let value = self.get_slot(index);

        debug_assert!(
            value.is_pointer() && value.as_pointer().descriptor().kind() == ObjectKind::BoxedValue
        );

        value.as_pointer().cast::<BoxedValue>()
    }

    /// Set the boxed value at the given slot index of a module scope.
    pub fn set_heap_item_slot(&mut self, index: usize, heap_item: HeapPtr<HeapItem>) {
        self.set_slot(index, Value::heap_item(heap_item));
    }

    /// Return the realm stored in this global scope.
    ///
    /// Should only be called on global scopes.
    #[inline]
    pub fn global_scope_realm(&self) -> HeapPtr<Realm> {
        debug_assert!(self.kind == ScopeKind::Global);
        self.get_slot(0).as_pointer().cast::<Realm>()
    }

    /// Return the source text module stored in this module scope, if one exists.
    ///
    /// Should only be called on module scopes.
    #[inline]
    pub fn module_scope_module(&self) -> Option<HeapPtr<SourceTextModule>> {
        debug_assert!(self.kind == ScopeKind::Module);

        let module = self.get_slot(0).as_pointer();
        if module.descriptor().kind() == ObjectKind::SourceTextModule {
            Some(module.cast::<SourceTextModule>())
        } else {
            None
        }
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
        is_strict: bool,
    ) -> EvalResult<Option<Handle<Value>>> {
        // Reuse handles while walking scope chain
        let mut object_handle = Handle::<ObjectValue>::empty(cx);
        let mut scope = Handle::<Scope>::empty(cx);
        scope.replace(**self);

        loop {
            // First check inline slots using scope names table
            let scope_names = scope.scope_names_ptr();
            if let Some(index) = scope_names.lookup_name(*name.as_flat()) {
                let slot_value = scope.get_slot(index);

                // If found in a module scope then must check if loading a module binding, meaning
                // we need to load the value from the BoxedValue.
                if scope.kind == ScopeKind::Module && scope_names.is_module_binding(index) {
                    let boxed_value = slot_value.as_pointer().cast::<BoxedValue>();
                    return Ok(Some(boxed_value.get().to_handle(cx)));
                } else {
                    return Ok(Some(slot_value.to_handle(cx)));
                }
            }

            // Then check scope object if one exists
            if let Some(object) = scope.object {
                object_handle.replace(object);

                // Name is an interned string (and cannot be a number) so is already a property key
                let key = name.cast::<PropertyKey>();

                if scope.has_object_binding(cx, object_handle, key)? {
                    // Must check if property exists again in GetBindingValue
                    let still_exists = has_property(cx, object_handle, key)?;
                    if !still_exists {
                        if is_strict {
                            return err_not_defined(cx, name);
                        } else {
                            return Ok(Some(cx.undefined()));
                        }
                    }

                    let value = get(cx, object_handle, key)?;
                    return Ok(Some(value));
                }
            }

            // Otherwise move to parent scope
            if let Some(parent) = scope.parent.as_ref() {
                scope.replace(*parent);
            } else {
                // Finally check for global lexical names
                let realm = scope.global_scope_realm();
                let value = realm.get_lexical_name(*name.as_flat());

                return Ok(value.map(|v| v.to_handle(cx)));
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
        is_strict: bool,
    ) -> EvalResult<bool> {
        // Reuse handles while walking scope chain
        let mut object_handle = Handle::<ObjectValue>::empty(cx);
        let mut scope = Handle::<Scope>::empty(cx);
        scope.replace(**self);

        loop {
            // First check inline slots using scope names table
            let scope_names = scope.scope_names_ptr();
            if let Some(index) = scope_names.lookup_name(*name.as_flat()) {
                // Check if storing to a constant
                if scope_names.is_immutable(index) {
                    return err_assign_constant(cx, *name.as_flat());
                }

                // Check if storing to a function expression name
                if scope_names.is_function_expression_name(index) {
                    // Error if reassigning in strict mode, otherwise is a no-op
                    if is_strict {
                        return err_assign_constant(cx, *name.as_flat());
                    } else {
                        return Ok(true);
                    }
                }

                // If found in a module scope then must check if storing to a module binding,
                // meaning we need to store the value in the BoxedValue.
                if scope.kind == ScopeKind::Module && scope_names.is_module_binding(index) {
                    let mut boxed_value = scope.get_module_slot(index);
                    boxed_value.set(*value);
                    return Ok(true);
                }

                scope.set_slot(index, *value);
                return Ok(true);
            }

            // Then check scope object if one exists
            if let Some(object) = scope.object {
                object_handle.replace(object);

                // Name is an interned string (and cannot be a number) so is already a property key
                let key = name.cast::<PropertyKey>();

                if scope.has_object_binding(cx, object_handle, key)? {
                    // Must check if property exists again in SetMutableBinding
                    let still_exists = has_property(cx, object_handle, key)?;
                    if !still_exists && is_strict {
                        return err_not_defined(cx, name);
                    }

                    let success = object_handle.set(cx, key, value, object_handle.into())?;
                    if !success && is_strict {
                        return err_cannot_set_property(cx, name);
                    }

                    // Name was found, even if the set failed
                    return Ok(true);
                }
            }

            // Otherwise move to parent scope
            if let Some(parent) = scope.parent.as_ref() {
                scope.replace(*parent);
            } else {
                // Otherwise check for a global lexical name
                let mut realm = scope.global_scope_realm();
                let success = realm.set_lexical_name(cx, *name.as_flat(), *value)?;

                return Ok(success);
            }
        }
    }

    /// Dynamically delete a binding in this scope, walking the scope chain until found. Return
    /// false only when we failed to delete an own non-configurable property, otherwise return true.
    ///
    /// The name must be an interned string.
    #[inline]
    pub fn lookup_delete(&mut self, cx: Context, name: Handle<StringValue>) -> EvalResult<bool> {
        // Reuse handles while walking scope chain
        let mut object_handle = Handle::<ObjectValue>::empty(cx);
        let mut scope = Handle::<Scope>::empty(cx);
        scope.replace(**self);

        loop {
            // Properties can be deleted off scope objects. This can be either the global object,
            // with target object, or vars added in a sloppy eval.
            if let Some(object) = scope.object {
                object_handle.replace(object);

                // Name is an interned string (and cannot be a number) so is already a property key
                let key = name.cast::<PropertyKey>();

                // If the property is found, delete it
                if has_property(cx, object_handle, key)? {
                    return object_handle.delete(cx, key);
                }
            }

            // Any bindings stored in VM scope slots cannot be deleted. The only deletable bindings
            // are vars and functions in eval scopes or dynamically created vars.
            if scope
                .scope_names_ptr()
                .lookup_name(*name.as_flat())
                .is_some()
            {
                return Ok(false);
            }

            // Move to parent scope
            if let Some(parent) = scope.parent.as_ref() {
                scope.replace(*parent);
            } else {
                return Ok(true);
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
        if !has_property(cx, object, key)? {
            return Ok(false);
        }

        if self.kind != ScopeKind::With {
            return Ok(true);
        }

        // With statements must also ignore properties in @@unscopables
        let unscopables_key = cx.well_known_symbols.unscopables();
        let unscopables = get(cx, object, unscopables_key)?;
        if unscopables.is_object() {
            let unscopables = unscopables.as_object();

            let value = get(cx, unscopables, key)?;
            let blocked = to_boolean(*value);
            if blocked {
                return Ok(false);
            }
        }

        Ok(true)
    }

    /// Return the object for this scope, creating it if necessary.
    pub fn ensure_scope_object(&mut self, cx: Context) -> Handle<ObjectValue> {
        if let Some(object) = self.object {
            return object.to_handle();
        }

        let object = ordinary_object_create(cx);
        self.object = Some(*object);
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
