use crate::{
    field_offset,
    js::{parser::scope_tree::REALM_SCOPE_SLOT_NAME, runtime::gc::HandleScope},
    must, set_uninit,
};

use super::{
    builtin_function::BuiltinFunction,
    bytecode::function::Closure,
    collections::{BsHashMap, BsHashMapField, InlineArray},
    error::{err_assign_constant, syntax_error},
    gc::{Handle, HeapObject, HeapPtr, HeapVisitor},
    global_names::has_restricted_global_property,
    interned_strings::InternedStrings,
    intrinsics::{
        global_object::set_default_global_bindings,
        intrinsics::{Intrinsic, Intrinsics},
        rust_runtime::return_undefined,
    },
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::ObjectValue,
    scope::Scope,
    scope_names::{ScopeFlags, ScopeNameFlags, ScopeNames},
    string_value::FlatString,
    Context, EvalResult, PropertyKey, Value,
};

/// Realms (https://tc39.es/ecma262/#sec-code-realms)
#[repr(C)]
pub struct Realm {
    descriptor: HeapPtr<ObjectDescriptor>,
    global_object: HeapPtr<ObjectValue>,
    /// Array of all global scopes in this realm. Each script has its own global scope which
    /// contains that script's lexical bindings.
    global_scopes: HeapPtr<GlobalScopes>,
    /// Map of all lexical bindings in this realm, mapped to the global scope that contains them
    /// along with their slot index in that global scope.
    lexical_names: HeapPtr<LexicalNamesMap>,
    /// An empty function in this realm. Used to form a dummy stack frame to set the current realm
    /// when the stack would otherwise be empty.
    empty_function: HeapPtr<Closure>,
    pub intrinsics: Intrinsics,
}

const INTRINSICS_BYTE_OFFSET: usize = field_offset!(Realm, intrinsics);

impl Realm {
    /// Realm initializes intrinsics but leaves other properties uninitialized. Must call
    /// `initialize` before using.
    pub fn new_uninit(cx: Context) -> Handle<Realm> {
        HandleScope::new(cx, |cx| {
            // Realm record must be created before setting up intrinsics, as realm must be referenced
            // during intrinsic creation.
            let size = Self::calculate_size_in_bytes();
            let mut realm = cx.alloc_uninit_with_size::<Realm>(size);

            set_uninit!(realm.descriptor, cx.base_descriptors.get(ObjectKind::Realm));
            set_uninit!(realm.global_object, HeapPtr::uninit());
            set_uninit!(realm.global_scopes, HeapPtr::uninit());
            set_uninit!(realm.lexical_names, HeapPtr::uninit());
            set_uninit!(realm.empty_function, HeapPtr::uninit());

            let realm = realm.to_handle();

            // Global object and scope are created here
            Intrinsics::initialize(cx, realm);

            realm
        })
    }

    #[inline]
    pub fn calculate_size_in_bytes() -> usize {
        INTRINSICS_BYTE_OFFSET + Intrinsics::calculate_size_in_bytes()
    }

    #[inline]
    pub fn global_object_ptr(&self) -> HeapPtr<ObjectValue> {
        self.global_object
    }

    #[inline]
    pub fn global_object(&self) -> Handle<ObjectValue> {
        self.global_object_ptr().to_handle()
    }

    #[inline]
    pub fn set_global_object(&mut self, global_object: HeapPtr<ObjectValue>) {
        self.global_object = global_object;
    }

    #[inline]
    pub fn default_global_scope(&self) -> Handle<Scope> {
        self.global_scopes.get(0).to_handle()
    }

    #[inline]
    pub fn empty_function_ptr(&self) -> HeapPtr<Closure> {
        self.empty_function
    }

    pub fn get_intrinsic_ptr(&self, intrinsic: Intrinsic) -> HeapPtr<ObjectValue> {
        self.intrinsics.get_ptr(intrinsic)
    }

    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> Handle<ObjectValue> {
        self.intrinsics.get(intrinsic)
    }

    /// Get the value associated with a lexical name in the realm's lexical names map, if one
    /// exists. The lexical name could be in any global scope.
    pub fn get_lexical_name(&self, name: HeapPtr<FlatString>) -> Option<Value> {
        let location = self.lexical_names.get(&name)?;

        let global_scope = self.global_scopes.get(location.global_scope_index);
        let value = global_scope.get_slot(location.slot_index as usize);

        Some(value)
    }

    /// Try to set the value associated with a lexical name in the realms lexical names map.
    /// Return whether the lexical name was found and set.
    pub fn set_lexical_name(
        &mut self,
        cx: Context,
        name: HeapPtr<FlatString>,
        value: Value,
    ) -> EvalResult<bool> {
        if let Some(location) = self.lexical_names.get(&name) {
            if location.is_immutable() {
                return err_assign_constant(cx, name);
            }

            let mut global_scope = self.global_scopes.get(location.global_scope_index);
            global_scope.set_slot(location.slot_index as usize, value);

            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl Handle<Realm> {
    fn lexical_names_field(&self) -> LexicalNamesMapField {
        LexicalNamesMapField(*self)
    }

    pub fn add_lexical_name(
        &mut self,
        cx: Context,
        name: Handle<FlatString>,
        global_scope_index: usize,
        slot_index: u32,
        is_immutable: bool,
    ) {
        let lexical_name_location =
            LexicalNameLocation::new(global_scope_index, slot_index, is_immutable);
        self.lexical_names_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(name.get_(), lexical_name_location);
    }

    /// Check if a set of lexical names can be declared in the realm. A lexical name cannot be
    /// declared if it conflicts with an existing global var or lexical name.
    pub fn can_declare_lexical_names(
        &self,
        cx: Context,
        names: &[Handle<FlatString>],
    ) -> EvalResult<()> {
        let global_object = self.global_object();

        // Reuse handle between iterations
        let mut key = Handle::<PropertyKey>::empty(cx);

        for name in names {
            key.replace(PropertyKey::string(cx, name.as_string()));

            let is_global_var_name = has_restricted_global_property(cx, global_object, key)?;
            let is_global_lex_name = self.get_lexical_name(name.get_()).is_some();

            if is_global_var_name || is_global_lex_name {
                return syntax_error(cx, &format!("redeclaration of {}", name.get_()));
            }
        }

        Ok(())
    }

    /// Check if a set of var names can be declared in the realm. A var name cannot be declared if
    /// it conflicts with an existing global lexical name.
    pub fn can_declare_var_names(
        &self,
        cx: Context,
        names: &[HeapPtr<FlatString>],
    ) -> EvalResult<()> {
        for name in names {
            if self.get_lexical_name(*name).is_some() {
                return syntax_error(cx, &format!("redeclaration of {}", name));
            }
        }

        Ok(())
    }

    /// Create a new global scope within this realm. Each script has its own global scope with its
    /// own set of lexical bindings, but all lexical bindings are accessible in the realm's map.
    pub fn new_global_scope(
        &mut self,
        cx: Context,
        scope_names: Handle<ScopeNames>,
    ) -> Handle<Scope> {
        let global_object = self.global_object();
        let mut global_scope = Scope::new_global(cx, scope_names, global_object);

        // Insert global scope into array of all global scopes
        let global_scope_index = self.global_scopes.len();
        GlobalScopes::maybe_grow_for_insertion(cx, *self)
            .insert_without_growing(global_scope.get_());

        // Handle is shared between iterations
        let mut name_handle = Handle::<FlatString>::empty(cx);

        // Statically initialize global scope slots
        for i in 0..scope_names.len() {
            if i == 0 {
                // The first global scope slot is always the realm
                global_scope.set_slot(0, self.get_().cast::<ObjectValue>().into());
            } else if scope_names
                .get_slot_name(i)
                .ptr_eq(&cx.names.this.as_string().as_flat())
            {
                // The global "this" binding is always the global object
                global_scope.set_slot(i, global_object.get_().into());
            } else {
                // All other scope bindings must be lexical bindings, so initialize to empty
                global_scope.set_slot(i, Value::empty());

                // And add to map of all lexical names
                name_handle.replace(scope_names.get_slot_name(i));

                let is_immutable = scope_names.is_immutable(i);
                self.add_lexical_name(cx, name_handle, global_scope_index, i as u32, is_immutable);
            }
        }

        global_scope
    }

    /// Initialize the set of global scopes. Start with a single "default" global scope which is
    /// used when a generic global scope is needed, e.g. for builtin function closures,
    /// function constructors, etc.
    pub fn init_global_scope(&mut self, cx: Context) {
        self.global_scopes = GlobalScopes::new(cx, GlobalScopes::MIN_CAPACITY);
        self.lexical_names = LexicalNamesMap::new_initial(cx, ObjectKind::LexicalNamesMap);

        // All global scopes have the realm in their first slot
        let binding_names = &[InternedStrings::get_str(cx, REALM_SCOPE_SLOT_NAME).as_flat()];
        let binding_flags = &[ScopeNameFlags::empty()];
        let scope_names =
            ScopeNames::new(cx, ScopeFlags::IS_VAR_SCOPE, binding_names, binding_flags);

        self.new_global_scope(cx, scope_names);

        let empty_function = BuiltinFunction::create_builtin_function_without_properties(
            cx,
            return_undefined,
            /* name */ None,
            *self,
            /* prototype */ None,
            /* is_constructor */ false,
        );
        self.empty_function = empty_function.get_();
    }
}

/// InitializeHostDefinedRealm (https://tc39.es/ecma262/#sec-initializehostdefinedrealm)
pub fn initialize_host_defined_realm(
    cx: Context,
    expose_gc: bool,
    expose_test262: bool,
) -> Handle<Realm> {
    HandleScope::new(cx, |cx| {
        let realm = Realm::new_uninit(cx);
        must!(set_default_global_bindings(cx, realm, expose_gc, expose_test262));

        realm
    })
}

impl HeapObject for HeapPtr<Realm> {
    fn byte_size(&self) -> usize {
        Realm::calculate_size_in_bytes()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.global_object);
        visitor.visit_pointer(&mut self.global_scopes);
        visitor.visit_pointer(&mut self.lexical_names);
        visitor.visit_pointer(&mut self.empty_function);
        self.intrinsics.visit_pointers(visitor);
    }
}

#[repr(C)]
pub struct GlobalScopes {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Number of global scopes stored in this array.
    len: usize,
    /// Array of global scopes, total size is the capacity of the global scope array.
    scopes: InlineArray<HeapPtr<Scope>>,
}

impl GlobalScopes {
    const MIN_CAPACITY: usize = 2;

    fn new(cx: Context, capacity: usize) -> HeapPtr<GlobalScopes> {
        let size = Self::calculate_size_in_bytes(capacity);
        let mut global_scopes = cx.alloc_uninit_with_size::<GlobalScopes>(size);

        set_uninit!(global_scopes.descriptor, cx.base_descriptors.get(ObjectKind::GlobalScopes));
        set_uninit!(global_scopes.len, 0);

        // Leave scopes array uninitialized
        global_scopes.scopes.init_with_uninit(capacity);

        global_scopes
    }

    const SCOPES_OFFSET: usize = field_offset!(GlobalScopes, scopes);

    fn calculate_size_in_bytes(len: usize) -> usize {
        Self::SCOPES_OFFSET + InlineArray::<HeapPtr<Scope>>::calculate_size_in_bytes(len)
    }

    fn len(&self) -> usize {
        self.len
    }

    fn capacity(&self) -> usize {
        self.scopes.len()
    }

    fn get(&self, i: usize) -> HeapPtr<Scope> {
        self.scopes.as_slice()[i]
    }

    /// Prepare global scopes array for insertion of another global scope. This will grow the array
    /// and update realm to point to new array if there is no room to insert another scope.
    pub fn maybe_grow_for_insertion(
        cx: Context,
        mut realm: Handle<Realm>,
    ) -> HeapPtr<GlobalScopes> {
        let old_global_scopes = realm.global_scopes;
        let old_len = old_global_scopes.len();
        let old_capacity = old_global_scopes.capacity();

        // Check if we already have enough room for an insertion
        if old_len < old_capacity {
            return old_global_scopes;
        }

        let old_global_scopes = old_global_scopes.to_handle();
        let mut new_global_scopes = GlobalScopes::new(cx, old_capacity * 2);
        new_global_scopes.len = old_len;

        // Copy data from old array to new array
        unsafe {
            std::ptr::copy_nonoverlapping(
                old_global_scopes.scopes.data_ptr(),
                new_global_scopes.scopes.data_mut_ptr(),
                old_len,
            );
        }

        realm.global_scopes = new_global_scopes;

        new_global_scopes
    }

    pub fn insert_without_growing(&mut self, global_scope: HeapPtr<Scope>) {
        self.scopes.set_unchecked(self.len(), global_scope);
        self.len += 1;
    }
}

impl HeapObject for HeapPtr<GlobalScopes> {
    fn byte_size(&self) -> usize {
        GlobalScopes::calculate_size_in_bytes(self.scopes.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        let len = self.len();
        for scope in &mut self.scopes.as_mut_slice()[..len] {
            visitor.visit_pointer(scope);
        }
    }
}

/// A lexical name is stored in a slot of a global scope.
#[derive(Clone)]
pub struct LexicalNameLocation {
    /// The index of the global scope in the global scopes array.
    global_scope_index: usize,
    /// The slot index of the lexical name in that global scope.
    slot_index: u32,
    /// Whether this is an immutable binding.
    is_immutable: bool,
}

impl LexicalNameLocation {
    pub fn new(global_scope_index: usize, slot_index: u32, is_immutable: bool) -> Self {
        Self { global_scope_index, slot_index, is_immutable }
    }

    pub fn is_immutable(&self) -> bool {
        self.is_immutable
    }
}

pub type LexicalNamesMap = BsHashMap<HeapPtr<FlatString>, LexicalNameLocation>;

#[derive(Clone)]
pub struct LexicalNamesMapField(Handle<Realm>);

impl BsHashMapField<HeapPtr<FlatString>, LexicalNameLocation> for LexicalNamesMapField {
    fn new_map(&self, cx: Context, capacity: usize) -> HeapPtr<LexicalNamesMap> {
        LexicalNamesMap::new(cx, ObjectKind::LexicalNamesMap, capacity)
    }

    fn get(&self, _: Context) -> HeapPtr<LexicalNamesMap> {
        self.0.lexical_names
    }

    fn set(&mut self, _: Context, map: HeapPtr<LexicalNamesMap>) {
        self.0.lexical_names = map;
    }
}

impl LexicalNamesMapField {
    pub fn byte_size(map: &HeapPtr<LexicalNamesMap>) -> usize {
        LexicalNamesMap::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(map: &mut HeapPtr<LexicalNamesMap>, visitor: &mut impl HeapVisitor) {
        map.visit_pointers(visitor);

        for (name, _) in map.iter_mut_gc_unsafe() {
            visitor.visit_pointer(name);
        }
    }
}
