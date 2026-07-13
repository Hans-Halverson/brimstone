use std::time::Instant;

use crate::{
    field_offset, handle_scope, impl_hash_map_instance, must_a,
    parser::scope_tree::REALM_SCOPE_SLOT_NAME,
    runtime::{
        Context, EvalResult, HeapItemKind, PropertyKey, Value,
        alloc_error::AllocResult,
        annex_b::init_annex_b_methods,
        builtin_function::BuiltinFunction,
        bytecode::function::ClosureObject,
        collections::{FastHasher, HashMapInstance, InlineArray, hash_map::BsHashMapField},
        error::{err_access_before_initialization, err_assign_constant, syntax_error},
        gc::{Handle, HeapItem, HeapPtr, HeapVisitor},
        gc_object::GcObject,
        global_names::{GlobalDeclaration, has_restricted_global_property},
        global_object::GlobalObject,
        interned_strings::InternedStrings,
        intrinsics::{
            globals::set_default_global_bindings,
            intrinsics::{Intrinsic, Intrinsics},
            rust_runtime::RuntimeFunction,
        },
        object_value::ObjectValue,
        scope::Scope,
        scope_names::{ScopeFlags, ScopeNameFlags, ScopeNames},
        shape::Shape,
        string_value::FlatString,
        test_262_object::Test262Object,
        test_shell::TestShell,
    },
    set_uninit,
};

/// Realms (https://tc39.es/ecma262/#sec-code-realms)
#[repr(C)]
pub struct Realm {
    shape: HeapPtr<Shape>,
    /// The global object for this realm. Holds all global properties.
    global_object: HeapPtr<GlobalObject>,
    /// Array of all global scopes in this realm. Each script has its own global scope which
    /// contains that script's lexical bindings.
    global_scopes: HeapPtr<GlobalScopes>,
    /// Map of all lexical bindings in this realm, mapped to the global scope that contains them
    /// along with their slot index in that global scope.
    lexical_names: HeapPtr<LexicalNamesMap>,
    /// An empty function in this realm. Used to form a dummy stack frame to set the current realm
    /// when the stack would otherwise be empty.
    empty_function: HeapPtr<ClosureObject>,
    /// Timestamp when this realm was created. Times returned from `performance.now` are relative
    /// to this timestamp.
    time_origin: Instant,
    pub intrinsics: Intrinsics,
}

const INTRINSICS_BYTE_OFFSET: usize = field_offset!(Realm, intrinsics);

impl Realm {
    /// InitializeHostDefinedRealm (https://tc39.es/ecma262/#sec-initializehostdefinedrealm)
    pub fn new(cx: Context) -> AllocResult<Handle<Realm>> {
        handle_scope!(cx, {
            let realm = Realm::new_uninit(cx)?;
            must_a!(set_default_global_bindings(cx, realm));
            Ok(realm)
        })
    }

    /// Realm initializes intrinsics but leaves other properties uninitialized. Must call
    /// `initialize` before using.
    fn new_uninit(cx: Context) -> AllocResult<Handle<Realm>> {
        handle_scope!(cx, {
            // Realm record must be created before setting up intrinsics, as realm must be referenced
            // during intrinsic creation.
            let size = Self::calculate_size_in_bytes();
            let mut realm = cx.alloc_uninit_with_size::<Realm>(size)?;

            set_uninit!(realm.shape, cx.shapes.get(HeapItemKind::Realm));
            set_uninit!(realm.global_object, HeapPtr::uninit());
            set_uninit!(realm.global_scopes, HeapPtr::uninit());
            set_uninit!(realm.lexical_names, HeapPtr::uninit());
            set_uninit!(realm.empty_function, HeapPtr::uninit());
            set_uninit!(realm.time_origin, Instant::now());

            let realm = realm.to_handle();

            // Global object and scope are created here
            Intrinsics::initialize(cx, realm)?;

            Ok(realm)
        })
    }

    #[inline]
    pub fn calculate_size_in_bytes() -> usize {
        INTRINSICS_BYTE_OFFSET + Intrinsics::calculate_size_in_bytes()
    }

    #[inline]
    pub fn global_object_ptr(&self) -> HeapPtr<GlobalObject> {
        self.global_object
    }

    #[inline]
    pub fn global_object(&self) -> Handle<GlobalObject> {
        self.global_object_ptr().to_handle()
    }

    #[inline]
    pub fn set_global_object(&mut self, global_object: HeapPtr<GlobalObject>) {
        self.global_object = global_object;
    }

    #[inline]
    pub fn default_global_scope(&self) -> Handle<Scope> {
        self.global_scopes.get(0).to_handle()
    }

    #[inline]
    pub fn empty_function_ptr(&self) -> HeapPtr<ClosureObject> {
        self.empty_function
    }

    pub fn time_origin(&self) -> Instant {
        self.time_origin
    }

    pub fn get_intrinsic_ptr(&self, intrinsic: Intrinsic) -> HeapPtr<ObjectValue> {
        self.intrinsics.get_ptr(intrinsic)
    }

    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> Handle<ObjectValue> {
        self.intrinsics.get(intrinsic)
    }

    /// Whether the name exists in the realm's lexical names map, in any global scope.
    ///
    /// Does not perform any validation checks like TDZ.
    pub fn has_lexical_name(&self, name: HeapPtr<FlatString>) -> bool {
        self.lexical_names.get(&name).is_some()
    }

    /// Get the value associated with a lexical name in the realm's lexical names map, if one
    /// exists. The lexical name could be in any global scope.
    pub fn get_lexical_name(
        &self,
        cx: Context,
        name: HeapPtr<FlatString>,
    ) -> EvalResult<Option<Value>> {
        let Some(location) = self.lexical_names.get(&name) else {
            return Ok(None);
        };

        let global_scope = self.global_scopes.get(location.global_scope_index);
        let value = global_scope.get_slot(location.slot_index as usize);

        // Perform TDZ check since binding may not yet be initialized
        if value.is_empty() {
            return err_access_before_initialization(cx, name.as_string().to_handle());
        }

        Ok(Some(value))
    }

    /// Try to set the value associated with a lexical name in the realms lexical names map.
    /// Performs validation checks like TDZ and immutability. Return whether the lexical name was
    /// found and successfully set.
    pub fn set_lexical_name(
        &mut self,
        cx: Context,
        name: HeapPtr<FlatString>,
        value: Value,
    ) -> EvalResult<bool> {
        if let Some(location) = self.lexical_names.get(&name) {
            let mut global_scope = self.global_scopes.get(location.global_scope_index);

            // Must perform TDZ check since binding may not be initialized
            let slot_value = global_scope.get_slot_mut(location.slot_index as usize);
            if slot_value.is_empty() {
                return err_access_before_initialization(cx, name.as_string().to_handle());
            }

            // Cannot reassign an immutable binding. This error has lower precedence than the TDZ.
            if location.is_immutable() {
                return err_assign_constant(cx, name);
            }

            *slot_value = value;

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
    ) -> AllocResult<()> {
        let lexical_name_location =
            LexicalNameLocation::new(global_scope_index, slot_index, is_immutable);
        self.lexical_names_field()
            .maybe_grow_for_insertion(cx)?
            .insert_without_growing(*name, lexical_name_location);

        // Safe since lexical names are interned strings and are identifiers so they cannot be
        // array-index strings.
        let key = name.cast::<PropertyKey>();

        // Global lexical bindings permanently shadow global object properties with the same name,
        // so invalidate any caches with this global property.
        if let Some(mut property) = self.global_object_ptr().lookup_named_property(*key) {
            property.invalidate_shadowed();
        }

        Ok(())
    }

    /// Check if a set of lexical names can be declared in the realm. A lexical name cannot be
    /// declared if it conflicts with an existing global var or lexical name.
    pub fn can_declare_lexical_names(
        &self,
        cx: Context,
        names: &[Handle<FlatString>],
    ) -> EvalResult<()> {
        let global_object = self.global_object().as_object();

        // Reuse handle between iterations
        let mut key = Handle::<PropertyKey>::empty(cx);

        for name in names {
            let name = *name;

            key.replace(PropertyKey::string(cx, name.as_string())?);

            let is_global_var_name = has_restricted_global_property(cx, global_object, key)?;
            let is_global_lex_name = self.has_lexical_name(*name);

            if is_global_var_name || is_global_lex_name {
                return syntax_error(cx, &format!("redeclaration of `{}`", *name));
            }
        }

        Ok(())
    }

    /// Check if a set of var names can be declared in the realm. A var name cannot be declared if
    /// it conflicts with an existing global lexical name.
    pub fn can_declare_var_names(
        &self,
        cx: Context,
        decls: &[GlobalDeclaration],
    ) -> EvalResult<()> {
        for decl in decls {
            if self.has_lexical_name(decl.name) {
                return syntax_error(cx, &format!("redeclaration of `{}`", decl.name));
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
    ) -> AllocResult<Handle<Scope>> {
        let global_object = self.global_object().as_object();
        let mut global_scope = Scope::new_global(cx, scope_names, global_object)?;

        // Insert global scope into array of all global scopes
        let global_scope_index = self.global_scopes.len();
        GlobalScopes::maybe_grow_for_insertion(cx, *self)?.insert_without_growing(*global_scope);

        // Handle is shared between iterations
        let mut name_handle = Handle::<FlatString>::empty(cx);

        // Statically initialize global scope slots
        for i in 0..scope_names.len() {
            if i == 0 {
                // The first global scope slot is always the realm
                global_scope.set_heap_item_slot(0, self.as_any());
            } else if scope_names
                .get_slot_name(i)
                .ptr_eq(&cx.names.this.as_string().as_flat())
            {
                // The global "this" binding is always the global object
                global_scope.set_slot(i, *global_object.as_value());
            } else {
                // All other scope bindings must be lexical bindings, so initialize to empty
                global_scope.set_slot(i, Value::empty());

                // And add to map of all lexical names
                name_handle.replace(scope_names.get_slot_name(i));

                let is_immutable = scope_names.is_immutable(i);
                self.add_lexical_name(cx, name_handle, global_scope_index, i as u32, is_immutable)?;
            }
        }

        Ok(global_scope)
    }

    /// Initialize the set of global scopes. Start with a single "default" global scope which is
    /// used when a generic global scope is needed, e.g. for builtin function closures,
    /// function constructors, etc.
    pub fn init_global_scope(&mut self, cx: Context) -> AllocResult<()> {
        self.global_scopes = GlobalScopes::new(cx, GlobalScopes::MIN_CAPACITY)?;
        self.lexical_names = LexicalNamesMap::new_initial(cx)?;

        // All global scopes have the realm in their first slot
        let binding_names = &[InternedStrings::alloc_static_wtf8_str(
            cx,
            &REALM_SCOPE_SLOT_NAME,
        )?];
        let binding_flags = &[ScopeNameFlags::empty()];
        let scope_names =
            ScopeNames::new(cx, ScopeFlags::IS_VAR_SCOPE, binding_names, binding_flags)?;

        self.new_global_scope(cx, scope_names)?;

        let empty_function = BuiltinFunction::create_builtin_function_without_properties(
            cx,
            RuntimeFunction::ReturnUndefined,
            /* name */ None,
            *self,
            /* prototype */ Some(self.get_intrinsic(Intrinsic::FunctionPrototype)),
            /* is_constructor */ false,
        )?;
        self.empty_function = *empty_function;

        Ok(())
    }

    /// Install optional, non-standard properties of the global object based on the configuration
    /// included in the Context's options.
    pub fn install_optional_globals(&mut self, cx: Context) -> AllocResult<()> {
        handle_scope!(cx, {
            if cx.options.annex_b {
                init_annex_b_methods(cx, *self)?;
            }

            if cx.options.expose_gc {
                GcObject::install(cx, *self)?;
            }

            if cx.options.expose_test_262 {
                Test262Object::install(cx, *self)?;
            }

            if cx.options.expose_test_shell_compat {
                TestShell::install(cx, *self)?;
            }

            Ok(())
        })
    }
}

impl HeapItem for Realm {
    fn byte_size(_: HeapPtr<Self>) -> usize {
        Realm::calculate_size_in_bytes()
    }

    fn visit_pointers(mut realm: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut realm.shape);
        visitor.visit_pointer(&mut realm.global_object);
        visitor.visit_pointer(&mut realm.global_scopes);
        visitor.visit_pointer(&mut realm.lexical_names);
        visitor.visit_pointer(&mut realm.empty_function);
        realm.intrinsics.visit_pointers(visitor);
    }
}

#[repr(C)]
pub struct GlobalScopes {
    shape: HeapPtr<Shape>,
    /// Number of global scopes stored in this array.
    len: usize,
    /// Array of global scopes, total size is the capacity of the global scope array.
    scopes: InlineArray<HeapPtr<Scope>>,
}

impl GlobalScopes {
    const MIN_CAPACITY: usize = 2;

    fn new(cx: Context, capacity: usize) -> AllocResult<HeapPtr<GlobalScopes>> {
        let size = Self::calculate_size_in_bytes(capacity);
        let mut global_scopes = cx.alloc_uninit_with_size::<GlobalScopes>(size)?;

        set_uninit!(global_scopes.shape, cx.shapes.get(HeapItemKind::GlobalScopes));
        set_uninit!(global_scopes.len, 0);

        // Leave scopes array uninitialized
        global_scopes.scopes.init_with_uninit(capacity);

        Ok(global_scopes)
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
    ) -> AllocResult<HeapPtr<GlobalScopes>> {
        let old_global_scopes = realm.global_scopes;
        let old_len = old_global_scopes.len();
        let old_capacity = old_global_scopes.capacity();

        // Check if we already have enough room for an insertion
        if old_len < old_capacity {
            return Ok(old_global_scopes);
        }

        let old_global_scopes = old_global_scopes.to_handle();
        let mut new_global_scopes = GlobalScopes::new(cx, old_capacity * 2)?;
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

        Ok(new_global_scopes)
    }

    pub fn insert_without_growing(&mut self, global_scope: HeapPtr<Scope>) {
        self.scopes.set_unchecked(self.len(), global_scope);
        self.len += 1;
    }
}

impl HeapItem for GlobalScopes {
    fn byte_size(global_scopes: HeapPtr<Self>) -> usize {
        GlobalScopes::calculate_size_in_bytes(global_scopes.scopes.len())
    }

    fn visit_pointers(mut global_scopes: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut global_scopes.shape);

        let len = global_scopes.len();
        for scope in &mut global_scopes.scopes.as_mut_slice()[..len] {
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

impl_hash_map_instance!(LexicalNamesMap, HeapPtr<FlatString>, LexicalNameLocation, FastHasher);

#[derive(Clone)]
pub struct LexicalNamesMapField(Handle<Realm>);

impl BsHashMapField<LexicalNamesMap> for LexicalNamesMapField {
    fn get(&self, _: Context) -> HeapPtr<LexicalNamesMap> {
        self.0.lexical_names
    }

    fn set_new(&mut self, cx: Context, capacity: usize) -> AllocResult<HeapPtr<LexicalNamesMap>> {
        let map = LexicalNamesMap::new(cx, capacity)?;
        self.0.lexical_names = map;
        Ok(map)
    }
}

impl HeapItem for LexicalNamesMap {
    fn byte_size(map: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(map.capacity())
    }

    fn visit_pointers(mut map: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        map.visit_map_pointers(visitor);

        for (name, _) in map.iter_mut_gc_unsafe() {
            visitor.visit_pointer(name);
        }
    }
}
