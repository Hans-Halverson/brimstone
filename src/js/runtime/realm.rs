use crate::{
    field_offset,
    js::{
        parser::{
            ast::{AstPtr, TemplateLiteral},
            scope_tree::REALM_SCOPE_SLOT_NAME,
        },
        runtime::gc::HandleScope,
    },
    must, set_uninit,
};

use super::{
    collections::{BsHashMap, BsHashMapField, InlineArray},
    environment::global_environment::GlobalEnvironment,
    execution_context::ExecutionContext,
    gc::{Handle, HeapObject, HeapPtr, HeapVisitor},
    interned_strings::InternedStrings,
    intrinsics::{
        global_object::set_default_global_bindings,
        intrinsics::{Intrinsic, Intrinsics},
    },
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::ObjectValue,
    scope::Scope,
    scope_names::ScopeNames,
    string_value::FlatString,
    Context, EvalResult, Value,
};

// 9.3 Realm Record
#[repr(C)]
pub struct Realm {
    descriptor: HeapPtr<ObjectDescriptor>,
    global_env: HeapPtr<GlobalEnvironment>,
    global_object: HeapPtr<ObjectValue>,
    /// Array of all global scopes in this realm. Each script has its own global scope which
    /// contains that script's lexical bindings.
    global_scopes: HeapPtr<GlobalScopes>,
    /// Map of all lexical bindings in this realm, mapped to the global scope that contains them
    /// along with their slot index in that global scope.
    lexical_names: HeapPtr<LexicalNamesMap>,
    template_map: HeapPtr<TemplateMap>,
    pub intrinsics: Intrinsics,
}

type TemplateMap = BsHashMap<AstPtr<TemplateLiteral>, HeapPtr<ObjectValue>>;

const INTRINSICS_BYTE_OFFSET: usize = field_offset!(Realm, intrinsics);

impl Realm {
    // 9.3.1 CreateRealm. Realm initializes intrinsics but leaves other properties uninitialized.
    // Must call `initialize` before using.
    pub fn new_uninit(cx: Context) -> Handle<Realm> {
        HandleScope::new(cx, |cx| {
            // Realm record must be created before setting up intrinsics, as realm must be referenced
            // during intrinsic creation.
            let size = Self::calculate_size_in_bytes();
            let mut realm = cx.alloc_uninit_with_size::<Realm>(size);

            set_uninit!(realm.descriptor, cx.base_descriptors.get(ObjectKind::Realm));
            set_uninit!(realm.global_env, HeapPtr::uninit());
            set_uninit!(realm.global_object, HeapPtr::uninit());
            set_uninit!(realm.global_scopes, HeapPtr::uninit());
            set_uninit!(realm.lexical_names, HeapPtr::uninit());
            set_uninit!(realm.template_map, HeapPtr::uninit());

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
    pub fn global_env(&self) -> Handle<GlobalEnvironment> {
        self.global_env.to_handle()
    }

    #[inline]
    pub fn default_global_scope(&self) -> Handle<Scope> {
        self.global_scopes.get(0).to_handle()
    }

    pub fn global_this_value(&self) -> Handle<ObjectValue> {
        self.global_env.global_this_value()
    }

    pub fn get_intrinsic_ptr(&self, intrinsic: Intrinsic) -> HeapPtr<ObjectValue> {
        self.intrinsics.get_ptr(intrinsic)
    }

    pub fn get_intrinsic(&self, intrinsic: Intrinsic) -> Handle<ObjectValue> {
        self.intrinsics.get(intrinsic)
    }

    pub fn get_template_object(
        &self,
        template_node: AstPtr<TemplateLiteral>,
    ) -> Option<Handle<ObjectValue>> {
        self.template_map
            .get(&template_node)
            .map(|template_object| template_object.to_handle())
    }

    /// Get the value associated with a lexical name in the realm's lexical names map, if one
    /// exists. The lexical name could be in any global scope.
    pub fn get_lexical_name(&self, name: HeapPtr<FlatString>) -> Option<Value> {
        let location = self.lexical_names.get(&name)?;

        let global_scope = self.global_scopes.get(location.global_scope_index);
        let value = global_scope.get_slot(location.slot_index);

        Some(value)
    }

    /// Try to set the value associated with a lexical name in the realms lexical names map.
    /// Return whether the lexical name was found and set.
    pub fn set_lexical_name(&mut self, name: HeapPtr<FlatString>, value: Value) -> bool {
        if let Some(location) = self.lexical_names.get(&name) {
            let mut global_scope = self.global_scopes.get(location.global_scope_index);
            global_scope.set_slot(location.slot_index, value);

            true
        } else {
            false
        }
    }
}

impl Handle<Realm> {
    fn template_map_field(&self) -> RealmTemplateMapField {
        RealmTemplateMapField(*self)
    }

    pub fn add_template_object(
        &mut self,
        cx: Context,
        template_node: AstPtr<TemplateLiteral>,
        template_object: Handle<ObjectValue>,
    ) {
        self.template_map_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(template_node, template_object.get_());
    }

    fn lexical_names_field(&self) -> LexicalNamesMapField {
        LexicalNamesMapField(*self)
    }

    pub fn add_lexical_name(
        &mut self,
        cx: Context,
        name: Handle<FlatString>,
        global_scope_index: usize,
        slot_index: usize,
    ) {
        let lexical_name_location = LexicalNameLocation::new(global_scope_index, slot_index);
        self.lexical_names_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(name.get_(), lexical_name_location);
    }

    // 9.3.3 SetRealmGlobalObject
    // Initializes remaining properties of realm that were not initialized in `new_uninit`.
    pub fn initialize(&mut self, cx: Context) {
        // Global object and scope were created in `Intrinsics::initialize`
        let global_object = self.global_object();
        self.global_env = GlobalEnvironment::new(cx, global_object, global_object).get_();
        self.template_map = TemplateMap::new_initial(cx, ObjectKind::RealmTemplateMap);
    }

    /// Create a new global scope within this realm. Each script has its own global scope with its
    /// own set of lexical bindings, but all lexical bindings are accessible in the realm's map.
    pub fn new_global_scope(&mut self, cx: Context, binding_names: &[String]) -> Handle<Scope> {
        let names = binding_names
            .iter()
            .map(|name| InternedStrings::get_str(cx, name).as_flat())
            .collect::<Vec<_>>();
        let scope_names = ScopeNames::new(cx, &names);

        let global_object = self.global_object();
        let mut global_scope = Scope::new_global(cx, scope_names, global_object);

        // Insert global scope into array of all global scopes
        let global_scope_index = self.global_scopes.len();
        GlobalScopes::maybe_grow_for_insertion(cx, *self)
            .insert_without_growing(global_scope.get_());

        // Statically initialize global scope slots
        for (i, name) in binding_names.iter().enumerate() {
            if i == 0 {
                // The first global scope slot is always the realm
                global_scope.set_slot(0, self.get_().cast::<ObjectValue>().into());
            } else if name == "this" {
                // The global "this" binding is always the global object
                global_scope.set_slot(i, global_object.get_().into());
            } else {
                // All other scope bindings must be lexical bindings, so initialize to empty
                global_scope.set_slot(i, Value::empty());

                // And add to map of all lexical names
                self.add_lexical_name(cx, names[i], global_scope_index, i);
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
        self.new_global_scope(cx, &[REALM_SCOPE_SLOT_NAME.to_owned()]);
    }
}

// 9.6 InitializeHostDefinedRealm
pub fn initialize_host_defined_realm(
    cx: Context,
    expose_gc: bool,
    expose_test262: bool,
) -> Handle<Realm> {
    HandleScope::new(cx, |mut cx| {
        let mut realm = Realm::new_uninit(cx);
        let exec_ctx = ExecutionContext::new(
            cx, /* function */ None, realm, /* script_or_module */ None,
            /* lexical_env */ None, /* variable_env */ None, /* private_env */ None,
            /* is_strict_mode */ false,
        );

        cx.push_execution_context(exec_ctx);

        realm.initialize(cx);
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
        visitor.visit_pointer(&mut self.global_env);
        visitor.visit_pointer(&mut self.global_object);
        visitor.visit_pointer(&mut self.global_scopes);
        visitor.visit_pointer(&mut self.lexical_names);
        visitor.visit_pointer(&mut self.template_map);
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
    slot_index: usize,
}

impl LexicalNameLocation {
    pub fn new(global_scope_index: usize, slot_index: usize) -> Self {
        Self { global_scope_index, slot_index }
    }
}

pub type LexicalNamesMap = BsHashMap<HeapPtr<FlatString>, LexicalNameLocation>;

#[derive(Clone)]
pub struct LexicalNamesMapField(Handle<Realm>);

impl BsHashMapField<HeapPtr<FlatString>, LexicalNameLocation> for LexicalNamesMapField {
    fn new(&self, cx: Context, capacity: usize) -> HeapPtr<LexicalNamesMap> {
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

pub struct RealmTemplateMapField(Handle<Realm>);

impl BsHashMapField<AstPtr<TemplateLiteral>, HeapPtr<ObjectValue>> for RealmTemplateMapField {
    fn new(&self, cx: Context, capacity: usize) -> HeapPtr<TemplateMap> {
        TemplateMap::new(cx, ObjectKind::RealmTemplateMap, capacity)
    }

    fn get(&self, _: Context) -> HeapPtr<TemplateMap> {
        self.0.template_map
    }

    fn set(&mut self, _: Context, map: HeapPtr<TemplateMap>) {
        self.0.template_map = map;
    }
}

impl RealmTemplateMapField {
    pub fn byte_size(map: &HeapPtr<TemplateMap>) -> usize {
        TemplateMap::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(map: &mut HeapPtr<TemplateMap>, visitor: &mut impl HeapVisitor) {
        map.visit_pointers(visitor);

        for (_, value) in map.iter_mut_gc_unsafe() {
            visitor.visit_pointer(value);
        }
    }
}
