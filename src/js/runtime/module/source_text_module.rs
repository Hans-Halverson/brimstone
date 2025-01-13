use std::{
    collections::HashSet,
    hash::Hash,
    num::NonZeroUsize,
    sync::{LazyLock, Mutex},
};

use indexmap::IndexSet;

use crate::{
    field_offset,
    js::runtime::{
        boxed_value::BoxedValue,
        bytecode::function::BytecodeFunction,
        collections::{BsArray, BsHashMap, BsHashMapField, BsVec, BsVecField, InlineArray},
        gc::{HandleScope, HeapItem, HeapObject, HeapVisitor},
        module::{
            import_attributes::ImportAttributes, module_namespace_object::ModuleNamespaceObject,
        },
        object_descriptor::{ObjectDescriptor, ObjectKind},
        object_value::ObjectValue,
        ordinary_object::object_create_with_optional_proto,
        promise_object::PromiseCapability,
        scope::Scope,
        string_value::FlatString,
        Context, Handle, HeapPtr, PropertyKey, Value,
    },
    set_uninit,
};

/// Abstract Module Records (https://tc39.es/ecma262/#sec-abstract-module-records)
/// Cyclic Modules Records (https://tc39.es/ecma262/#sec-cyclic-module-records)
/// Source Text Module Records (https://tc39.es/ecma262/#sec-source-text-module-records)
///
/// Combination of SourceTextModule and its parent classes since it is the only type of module.
#[repr(C)]
pub struct SourceTextModule {
    descriptor: HeapPtr<ObjectDescriptor>,
    /// Unique identifier for this module. Can be used as a stable identifier.
    id: ModuleId,
    /// State of the module during load/link/evaluation.
    state: ModuleState,
    /// Whether this module has top-level await.
    has_top_level_await: bool,
    /// If set this module is async or has an async dependency. Contains a counter used to determine
    /// the order where [[AsyncEvaluation]] was set.
    async_evaluation_index: Option<NonZeroUsize>,
    /// Function that evaluates the module when called in the module scope.
    program_function: HeapPtr<BytecodeFunction>,
    /// Scope for the module. Program function is executed in this scope.
    module_scope: HeapPtr<Scope>,
    /// The `import.meta` object for this module. This is lazily created when first accessed.
    import_meta: Option<HeapPtr<ObjectValue>>,
    /// Namespace object for this module. This is lazily created when first accessed.
    namespace_object: Option<HeapPtr<ModuleNamespaceObject>>,
    /// Map of all exported names to the BoxedValue that contains the exported value. This is lazily
    /// created when the namespace object is first accessed.
    exports: Option<HeapPtr<ExportMap>>,
    /// The set of module specifiers and attributes requested by this module in imports and re-exports.
    requested_modules: HeapPtr<ModuleRequestArray>,
    /// Each index corresponds to the specifier at the same index in `requested_module_specifiers`,
    /// and contains the module object if one has been loaded.
    loaded_modules: HeapPtr<ModuleOptionArray>,
    // Indices used during DFS in the link and evaluate phases.
    dfs_index: u32,
    dfs_ancestor_index: u32,
    /// The first visited module in a cycle, aka the root of the strongly connected component in DFS.
    cycle_root: Option<HeapPtr<SourceTextModule>>,
    /// Only set on cycle roots, contains the promise that will be settled for the entire evaluation
    /// of the cycle.
    top_level_capability: Option<HeapPtr<PromiseCapability>>,
    /// The error that was thrown during evaluation, if any.
    evaluation_error: Option<Value>,
    /// The number of remaining async dependency modules that have yet to finish executing.
    pending_async_dependencies: usize,
    /// If this module is evaluating asynchronously, this is the list of importers of this module
    /// that will not start execution until this module has completed execution.
    async_parent_modules: Option<HeapPtr<AsyncParentModulesVec>>,
    /// All import and export entries in the module.
    entries: InlineArray<ModuleEntry>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ModuleState {
    New,
    Unlinked,
    Linking,
    Linked,
    Evaluating,
    EvaluatingAsync,
    Evaluated,
}

type ModuleRequestArray = BsArray<HeapModuleRequest>;

type ModuleOptionArray = BsArray<Option<HeapPtr<SourceTextModule>>>;

/// Key is the name of the export.
/// - If export is a namespace export then value is the SourceTextModule whose namespace is exported
/// - Otherwise value is a BoxedValue which holds the exported value
pub type ExportMap = BsHashMap<PropertyKey, HeapPtr<HeapItem>>;

impl SourceTextModule {
    pub fn new(
        cx: Context,
        program_function: Handle<BytecodeFunction>,
        module_scope: Handle<Scope>,
        requested_modules: &IndexSet<ModuleRequest>,
        imports: &[ImportEntry],
        local_exports: &[LocalExportEntry],
        named_re_exports: &[NamedReExportEntry],
        direct_re_exports: &[DirectReExportEntry],
        has_top_level_await: bool,
    ) -> Handle<SourceTextModule> {
        // First create arrays for requested and loaded modules. Requested modules are initialized
        // from arguments, loaded modules are initialized to None.
        let num_module_requests = requested_modules.len();
        let mut heap_requested_modules =
            BsArray::new_uninit(cx, ObjectKind::ModuleRequestArray, num_module_requests)
                .to_handle();
        for (dst, src) in heap_requested_modules
            .as_mut_slice()
            .iter_mut()
            .zip(requested_modules.iter())
        {
            *dst = src.to_heap();
        }

        let loaded_modules =
            BsArray::new(cx, ObjectKind::ValueArray, requested_modules.len(), None).to_handle();

        // Then create the uninitialized module object
        let num_entries =
            imports.len() + local_exports.len() + named_re_exports.len() + direct_re_exports.len();
        let size = Self::calculate_size_in_bytes(num_entries);
        let mut object = cx.alloc_uninit_with_size::<SourceTextModule>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::SourceTextModule));
        set_uninit!(object.id, next_module_id());
        set_uninit!(object.state, ModuleState::New);
        set_uninit!(object.has_top_level_await, has_top_level_await);
        set_uninit!(object.async_evaluation_index, None);
        set_uninit!(object.program_function, *program_function);
        set_uninit!(object.module_scope, *module_scope);
        set_uninit!(object.import_meta, None);
        set_uninit!(object.namespace_object, None);
        set_uninit!(object.exports, None);
        set_uninit!(object.requested_modules, *heap_requested_modules);
        set_uninit!(object.loaded_modules, *loaded_modules);
        set_uninit!(object.dfs_index, 0);
        set_uninit!(object.dfs_ancestor_index, 0);
        set_uninit!(object.cycle_root, None);
        set_uninit!(object.top_level_capability, None);
        set_uninit!(object.evaluation_error, None);
        set_uninit!(object.pending_async_dependencies, 0);
        set_uninit!(object.async_parent_modules, None);

        let entries = &mut object.entries;
        entries.init_with_uninit(num_entries);

        // Add all import and export entries to the module
        let mut i = 0;
        for entry in imports {
            entries.set_unchecked(i, ModuleEntry::Import(entry.to_heap()));
            i += 1;
        }
        for entry in local_exports {
            entries.set_unchecked(i, ModuleEntry::LocalExport(entry.to_heap()));
            i += 1;
        }
        for entry in named_re_exports {
            entries.set_unchecked(i, ModuleEntry::NamedReExport(entry.to_heap()));
            i += 1;
        }
        for entry in direct_re_exports {
            entries.set_unchecked(i, ModuleEntry::DirectReExport(entry.to_heap()));
            i += 1;
        }

        object.to_handle()
    }

    const ENTRIES_OFFSET: usize = field_offset!(SourceTextModule, entries);

    #[inline]
    fn calculate_size_in_bytes(num_entries: usize) -> usize {
        let entries_size = InlineArray::<ModuleEntry>::calculate_size_in_bytes(num_entries);
        Self::ENTRIES_OFFSET + entries_size
    }

    #[inline]
    pub fn id(&self) -> ModuleId {
        self.id
    }

    #[inline]
    pub fn state(&self) -> ModuleState {
        self.state
    }

    #[inline]
    pub fn set_state(&mut self, state: ModuleState) {
        self.state = state;
    }

    #[inline]
    pub fn has_top_level_await(&self) -> bool {
        self.has_top_level_await
    }

    #[inline]
    pub fn is_async_evaluation(&self) -> bool {
        self.async_evaluation_index.is_some()
    }

    #[inline]
    pub fn async_evaluation_index(&self) -> Option<NonZeroUsize> {
        self.async_evaluation_index
    }

    #[inline]
    pub fn set_async_evaluation(&mut self, mut cx: Context, is_async_evaluation: bool) {
        if is_async_evaluation {
            // Increment the async evaluation counter so we can track the order [[AsyncEvaluation]]
            // was set.
            let index = cx.async_evaluation_counter;
            cx.async_evaluation_counter = index.checked_add(1).unwrap();
            self.async_evaluation_index = Some(index);
        } else {
            self.async_evaluation_index = None;
        }
    }

    #[inline]
    pub fn dfs_index(&self) -> u32 {
        self.dfs_index
    }

    #[inline]
    pub fn set_dfs_index(&mut self, dfs_index: u32) {
        self.dfs_index = dfs_index;
    }

    #[inline]
    pub fn dfs_ancestor_index(&self) -> u32 {
        self.dfs_ancestor_index
    }

    #[inline]
    pub fn set_dfs_ancestor_index(&mut self, dfs_ancestor_index: u32) {
        self.dfs_ancestor_index = dfs_ancestor_index;
    }

    #[inline]
    pub fn cycle_root_ptr(&self) -> Option<HeapPtr<SourceTextModule>> {
        self.cycle_root
    }

    #[inline]
    pub fn cycle_root(&self) -> Option<Handle<SourceTextModule>> {
        self.cycle_root_ptr().map(|root| root.to_handle())
    }

    #[inline]
    pub fn set_cycle_root(&mut self, cycle_root: HeapPtr<SourceTextModule>) {
        self.cycle_root = Some(cycle_root);
    }

    #[inline]
    pub fn top_level_capability_ptr(&self) -> Option<HeapPtr<PromiseCapability>> {
        self.top_level_capability
    }

    #[inline]
    pub fn set_top_level_capability(&mut self, capability: HeapPtr<PromiseCapability>) {
        self.top_level_capability = Some(capability);
    }

    #[inline]
    pub fn evaluation_error_ptr(&self) -> Option<Value> {
        self.evaluation_error
    }

    #[inline]
    pub fn evaluation_error(&self, cx: Context) -> Option<Handle<Value>> {
        self.evaluation_error_ptr().map(|error| error.to_handle(cx))
    }

    #[inline]
    pub fn set_evaluation_error(&mut self, error: Value) {
        self.evaluation_error = Some(error);
    }

    #[inline]
    pub fn pending_async_dependencies(&self) -> usize {
        self.pending_async_dependencies
    }

    #[inline]
    pub fn inc_pending_async_dependencies(&mut self) {
        self.pending_async_dependencies += 1;
    }

    #[inline]
    pub fn dec_pending_async_dependencies(&mut self) {
        self.pending_async_dependencies -= 1;
    }

    #[inline]
    pub fn async_parent_modules_ptr(&self) -> Option<HeapPtr<AsyncParentModulesVec>> {
        self.async_parent_modules
    }

    #[inline]
    pub fn async_parent_modules(&self) -> Option<Handle<AsyncParentModulesVec>> {
        self.async_parent_modules_ptr()
            .map(|modules| modules.to_handle())
    }

    #[inline]
    pub fn program_function_ptr(&self) -> HeapPtr<BytecodeFunction> {
        self.program_function
    }

    #[inline]
    pub fn program_function(&self) -> Handle<BytecodeFunction> {
        self.program_function_ptr().to_handle()
    }

    #[inline]
    pub fn module_scope_ptr(&self) -> HeapPtr<Scope> {
        self.module_scope
    }

    #[inline]
    pub fn module_scope(&self) -> Handle<Scope> {
        self.module_scope_ptr().to_handle()
    }

    #[inline]
    pub fn exports_ptr(&self) -> HeapPtr<ExportMap> {
        self.exports.unwrap()
    }

    #[inline]
    pub fn requested_modules(&self) -> Handle<ModuleRequestArray> {
        self.requested_modules.to_handle()
    }

    #[inline]
    pub fn loaded_modules(&self) -> Handle<ModuleOptionArray> {
        self.loaded_modules.to_handle()
    }

    pub fn lookup_module_request_index(&self, module_request: &HeapModuleRequest) -> Option<usize> {
        self.requested_modules
            .as_slice()
            .iter()
            .position(|request| request == module_request)
    }

    pub fn has_loaded_module_at(&self, index: usize) -> bool {
        self.loaded_modules.as_slice()[index].is_some()
    }

    pub fn get_loaded_module_at(&self, index: usize) -> HeapPtr<SourceTextModule> {
        self.loaded_modules.as_slice()[index].unwrap()
    }

    pub fn set_loaded_module_at(&mut self, index: usize, module: HeapPtr<SourceTextModule>) {
        self.loaded_modules.as_mut_slice()[index] = Some(module);
    }

    #[inline]
    pub fn entries_as_slice(&self) -> &[ModuleEntry] {
        self.entries.as_slice()
    }

    pub fn source_file_path(&self) -> Handle<FlatString> {
        let source_file = self.program_function_ptr().source_file_ptr().unwrap();
        source_file.path()
    }
}

impl HeapPtr<SourceTextModule> {
    /// ResolveExport (https://tc39.es/ecma262/#sec-resolveexport)
    pub fn resolve_export(
        &self,
        cx: Context,
        export_name: HeapPtr<FlatString>,
        resolve_set: &mut Vec<(HeapPtr<FlatString>, HeapPtr<SourceTextModule>)>,
    ) -> ResolveExportResult {
        let is_circular_import = resolve_set
            .iter()
            .any(|(name, module)| self.ptr_eq(module) && name == &export_name);
        if is_circular_import {
            return ResolveExportResult::Circular;
        }

        resolve_set.push((export_name, *self));

        // Check if there is a local export entry with the given export name
        for entry in self.entries.as_slice() {
            if let ModuleEntry::LocalExport(entry) = entry {
                if entry.export_name == export_name {
                    let boxed_value = self.module_scope.get_module_slot(entry.slot_index);

                    return ResolveExportResult::Resolved {
                        name: ResolveExportName::Local { name: entry.local_name, boxed_value },
                        module: *self,
                    };
                }
            }
        }

        // Next check if there is a named re-export entry with the given export name
        for entry in self.entries.as_slice() {
            if let ModuleEntry::NamedReExport(entry) = entry {
                if entry.export_name == export_name {
                    let imported_module = self.get_imported_module(&entry.module_request);

                    if let Some(import_name) = entry.import_name {
                        // Re-export of a named import tries to resolve in other module
                        return imported_module.resolve_export(cx, import_name, resolve_set);
                    } else {
                        // Otherwise this is the named re-export of a namespace object
                        return ResolveExportResult::Resolved {
                            name: ResolveExportName::Namespace,
                            module: imported_module,
                        };
                    }
                }
            }
        }

        // A default export was not explicitly defined by this module
        if export_name == cx.names.default.as_string().as_flat() {
            return ResolveExportResult::None;
        }

        // Star resolution is lazily initialized the first time the export name is resolved
        let mut star_resolution = ResolveExportResult::None;

        for entry in self.entries.as_slice() {
            if let ModuleEntry::DirectReExport(entry) = entry {
                let imported_module = self.get_imported_module(&entry.module_request);
                let resolution = imported_module.resolve_export(cx, export_name, resolve_set);

                match resolution {
                    ResolveExportResult::Ambiguous => return ResolveExportResult::Ambiguous,
                    ResolveExportResult::Resolved {
                        module: resolution_module,
                        name: resolution_name,
                        ..
                    } => {
                        // Multiple resolutions must match or the match is ambiguous. Two
                        // resolutions match iff they are for the same module and the same name.
                        if let ResolveExportResult::Resolved {
                            module: star_resolution_module,
                            name: star_resolution_name,
                            ..
                        } = star_resolution
                        {
                            // Check that the same module is resolved to
                            if !resolution_module.ptr_eq(&star_resolution_module) {
                                return ResolveExportResult::Ambiguous;
                            }

                            // Check that the same name is resolved to within that module
                            let same_name = match (resolution_name, star_resolution_name) {
                                (
                                    ResolveExportName::Local { name: name1, .. },
                                    ResolveExportName::Local { name: name2, .. },
                                ) => name1 == name2,
                                (ResolveExportName::Namespace, ResolveExportName::Namespace) => {
                                    true
                                }
                                _ => false,
                            };

                            if !same_name {
                                return ResolveExportResult::Ambiguous;
                            }
                        } else {
                            // Star resolution is lazily initialized on the first resolution
                            star_resolution = resolution;
                        }
                    }
                    ResolveExportResult::Circular | ResolveExportResult::None => {}
                }
            }
        }

        star_resolution
    }

    /// GetImportedModule (https://tc39.es/ecma262/#sec-GetImportedModule)
    pub fn get_imported_module(
        &self,
        module_request: &HeapModuleRequest,
    ) -> HeapPtr<SourceTextModule> {
        let index = self.lookup_module_request_index(module_request).unwrap();
        self.get_loaded_module_at(index)
    }

    /// GetExportedNames (https://tc39.es/ecma262/#sec-getexportednames)
    fn get_exported_names(
        &self,
        cx: Context,
        exported_names: &mut HashSet<Handle<FlatString>>,
        visited_set: &mut HashSet<ModuleId>,
    ) {
        // Reached a circular import
        if !visited_set.insert(self.id()) {
            return;
        }

        for entry in self.entries_as_slice() {
            match entry {
                // Named exports are added to the set of exported names
                ModuleEntry::LocalExport(HeapLocalExportEntry { export_name, .. })
                | ModuleEntry::NamedReExport(HeapNamedReExportEntry { export_name, .. }) => {
                    exported_names.insert(export_name.to_handle());
                }
                ModuleEntry::DirectReExport(named_re_export) => {
                    // Add all names from the requested module to the set of exported names, excluding
                    // the default export.
                    let requested_module =
                        self.get_imported_module(&named_re_export.module_request);

                    let mut re_exported_names = HashSet::new();
                    requested_module.get_exported_names(cx, &mut re_exported_names, visited_set);

                    for export_name in re_exported_names {
                        if *export_name != cx.names.default.as_string().as_flat() {
                            exported_names.insert(export_name);
                        }
                    }
                }
                ModuleEntry::Import(_) => {}
            }
        }
    }
}

impl Handle<SourceTextModule> {
    #[inline]
    fn async_parent_modules_field(&self) -> AsyncParentModulesField {
        AsyncParentModulesField(self.to_handle())
    }

    #[inline]
    fn exports_field(&self) -> ExportMapField {
        ExportMapField(*self)
    }

    pub fn insert_export(
        &self,
        cx: Context,
        export_name: Handle<PropertyKey>,
        boxed_value_or_module: Handle<HeapItem>,
    ) -> bool {
        self.exports_field()
            .maybe_grow_for_insertion(cx)
            .insert_without_growing(*export_name, *boxed_value_or_module)
    }

    pub fn push_async_parent_module(
        &mut self,
        cx: Context,
        parent_module: Handle<SourceTextModule>,
    ) {
        // Lazily initialize the async parent modules vec
        if self.async_parent_modules.is_none() {
            let async_parent_modules = BsVec::new(cx, ObjectKind::ValueVec, 4);
            self.async_parent_modules = Some(async_parent_modules);
        }

        self.async_parent_modules_field()
            .maybe_grow_for_push(cx)
            .push_without_growing(*parent_module);
    }

    /// GetModuleNamespace (https://tc39.es/ecma262/#sec-getmodulenamespace)
    ///
    /// Returns the namespace object used when this module is namespace imported. Lazily creates the
    /// cached namespace object and exports map.
    pub fn get_namespace_object(&mut self, cx: Context) -> HeapPtr<ModuleNamespaceObject> {
        if let Some(namespace_object) = self.namespace_object {
            return namespace_object;
        }

        HandleScope::new(cx, |cx| {
            // Lazily initialize the exports map
            self.exports = Some(ExportMap::new(cx, ObjectKind::ExportMap, 4));

            let mut exported_names = HashSet::new();
            self.get_exported_names(cx, &mut exported_names, &mut HashSet::new());

            // Share handle between iterations
            let mut key_handle: Handle<PropertyKey> = Handle::empty(cx);
            let mut boxed_value_or_module_handle: Handle<HeapItem> = Handle::empty(cx);

            for export_name in exported_names {
                // First convert the export name to a PropertyKey
                key_handle.replace(PropertyKey::string(cx, export_name.as_string()));

                let result = self.resolve_export(cx, *export_name, &mut vec![]);

                // Ignore unresolved exports, this will lead to a linker error later
                if let ResolveExportResult::Resolved { name, module } = result {
                    match name {
                        ResolveExportName::Local { boxed_value, .. } => {
                            boxed_value_or_module_handle.replace(boxed_value.as_heap_item());
                            self.insert_export(cx, key_handle, boxed_value_or_module_handle);
                        }
                        ResolveExportName::Namespace => {
                            boxed_value_or_module_handle.replace(module.as_heap_item());
                            self.insert_export(cx, key_handle, boxed_value_or_module_handle);
                        }
                    }
                }
            }
        });

        // Create and cache the namespace object for this module
        let namespace_object = ModuleNamespaceObject::new(cx, *self);
        self.namespace_object = Some(namespace_object);

        namespace_object
    }

    /// Returns the `import.meta` object for this module. Lazily creates and caches the object when
    /// first accessed.
    pub fn get_import_meta_object(&mut self, cx: Context) -> HeapPtr<ObjectValue> {
        if let Some(import_meta) = self.import_meta {
            return import_meta;
        }

        // No properties are added to the `import.meta` object - this is up to the implementation
        let object =
            object_create_with_optional_proto::<ObjectValue>(cx, ObjectKind::OrdinaryObject, None);

        self.import_meta = Some(object);

        object
    }
}

#[derive(Clone, Copy)]
pub enum ResolveExportResult {
    Resolved { name: ResolveExportName, module: HeapPtr<SourceTextModule> },
    Ambiguous,
    Circular,
    None,
}

#[derive(Clone, Copy)]
pub enum ResolveExportName {
    /// No local name since this is a namespace import.
    Namespace,
    /// The local name of the binding in the module.
    Local { name: HeapPtr<FlatString>, boxed_value: HeapPtr<BoxedValue> },
}

impl HeapObject for HeapPtr<SourceTextModule> {
    fn byte_size(&self) -> usize {
        SourceTextModule::calculate_size_in_bytes(self.entries.len())
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_pointer(&mut self.program_function);
        visitor.visit_pointer(&mut self.module_scope);
        visitor.visit_pointer_opt(&mut self.import_meta);
        visitor.visit_pointer_opt(&mut self.namespace_object);
        visitor.visit_pointer_opt(&mut self.exports);
        visitor.visit_pointer(&mut self.requested_modules);
        visitor.visit_pointer(&mut self.loaded_modules);
        visitor.visit_pointer_opt(&mut self.cycle_root);
        visitor.visit_pointer_opt(&mut self.top_level_capability);

        if let Some(error) = self.evaluation_error.as_mut() {
            visitor.visit_value(error);
        }

        visitor.visit_pointer_opt(&mut self.async_parent_modules);

        for entry in self.entries.as_mut_slice() {
            match entry {
                ModuleEntry::Import(import_entry) => {
                    import_entry.module_request.visit_pointers(visitor);
                    visitor.visit_pointer_opt(&mut import_entry.import_name);
                    visitor.visit_pointer(&mut import_entry.local_name);
                }
                ModuleEntry::LocalExport(local_export_entry) => {
                    visitor.visit_pointer(&mut local_export_entry.export_name);
                    visitor.visit_pointer(&mut local_export_entry.local_name);
                }
                ModuleEntry::NamedReExport(named_re_export_entry) => {
                    visitor.visit_pointer(&mut named_re_export_entry.export_name);
                    visitor.visit_pointer_opt(&mut named_re_export_entry.import_name);
                    named_re_export_entry.module_request.visit_pointers(visitor);
                }
                ModuleEntry::DirectReExport(direct_re_export_entry) => {
                    direct_re_export_entry
                        .module_request
                        .visit_pointers(visitor);
                }
            }
        }
    }
}

pub type ModuleId = usize;

static NEXT_MODULE_ID: LazyLock<Mutex<usize>> = LazyLock::new(|| Mutex::new(0));

fn next_module_id() -> ModuleId {
    let mut next_module_id = NEXT_MODULE_ID.lock().unwrap();
    let module_id = *next_module_id;
    *next_module_id += 1;

    module_id
}

/// A request to import a module with the given attributes.
#[derive(Clone, Copy, PartialEq)]
pub struct HeapModuleRequest {
    /// The module specifier as a string.
    pub specifier: HeapPtr<FlatString>,
    /// The import attributes, if any were specified.
    pub attributes: Option<HeapPtr<ImportAttributes>>,
}

#[derive(Clone, Copy)]
pub struct ModuleRequest {
    pub specifier: Handle<FlatString>,
    pub attributes: Option<Handle<ImportAttributes>>,
}

impl HeapModuleRequest {
    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.specifier);
        visitor.visit_pointer_opt(&mut self.attributes);
    }
}

impl ModuleRequest {
    pub fn from_heap(module_request: &HeapModuleRequest) -> ModuleRequest {
        ModuleRequest {
            specifier: module_request.specifier.to_handle(),
            attributes: module_request.attributes.map(|a| a.to_handle()),
        }
    }

    pub fn to_heap(self) -> HeapModuleRequest {
        HeapModuleRequest {
            specifier: *self.specifier,
            attributes: self.attributes.map(|a| *a),
        }
    }
}

impl PartialEq for ModuleRequest {
    fn eq(&self, other: &Self) -> bool {
        self.specifier == other.specifier
            && self.attributes.as_deref() == other.attributes.as_deref()
    }
}

impl Eq for ModuleRequest {}

impl Hash for ModuleRequest {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Ignore attributes for hashing
        self.specifier.hash(state);
    }
}

pub enum ModuleEntry {
    Import(HeapImportEntry),
    LocalExport(HeapLocalExportEntry),
    NamedReExport(HeapNamedReExportEntry),
    DirectReExport(HeapDirectReExportEntry),
}

/// ImportEntry (https://tc39.es/ecma262/#table-importentry-record-fields)
pub struct HeapImportEntry {
    /// Module that binding is being imported from.
    pub module_request: HeapModuleRequest,
    /// Name of the binding in the module it was declared in. If None then this entry is for the
    /// namespace object, meaning the slot_index may refer to an unboxed value.
    pub import_name: Option<HeapPtr<FlatString>>,
    /// Name of the imported binding in this module.
    pub local_name: HeapPtr<FlatString>,
    /// Slot in the module scope where the imported binding is stored.
    pub slot_index: usize,
    /// Whether this binding is exported
    pub is_exported: bool,
}

pub struct ImportEntry {
    pub module_request: ModuleRequest,
    pub import_name: Option<Handle<FlatString>>,
    pub local_name: Handle<FlatString>,
    pub slot_index: usize,
    pub is_exported: bool,
}

impl ImportEntry {
    pub fn from_heap(entry: &HeapImportEntry) -> ImportEntry {
        ImportEntry {
            module_request: ModuleRequest::from_heap(&entry.module_request),
            import_name: entry.import_name.map(|name| name.to_handle()),
            local_name: entry.local_name.to_handle(),
            slot_index: entry.slot_index,
            is_exported: entry.is_exported,
        }
    }

    fn to_heap(&self) -> HeapImportEntry {
        HeapImportEntry {
            module_request: self.module_request.to_heap(),
            import_name: self.import_name.map(|name| *name),
            local_name: *self.local_name,
            slot_index: self.slot_index,
            is_exported: self.is_exported,
        }
    }
}

/// Subset of ExportEntry (https://tc39.es/ecma262/#table-exportentry-records) corresponding to
/// the field SourceTextModule.[[LocalExportEntries]].
///
/// Corresponds to regular exports of local bindings including default exports.
pub struct HeapLocalExportEntry {
    /// The name of the export, i.e. the name that importers must reference.
    export_name: HeapPtr<FlatString>,
    /// The name of the exported binding within its module.
    local_name: HeapPtr<FlatString>,
    /// Slot in the module scope where the exported binding is stored.
    slot_index: usize,
}

pub struct LocalExportEntry {
    pub export_name: Handle<FlatString>,
    pub local_name: Handle<FlatString>,
    pub slot_index: usize,
}

impl LocalExportEntry {
    fn to_heap(&self) -> HeapLocalExportEntry {
        HeapLocalExportEntry {
            export_name: *self.export_name,
            local_name: *self.local_name,
            slot_index: self.slot_index,
        }
    }
}

/// Subset of ExportEntry (https://tc39.es/ecma262/#table-exportentry-records) corresponding to
/// the field SourceTextModule.[[IndirectExportEntries]].
///
/// Corresponds to named re-exports of bindings from other modules, including both
/// `export {x} from "mod"` and `export * as x from "mod`.
pub struct HeapNamedReExportEntry {
    /// The name of the export, i.e. the name that importers must reference.
    pub export_name: HeapPtr<FlatString>,
    /// Name of the re-exported binding within its module. If None this is a named re-export of
    /// a namespace object.
    import_name: Option<HeapPtr<FlatString>>,
    /// Module that is having a bindings re-exported.
    module_request: HeapModuleRequest,
}

pub struct NamedReExportEntry {
    pub export_name: Handle<FlatString>,
    pub import_name: Option<Handle<FlatString>>,
    pub module_request: ModuleRequest,
}

impl NamedReExportEntry {
    fn to_heap(&self) -> HeapNamedReExportEntry {
        HeapNamedReExportEntry {
            export_name: *self.export_name,
            import_name: self.import_name.map(|name| *name),
            module_request: self.module_request.to_heap(),
        }
    }
}

/// Subset of ExportEntry (https://tc39.es/ecma262/#table-exportentry-records) corresponding to
/// the field SourceTextModule.[[StarExportEntries]].
///
/// Coressponds to the direct re-export of all bindings from another module,
/// i.e. `export * from "mod"`.
pub struct HeapDirectReExportEntry {
    /// Module that is having its bindings re-exported.
    module_request: HeapModuleRequest,
}

pub struct DirectReExportEntry {
    pub module_request: ModuleRequest,
}

impl DirectReExportEntry {
    fn to_heap(&self) -> HeapDirectReExportEntry {
        HeapDirectReExportEntry { module_request: self.module_request.to_heap() }
    }
}

pub fn module_request_array_byte_size(array: HeapPtr<ModuleRequestArray>) -> usize {
    ModuleRequestArray::calculate_size_in_bytes(array.len())
}

pub fn module_request_array_visit_pointers(
    array: &mut HeapPtr<ModuleRequestArray>,
    visitor: &mut impl HeapVisitor,
) {
    array.visit_pointers(visitor);

    for module_request in array.as_mut_slice() {
        module_request.visit_pointers(visitor);
    }
}

#[derive(Clone)]
pub struct ExportMapField(Handle<SourceTextModule>);

impl BsHashMapField<PropertyKey, HeapPtr<HeapItem>> for ExportMapField {
    fn new_map(&self, cx: Context, capacity: usize) -> HeapPtr<ExportMap> {
        ExportMap::new(cx, ObjectKind::ExportMap, capacity)
    }

    fn get(&self, _: Context) -> HeapPtr<ExportMap> {
        self.0.exports.unwrap()
    }

    fn set(&mut self, _: Context, map: HeapPtr<ExportMap>) {
        self.0.exports = Some(map);
    }
}

impl ExportMapField {
    pub fn byte_size(map: &HeapPtr<ExportMap>) -> usize {
        ExportMap::calculate_size_in_bytes(map.capacity())
    }

    pub fn visit_pointers(map: &mut HeapPtr<ExportMap>, visitor: &mut impl HeapVisitor) {
        map.visit_pointers(visitor);

        for (key, value) in map.iter_mut_gc_unsafe() {
            visitor.visit_property_key(key);
            visitor.visit_pointer(value);
        }
    }
}

type AsyncParentModulesVec = BsVec<HeapPtr<SourceTextModule>>;

struct AsyncParentModulesField(Handle<SourceTextModule>);

impl BsVecField<HeapPtr<SourceTextModule>> for AsyncParentModulesField {
    fn new_vec(cx: Context, capacity: usize) -> HeapPtr<AsyncParentModulesVec> {
        BsVec::new(cx, ObjectKind::ValueVec, capacity)
    }

    fn get(&self) -> HeapPtr<AsyncParentModulesVec> {
        self.0.async_parent_modules.unwrap()
    }

    fn set(&mut self, vec: HeapPtr<AsyncParentModulesVec>) {
        self.0.async_parent_modules = Some(vec);
    }
}
