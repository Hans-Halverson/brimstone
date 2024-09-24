use std::sync::{LazyLock, Mutex};

use crate::{
    field_offset,
    js::runtime::{
        boxed_value::BoxedValue,
        bytecode::function::BytecodeFunction,
        collections::{BsArray, InlineArray},
        gc::{HeapObject, HeapVisitor},
        object_descriptor::{ObjectDescriptor, ObjectKind},
        object_value::ObjectValue,
        scope::Scope,
        string_value::FlatString,
        Context, Handle, HeapPtr,
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
    /// Function that evaluates the module when called in the module scope.
    program_function: HeapPtr<BytecodeFunction>,
    /// Scope for the module. Program function is executed in this scope.
    module_scope: HeapPtr<Scope>,
    /// The `import.meta` object for this module. This is lazily created when first accessed.
    import_meta: Option<HeapPtr<ObjectValue>>,
    /// The set of module specifiers that are requested by this module in imports and re-exports.
    requested_module_specifiers: HeapPtr<StringArray>,
    /// Each index corresponds to the specifier at the same index in `requested_module_specifiers`,
    /// and contains the module object if one has been loaded.
    loaded_modules: HeapPtr<ModuleOptionArray>,
    // Indices used during DFS in the link and evaluate phases.
    dfs_index: u32,
    dfs_ancestor_index: u32,
    /// All import and export entries in the module.
    entries: InlineArray<ModuleEntry>,
}

#[derive(Clone, Copy, PartialEq)]
pub enum ModuleState {
    New,
    Unlinked,
    Linking,
    Linked,
    #[allow(unused)]
    Evaluating,
    #[allow(unused)]
    EvaluatingAsync,
    #[allow(unused)]
    Evaluated,
}

type StringArray = BsArray<HeapPtr<FlatString>>;

type ModuleOptionArray = BsArray<Option<HeapPtr<SourceTextModule>>>;

impl SourceTextModule {
    pub fn new(
        cx: Context,
        program_function: Handle<BytecodeFunction>,
        module_scope: Handle<Scope>,
        requested_module_specifiers: &[Handle<FlatString>],
        imports: &[ImportEntry],
        local_exports: &[LocalExportEntry],
        named_re_exports: &[NamedReExportEntry],
        direct_re_exports: &[DirectReExportEntry],
    ) -> Handle<SourceTextModule> {
        // First create arrays for requested and loaded modules. Requested modules are initialized
        // from arguments, loaded modules are initialized to None.
        let num_specifiers = requested_module_specifiers.len();
        let mut heap_requested_module_specifiers =
            BsArray::new_uninit(cx, ObjectKind::ValueArray, num_specifiers).to_handle();
        for (dst, src) in heap_requested_module_specifiers
            .as_mut_slice()
            .iter_mut()
            .zip(requested_module_specifiers.iter())
        {
            *dst = src.get_();
        }

        let loaded_modules =
            BsArray::new(cx, ObjectKind::ValueArray, requested_module_specifiers.len(), None)
                .to_handle();

        // Then create the uninitialized module object
        let num_entries =
            imports.len() + local_exports.len() + named_re_exports.len() + direct_re_exports.len();
        let size = Self::calculate_size_in_bytes(num_entries);
        let mut object = cx.alloc_uninit_with_size::<SourceTextModule>(size);

        set_uninit!(object.descriptor, cx.base_descriptors.get(ObjectKind::SourceTextModule));
        set_uninit!(object.id, next_module_id());
        set_uninit!(object.state, ModuleState::New);
        set_uninit!(object.program_function, program_function.get_());
        set_uninit!(object.module_scope, module_scope.get_());
        set_uninit!(object.import_meta, None);
        set_uninit!(object.requested_module_specifiers, heap_requested_module_specifiers.get_());
        set_uninit!(object.loaded_modules, loaded_modules.get_());
        set_uninit!(object.dfs_index, 0);
        set_uninit!(object.dfs_ancestor_index, 0);

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
    pub fn program_function_ptr(&self) -> HeapPtr<BytecodeFunction> {
        self.program_function
    }

    #[inline]
    #[allow(unused)]
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
    pub fn requested_module_specifiers(&self) -> Handle<StringArray> {
        self.requested_module_specifiers.to_handle()
    }

    #[inline]
    pub fn loaded_modules(&self) -> Handle<ModuleOptionArray> {
        self.loaded_modules.to_handle()
    }

    pub fn lookup_specifier_index(&self, module_specifier: HeapPtr<FlatString>) -> Option<usize> {
        self.requested_module_specifiers
            .as_slice()
            .iter()
            .position(|&specifier| specifier == module_specifier)
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

    pub fn source_file_path(&self) -> String {
        let source_file = self.program_function_ptr().source_file_ptr().unwrap();
        source_file.name().to_string()
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
                    let imported_module = self.get_imported_module(entry.module_request);

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
                let imported_module = self.get_imported_module(entry.module_request);
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
        module_specifier: HeapPtr<FlatString>,
    ) -> HeapPtr<SourceTextModule> {
        let index = self.lookup_specifier_index(module_specifier).unwrap();
        self.get_loaded_module_at(index)
    }

    /// GetModuleNamespace (https://tc39.es/ecma262/#sec-getmodulenamespace)
    pub fn get_namespace_object(&self, _: Context) -> HeapPtr<ObjectValue> {
        unimplemented!("get namespace object")
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
        visitor.visit_pointer(&mut self.requested_module_specifiers);
        visitor.visit_pointer(&mut self.loaded_modules);

        for entry in self.entries.as_mut_slice() {
            match entry {
                ModuleEntry::Import(import_entry) => {
                    visitor.visit_pointer(&mut import_entry.module_request);
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
                    visitor.visit_pointer(&mut named_re_export_entry.module_request);
                }
                ModuleEntry::DirectReExport(direct_re_export_entry) => {
                    visitor.visit_pointer(&mut direct_re_export_entry.module_request);
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

pub enum ModuleEntry {
    Import(HeapImportEntry),
    LocalExport(HeapLocalExportEntry),
    NamedReExport(HeapNamedReExportEntry),
    DirectReExport(HeapDirectReExportEntry),
}

/// ImportEntry (https://tc39.es/ecma262/#table-importentry-record-fields)
pub struct HeapImportEntry {
    /// Name of the module that binding is being imported from.
    pub module_request: HeapPtr<FlatString>,
    /// Name of the binding in the module is was declared in. If None then this entry is for the
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
    pub module_request: Handle<FlatString>,
    pub import_name: Option<Handle<FlatString>>,
    pub local_name: Handle<FlatString>,
    pub slot_index: usize,
    pub is_exported: bool,
}

impl ImportEntry {
    fn to_heap(&self) -> HeapImportEntry {
        HeapImportEntry {
            module_request: self.module_request.get_(),
            import_name: self.import_name.map(|name| name.get_()),
            local_name: self.local_name.get_(),
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
            export_name: self.export_name.get_(),
            local_name: self.local_name.get_(),
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
    /// Name of the module that is having a bindings re-exported.
    module_request: HeapPtr<FlatString>,
}

pub struct NamedReExportEntry {
    pub export_name: Handle<FlatString>,
    pub import_name: Option<Handle<FlatString>>,
    pub module_request: Handle<FlatString>,
}

impl NamedReExportEntry {
    fn to_heap(&self) -> HeapNamedReExportEntry {
        HeapNamedReExportEntry {
            export_name: self.export_name.get_(),
            import_name: self.import_name.map(|name| name.get_()),
            module_request: self.module_request.get_(),
        }
    }
}

/// Subset of ExportEntry (https://tc39.es/ecma262/#table-exportentry-records) corresponding to
/// the field SourceTextModule.[[StarExportEntries]].
///
/// Coressponds to the direct re-export of all bindings from another module,
/// i.e. `export * from "mod"`.
pub struct HeapDirectReExportEntry {
    /// Name of the module that is having its bindings re-exported.
    module_request: HeapPtr<FlatString>,
}

pub struct DirectReExportEntry {
    pub module_request: Handle<FlatString>,
}

impl DirectReExportEntry {
    fn to_heap(&self) -> HeapDirectReExportEntry {
        HeapDirectReExportEntry { module_request: self.module_request.get_() }
    }
}
