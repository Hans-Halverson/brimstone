use std::sync::{LazyLock, Mutex};

use crate::{
    field_offset,
    js::runtime::{
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
    /// All import and export entries in the module.
    entries: InlineArray<ModuleEntry>,
}

#[derive(Clone, Copy, PartialEq)]
pub enum ModuleState {
    New,
    Unlinked,
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
    pub fn program_function_ptr(&self) -> HeapPtr<BytecodeFunction> {
        self.program_function
    }

    #[inline]
    #[allow(unused)]
    pub fn program_function(&self) -> Handle<BytecodeFunction> {
        self.program_function_ptr().to_handle()
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

    pub fn set_loaded_module_at(&mut self, index: usize, module: HeapPtr<SourceTextModule>) {
        self.loaded_modules.as_mut_slice()[index] = Some(module);
    }

    pub fn source_file_path(&self) -> String {
        let source_file = self.program_function_ptr().source_file_ptr().unwrap();
        source_file.name().to_string()
    }
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

enum ModuleEntry {
    Import(HeapImportEntry),
    LocalExport(HeapLocalExportEntry),
    NamedReExport(HeapNamedReExportEntry),
    DirectReExport(HeapDirectReExportEntry),
}

/// ImportEntry, https://tc39.es/ecma262/#table-importentry-record-fields
struct HeapImportEntry {
    /// Name of the module that binding is being imported from.
    module_request: HeapPtr<FlatString>,
    /// Name of the binding in the module is was declared in. If None then this entry is for the
    /// namespace object.
    import_name: Option<HeapPtr<FlatString>>,
    /// Name of the imported binding in this module.
    local_name: HeapPtr<FlatString>,
    /// Slot in the module scope where the imported binding is stored.
    #[allow(unused)]
    slot_index: usize,
}

pub struct ImportEntry {
    pub module_request: Handle<FlatString>,
    pub import_name: Option<Handle<FlatString>>,
    pub local_name: Handle<FlatString>,
    pub slot_index: usize,
}

impl ImportEntry {
    fn to_heap(&self) -> HeapImportEntry {
        HeapImportEntry {
            module_request: self.module_request.get_(),
            import_name: self.import_name.map(|name| name.get_()),
            local_name: self.local_name.get_(),
            slot_index: self.slot_index,
        }
    }
}

/// Subset of ExportEntry (https://tc39.es/ecma262/#table-exportentry-records) corresponding to
/// the field SourceTextModule.[[LocalExportEntries]].
///
/// Corresponds to regular exports of local bindings including default exports.
struct HeapLocalExportEntry {
    /// The name of the export, i.e. the name that importers must reference.
    export_name: HeapPtr<FlatString>,
    /// The name of the exported binding within its module.
    local_name: HeapPtr<FlatString>,
    /// Slot in the module scope where the exported binding is stored.
    #[allow(unused)]
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
struct HeapNamedReExportEntry {
    /// The name of the export, i.e. the name that importers must reference.
    export_name: HeapPtr<FlatString>,
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
struct HeapDirectReExportEntry {
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
