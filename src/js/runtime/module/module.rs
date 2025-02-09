use std::{
    collections::HashSet,
    sync::{LazyLock, Mutex},
};

use crate::{
    heap_trait_object,
    js::runtime::{
        boxed_value::BoxedValue, gc::HeapItem, promise_object::PromiseObject,
        string_value::FlatString, Context, EvalResult, Handle, HeapPtr,
    },
};

use super::{
    module_namespace_object::ModuleNamespaceObject, source_text_module::SourceTextModule,
    synthetic_module::SyntheticModule,
};

/// A generic module. May be a SourceTextModule or a SyntheticModule.
pub trait Module {
    fn as_enum(&self) -> ModuleEnum;

    fn as_source_text_module(&self) -> Option<Handle<SourceTextModule>> {
        None
    }

    fn load_requested_modules(&self, cx: Context) -> Handle<PromiseObject>;

    fn get_exported_names(
        &self,
        cx: Context,
        exported_names: &mut HashSet<Handle<FlatString>>,
        visited_set: &mut HashSet<ModuleId>,
    );

    fn resolve_export(
        &self,
        cx: Context,
        export_name: HeapPtr<FlatString>,
        resolve_set: &mut Vec<(HeapPtr<FlatString>, HeapPtr<SourceTextModule>)>,
    ) -> ResolveExportResult;

    fn link(&self, cx: Context) -> EvalResult<()>;

    fn evaluate(&self, cx: Context) -> Handle<PromiseObject>;

    fn get_namespace_object(&mut self, cx: Context) -> HeapPtr<ModuleNamespaceObject>;
}

pub enum ModuleEnum {
    SourceText(Handle<SourceTextModule>),
    Synthetic(Handle<SyntheticModule>),
}

#[derive(Clone, Copy)]
pub enum ResolveExportResult {
    Resolved { name: ResolveExportName, module: DynModule },
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

heap_trait_object!(Module, DynModule, HeapDynModule, into_dyn_module);

impl DynModule {
    pub fn as_heap_item(self) -> Handle<HeapItem> {
        self.data.cast()
    }
}

pub type ModuleId = usize;

static NEXT_MODULE_ID: LazyLock<Mutex<usize>> = LazyLock::new(|| Mutex::new(0));

pub fn next_module_id() -> ModuleId {
    let mut next_module_id = NEXT_MODULE_ID.lock().unwrap();
    let module_id = *next_module_id;
    *next_module_id += 1;

    module_id
}
