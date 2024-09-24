use crate::{
    js::runtime::{
        error::syntax_error, module::source_text_module::ModuleState, object_value::ObjectValue,
        Context, EvalResult, Handle, HeapPtr,
    },
    maybe,
};

use super::source_text_module::{
    ModuleEntry, ResolveExportName, ResolveExportResult, SourceTextModule,
};

struct GraphLinker {
    stack: Vec<Handle<SourceTextModule>>,
}

impl GraphLinker {
    fn new() -> Self {
        Self { stack: vec![] }
    }

    /// Link (https://tc39.es/ecma262/#sec-moduledeclarationlinking)
    fn link(&mut self, cx: Context, module: Handle<SourceTextModule>) -> EvalResult<()> {
        // Assert state precondition
        debug_assert!(matches!(
            module.state(),
            ModuleState::Unlinked
                | ModuleState::Linked
                | ModuleState::EvaluatingAsync
                | ModuleState::Evaluated
        ));

        match self.inner_link(cx, module, 0) {
            EvalResult::Ok(_) => {
                // Assert state postcondition
                debug_assert!(matches!(
                    module.state(),
                    ModuleState::Linked | ModuleState::EvaluatingAsync | ModuleState::Evaluated
                ));
                debug_assert!(self.stack.is_empty());

                ().into()
            }
            EvalResult::Throw(error) => {
                // Any error should reset all modules in stack to unlinked
                for module in &mut self.stack {
                    debug_assert!(module.state() == ModuleState::Linking);
                    module.set_state(ModuleState::Unlinked);
                }

                // Assert state postcondition
                debug_assert!(module.state() == ModuleState::Unlinked);

                EvalResult::Throw(error)
            }
        }
    }

    /// InnerModuleLinking (https://tc39.es/ecma262/#sec-InnerModuleLinking)
    fn inner_link(
        &mut self,
        cx: Context,
        mut module: Handle<SourceTextModule>,
        index: u32,
    ) -> EvalResult<u32> {
        // Only start linking when in the unlinked state
        if module.state() != ModuleState::Unlinked {
            // Check for valid states
            debug_assert!(matches!(
                module.state(),
                ModuleState::Linking
                    | ModuleState::Linked
                    | ModuleState::EvaluatingAsync
                    | ModuleState::Evaluated
            ));

            return index.into();
        }

        module.set_state(ModuleState::Linking);
        module.set_dfs_index(index);
        module.set_dfs_ancestor_index(index);

        self.stack.push(module);

        let mut index = index + 1;

        let loaded_modules = module.loaded_modules();
        for i in 0..loaded_modules.len() {
            let required_module = loaded_modules.as_slice()[i].unwrap().to_handle();

            index = maybe!(self.inner_link(cx, required_module, index));

            debug_assert!(matches!(
                required_module.state(),
                ModuleState::Linking
                    | ModuleState::Linked
                    | ModuleState::EvaluatingAsync
                    | ModuleState::Evaluated
            ));

            if required_module.state() == ModuleState::Linking {
                let new_index = module
                    .dfs_ancestor_index()
                    .min(required_module.dfs_ancestor_index());
                module.set_dfs_ancestor_index(new_index)
            }
        }

        maybe!(initialize_environment(cx, module));

        debug_assert!(module.dfs_ancestor_index() <= module.dfs_index());

        if module.dfs_index() == module.dfs_ancestor_index() {
            loop {
                let mut required_module = self.stack.pop().unwrap();
                required_module.set_state(ModuleState::Linked);

                if required_module.ptr_eq(&module.get_()) {
                    break;
                }
            }
        }

        index.into()
    }
}

/// InitializeEnvironment (https://tc39.es/ecma262/#sec-source-text-module-record-initialize-environment)
fn initialize_environment(cx: Context, module: Handle<SourceTextModule>) -> EvalResult<()> {
    // Check that all re-exported names are resolvable
    for entry in module.entries_as_slice() {
        if let ModuleEntry::NamedReExport(entry) = entry {
            let resolution = module.resolve_export(cx, entry.export_name, &mut vec![]);
            if !matches!(resolution, ResolveExportResult::Resolved { .. }) {
                return syntax_error(cx, "could not resolve module specifier");
            }
        }
    }

    // Initialize import bindings
    for i in 0..module.entries_as_slice().len() {
        if let ModuleEntry::Import(entry) = &module.entries_as_slice()[i] {
            let imported_module = module.get_imported_module(entry.module_request);

            if let Some(import_name) = entry.import_name {
                let resolution = imported_module.resolve_export(cx, import_name, &mut vec![]);

                match resolution {
                    // Regular imports are linked to their corresponding export by referencing
                    // the same boxed value.
                    ResolveExportResult::Resolved {
                        name: ResolveExportName::Local { boxed_value, .. },
                        ..
                    } => {
                        module
                            .module_scope_ptr()
                            .set_slot(entry.slot_index, boxed_value.cast::<ObjectValue>().into());
                    }
                    // Namespace object may be stored as a module or scope value
                    ResolveExportResult::Resolved {
                        name: ResolveExportName::Namespace,
                        module: resolved_module,
                    } => {
                        let namespace_object = resolved_module.get_namespace_object(cx);
                        set_namespace_object(
                            module.get_(),
                            namespace_object,
                            entry.slot_index,
                            entry.is_exported,
                        );
                    }
                    _ => return syntax_error(cx, "could not resolve module specifier"),
                }
            } else {
                // Namespace object may be stored as a module or scope value
                let namespace_object = imported_module.get_namespace_object(cx);
                set_namespace_object(
                    module.get_(),
                    namespace_object,
                    entry.slot_index,
                    entry.is_exported,
                );
            }
        }
    }

    ().into()
}

fn set_namespace_object(
    module: HeapPtr<SourceTextModule>,
    namespace_object: HeapPtr<ObjectValue>,
    slot_index: usize,
    is_exported: bool,
) {
    if is_exported {
        let mut boxed_value = module.module_scope_ptr().get_module_slot(slot_index);
        boxed_value.set(namespace_object.into());
    } else {
        module
            .module_scope_ptr()
            .set_slot(slot_index, namespace_object.into());
    }
}

pub fn link(cx: Context, module: Handle<SourceTextModule>) -> EvalResult<()> {
    let mut linker = GraphLinker::new();
    linker.link(cx, module)
}
