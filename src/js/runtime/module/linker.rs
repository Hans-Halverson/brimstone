use crate::{
    handle_scope,
    js::runtime::{
        boxed_value::BoxedValue,
        error::syntax_error,
        module::{module::ModuleEnum, source_text_module::ModuleState},
        Context, EvalResult, Handle,
    },
};

use super::{
    module::{DynModule, Module, ResolveExportName, ResolveExportResult},
    source_text_module::{ImportEntry, ModuleEntry, SourceTextModule},
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

        match self.inner_link(cx, module.as_dyn_module(), 0) {
            Ok(_) => {
                // Assert state postcondition
                debug_assert!(matches!(
                    module.state(),
                    ModuleState::Linked | ModuleState::EvaluatingAsync | ModuleState::Evaluated
                ));
                debug_assert!(self.stack.is_empty());

                Ok(())
            }
            Err(error) => {
                // Any error should reset all modules in stack to unlinked
                for module in &mut self.stack {
                    debug_assert!(module.state() == ModuleState::Linking);
                    module.set_state(ModuleState::Unlinked);
                }

                // Assert state postcondition
                debug_assert!(module.state() == ModuleState::Unlinked);

                Err(error)
            }
        }
    }

    /// InnerModuleLinking (https://tc39.es/ecma262/#sec-InnerModuleLinking)
    fn inner_link(&mut self, cx: Context, module: DynModule, index: u32) -> EvalResult<u32> {
        let mut module = match module.as_enum() {
            ModuleEnum::Synthetic(module) => {
                module.link(cx)?;
                return Ok(index);
            }
            ModuleEnum::SourceText(module) => module,
        };

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

            return Ok(index);
        }

        module.set_state(ModuleState::Linking);
        module.set_dfs_index(index);
        module.set_dfs_ancestor_index(index);

        self.stack.push(module);

        let mut index = index + 1;

        let loaded_modules = module.loaded_modules();
        for i in 0..loaded_modules.len() {
            let required_module = DynModule::from_heap(&loaded_modules.as_slice()[i].unwrap());

            index = self.inner_link(cx, required_module, index)?;

            if let Some(required_module) = required_module.as_source_text_module() {
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
        }

        // Can isolate InitializeEnvironment in a separate handle scope since handles cannot be
        // stored anywhere else and escape.
        handle_scope!(cx, initialize_environment(cx, module))?;

        debug_assert!(module.dfs_ancestor_index() <= module.dfs_index());

        if module.dfs_index() == module.dfs_ancestor_index() {
            loop {
                let mut required_module = self.stack.pop().unwrap();
                required_module.set_state(ModuleState::Linked);

                if required_module.ptr_eq(&module) {
                    break;
                }
            }
        }

        Ok(index)
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
        if let ModuleEntry::Import(heap_entry) = &module.entries_as_slice()[i] {
            let entry = ImportEntry::from_heap(heap_entry);

            let mut imported_module = module.get_imported_module(&entry.module_request.to_heap());

            if let Some(import_name) = entry.import_name {
                let resolution = imported_module.resolve_export(cx, *import_name, &mut vec![]);

                match resolution {
                    // Regular imports are linked to their corresponding export by referencing
                    // the same boxed value.
                    ResolveExportResult::Resolved {
                        name: ResolveExportName::Local { boxed_value, .. },
                        ..
                    } => {
                        module
                            .module_scope_ptr()
                            .set_heap_item_slot(entry.slot_index, boxed_value.as_heap_item());
                    }
                    // Namespace object may be stored as a module or scope value
                    ResolveExportResult::Resolved {
                        name: ResolveExportName::Namespace,
                        module: mut resolved_module,
                    } => {
                        // May allocate
                        let namespace_object = resolved_module.get_namespace_object(cx).to_handle();

                        // The BoxedValue for namespace re-exports has not yet been created (unlike
                        // all other exports, which are actual bindings whose BoxedValue is created
                        // when creating the the module scope).
                        let boxed_value = BoxedValue::new(cx, namespace_object.into());
                        module
                            .module_scope_ptr()
                            .set_heap_item_slot(entry.slot_index, boxed_value.as_heap_item());
                    }
                    _ => return syntax_error(cx, "could not resolve module specifier"),
                }
            } else {
                // Namespace object may be stored as a module or scope value
                let namespace_object = imported_module.get_namespace_object(cx);
                let namespace_object = namespace_object.as_value();
                let slot_index = entry.slot_index;

                if entry.is_exported {
                    let mut boxed_value = module.module_scope_ptr().get_module_slot(slot_index);
                    boxed_value.set(namespace_object);
                } else {
                    module
                        .module_scope_ptr()
                        .set_slot(slot_index, namespace_object);
                }
            }
        }
    }

    Ok(())
}

pub fn link(cx: Context, module: Handle<SourceTextModule>) -> EvalResult<()> {
    handle_scope!(cx, {
        let mut linker = GraphLinker::new();
        linker.link(cx, module)
    })
}
