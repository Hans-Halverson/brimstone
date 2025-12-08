use std::collections::HashSet;

use crate::{
    completion_value, must,
    runtime::{
        abstract_operations::call_object,
        alloc_error::AllocResult,
        boxed_value::BoxedValue,
        gc::{HeapItem, HeapVisitor},
        heap_item_descriptor::{HeapItemDescriptor, HeapItemKind},
        interned_strings::InternedStrings,
        intrinsics::intrinsics::Intrinsic,
        module::module::next_module_id,
        promise_object::{coerce_to_ordinary_promise, PromiseCapability, PromiseObject},
        rust_vtables::extract_module_vtable,
        scope::Scope,
        scope_names::{ScopeFlags, ScopeNameFlags, ScopeNames},
        string_value::FlatString,
        Context, EvalResult, Handle, HeapPtr, Realm, Value,
    },
    set_uninit,
};

use super::{
    module::{DynModule, Module, ModuleEnum, ModuleId, ResolveExportName, ResolveExportResult},
    module_namespace_object::ModuleNamespaceObject,
    source_text_module::SourceTextModule,
};

#[repr(C)]
pub struct SyntheticModule {
    descriptor: HeapPtr<HeapItemDescriptor>,
    /// Unique identifier for this module. Can be used as a stable identifier.
    id: ModuleId,
    /// The kind of synthetic module.
    kind: SyntheticModuleKind,
    /// Scope for the module.
    module_scope: HeapPtr<Scope>,
    /// Namespace object for this module. This is lazily created when first accessed.
    namespace_object: Option<HeapPtr<ModuleNamespaceObject>>,
}

pub enum SyntheticModuleKind {
    /// A module which sets the given value as the default export.
    DefaultExport(Value),
}

impl SyntheticModule {
    pub const MODULE_VTABLE: *const () = extract_module_vtable::<Self>();

    /// Create a new SyntheticModule with the given exported names. Caller is responsible for
    /// initializing the kind field.
    fn new(
        cx: Context,
        realm: Handle<Realm>,
        export_names: &[Handle<FlatString>],
    ) -> AllocResult<HeapPtr<SyntheticModule>> {
        // Create module scope for the module, with an entry for each exported name
        let name_flags = export_names
            .iter()
            .map(|_| ScopeNameFlags::empty())
            .collect::<Vec<_>>();
        let scope_names = ScopeNames::new(cx, ScopeFlags::empty(), export_names, &name_flags)?;
        let mut module_scope = Scope::new_module(cx, scope_names, realm.global_object())?;

        // Initialize all scope entries to undefined
        for i in 0..scope_names.len() {
            let boxed_value = BoxedValue::new(cx, cx.undefined())?;
            module_scope.set_heap_item_slot(i, boxed_value.as_heap_item());
        }

        let mut module = cx.alloc_uninit::<SyntheticModule>()?;

        // Note that kind is not initialized here, as it is initialized by the caller
        set_uninit!(module.descriptor, cx.base_descriptors.get(HeapItemKind::SyntheticModule));
        set_uninit!(module.id, next_module_id());
        set_uninit!(module.module_scope, *module_scope);
        set_uninit!(module.namespace_object, None);

        Ok(module)
    }

    pub fn new_default_export(
        cx: Context,
        realm: Handle<Realm>,
        default_export_value: Handle<Value>,
    ) -> AllocResult<Handle<SyntheticModule>> {
        let default_export_name = cx.names.default.as_string().as_flat().to_handle();
        let mut module = Self::new(cx, realm, &[default_export_name])?;

        set_uninit!(module.kind, SyntheticModuleKind::DefaultExport(*default_export_value));

        Ok(module.to_handle())
    }

    fn calculate_size_in_bytes() -> usize {
        std::mem::size_of::<SyntheticModule>()
    }

    #[inline]
    pub fn id(&self) -> ModuleId {
        self.id
    }

    #[inline]
    pub fn module_scope_ptr(&self) -> HeapPtr<Scope> {
        self.module_scope
    }
}

impl Handle<SyntheticModule> {
    pub fn as_dyn_module(&self) -> DynModule {
        self.into_dyn_module()
    }

    fn evaluate_default_export_module(
        &self,
        cx: Context,
        default_export_value: Handle<Value>,
    ) -> EvalResult<Handle<Value>> {
        // Find the default export name in the module scope and then set the default export value
        let default_export_name = cx.names.default.as_string().as_flat();
        let scope_index = self
            .module_scope_ptr()
            .scope_names_ptr()
            .lookup_name(default_export_name)
            .unwrap();

        let mut export_boxed_value = self.module_scope_ptr().get_module_slot(scope_index);
        export_boxed_value.set(*default_export_value);

        Ok(cx.undefined())
    }
}

impl Module for Handle<SyntheticModule> {
    fn as_enum(&self) -> ModuleEnum {
        ModuleEnum::Synthetic(*self)
    }

    fn load_requested_modules(&self, cx: Context) -> AllocResult<Handle<PromiseObject>> {
        Ok(must!(coerce_to_ordinary_promise(cx, cx.undefined())))
    }

    fn get_exported_names(
        &self,
        _: Context,
        exported_names: &mut HashSet<Handle<FlatString>>,
        visited_set: &mut HashSet<ModuleId>,
    ) {
        visited_set.insert(self.id());

        let scope_names = self.module_scope_ptr().scope_names_ptr();
        for name in scope_names.name_ptrs() {
            exported_names.insert(name.to_handle());
        }
    }

    fn resolve_export(
        &self,
        cx: Context,
        export_name: HeapPtr<FlatString>,
        _: &mut Vec<(HeapPtr<FlatString>, HeapPtr<SourceTextModule>)>,
    ) -> AllocResult<ResolveExportResult> {
        // Resolve the export name by looking through module bindings, returning one if found
        let module_scope = self.module_scope_ptr();
        let export_name = InternedStrings::get(cx, export_name)?;

        if let Some(scope_index) = module_scope.scope_names_ptr().lookup_name(export_name) {
            let boxed_value = module_scope.get_module_slot(scope_index);
            Ok(ResolveExportResult::Resolved {
                name: ResolveExportName::Local { name: export_name, boxed_value },
                module: self.as_dyn_module(),
            })
        } else {
            Ok(ResolveExportResult::None)
        }
    }

    fn link(&self, _: Context) -> EvalResult<()> {
        // Left empty, module scope and bindings were already created during module creation
        Ok(())
    }

    fn evaluate(&self, cx: Context) -> AllocResult<Handle<PromiseObject>> {
        let result = match self.kind {
            SyntheticModuleKind::DefaultExport(default_export_value) => {
                self.evaluate_default_export_module(cx, default_export_value.to_handle(cx))
            }
        };

        let promise_constructor = cx.get_intrinsic(Intrinsic::PromiseConstructor);
        let capability = must!(PromiseCapability::new(cx, promise_constructor.into()));
        let promise = capability.promise().cast::<PromiseObject>();

        match completion_value!(result) {
            Ok(result) => {
                must!(call_object(cx, capability.resolve(), cx.undefined(), &[result]));
            }
            Err(error) => {
                must!(call_object(cx, capability.reject(), cx.undefined(), &[error]));
            }
        }

        Ok(promise)
    }

    /// Returns the namespace object used when this module is namespace imported. Lazily creates the
    /// cached namespace object and exports map.
    fn get_namespace_object(&mut self, cx: Context) -> AllocResult<HeapPtr<ModuleNamespaceObject>> {
        if let Some(namespace_object) = self.namespace_object {
            return Ok(namespace_object);
        }

        // Create and cache the namespace object for this module
        let namespace_object = ModuleNamespaceObject::new(cx, self.as_dyn_module())?;
        self.namespace_object = Some(namespace_object);

        Ok(namespace_object)
    }
}

impl HeapItem for HeapPtr<SyntheticModule> {
    fn byte_size(&self) -> usize {
        SyntheticModule::calculate_size_in_bytes()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);

        match &mut self.kind {
            SyntheticModuleKind::DefaultExport(value) => visitor.visit_value(value),
        }

        visitor.visit_pointer(&mut self.module_scope);
        visitor.visit_pointer_opt(&mut self.namespace_object);
    }
}
