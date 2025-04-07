use brimstone_macros::wrap_ordinary_object;

use crate::{
    extend_object,
    runtime::{
        boxed_value::BoxedValue,
        error::reference_error,
        gc::{HeapObject, HeapVisitor},
        module::module::Module,
        object_descriptor::ObjectKind,
        object_value::VirtualObject,
        ordinary_object::{
            object_create_with_optional_proto, ordinary_define_own_property, ordinary_delete,
            ordinary_get, ordinary_get_own_property, ordinary_has_property,
            ordinary_own_string_symbol_property_keys,
        },
        property::Property,
        rust_vtables::extract_virtual_object_vtable,
        type_utilities::same_value,
        Context, EvalResult, Handle, HeapPtr, PropertyDescriptor, PropertyKey, Value,
    },
    set_uninit,
};

use super::{
    module::{DynModule, HeapDynModule, ModuleEnum},
    source_text_module::SourceTextModule,
    synthetic_module::SyntheticModule,
};

// Module Namespace Exotic Objects (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects)
extend_object! {
    pub struct ModuleNamespaceObject {
        // The module which the namespace object holds the exports of. Exports are actually stored
        // within the module itself and the namespace object is just a proxy to access them.
        module: HeapDynModule,
    }
}

impl ModuleNamespaceObject {
    pub const VIRTUAL_OBJECT_VTABLE: *const () = extract_virtual_object_vtable::<Self>();

    pub fn new(cx: Context, module: DynModule) -> HeapPtr<ModuleNamespaceObject> {
        // Module namespace object does not have a prototype. This satisfies:
        // - [[GetPrototypeOf]] (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-getprototypeof)
        // - [[SetPrototypeOf]] (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-setprototypeof-v)
        let mut object = object_create_with_optional_proto::<ModuleNamespaceObject>(
            cx,
            ObjectKind::ModuleNamespaceObject,
            None,
        );

        // Mark as non-extensible. This satisifes:
        // - [[IsExtensible]] (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-isextensible)
        // - [[PreventExtensions]] (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-preventextensions)
        object.as_object().set_is_extensible_field(false);

        set_uninit!(object.module, module.to_heap());

        let object = object.to_handle();

        // Module Namepace Objects [ %Symbol.toStringTag% ] (https://tc39.es/ecma262/#sec-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.as_object().set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.module().as_string().into(), false, false, false),
        );

        *object
    }
}

impl HeapPtr<ModuleNamespaceObject> {
    /// Looks up an export name from the module. Returns the exported value if an export with that
    /// name exists, otherwise returns `None`.
    ///
    /// Key is guaranteed to not be a symbol.
    ///
    /// Error if the export binding has not yet been initialized.
    fn lookup_export(&self, cx: Context, key: PropertyKey) -> EvalResult<Option<Value>> {
        let value = match DynModule::from_heap(&self.module).as_enum() {
            ModuleEnum::SourceText(module) => {
                Self::lookup_export_source_text_module(cx, module, key)?
            }
            ModuleEnum::Synthetic(module) => Self::lookup_export_synthetic_module(cx, module, key),
        };

        if let Some(value) = value {
            if value.is_empty() {
                return reference_error(cx, "module value is not initialized");
            }
        }

        Ok(value)
    }

    fn lookup_export_source_text_module(
        cx: Context,
        module: Handle<SourceTextModule>,
        key: PropertyKey,
    ) -> EvalResult<Option<Value>> {
        let exports = module.exports_ptr();
        let heap_item = match exports.get(&key) {
            Some(heap_item) => heap_item,
            None => return Ok(None),
        };

        if heap_item.descriptor().kind() == ObjectKind::BoxedValue {
            let boxed_value = heap_item.cast::<BoxedValue>();
            Ok(Some(boxed_value.get()))
        } else {
            // Otherwise must be a module - either a SourceTextModule or SyntheticModule - which
            // represents a namespace export of that module.
            debug_assert!(
                heap_item.descriptor().kind() == ObjectKind::SourceTextModule
                    || heap_item.descriptor().kind() == ObjectKind::SyntheticModule
            );

            let namespace_object = if heap_item.descriptor().kind() == ObjectKind::SourceTextModule
            {
                let mut module = heap_item.cast::<SourceTextModule>().to_handle();
                module.get_namespace_object(cx)
            } else {
                let mut module = heap_item.cast::<SyntheticModule>().to_handle();
                module.get_namespace_object(cx)
            };

            Ok(Some(namespace_object.as_value()))
        }
    }

    fn lookup_export_synthetic_module(
        cx: Context,
        module: Handle<SyntheticModule>,
        key: PropertyKey,
    ) -> Option<Value> {
        // Coerce to string, which is guaranteed to be flat
        let name = key.to_handle(cx).to_value(cx).as_string().as_flat();
        let scope_names = module.module_scope_ptr().scope_names_ptr();

        if let Some(scope_index) = scope_names.lookup_name(*name) {
            let value = module.module_scope_ptr().get_module_slot(scope_index).get();
            Some(value)
        } else {
            None
        }
    }
}

impl Handle<ModuleNamespaceObject> {
    #[inline]
    fn has_property_non_symbol(&self, cx: Context, key: Handle<PropertyKey>) -> bool {
        match DynModule::from_heap(&self.module).as_enum() {
            // Check the exports map
            ModuleEnum::SourceText(module) => module.exports_ptr().contains_key(&key),
            // Check the module scope names
            ModuleEnum::Synthetic(module) => {
                // Coerce to string, which is guaranteed to be flat
                let name = key.to_value(cx).as_string().as_flat();
                let scope_names = module.module_scope_ptr().scope_names_ptr();
                scope_names.lookup_name(*name).is_some()
            }
        }
    }
}

#[wrap_ordinary_object]
impl VirtualObject for Handle<ModuleNamespaceObject> {
    /// [[GetOwnProperty]] (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-getownproperty-p)
    fn get_own_property(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
    ) -> EvalResult<Option<PropertyDescriptor>> {
        if key.is_symbol() {
            return Ok(ordinary_get_own_property(cx, (*self).into(), key));
        }

        match self.lookup_export(cx, *key)? {
            None => Ok(None),
            Some(value) => {
                let value = value.to_handle(cx);
                let desc = PropertyDescriptor::data(value, true, true, false);
                Ok(Some(desc))
            }
        }
    }

    /// [[DefineOwnProperty]] (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-defineownproperty-p-desc)
    fn define_own_property(
        &mut self,
        cx: Context,
        key: Handle<PropertyKey>,
        desc: PropertyDescriptor,
    ) -> EvalResult<bool> {
        if key.is_symbol() {
            return ordinary_define_own_property(cx, (*self).into(), key, desc);
        }

        let current_desc = match self.get_own_property(cx, key)? {
            None => return Ok(false),
            Some(desc) => desc,
        };

        // Property descriptor attributes must match (writable, enumerable, non-configurable)
        if desc.is_configurable == Some(true)
            || desc.is_enumerable == Some(false)
            || desc.is_accessor_descriptor()
            || desc.is_writable == Some(false)
        {
            return Ok(false);
        }

        // If a value was provided it must match the current value
        if let Some(desc_value) = desc.value {
            return Ok(same_value(desc_value, current_desc.value.unwrap()));
        }

        Ok(true)
    }

    /// [[HasProperty]] (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-hasproperty-p)
    fn has_property(&self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
        if key.is_symbol() {
            return ordinary_has_property(cx, (*self).into(), key);
        }

        Ok(self.has_property_non_symbol(cx, key))
    }

    /// [[Get]] (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-get-p-receiver)
    fn get(
        &self,
        cx: Context,
        key: Handle<PropertyKey>,
        receiver: Handle<Value>,
    ) -> EvalResult<Handle<Value>> {
        if key.is_symbol() {
            return ordinary_get(cx, (*self).into(), key, receiver);
        }

        match self.lookup_export(cx, *key)? {
            None => Ok(cx.undefined()),
            Some(value) => Ok(value.to_handle(cx)),
        }
    }

    /// [[Set]] (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-set-p-v-receiver)
    fn set(
        &mut self,
        _: Context,
        _: Handle<PropertyKey>,
        _: Handle<Value>,
        _: Handle<Value>,
    ) -> EvalResult<bool> {
        Ok(false)
    }

    /// [[Delete]] (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-delete-p)
    fn delete(&mut self, cx: Context, key: Handle<PropertyKey>) -> EvalResult<bool> {
        if key.is_symbol() {
            return ordinary_delete(cx, (*self).into(), key);
        }

        Ok(!self.has_property_non_symbol(cx, key))
    }

    /// [[OwnPropertyKeys]] (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-ownpropertykeys)
    fn own_property_keys(&self, cx: Context) -> EvalResult<Vec<Handle<Value>>> {
        let mut string_keys = match DynModule::from_heap(&self.module).as_enum() {
            ModuleEnum::SourceText(module) => {
                // Gather all exported keys
                let raw_keys = module
                    .exports_ptr()
                    .iter_gc_unsafe()
                    .map(|(key, _)| key.to_handle(cx))
                    .collect::<Vec<_>>();

                // Convert all keys to strings
                raw_keys
                    .into_iter()
                    .map(|key| {
                        // Safe since key is guaranteed to not be a symbol
                        key.to_value(cx).as_string().as_flat()
                    })
                    .collect::<Vec<_>>()
            }
            // Synthetic modules have only the string keys in their module scope
            ModuleEnum::Synthetic(module) => module
                .module_scope_ptr()
                .scope_names_ptr()
                .name_ptrs()
                .iter()
                .map(|name| name.to_handle())
                .collect::<Vec<_>>(),
        };

        // Sort the keys in lexicographic order
        string_keys.sort_unstable();

        // Finally convert to vector of values
        let mut keys = string_keys
            .into_iter()
            .map(|key| key.into())
            .collect::<Vec<_>>();

        // Add keys on object which may only be symbol keys
        ordinary_own_string_symbol_property_keys(self.as_object(), &mut keys);

        Ok(keys)
    }
}

impl HeapObject for HeapPtr<ModuleNamespaceObject> {
    fn byte_size(&self) -> usize {
        size_of::<ModuleNamespaceObject>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        self.visit_object_pointers(visitor);
        self.module.visit_pointers(visitor);
    }
}
