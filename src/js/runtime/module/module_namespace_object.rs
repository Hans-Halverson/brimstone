use wrap_ordinary_object::wrap_ordinary_object;

use crate::{
    extend_object,
    js::runtime::{
        boxed_value::BoxedValue,
        error::reference_error,
        gc::{HeapObject, HeapVisitor},
        object_descriptor::ObjectKind,
        object_value::VirtualObject,
        ordinary_object::{
            object_create_with_optional_proto, ordinary_define_own_property, ordinary_delete,
            ordinary_get, ordinary_get_own_property, ordinary_has_property,
            ordinary_own_string_symbol_property_keys,
        },
        property::Property,
        type_utilities::same_value,
        Context, EvalResult, Handle, HeapPtr, PropertyDescriptor, PropertyKey, Value,
    },
    set_uninit,
};

use super::source_text_module::SourceTextModule;

// Module Namespace Exotic Objects (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects)
extend_object! {
    pub struct ModuleNamespaceObject {
        // The module which the namespace object holds the exports of. Exports are actually stored
        // within the module itself and the namespace object is just a proxy to access them.
        module: HeapPtr<SourceTextModule>,
    }
}

impl ModuleNamespaceObject {
    pub fn new(cx: Context, module: Handle<SourceTextModule>) -> HeapPtr<ModuleNamespaceObject> {
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

        set_uninit!(object.module, module.get_());

        let object = object.to_handle();

        // Module Namepace Objects [ %Symbol.toStringTag% ] (https://tc39.es/ecma262/#sec-%symbol.tostringtag%)
        let to_string_tag_key = cx.well_known_symbols.to_string_tag();
        object.as_object().set_property(
            cx,
            to_string_tag_key,
            Property::data(cx.names.module().as_string().into(), false, false, false),
        );

        object.get_()
    }
}

impl HeapPtr<ModuleNamespaceObject> {
    /// Looks up an export name from the module. Returns the exported value if an export with that
    /// name exists, otherwise returns `None`.
    ///
    /// Error if the export binding has not yet been initialized.
    fn lookup_export(&self, cx: Context, key: PropertyKey) -> EvalResult<Option<Value>> {
        let exports = self.module.exports_ptr();
        let heap_item = match exports.get(&key) {
            Some(heap_item) => heap_item,
            None => return Ok(None),
        };

        if heap_item.descriptor().kind() == ObjectKind::BoxedValue {
            let boxed_value = heap_item.cast::<BoxedValue>();
            let value = boxed_value.get();

            // Error if value has TDZ and has not yet been initialized
            if value.is_empty() {
                reference_error(cx, "module value is not initialized")
            } else {
                Ok(Some(value))
            }
        } else {
            // Otherwise a SourceTextModule is which represents a namespace export of that module
            debug_assert!(heap_item.descriptor().kind() == ObjectKind::SourceTextModule);

            let mut module = heap_item.cast::<SourceTextModule>().to_handle();
            let namespace_object = module.get_namespace_object(cx).as_object();

            Ok(Some(namespace_object.into()))
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

        match self.lookup_export(cx, key.get())? {
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

        Ok(self.module.exports_ptr().contains_key(&key.get()))
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

        match self.lookup_export(cx, key.get())? {
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

        Ok(!self.module.exports_ptr().contains_key(&key.get()))
    }

    /// [[OwnPropertyKeys]] (https://tc39.es/ecma262/#sec-module-namespace-exotic-objects-ownpropertykeys)
    fn own_property_keys(&self, cx: Context) -> EvalResult<Vec<Handle<Value>>> {
        // Gather all exported keys
        let raw_keys = self
            .module
            .exports_ptr()
            .iter_gc_unsafe()
            .map(|(key, _)| key.to_handle(cx))
            .collect::<Vec<_>>();

        // Convert all keys to strings
        let mut string_keys = raw_keys
            .into_iter()
            .map(|key| {
                // Safe since key is guaranteed to not be a symbol
                key.to_value(cx).as_string().as_flat()
            })
            .collect::<Vec<_>>();

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
        visitor.visit_pointer(&mut self.module);
    }
}
