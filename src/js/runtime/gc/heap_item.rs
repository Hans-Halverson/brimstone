use crate::js::runtime::{
    accessor::Accessor,
    arguments_object::{MappedArgumentsObject, UnmappedArgumentsObject},
    array_object::ArrayObject,
    array_properties::{DenseArrayProperties, SparseArrayProperties},
    async_generator_object::{AsyncGeneratorObject, AsyncGeneratorRequest},
    boxed_value::BoxedValue,
    bytecode::{
        constant_table::ConstantTable,
        exception_handlers::ExceptionHandlers,
        function::{BytecodeFunction, Closure},
    },
    class_names::ClassNames,
    collections::{
        array::{
            byte_array_byte_size, byte_array_visit_pointers, value_array_byte_size,
            value_array_visit_pointers,
        },
        vec::{value_vec_byte_size, value_vec_visit_pointers},
    },
    context::GlobalSymbolRegistryField,
    for_in_iterator::ForInIterator,
    generator_object::GeneratorObject,
    global_names::GlobalNames,
    interned_strings::{InternedStringsMapField, InternedStringsSetField},
    intrinsics::{
        array_buffer_constructor::ArrayBufferObject,
        array_iterator::ArrayIterator,
        async_from_sync_iterator_prototype::AsyncFromSyncIterator,
        bigint_constructor::BigIntObject,
        boolean_constructor::BooleanObject,
        data_view_constructor::DataViewObject,
        date_object::DateObject,
        error_constructor::ErrorObject,
        finalization_registry_object::{FinalizationRegistryCells, FinalizationRegistryObject},
        map_iterator::MapIterator,
        map_object::{MapObject, MapObjectMapField},
        number_constructor::NumberObject,
        object_prototype::ObjectPrototype,
        regexp_constructor::RegExpObject,
        regexp_string_iterator::RegExpStringIterator,
        set_iterator::SetIterator,
        set_object::{SetObject, SetObjectSetField},
        string_iterator::StringIterator,
        symbol_constructor::SymbolObject,
        typed_array::{
            BigInt64Array, BigUInt64Array, Float32Array, Float64Array, Int16Array, Int32Array,
            Int8Array, UInt16Array, UInt32Array, UInt8Array, UInt8ClampedArray,
        },
        weak_map_object::{WeakMapObject, WeakMapObjectMapField},
        weak_ref_constructor::WeakRefObject,
        weak_set_object::{WeakSetObject, WeakSetObjectSetField},
    },
    module::{
        module_namespace_object::ModuleNamespaceObject,
        source_text_module::{ExportMapField, SourceTextModule},
    },
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::{NamedPropertiesMapField, ObjectValue},
    promise_object::{PromiseCapability, PromiseObject, PromiseReaction},
    proxy_object::ProxyObject,
    realm::{GlobalScopes, LexicalNamesMapField},
    regexp::compiled_regexp::CompiledRegExpObject,
    scope::Scope,
    scope_names::ScopeNames,
    source_file::SourceFile,
    string_object::StringObject,
    string_value::StringValue,
    value::{BigIntValue, SymbolValue},
    Realm,
};

use super::{HeapObject, HeapPtr, HeapVisitor};

/// An arbitrary heap item. Only common field between heap items is their descriptor, which can be
/// used to determine the true type of the heap item.
#[repr(C)]
pub struct HeapItem {
    descriptor: HeapPtr<ObjectDescriptor>,
}

impl HeapItem {
    pub fn descriptor(&self) -> HeapPtr<ObjectDescriptor> {
        self.descriptor
    }

    pub fn set_descriptor(&mut self, descriptor: HeapPtr<ObjectDescriptor>) {
        self.descriptor = descriptor;
    }
}

impl HeapObject for HeapPtr<HeapItem> {
    fn byte_size(&self) -> usize {
        match self.descriptor().kind() {
            ObjectKind::Descriptor => self.cast::<ObjectDescriptor>().byte_size(),
            ObjectKind::OrdinaryObject => self.cast::<ObjectValue>().byte_size(),
            ObjectKind::Proxy => self.cast::<ProxyObject>().byte_size(),
            ObjectKind::BooleanObject => self.cast::<BooleanObject>().byte_size(),
            ObjectKind::NumberObject => self.cast::<NumberObject>().byte_size(),
            ObjectKind::StringObject => self.cast::<StringObject>().byte_size(),
            ObjectKind::SymbolObject => self.cast::<SymbolObject>().byte_size(),
            ObjectKind::BigIntObject => self.cast::<BigIntObject>().byte_size(),
            ObjectKind::ArrayObject => self.cast::<ArrayObject>().byte_size(),
            ObjectKind::RegExpObject => self.cast::<RegExpObject>().byte_size(),
            ObjectKind::ErrorObject => self.cast::<ErrorObject>().byte_size(),
            ObjectKind::DateObject => self.cast::<DateObject>().byte_size(),
            ObjectKind::SetObject => self.cast::<SetObject>().byte_size(),
            ObjectKind::MapObject => self.cast::<MapObject>().byte_size(),
            ObjectKind::WeakRefObject => self.cast::<WeakRefObject>().byte_size(),
            ObjectKind::WeakSetObject => self.cast::<WeakSetObject>().byte_size(),
            ObjectKind::WeakMapObject => self.cast::<WeakMapObject>().byte_size(),
            ObjectKind::FinalizationRegistryObject => {
                self.cast::<FinalizationRegistryObject>().byte_size()
            }
            ObjectKind::MappedArgumentsObject => self.cast::<MappedArgumentsObject>().byte_size(),
            ObjectKind::UnmappedArgumentsObject => {
                self.cast::<UnmappedArgumentsObject>().byte_size()
            }
            ObjectKind::Int8Array => self.cast::<Int8Array>().byte_size(),
            ObjectKind::UInt8Array => self.cast::<UInt8Array>().byte_size(),
            ObjectKind::UInt8ClampedArray => self.cast::<UInt8ClampedArray>().byte_size(),
            ObjectKind::Int16Array => self.cast::<Int16Array>().byte_size(),
            ObjectKind::UInt16Array => self.cast::<UInt16Array>().byte_size(),
            ObjectKind::Int32Array => self.cast::<Int32Array>().byte_size(),
            ObjectKind::UInt32Array => self.cast::<UInt32Array>().byte_size(),
            ObjectKind::BigInt64Array => self.cast::<BigInt64Array>().byte_size(),
            ObjectKind::BigUInt64Array => self.cast::<BigUInt64Array>().byte_size(),
            ObjectKind::Float32Array => self.cast::<Float32Array>().byte_size(),
            ObjectKind::Float64Array => self.cast::<Float64Array>().byte_size(),
            ObjectKind::ArrayBufferObject => self.cast::<ArrayBufferObject>().byte_size(),
            ObjectKind::DataViewObject => self.cast::<DataViewObject>().byte_size(),
            ObjectKind::ArrayIterator => self.cast::<ArrayIterator>().byte_size(),
            ObjectKind::StringIterator => self.cast::<StringIterator>().byte_size(),
            ObjectKind::SetIterator => self.cast::<SetIterator>().byte_size(),
            ObjectKind::MapIterator => self.cast::<MapIterator>().byte_size(),
            ObjectKind::RegExpStringIterator => self.cast::<RegExpStringIterator>().byte_size(),
            ObjectKind::ForInIterator => self.cast::<ForInIterator>().byte_size(),
            ObjectKind::AsyncFromSyncIterator => self.cast::<AsyncFromSyncIterator>().byte_size(),
            ObjectKind::ObjectPrototype => self.cast::<ObjectPrototype>().byte_size(),
            ObjectKind::String => self.cast::<StringValue>().byte_size(),
            ObjectKind::Symbol => self.cast::<SymbolValue>().byte_size(),
            ObjectKind::BigInt => self.cast::<BigIntValue>().byte_size(),
            ObjectKind::Accessor => self.cast::<Accessor>().byte_size(),
            ObjectKind::Promise => self.cast::<PromiseObject>().byte_size(),
            ObjectKind::PromiseReaction => self.cast::<PromiseReaction>().byte_size(),
            ObjectKind::PromiseCapability => self.cast::<PromiseCapability>().byte_size(),
            ObjectKind::Realm => self.cast::<Realm>().byte_size(),
            ObjectKind::Closure => self.cast::<Closure>().byte_size(),
            ObjectKind::BytecodeFunction => self.cast::<BytecodeFunction>().byte_size(),
            ObjectKind::ConstantTable => self.cast::<ConstantTable>().byte_size(),
            ObjectKind::ExceptionHandlers => self.cast::<ExceptionHandlers>().byte_size(),
            ObjectKind::SourceFile => self.cast::<SourceFile>().byte_size(),
            ObjectKind::Scope => self.cast::<Scope>().byte_size(),
            ObjectKind::ScopeNames => self.cast::<ScopeNames>().byte_size(),
            ObjectKind::GlobalNames => self.cast::<GlobalNames>().byte_size(),
            ObjectKind::ClassNames => self.cast::<ClassNames>().byte_size(),
            ObjectKind::SourceTextModule => self.cast::<SourceTextModule>().byte_size(),
            ObjectKind::ModuleNamespaceObject => self.cast::<ModuleNamespaceObject>().byte_size(),
            ObjectKind::Generator => self.cast::<GeneratorObject>().byte_size(),
            ObjectKind::AsyncGenerator => self.cast::<AsyncGeneratorObject>().byte_size(),
            ObjectKind::AsyncGeneratorRequest => self.cast::<AsyncGeneratorRequest>().byte_size(),
            ObjectKind::DenseArrayProperties => self.cast::<DenseArrayProperties>().byte_size(),
            ObjectKind::SparseArrayProperties => self.cast::<SparseArrayProperties>().byte_size(),
            ObjectKind::CompiledRegExpObject => self.cast::<CompiledRegExpObject>().byte_size(),
            ObjectKind::BoxedValue => self.cast::<BoxedValue>().byte_size(),
            ObjectKind::ObjectNamedPropertiesMap => {
                NamedPropertiesMapField::byte_size(&self.cast())
            }
            ObjectKind::MapObjectValueMap => MapObjectMapField::byte_size(&self.cast()),
            ObjectKind::SetObjectValueSet => SetObjectSetField::byte_size(&self.cast()),
            ObjectKind::ExportMap => ExportMapField::byte_size(&self.cast()),
            ObjectKind::WeakMapObjectWeakValueMap => WeakMapObjectMapField::byte_size(&self.cast()),
            ObjectKind::WeakSetObjectWeakValueSet => WeakSetObjectSetField::byte_size(&self.cast()),
            ObjectKind::GlobalSymbolRegistryMap => {
                GlobalSymbolRegistryField::byte_size(&self.cast())
            }
            ObjectKind::InternedStringsMap => InternedStringsMapField::byte_size(&self.cast()),
            ObjectKind::InternedStringsSet => InternedStringsSetField::byte_size(&self.cast()),
            ObjectKind::LexicalNamesMap => LexicalNamesMapField::byte_size(&self.cast()),
            ObjectKind::ValueArray => value_array_byte_size(self.cast()),
            ObjectKind::ByteArray => byte_array_byte_size(self.cast()),
            ObjectKind::FinalizationRegistryCells => {
                self.cast::<FinalizationRegistryCells>().byte_size()
            }
            ObjectKind::GlobalScopes => self.cast::<GlobalScopes>().byte_size(),
            ObjectKind::ValueVec => value_vec_byte_size(self.cast()),
            ObjectKind::Last => unreachable!("No objects are created with this descriptor"),
        }
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        match self.descriptor().kind() {
            ObjectKind::Descriptor => self.cast::<ObjectDescriptor>().visit_pointers(visitor),
            ObjectKind::OrdinaryObject => self.cast::<ObjectValue>().visit_pointers(visitor),
            ObjectKind::Proxy => self.cast::<ProxyObject>().visit_pointers(visitor),
            ObjectKind::BooleanObject => self.cast::<BooleanObject>().visit_pointers(visitor),
            ObjectKind::NumberObject => self.cast::<NumberObject>().visit_pointers(visitor),
            ObjectKind::StringObject => self.cast::<StringObject>().visit_pointers(visitor),
            ObjectKind::SymbolObject => self.cast::<SymbolObject>().visit_pointers(visitor),
            ObjectKind::BigIntObject => self.cast::<BigIntObject>().visit_pointers(visitor),
            ObjectKind::ArrayObject => self.cast::<ArrayObject>().visit_pointers(visitor),
            ObjectKind::RegExpObject => self.cast::<RegExpObject>().visit_pointers(visitor),
            ObjectKind::ErrorObject => self.cast::<ErrorObject>().visit_pointers(visitor),
            ObjectKind::DateObject => self.cast::<DateObject>().visit_pointers(visitor),
            ObjectKind::SetObject => self.cast::<SetObject>().visit_pointers(visitor),
            ObjectKind::MapObject => self.cast::<MapObject>().visit_pointers(visitor),
            ObjectKind::WeakRefObject => self.cast::<WeakRefObject>().visit_pointers(visitor),
            ObjectKind::WeakSetObject => self.cast::<WeakSetObject>().visit_pointers(visitor),
            ObjectKind::WeakMapObject => self.cast::<WeakMapObject>().visit_pointers(visitor),
            ObjectKind::FinalizationRegistryObject => self
                .cast::<FinalizationRegistryObject>()
                .visit_pointers(visitor),
            ObjectKind::MappedArgumentsObject => {
                self.cast::<MappedArgumentsObject>().visit_pointers(visitor)
            }
            ObjectKind::UnmappedArgumentsObject => self
                .cast::<UnmappedArgumentsObject>()
                .visit_pointers(visitor),
            ObjectKind::Int8Array => self.cast::<Int8Array>().visit_pointers(visitor),
            ObjectKind::UInt8Array => self.cast::<UInt8Array>().visit_pointers(visitor),
            ObjectKind::UInt8ClampedArray => {
                self.cast::<UInt8ClampedArray>().visit_pointers(visitor)
            }
            ObjectKind::Int16Array => self.cast::<Int16Array>().visit_pointers(visitor),
            ObjectKind::UInt16Array => self.cast::<UInt16Array>().visit_pointers(visitor),
            ObjectKind::Int32Array => self.cast::<Int32Array>().visit_pointers(visitor),
            ObjectKind::UInt32Array => self.cast::<UInt32Array>().visit_pointers(visitor),
            ObjectKind::BigInt64Array => self.cast::<BigInt64Array>().visit_pointers(visitor),
            ObjectKind::BigUInt64Array => self.cast::<BigUInt64Array>().visit_pointers(visitor),
            ObjectKind::Float32Array => self.cast::<Float32Array>().visit_pointers(visitor),
            ObjectKind::Float64Array => self.cast::<Float64Array>().visit_pointers(visitor),
            ObjectKind::ArrayBufferObject => {
                self.cast::<ArrayBufferObject>().visit_pointers(visitor)
            }
            ObjectKind::DataViewObject => self.cast::<DataViewObject>().visit_pointers(visitor),
            ObjectKind::ArrayIterator => self.cast::<ArrayIterator>().visit_pointers(visitor),
            ObjectKind::StringIterator => self.cast::<StringIterator>().visit_pointers(visitor),
            ObjectKind::SetIterator => self.cast::<SetIterator>().visit_pointers(visitor),
            ObjectKind::MapIterator => self.cast::<MapIterator>().visit_pointers(visitor),
            ObjectKind::RegExpStringIterator => {
                self.cast::<RegExpStringIterator>().visit_pointers(visitor)
            }
            ObjectKind::ForInIterator => self.cast::<ForInIterator>().visit_pointers(visitor),
            ObjectKind::AsyncFromSyncIterator => {
                self.cast::<AsyncFromSyncIterator>().visit_pointers(visitor)
            }
            ObjectKind::ObjectPrototype => self.cast::<ObjectPrototype>().visit_pointers(visitor),
            ObjectKind::String => self.cast::<StringValue>().visit_pointers(visitor),
            ObjectKind::Symbol => self.cast::<SymbolValue>().visit_pointers(visitor),
            ObjectKind::BigInt => self.cast::<BigIntValue>().visit_pointers(visitor),
            ObjectKind::Accessor => self.cast::<Accessor>().visit_pointers(visitor),
            ObjectKind::Promise => self.cast::<PromiseObject>().visit_pointers(visitor),
            ObjectKind::PromiseReaction => self.cast::<PromiseReaction>().visit_pointers(visitor),
            ObjectKind::PromiseCapability => {
                self.cast::<PromiseCapability>().visit_pointers(visitor)
            }
            ObjectKind::Realm => self.cast::<Realm>().visit_pointers(visitor),
            ObjectKind::Closure => self.cast::<Closure>().visit_pointers(visitor),
            ObjectKind::BytecodeFunction => self.cast::<BytecodeFunction>().visit_pointers(visitor),
            ObjectKind::ConstantTable => self.cast::<ConstantTable>().visit_pointers(visitor),
            ObjectKind::ExceptionHandlers => {
                self.cast::<ExceptionHandlers>().visit_pointers(visitor)
            }
            ObjectKind::SourceFile => self.cast::<SourceFile>().visit_pointers(visitor),
            ObjectKind::Scope => self.cast::<Scope>().visit_pointers(visitor),
            ObjectKind::ScopeNames => self.cast::<ScopeNames>().visit_pointers(visitor),
            ObjectKind::GlobalNames => self.cast::<GlobalNames>().visit_pointers(visitor),
            ObjectKind::ClassNames => self.cast::<ClassNames>().visit_pointers(visitor),
            ObjectKind::SourceTextModule => self.cast::<SourceTextModule>().visit_pointers(visitor),
            ObjectKind::ModuleNamespaceObject => {
                self.cast::<ModuleNamespaceObject>().visit_pointers(visitor)
            }
            ObjectKind::Generator => self.cast::<GeneratorObject>().visit_pointers(visitor),
            ObjectKind::AsyncGenerator => {
                self.cast::<AsyncGeneratorObject>().visit_pointers(visitor)
            }
            ObjectKind::AsyncGeneratorRequest => {
                self.cast::<AsyncGeneratorRequest>().visit_pointers(visitor)
            }
            ObjectKind::DenseArrayProperties => {
                self.cast::<DenseArrayProperties>().visit_pointers(visitor)
            }
            ObjectKind::SparseArrayProperties => {
                self.cast::<SparseArrayProperties>().visit_pointers(visitor)
            }
            ObjectKind::CompiledRegExpObject => {
                self.cast::<CompiledRegExpObject>().visit_pointers(visitor)
            }
            ObjectKind::BoxedValue => self.cast::<BoxedValue>().visit_pointers(visitor),
            ObjectKind::ObjectNamedPropertiesMap => {
                NamedPropertiesMapField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::MapObjectValueMap => {
                MapObjectMapField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::SetObjectValueSet => {
                SetObjectSetField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::ExportMap => ExportMapField::visit_pointers(self.cast_mut(), visitor),
            ObjectKind::WeakMapObjectWeakValueMap => {
                WeakMapObjectMapField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::WeakSetObjectWeakValueSet => {
                WeakSetObjectSetField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::GlobalSymbolRegistryMap => {
                GlobalSymbolRegistryField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::InternedStringsMap => {
                InternedStringsMapField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::InternedStringsSet => {
                InternedStringsSetField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::LexicalNamesMap => {
                LexicalNamesMapField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::ValueArray => value_array_visit_pointers(self.cast_mut(), visitor),
            ObjectKind::ByteArray => byte_array_visit_pointers(self.cast_mut(), visitor),
            ObjectKind::FinalizationRegistryCells => self
                .cast::<FinalizationRegistryCells>()
                .visit_pointers(visitor),
            ObjectKind::GlobalScopes => self.cast::<GlobalScopes>().visit_pointers(visitor),
            ObjectKind::ValueVec => value_vec_visit_pointers(self.cast_mut(), visitor),
            ObjectKind::Last => unreachable!("No objects are created with this descriptor"),
        }
    }
}
