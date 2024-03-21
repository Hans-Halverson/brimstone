use crate::js::runtime::{
    arguments_object::{
        ArgAccessorClosureEnvironment, LegacyMappedArgumentsObject, MappedArgumentsObject,
        UnmappedArgumentsObject,
    },
    array_object::ArrayObject,
    array_properties::{DenseArrayProperties, SparseArrayProperties},
    bound_function_object::BoundFunctionObject,
    builtin_function::BuiltinFunction,
    bytecode::{
        constant_table::ConstantTable,
        exception_handlers::ExceptionHandlers,
        function::{BytecodeFunction, Closure},
    },
    context::GlobalSymbolRegistryField,
    environment::{
        declarative_environment::{DeclarativeEnvironment, DeclarativeEnvironmentBindingsMapField},
        function_environment::FunctionEnvironment,
        global_environment::{GlobalEnvironment, GlobalEnvironmentVarNamesField},
        module_environment::ModuleEnvironment,
        object_environment::ObjectEnvironment,
        private_environment::{PrivateEnvironment, PrivateEnvironmentNamesField},
    },
    eval::script::Script,
    execution_context::ExecutionContext,
    for_in_iterator::ForInIterator,
    function::{Function, FunctionFieldsArray, FunctionPrivateMethodsArray},
    global_names::GlobalNames,
    interned_strings::{InternedStringsMapField, InternedStringsSetField},
    intrinsics::{
        array_buffer_constructor::{ArrayBufferDataField, ArrayBufferObject},
        array_iterator::ArrayIterator,
        bigint_constructor::BigIntObject,
        boolean_constructor::BooleanObject,
        data_view_constructor::DataViewObject,
        date_object::DateObject,
        error_constructor::ErrorObject,
        finalization_registry_object::{FinalizationRegistryCells, FinalizationRegistryObject},
        function_prototype::FunctionPrototype,
        map_iterator::MapIterator,
        map_object::{MapObject, MapObjectMapField},
        number_constructor::NumberObject,
        object_prototype::ObjectPrototype,
        proxy_constructor::RevokeProxyClosureEnvironment,
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
    object_descriptor::{ObjectDescriptor, ObjectKind},
    object_value::{NamedPropertiesMapField, ObjectValue},
    proxy_object::ProxyObject,
    realm::{GlobalScopes, LexicalNamesMapField, RealmTemplateMapField},
    regexp::compiled_regexp::CompiledRegExpObject,
    scope::Scope,
    scope_names::ScopeNames,
    string_object::StringObject,
    string_value::StringValue,
    value::{AccessorValue, BigIntValue, SymbolValue},
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
            ObjectKind::Function => self.cast::<Function>().byte_size(),
            ObjectKind::BuiltinFunction => self.cast::<BuiltinFunction>().byte_size(),
            ObjectKind::BoundFunctionObject => self.cast::<BoundFunctionObject>().byte_size(),
            ObjectKind::MappedArgumentsObject => self.cast::<MappedArgumentsObject>().byte_size(),
            ObjectKind::LegacyMappedArgumentsObject => {
                self.cast::<LegacyMappedArgumentsObject>().byte_size()
            }
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
            ObjectKind::ObjectPrototype => self.cast::<ObjectPrototype>().byte_size(),
            ObjectKind::FunctionPrototype => self.cast::<FunctionPrototype>().byte_size(),
            ObjectKind::String => self.cast::<StringValue>().byte_size(),
            ObjectKind::Symbol => self.cast::<SymbolValue>().byte_size(),
            ObjectKind::BigInt => self.cast::<BigIntValue>().byte_size(),
            ObjectKind::Accessor => self.cast::<AccessorValue>().byte_size(),
            ObjectKind::ExecutionContext => self.cast::<ExecutionContext>().byte_size(),
            ObjectKind::Realm => self.cast::<Realm>().byte_size(),
            ObjectKind::Script => self.cast::<Script>().byte_size(),
            ObjectKind::Closure => self.cast::<Closure>().byte_size(),
            ObjectKind::BytecodeFunction => self.cast::<BytecodeFunction>().byte_size(),
            ObjectKind::ConstantTable => self.cast::<ConstantTable>().byte_size(),
            ObjectKind::ExceptionHandlers => self.cast::<ExceptionHandlers>().byte_size(),
            ObjectKind::Scope => self.cast::<Scope>().byte_size(),
            ObjectKind::ScopeNames => self.cast::<ScopeNames>().byte_size(),
            ObjectKind::GlobalNames => self.cast::<GlobalNames>().byte_size(),
            ObjectKind::DeclarativeEnvironment => self.cast::<DeclarativeEnvironment>().byte_size(),
            ObjectKind::FunctionEnvironment => self.cast::<FunctionEnvironment>().byte_size(),
            ObjectKind::GlobalEnvironment => self.cast::<GlobalEnvironment>().byte_size(),
            ObjectKind::ModuleEnvironment => self.cast::<ModuleEnvironment>().byte_size(),
            ObjectKind::ObjectEnvironment => self.cast::<ObjectEnvironment>().byte_size(),
            ObjectKind::PrivateEnvironment => self.cast::<PrivateEnvironment>().byte_size(),
            ObjectKind::DenseArrayProperties => self.cast::<DenseArrayProperties>().byte_size(),
            ObjectKind::SparseArrayProperties => self.cast::<SparseArrayProperties>().byte_size(),
            ObjectKind::CompiledRegExpObject => self.cast::<CompiledRegExpObject>().byte_size(),
            ObjectKind::ArgAccessorClosureEnvironment => {
                self.cast::<ArgAccessorClosureEnvironment>().byte_size()
            }
            ObjectKind::RevokeProxyClosureEnvironment => {
                self.cast::<RevokeProxyClosureEnvironment>().byte_size()
            }
            ObjectKind::ObjectNamedPropertiesMap => {
                NamedPropertiesMapField::byte_size(&self.cast())
            }
            ObjectKind::MapObjectValueMap => MapObjectMapField::byte_size(&self.cast()),
            ObjectKind::SetObjectValueSet => SetObjectSetField::byte_size(&self.cast()),
            ObjectKind::WeakMapObjectWeakValueMap => WeakMapObjectMapField::byte_size(&self.cast()),
            ObjectKind::WeakSetObjectWeakValueSet => WeakSetObjectSetField::byte_size(&self.cast()),
            ObjectKind::DeclarativeEnvironmentBindingsMap => {
                DeclarativeEnvironmentBindingsMapField::byte_size(&self.cast())
            }
            ObjectKind::RealmTemplateMap => RealmTemplateMapField::byte_size(&self.cast()),
            ObjectKind::PrivateEnvironmentNameMap => {
                PrivateEnvironmentNamesField::byte_size(&self.cast())
            }
            ObjectKind::GlobalEnvironmentNameSet => {
                GlobalEnvironmentVarNamesField::byte_size(&self.cast())
            }
            ObjectKind::GlobalSymbolRegistryMap => {
                GlobalSymbolRegistryField::byte_size(&self.cast())
            }
            ObjectKind::InternedStringsMap => InternedStringsMapField::byte_size(&self.cast()),
            ObjectKind::InternedStringsSet => InternedStringsSetField::byte_size(&self.cast()),
            ObjectKind::LexicalNamesMap => LexicalNamesMapField::byte_size(&self.cast()),
            ObjectKind::ArrayBufferDataArray => ArrayBufferDataField::byte_size(&self.cast()),
            ObjectKind::FunctionFieldsArray => FunctionFieldsArray::byte_size(&self.cast()),
            ObjectKind::FunctionPrivateMethodsArray => {
                FunctionPrivateMethodsArray::byte_size(&self.cast())
            }
            ObjectKind::FinalizationRegistryCells => {
                self.cast::<FinalizationRegistryCells>().byte_size()
            }
            ObjectKind::GlobalScopes => self.cast::<GlobalScopes>().byte_size(),
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
            ObjectKind::Function => self.cast::<Function>().visit_pointers(visitor),
            ObjectKind::BuiltinFunction => self.cast::<BuiltinFunction>().visit_pointers(visitor),
            ObjectKind::BoundFunctionObject => {
                self.cast::<BoundFunctionObject>().visit_pointers(visitor)
            }
            ObjectKind::MappedArgumentsObject => {
                self.cast::<MappedArgumentsObject>().visit_pointers(visitor)
            }
            ObjectKind::LegacyMappedArgumentsObject => self
                .cast::<LegacyMappedArgumentsObject>()
                .visit_pointers(visitor),
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
            ObjectKind::ObjectPrototype => self.cast::<ObjectPrototype>().visit_pointers(visitor),
            ObjectKind::FunctionPrototype => {
                self.cast::<FunctionPrototype>().visit_pointers(visitor)
            }
            ObjectKind::String => self.cast::<StringValue>().visit_pointers(visitor),
            ObjectKind::Symbol => self.cast::<SymbolValue>().visit_pointers(visitor),
            ObjectKind::BigInt => self.cast::<BigIntValue>().visit_pointers(visitor),
            ObjectKind::Accessor => self.cast::<AccessorValue>().visit_pointers(visitor),
            ObjectKind::ExecutionContext => self.cast::<ExecutionContext>().visit_pointers(visitor),
            ObjectKind::Realm => self.cast::<Realm>().visit_pointers(visitor),
            ObjectKind::Script => self.cast::<Script>().visit_pointers(visitor),
            ObjectKind::Closure => self.cast::<Closure>().visit_pointers(visitor),
            ObjectKind::BytecodeFunction => self.cast::<BytecodeFunction>().visit_pointers(visitor),
            ObjectKind::ConstantTable => self.cast::<ConstantTable>().visit_pointers(visitor),
            ObjectKind::ExceptionHandlers => {
                self.cast::<ExceptionHandlers>().visit_pointers(visitor)
            }
            ObjectKind::Scope => self.cast::<Scope>().visit_pointers(visitor),
            ObjectKind::ScopeNames => self.cast::<ScopeNames>().visit_pointers(visitor),
            ObjectKind::GlobalNames => self.cast::<GlobalNames>().visit_pointers(visitor),
            ObjectKind::DeclarativeEnvironment => self
                .cast::<DeclarativeEnvironment>()
                .visit_pointers(visitor),
            ObjectKind::FunctionEnvironment => {
                self.cast::<FunctionEnvironment>().visit_pointers(visitor)
            }
            ObjectKind::GlobalEnvironment => {
                self.cast::<GlobalEnvironment>().visit_pointers(visitor)
            }
            ObjectKind::ModuleEnvironment => {
                self.cast::<ModuleEnvironment>().visit_pointers(visitor)
            }
            ObjectKind::ObjectEnvironment => {
                self.cast::<ObjectEnvironment>().visit_pointers(visitor)
            }
            ObjectKind::PrivateEnvironment => {
                self.cast::<PrivateEnvironment>().visit_pointers(visitor)
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
            ObjectKind::ArgAccessorClosureEnvironment => self
                .cast::<ArgAccessorClosureEnvironment>()
                .visit_pointers(visitor),
            ObjectKind::RevokeProxyClosureEnvironment => self
                .cast::<RevokeProxyClosureEnvironment>()
                .visit_pointers(visitor),
            ObjectKind::ObjectNamedPropertiesMap => {
                NamedPropertiesMapField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::MapObjectValueMap => {
                MapObjectMapField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::SetObjectValueSet => {
                SetObjectSetField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::WeakMapObjectWeakValueMap => {
                WeakMapObjectMapField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::WeakSetObjectWeakValueSet => {
                WeakSetObjectSetField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::DeclarativeEnvironmentBindingsMap => {
                DeclarativeEnvironmentBindingsMapField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::RealmTemplateMap => {
                RealmTemplateMapField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::PrivateEnvironmentNameMap => {
                PrivateEnvironmentNamesField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::GlobalEnvironmentNameSet => {
                GlobalEnvironmentVarNamesField::visit_pointers(self.cast_mut(), visitor)
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
            ObjectKind::ArrayBufferDataArray => {
                ArrayBufferDataField::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::FunctionFieldsArray => {
                FunctionFieldsArray::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::FunctionPrivateMethodsArray => {
                FunctionPrivateMethodsArray::visit_pointers(self.cast_mut(), visitor)
            }
            ObjectKind::FinalizationRegistryCells => self
                .cast::<FinalizationRegistryCells>()
                .visit_pointers(visitor),
            ObjectKind::GlobalScopes => self.cast::<GlobalScopes>().visit_pointers(visitor),
            ObjectKind::Last => unreachable!("No objects are created with this descriptor"),
        }
    }
}
