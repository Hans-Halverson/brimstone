use crate::{
    runtime::{
        Realm,
        accessor::Accessor,
        arguments_object::{MappedArgumentsObject, UnmappedArgumentsObject},
        array_object::ArrayObject,
        array_properties::{DenseArrayProperties, SparseArrayPropertiesMap},
        async_generator_object::{AsyncGeneratorObject, AsyncGeneratorRequest},
        boxed_value::BoxedValue,
        builtin_generator::BuiltinGenerator,
        bytecode::{
            constant_table::ConstantTable,
            exception_handlers::ExceptionHandlers,
            function::{BytecodeFunction, Closure},
            generator::FunctionVec,
        },
        class_names::ClassNames,
        collections::{
            BsWeakVec,
            array::{ByteArray, U32Array, ValueArray},
        },
        context::{GlobalSymbolRegistryMap, ModuleCacheMap},
        for_in_iterator::ForInIterator,
        gc::{HeapPtr, HeapVisitor},
        generator_object::GeneratorObject,
        global_names::GlobalNames,
        heap_item_descriptor::HeapItemDescriptor,
        interned_strings::InternedStringsSet,
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
            iterator_constructor::WrappedValidIterator,
            iterator_helper_object::IteratorHelperObject,
            map_iterator::MapIterator,
            map_object::{MapObject, ValueIndexMap},
            number_constructor::NumberObject,
            object_prototype::ObjectPrototype,
            raw_json_object::RawJSONObject,
            regexp_constructor::RegExpObject,
            regexp_string_iterator::RegExpStringIterator,
            set_iterator::SetIterator,
            set_object::{SetObject, ValueIndexSet},
            string_iterator::StringIterator,
            symbol_constructor::SymbolObject,
            temporal::{
                duration_object::DurationObject, instant_object::InstantObject,
                plain_date_object::PlainDateObject, plain_date_time_object::PlainDateTimeObject,
                plain_month_day_object::PlainMonthDayObject, plain_time_object::PlainTimeObject,
                plain_year_month_object::PlainYearMonthObject,
                zoned_date_time_object::ZonedDateTimeObject,
            },
            typed_array::{
                BigInt64Array, BigUInt64Array, Float16Array, Float32Array, Float64Array, Int8Array,
                Int16Array, Int32Array, UInt8Array, UInt8ClampedArray, UInt16Array, UInt32Array,
            },
            weak_map_object::{WeakMapObject, WeakValueMap},
            weak_ref_constructor::WeakRefObject,
            weak_set_object::{WeakSetObject, WeakValueSet},
        },
        module::{
            import_attributes::ImportAttributes,
            module_namespace_object::ModuleNamespaceObject,
            source_text_module::{
                ExportMap, ModuleOptionArray, ModuleRequestArray, SourceTextModule,
                SourceTextModuleVec,
            },
            synthetic_module::SyntheticModule,
        },
        object_value::{NamedPropertiesMap, ObjectValue},
        promise_object::{PromiseCapability, PromiseObject, PromiseReaction},
        proxy_object::ProxyObject,
        realm::{GlobalScopes, LexicalNamesMap},
        regexp::compiled_regexp::CompiledRegExpObject,
        scope::Scope,
        scope_names::ScopeNames,
        source_file::SourceFile,
        stack_trace::StackFrameInfoArray,
        string_object::StringObject,
        string_value::StringValue,
        value::{BigIntValue, SymbolValue},
    },
    unit,
};

/// Trait implemented by all items stored on the heap. This includes both JS objects and non-object
/// items like strings and descriptors.
pub trait HeapItem: Sized {
    /// Size of this heap item in bytes. Not guaranteed to be aligned.
    fn byte_size(item: HeapPtr<Self>) -> usize;

    /// Call the provided visit function on all pointer fields in this item. Pass a mutable
    /// reference to the fields themselves so they can be updated in copying collection.
    fn visit_pointers(item: HeapPtr<Self>, visitor: &mut impl HeapVisitor);
}

/// Marker trait that denotes an object on the managed heap
pub trait IsHeapItem: Sized {}

impl<T: HeapItem> IsHeapItem for T {}

pub trait WithHeapItemKind {
    const KIND: HeapItemKind;
}

impl<T: WithHeapItemKind> HeapPtr<T> {
    /// Whether this is a heap item of a particular type.
    #[inline]
    pub fn is<U: WithHeapItemKind>(&self) -> bool {
        self.as_any().descriptor().kind() == U::KIND
    }

    /// Return this value as a heap item of a particular type, or None if it is not of that type.
    #[inline]
    pub fn as_opt<U: WithHeapItemKind>(&self) -> Option<HeapPtr<U>> {
        if self.is::<U>() {
            Some(self.cast())
        } else {
            None
        }
    }

    #[inline]
    pub fn as_any(&self) -> HeapPtr<AnyHeapItem> {
        self.cast()
    }
}

macro_rules! register_heap_items {
    ($(($kind:ident, $item_name:ident),)*) => {
        $(
            impl WithHeapItemKind for $item_name {
                const KIND: HeapItemKind = HeapItemKind::$kind;
            }
        )*

        /// Type of an item in the heap. May be a JS object or non-object data stored on the heap,
        /// e.g. descriptors and realms.
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(u8)]
        pub enum HeapItemKind {
            $($kind,)*
        }

        impl HeapItemKind {
            pub const COUNT: usize = <[()]>::len(&[$(unit!($kind)),*]);
        }

        pub fn byte_size_for_kind(item: HeapPtr<AnyHeapItem>, kind: HeapItemKind) -> usize {
            match kind {
                $(HeapItemKind::$kind => $item_name::byte_size(item.cast()),)*
            }
        }

        pub fn visit_pointers_for_kind(item: HeapPtr<AnyHeapItem>, visitor: &mut impl HeapVisitor, kind: HeapItemKind) {
            match kind {
                $(HeapItemKind::$kind => $item_name::visit_pointers(item.cast(), visitor),)*
            }
        }

        impl HeapItem for AnyHeapItem {
            /// Size of this heap item in bytes, dispatched based on the kind of heap item.
            fn byte_size(any: HeapPtr<Self>) -> usize {
                byte_size_for_kind(any, any.descriptor().kind())
            }

            /// Visit all pointer fields in this heap item, dispatched based on the kind of heap item.
            fn visit_pointers(any: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
                visit_pointers_for_kind(any, visitor, any.descriptor().kind());
            }
        }
    };
}

register_heap_items!(
    (Descriptor, HeapItemDescriptor),
    (OrdinaryObject, ObjectValue),
    (Proxy, ProxyObject),
    (BooleanObject, BooleanObject),
    (NumberObject, NumberObject),
    (StringObject, StringObject),
    (SymbolObject, SymbolObject),
    (BigIntObject, BigIntObject),
    (ArrayObject, ArrayObject),
    (RegExpObject, RegExpObject),
    (ErrorObject, ErrorObject),
    (DateObject, DateObject),
    (SetObject, SetObject),
    (MapObject, MapObject),
    (WeakRefObject, WeakRefObject),
    (WeakSetObject, WeakSetObject),
    (WeakMapObject, WeakMapObject),
    (FinalizationRegistryObject, FinalizationRegistryObject),
    (RawJSONObject, RawJSONObject),
    (MappedArgumentsObject, MappedArgumentsObject),
    (UnmappedArgumentsObject, UnmappedArgumentsObject),
    (Int8Array, Int8Array),
    (UInt8Array, UInt8Array),
    (UInt8ClampedArray, UInt8ClampedArray),
    (Int16Array, Int16Array),
    (UInt16Array, UInt16Array),
    (Int32Array, Int32Array),
    (UInt32Array, UInt32Array),
    (BigInt64Array, BigInt64Array),
    (BigUInt64Array, BigUInt64Array),
    (Float16Array, Float16Array),
    (Float32Array, Float32Array),
    (Float64Array, Float64Array),
    (ArrayBufferObject, ArrayBufferObject),
    (DataViewObject, DataViewObject),
    (DurationObject, DurationObject),
    (InstantObject, InstantObject),
    (PlainDateObject, PlainDateObject),
    (PlainDateTimeObject, PlainDateTimeObject),
    (PlainMonthDayObject, PlainMonthDayObject),
    (PlainTimeObject, PlainTimeObject),
    (PlainYearMonthObject, PlainYearMonthObject),
    (ZonedDateTimeObject, ZonedDateTimeObject),
    (ArrayIterator, ArrayIterator),
    (StringIterator, StringIterator),
    (SetIterator, SetIterator),
    (MapIterator, MapIterator),
    (RegExpStringIterator, RegExpStringIterator),
    (ForInIterator, ForInIterator),
    (AsyncFromSyncIterator, AsyncFromSyncIterator),
    (WrappedValidIterator, WrappedValidIterator),
    (IteratorHelperObject, IteratorHelperObject),
    (ObjectPrototype, ObjectPrototype),
    (String, StringValue),
    (Symbol, SymbolValue),
    (BigInt, BigIntValue),
    (Accessor, Accessor),
    (Promise, PromiseObject),
    (PromiseReaction, PromiseReaction),
    (PromiseCapability, PromiseCapability),
    (Realm, Realm),
    (Closure, Closure),
    (BytecodeFunction, BytecodeFunction),
    (ConstantTable, ConstantTable),
    (ExceptionHandlers, ExceptionHandlers),
    (SourceFile, SourceFile),
    (Scope, Scope),
    (ScopeNames, ScopeNames),
    (GlobalNames, GlobalNames),
    (ClassNames, ClassNames),
    (SourceTextModule, SourceTextModule),
    (SyntheticModule, SyntheticModule),
    (ModuleNamespaceObject, ModuleNamespaceObject),
    (ImportAttributes, ImportAttributes),
    (Generator, GeneratorObject),
    (AsyncGenerator, AsyncGeneratorObject),
    (AsyncGeneratorRequest, AsyncGeneratorRequest),
    (BuiltinGenerator, BuiltinGenerator),
    (DenseArrayProperties, DenseArrayProperties),
    (SparseArrayPropertiesMap, SparseArrayPropertiesMap),
    (CompiledRegExpObject, CompiledRegExpObject),
    (BoxedValue, BoxedValue),
    (NamedPropertiesMap, NamedPropertiesMap),
    (ValueIndexMap, ValueIndexMap),
    (ValueIndexSet, ValueIndexSet),
    (ExportMap, ExportMap),
    (WeakValueMap, WeakValueMap),
    (WeakValueSet, WeakValueSet),
    (GlobalSymbolRegistryMap, GlobalSymbolRegistryMap),
    (InternedStringsSet, InternedStringsSet),
    (LexicalNamesMap, LexicalNamesMap),
    (ModuleCacheMap, ModuleCacheMap),
    (ValueArray, ValueArray),
    (ByteArray, ByteArray),
    (U32Array, U32Array),
    (ModuleRequestArray, ModuleRequestArray),
    (ModuleOptionArray, ModuleOptionArray),
    (StackFrameInfoArray, StackFrameInfoArray),
    (FinalizationRegistryCells, FinalizationRegistryCells),
    (GlobalScopes, GlobalScopes),
    (FunctionVec, FunctionVec),
    (SourceTextModuleVec, SourceTextModuleVec),
    (WeakVec, BsWeakVec),
);

/// An arbitrary heap item. Only common field between heap items is their descriptor, which can be
/// used to determine the true type of the heap item.
#[repr(C)]
pub struct AnyHeapItem {
    descriptor: HeapPtr<HeapItemDescriptor>,
}

impl AnyHeapItem {
    pub fn descriptor(&self) -> HeapPtr<HeapItemDescriptor> {
        self.descriptor
    }

    pub fn set_descriptor(&mut self, descriptor: HeapPtr<HeapItemDescriptor>) {
        self.descriptor = descriptor;
    }
}

impl HeapPtr<AnyHeapItem> {
    /// Whether this is a heap item of a particular type.
    #[inline]
    pub fn is<U: WithHeapItemKind>(&self) -> bool {
        self.descriptor().kind() == U::KIND
    }

    /// Return this value as a heap item of a particular type, or None if it is not of that type.
    #[inline]
    pub fn as_opt<U: WithHeapItemKind>(&self) -> Option<HeapPtr<U>> {
        if self.is::<U>() {
            Some(self.cast())
        } else {
            None
        }
    }
}
