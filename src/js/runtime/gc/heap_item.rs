use crate::{
    runtime::{
        BigIntValue, Realm, SymbolValue,
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
            function::{BytecodeFunction, ClosureObject},
            generator::FunctionVec,
        },
        class_names::ClassNames,
        collections::{
            WeakValueVec,
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
            array_buffer_object::ArrayBufferObject,
            array_iterator_object::ArrayIteratorObject,
            async_from_sync_iterator_object::AsyncFromSyncIteratorObject,
            bigint_object::BigIntObject,
            boolean_object::BooleanObject,
            data_view_object::DataViewObject,
            date_object::DateObject,
            error_object::ErrorObject,
            finalization_registry_object::{FinalizationRegistryCells, FinalizationRegistryObject},
            iterator_helper_object::IteratorHelperObject,
            map_iterator_object::MapIteratorObject,
            map_object::{MapObject, ValueIndexMap},
            number_object::NumberObject,
            object_prototype_object::ObjectPrototypeObject,
            raw_json_object::RawJSONObject,
            regexp_object::RegExpObject,
            regexp_string_iterator_object::RegExpStringIteratorObject,
            set_iterator_object::SetIteratorObject,
            set_object::{SetObject, ValueIndexSet},
            string_iterator_object::StringIteratorObject,
            symbol_object::SymbolObject,
            temporal::{
                duration_object::DurationObject, instant_object::InstantObject,
                plain_date_object::PlainDateObject, plain_date_time_object::PlainDateTimeObject,
                plain_month_day_object::PlainMonthDayObject, plain_time_object::PlainTimeObject,
                plain_year_month_object::PlainYearMonthObject,
                zoned_date_time_object::ZonedDateTimeObject,
            },
            typed_array::{
                BigInt64ArrayObject, BigUInt64ArrayObject, Float16ArrayObject, Float32ArrayObject,
                Float64ArrayObject, Int8ArrayObject, Int16ArrayObject, Int32ArrayObject,
                UInt8ArrayObject, UInt8ClampedArrayObject, UInt16ArrayObject, UInt32ArrayObject,
            },
            weak_map_object::{WeakMapObject, WeakValueMap},
            weak_ref_object::WeakRefObject,
            weak_set_object::{WeakSetObject, WeakValueSet},
            wrapped_valid_iterator_object::WrappedValidIteratorObject,
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
        object_value::NamedPropertiesMap,
        ordinary_object::OrdinaryObject,
        promise_object::{PromiseCapability, PromiseObject, PromiseReaction},
        proxy_object::ProxyObject,
        realm::{GlobalScopes, LexicalNamesMap},
        regexp::compiled_regexp::CompiledRegExp,
        scope::Scope,
        scope_names::ScopeNames,
        source_file::SourceFile,
        stack_trace::StackFrameInfoArray,
        string_object::StringObject,
        string_value::StringValue,
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

impl<T> HeapPtr<T> {
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
    ($(($name:ident),)*) => {
        $(
            impl WithHeapItemKind for $name {
                const KIND: HeapItemKind = HeapItemKind::$name;
            }
        )*

        /// Type of an item in the heap. May be a JS object or non-object data stored on the heap,
        /// e.g. descriptors and realms.
        #[derive(Clone, Copy, Debug, PartialEq)]
        #[repr(u8)]
        pub enum HeapItemKind {
            $($name,)*
        }

        impl HeapItemKind {
            pub const COUNT: usize = <[()]>::len(&[$(unit!($name)),*]);
        }

        pub fn byte_size_for_kind(item: HeapPtr<AnyHeapItem>, kind: HeapItemKind) -> usize {
            match kind {
                $(HeapItemKind::$name => $name::byte_size(item.cast()),)*
            }
        }

        pub fn visit_pointers_for_kind(item: HeapPtr<AnyHeapItem>, visitor: &mut impl HeapVisitor, kind: HeapItemKind) {
            match kind {
                $(HeapItemKind::$name => $name::visit_pointers(item.cast(), visitor),)*
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
    (HeapItemDescriptor),
    (OrdinaryObject),
    (ProxyObject),
    (BooleanObject),
    (NumberObject),
    (StringObject),
    (SymbolObject),
    (BigIntObject),
    (ArrayObject),
    (RegExpObject),
    (ErrorObject),
    (DateObject),
    (SetObject),
    (MapObject),
    (WeakRefObject),
    (WeakSetObject),
    (WeakMapObject),
    (FinalizationRegistryObject),
    (RawJSONObject),
    (MappedArgumentsObject),
    (UnmappedArgumentsObject),
    (Int8ArrayObject),
    (UInt8ArrayObject),
    (UInt8ClampedArrayObject),
    (Int16ArrayObject),
    (UInt16ArrayObject),
    (Int32ArrayObject),
    (UInt32ArrayObject),
    (BigInt64ArrayObject),
    (BigUInt64ArrayObject),
    (Float16ArrayObject),
    (Float32ArrayObject),
    (Float64ArrayObject),
    (ArrayBufferObject),
    (DataViewObject),
    (DurationObject),
    (InstantObject),
    (PlainDateObject),
    (PlainDateTimeObject),
    (PlainMonthDayObject),
    (PlainTimeObject),
    (PlainYearMonthObject),
    (ZonedDateTimeObject),
    (ArrayIteratorObject),
    (StringIteratorObject),
    (SetIteratorObject),
    (MapIteratorObject),
    (RegExpStringIteratorObject),
    (ForInIterator),
    (AsyncFromSyncIteratorObject),
    (WrappedValidIteratorObject),
    (IteratorHelperObject),
    (ObjectPrototypeObject),
    (StringValue),
    (SymbolValue),
    (BigIntValue),
    (Accessor),
    (PromiseObject),
    (PromiseReaction),
    (PromiseCapability),
    (Realm),
    (ClosureObject),
    (BytecodeFunction),
    (ConstantTable),
    (ExceptionHandlers),
    (SourceFile),
    (Scope),
    (ScopeNames),
    (GlobalNames),
    (ClassNames),
    (SourceTextModule),
    (SyntheticModule),
    (ModuleNamespaceObject),
    (ImportAttributes),
    (GeneratorObject),
    (AsyncGeneratorObject),
    (AsyncGeneratorRequest),
    (BuiltinGenerator),
    (DenseArrayProperties),
    (SparseArrayPropertiesMap),
    (CompiledRegExp),
    (BoxedValue),
    (NamedPropertiesMap),
    (ValueIndexMap),
    (ValueIndexSet),
    (ExportMap),
    (WeakValueMap),
    (WeakValueSet),
    (GlobalSymbolRegistryMap),
    (InternedStringsSet),
    (LexicalNamesMap),
    (ModuleCacheMap),
    (ValueArray),
    (ByteArray),
    (U32Array),
    (ModuleRequestArray),
    (ModuleOptionArray),
    (StackFrameInfoArray),
    (FinalizationRegistryCells),
    (GlobalScopes),
    (FunctionVec),
    (SourceTextModuleVec),
    (WeakValueVec),
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

/// Storage for a value whose natural alignment exceeds the 8-byte alignment of the managed heap.
#[repr(C, packed(8))]
pub struct HeapUnaligned<T>(T);

impl<T> HeapUnaligned<T> {
    #[inline]
    pub fn new(value: T) -> Self {
        HeapUnaligned(value)
    }

    #[inline]
    pub fn get(&self) -> T {
        unsafe { std::ptr::read_unaligned(&raw const self.0) }
    }
}
