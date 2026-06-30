use crate::runtime::{
    Context, HeapItemKind,
    alloc_error::AllocResult,
    arguments_object::MappedArgumentsObject,
    array_object::ArrayObject,
    gc::{HeapPtr, HeapVisitor},
    heap_item_descriptor::{DescFlags, HeapItemDescriptor},
    intrinsics::typed_array::{
        BigInt64ArrayObject, BigUInt64ArrayObject, Float16ArrayObject, Float32ArrayObject,
        Float64ArrayObject, Int8ArrayObject, Int16ArrayObject, Int32ArrayObject, UInt8ArrayObject,
        UInt8ClampedArrayObject, UInt16ArrayObject, UInt32ArrayObject,
    },
    module::module_namespace_object::ModuleNamespaceObject,
    ordinary_object::OrdinaryObject,
    proxy_object::ProxyObject,
    string_object::StringObject,
};

/// Central registry for all descriptors in a context.
pub struct DescriptorRegistry {
    descriptors: Vec<HeapPtr<HeapItemDescriptor>>,
}

impl DescriptorRegistry {
    pub fn uninit_empty() -> Self {
        DescriptorRegistry { descriptors: vec![] }
    }

    pub fn uninit() -> Self {
        let mut descriptors = vec![];

        descriptors.reserve_exact(HeapItemKind::COUNT);
        unsafe { descriptors.set_len(HeapItemKind::COUNT) };

        DescriptorRegistry { descriptors }
    }

    pub fn new(cx: Context) -> AllocResult<DescriptorRegistry> {
        let mut base_descriptors = Self::uninit();
        let descriptors = &mut base_descriptors.descriptors;

        // First set up the descriptor descriptor since it is needed for all others
        let descriptor = HeapItemDescriptor::new_descriptor_descriptor(cx)?.to_handle();
        descriptors[HeapItemKind::Descriptor as usize] = *descriptor;

        macro_rules! register_descriptor {
            ($object_kind:expr, $object_ty:ty, $flags:expr) => {
                let desc =
                    HeapItemDescriptor::new::<$object_ty>(cx, descriptor, $object_kind, $flags)?;
                descriptors[$object_kind as usize] = desc;
            };
        }

        macro_rules! ordinary_object_descriptor {
            ($object_kind:expr) => {
                register_descriptor!($object_kind, OrdinaryObject, DescFlags::IS_OBJECT);
            };
        }

        macro_rules! other_heap_item_descriptor {
            ($object_kind:expr) => {
                register_descriptor!($object_kind, OrdinaryObject, DescFlags::empty());
            };
        }

        ordinary_object_descriptor!(HeapItemKind::OrdinaryObject);
        register_descriptor!(HeapItemKind::ProxyObject, ProxyObject, DescFlags::IS_OBJECT);

        ordinary_object_descriptor!(HeapItemKind::BooleanObject);
        ordinary_object_descriptor!(HeapItemKind::NumberObject);
        register_descriptor!(HeapItemKind::StringObject, StringObject, DescFlags::IS_OBJECT);
        ordinary_object_descriptor!(HeapItemKind::SymbolObject);
        ordinary_object_descriptor!(HeapItemKind::BigIntObject);
        register_descriptor!(HeapItemKind::ArrayObject, ArrayObject, DescFlags::IS_OBJECT);
        ordinary_object_descriptor!(HeapItemKind::RegExpObject);
        ordinary_object_descriptor!(HeapItemKind::ErrorObject);
        ordinary_object_descriptor!(HeapItemKind::DateObject);
        ordinary_object_descriptor!(HeapItemKind::SetObject);
        ordinary_object_descriptor!(HeapItemKind::MapObject);
        ordinary_object_descriptor!(HeapItemKind::WeakRefObject);
        ordinary_object_descriptor!(HeapItemKind::WeakSetObject);
        ordinary_object_descriptor!(HeapItemKind::WeakMapObject);
        ordinary_object_descriptor!(HeapItemKind::FinalizationRegistryObject);
        ordinary_object_descriptor!(HeapItemKind::RawJSONObject);

        register_descriptor!(
            HeapItemKind::MappedArgumentsObject,
            MappedArgumentsObject,
            DescFlags::IS_OBJECT
        );
        ordinary_object_descriptor!(HeapItemKind::UnmappedArgumentsObject);

        register_descriptor!(HeapItemKind::Int8ArrayObject, Int8ArrayObject, DescFlags::IS_OBJECT);
        register_descriptor!(
            HeapItemKind::UInt8ArrayObject,
            UInt8ArrayObject,
            DescFlags::IS_OBJECT
        );
        register_descriptor!(
            HeapItemKind::UInt8ClampedArrayObject,
            UInt8ClampedArrayObject,
            DescFlags::IS_OBJECT
        );
        register_descriptor!(
            HeapItemKind::Int16ArrayObject,
            Int16ArrayObject,
            DescFlags::IS_OBJECT
        );
        register_descriptor!(
            HeapItemKind::UInt16ArrayObject,
            UInt16ArrayObject,
            DescFlags::IS_OBJECT
        );
        register_descriptor!(
            HeapItemKind::Int32ArrayObject,
            Int32ArrayObject,
            DescFlags::IS_OBJECT
        );
        register_descriptor!(
            HeapItemKind::UInt32ArrayObject,
            UInt32ArrayObject,
            DescFlags::IS_OBJECT
        );
        register_descriptor!(
            HeapItemKind::BigInt64ArrayObject,
            BigInt64ArrayObject,
            DescFlags::IS_OBJECT
        );
        register_descriptor!(
            HeapItemKind::BigUInt64ArrayObject,
            BigUInt64ArrayObject,
            DescFlags::IS_OBJECT
        );
        register_descriptor!(
            HeapItemKind::Float16ArrayObject,
            Float16ArrayObject,
            DescFlags::IS_OBJECT
        );
        register_descriptor!(
            HeapItemKind::Float32ArrayObject,
            Float32ArrayObject,
            DescFlags::IS_OBJECT
        );
        register_descriptor!(
            HeapItemKind::Float64ArrayObject,
            Float64ArrayObject,
            DescFlags::IS_OBJECT
        );

        ordinary_object_descriptor!(HeapItemKind::ArrayBufferObject);
        ordinary_object_descriptor!(HeapItemKind::DataViewObject);

        ordinary_object_descriptor!(HeapItemKind::DurationObject);
        ordinary_object_descriptor!(HeapItemKind::InstantObject);
        ordinary_object_descriptor!(HeapItemKind::PlainDateObject);
        ordinary_object_descriptor!(HeapItemKind::PlainDateTimeObject);
        ordinary_object_descriptor!(HeapItemKind::PlainMonthDayObject);
        ordinary_object_descriptor!(HeapItemKind::PlainTimeObject);
        ordinary_object_descriptor!(HeapItemKind::PlainYearMonthObject);
        ordinary_object_descriptor!(HeapItemKind::ZonedDateTimeObject);

        ordinary_object_descriptor!(HeapItemKind::ArrayIteratorObject);
        ordinary_object_descriptor!(HeapItemKind::StringIteratorObject);
        ordinary_object_descriptor!(HeapItemKind::SetIteratorObject);
        ordinary_object_descriptor!(HeapItemKind::MapIteratorObject);
        ordinary_object_descriptor!(HeapItemKind::RegExpStringIteratorObject);
        other_heap_item_descriptor!(HeapItemKind::ForInIterator);
        ordinary_object_descriptor!(HeapItemKind::AsyncFromSyncIteratorObject);
        ordinary_object_descriptor!(HeapItemKind::WrappedValidIteratorObject);
        ordinary_object_descriptor!(HeapItemKind::IteratorHelperObject);

        ordinary_object_descriptor!(HeapItemKind::ObjectPrototypeObject);

        other_heap_item_descriptor!(HeapItemKind::String);
        other_heap_item_descriptor!(HeapItemKind::Symbol);
        other_heap_item_descriptor!(HeapItemKind::BigInt);
        other_heap_item_descriptor!(HeapItemKind::Accessor);

        ordinary_object_descriptor!(HeapItemKind::PromiseObject);
        other_heap_item_descriptor!(HeapItemKind::PromiseReaction);
        other_heap_item_descriptor!(HeapItemKind::PromiseCapability);

        other_heap_item_descriptor!(HeapItemKind::Realm);

        ordinary_object_descriptor!(HeapItemKind::ClosureObject);
        other_heap_item_descriptor!(HeapItemKind::BytecodeFunction);
        other_heap_item_descriptor!(HeapItemKind::ConstantTable);
        other_heap_item_descriptor!(HeapItemKind::ExceptionHandlers);
        other_heap_item_descriptor!(HeapItemKind::SourceFile);

        other_heap_item_descriptor!(HeapItemKind::Scope);
        other_heap_item_descriptor!(HeapItemKind::ScopeNames);
        other_heap_item_descriptor!(HeapItemKind::GlobalNames);
        other_heap_item_descriptor!(HeapItemKind::ClassNames);

        other_heap_item_descriptor!(HeapItemKind::SourceTextModule);
        other_heap_item_descriptor!(HeapItemKind::SyntheticModule);
        register_descriptor!(
            HeapItemKind::ModuleNamespaceObject,
            ModuleNamespaceObject,
            DescFlags::IS_OBJECT
        );
        other_heap_item_descriptor!(HeapItemKind::ImportAttributes);

        ordinary_object_descriptor!(HeapItemKind::GeneratorObject);
        ordinary_object_descriptor!(HeapItemKind::AsyncGeneratorObject);
        other_heap_item_descriptor!(HeapItemKind::AsyncGeneratorRequest);
        other_heap_item_descriptor!(HeapItemKind::BuiltinGenerator);

        other_heap_item_descriptor!(HeapItemKind::DenseArrayProperties);
        other_heap_item_descriptor!(HeapItemKind::SparseArrayPropertiesMap);

        other_heap_item_descriptor!(HeapItemKind::CompiledRegExp);

        other_heap_item_descriptor!(HeapItemKind::BoxedValue);

        other_heap_item_descriptor!(HeapItemKind::NamedPropertiesMap);
        other_heap_item_descriptor!(HeapItemKind::ValueIndexMap);
        other_heap_item_descriptor!(HeapItemKind::ValueIndexSet);
        other_heap_item_descriptor!(HeapItemKind::ExportMap);
        other_heap_item_descriptor!(HeapItemKind::WeakValueMap);
        other_heap_item_descriptor!(HeapItemKind::WeakValueSet);
        other_heap_item_descriptor!(HeapItemKind::GlobalSymbolRegistryMap);
        other_heap_item_descriptor!(HeapItemKind::InternedStringsSet);
        other_heap_item_descriptor!(HeapItemKind::LexicalNamesMap);
        other_heap_item_descriptor!(HeapItemKind::ModuleCacheMap);

        other_heap_item_descriptor!(HeapItemKind::ValueArray);
        other_heap_item_descriptor!(HeapItemKind::ByteArray);
        other_heap_item_descriptor!(HeapItemKind::U32Array);
        other_heap_item_descriptor!(HeapItemKind::ModuleRequestArray);
        other_heap_item_descriptor!(HeapItemKind::ModuleOptionArray);
        other_heap_item_descriptor!(HeapItemKind::StackFrameInfoArray);
        other_heap_item_descriptor!(HeapItemKind::FinalizationRegistryCells);
        other_heap_item_descriptor!(HeapItemKind::GlobalScopes);

        other_heap_item_descriptor!(HeapItemKind::FunctionVec);
        other_heap_item_descriptor!(HeapItemKind::SourceTextModuleVec);
        other_heap_item_descriptor!(HeapItemKind::WeakVec);

        Ok(base_descriptors)
    }

    pub fn get(&self, kind: HeapItemKind) -> HeapPtr<HeapItemDescriptor> {
        self.descriptors[kind as usize]
    }

    pub fn visit_roots(&mut self, visitor: &mut impl HeapVisitor) {
        for descriptor in &mut self.descriptors {
            visitor.visit_pointer(descriptor);
        }
    }
}
