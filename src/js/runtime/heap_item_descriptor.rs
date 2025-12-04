use std::mem::size_of;

use bitflags::bitflags;

use crate::{
    runtime::{
        alloc_error::AllocResult, arguments_object::MappedArgumentsObject,
        module::module_namespace_object::ModuleNamespaceObject, ordinary_object::OrdinaryObject,
        rust_vtables::extract_virtual_object_vtable, Value,
    },
    set_uninit,
};

use super::{
    array_object::ArrayObject,
    gc::{Handle, HeapItem, HeapPtr, HeapVisitor},
    intrinsics::typed_array::{
        BigInt64Array, BigUInt64Array, Float16Array, Float32Array, Float64Array, Int16Array,
        Int32Array, Int8Array, UInt16Array, UInt32Array, UInt8Array, UInt8ClampedArray,
    },
    object_value::{VirtualObject, VirtualObjectVtable},
    proxy_object::ProxyObject,
    string_object::StringObject,
    Context,
};

#[repr(C)]
pub struct HeapItemDescriptor {
    /// Always the singleton descriptor descriptor
    descriptor: HeapPtr<HeapItemDescriptor>,
    /// Rust VirtualObject vtable, used for dynamic dispatch to some object methods
    vtable: VirtualObjectVtable,
    /// Object's type
    kind: HeapItemKind,
    /// Bitflags for object
    flags: DescFlags,
}

/// Type of an item in the heap. May be a JS object or non-object data stored on the heap,
/// e.g. descriptors and realms.
#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum HeapItemKind {
    // The descriptor for a descriptor
    Descriptor,

    // All objects
    OrdinaryObject,
    Proxy,

    BooleanObject,
    NumberObject,
    StringObject,
    SymbolObject,
    BigIntObject,
    ArrayObject,
    RegExpObject,
    ErrorObject,
    DateObject,
    SetObject,
    MapObject,
    WeakRefObject,
    WeakSetObject,
    WeakMapObject,
    FinalizationRegistryObject,

    MappedArgumentsObject,
    UnmappedArgumentsObject,

    Int8Array,
    UInt8Array,
    UInt8ClampedArray,
    Int16Array,
    UInt16Array,
    Int32Array,
    UInt32Array,
    BigInt64Array,
    BigUInt64Array,
    Float16Array,
    Float32Array,
    Float64Array,

    ArrayBufferObject,
    DataViewObject,

    ArrayIterator,
    StringIterator,
    SetIterator,
    MapIterator,
    RegExpStringIterator,
    ForInIterator,
    AsyncFromSyncIterator,
    WrappedValidIterator,
    IteratorHelperObject,

    ObjectPrototype,

    // Other heap items
    String,
    Symbol,
    BigInt,
    Accessor,

    Promise,
    PromiseReaction,
    PromiseCapability,

    Realm,

    Closure,
    BytecodeFunction,
    ConstantTable,
    ExceptionHandlers,
    SourceFile,

    Scope,
    ScopeNames,
    GlobalNames,
    ClassNames,

    SourceTextModule,
    SyntheticModule,
    ModuleNamespaceObject,
    ImportAttributes,

    Generator,
    AsyncGenerator,
    AsyncGeneratorRequest,

    DenseArrayProperties,
    SparseArrayProperties,

    CompiledRegExpObject,

    BoxedValue,

    // Hash maps
    ObjectNamedPropertiesMap,
    MapObjectValueMap,
    SetObjectValueSet,
    ExportMap,
    WeakSetObjectWeakValueSet,
    WeakMapObjectWeakValueMap,
    GlobalSymbolRegistryMap,
    InternedStringsSet,
    LexicalNamesMap,
    ModuleCacheMap,

    // Arrays
    ValueArray,
    ByteArray,
    U32Array,
    ModuleRequestArray,
    ModuleOptionArray,
    StackFrameInfoArray,
    FinalizationRegistryCells,
    GlobalScopes,

    // Vectors
    ValueVec,

    // Numerical value is the number of kinds in the enum
    Last,
}

impl HeapItemKind {
    const fn count() -> usize {
        HeapItemKind::Last as usize
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct DescFlags: u8 {
        /// Whether this heap item is an object value
        const IS_OBJECT = 1 << 0;
    }
}

impl HeapItemDescriptor {
    pub fn new<T>(
        cx: Context,
        descriptor: Handle<HeapItemDescriptor>,
        kind: HeapItemKind,
        flags: DescFlags,
    ) -> AllocResult<HeapPtr<HeapItemDescriptor>>
    where
        Handle<T>: VirtualObject,
    {
        let mut desc = cx.alloc_uninit::<HeapItemDescriptor>()?;

        set_uninit!(desc.descriptor, *descriptor);
        set_uninit!(desc.vtable, extract_virtual_object_vtable::<T>());
        set_uninit!(desc.kind, kind);
        set_uninit!(desc.flags, flags);

        Ok(desc)
    }

    #[inline]
    pub const fn kind(&self) -> HeapItemKind {
        self.kind
    }

    #[inline]
    pub const fn vtable(&self) -> VirtualObjectVtable {
        self.vtable
    }

    #[inline]
    pub fn is_object(&self) -> bool {
        self.flags.contains(DescFlags::IS_OBJECT)
    }
}

pub struct BaseDescriptors {
    descriptors: Vec<HeapPtr<HeapItemDescriptor>>,
}

impl BaseDescriptors {
    pub fn uninit_empty() -> Self {
        BaseDescriptors { descriptors: vec![] }
    }

    pub fn uninit() -> Self {
        let mut descriptors = vec![];

        descriptors.reserve_exact(HeapItemKind::count());
        unsafe { descriptors.set_len(HeapItemKind::count()) };

        BaseDescriptors { descriptors }
    }

    pub fn new(cx: Context) -> AllocResult<BaseDescriptors> {
        let mut base_descriptors = Self::uninit();
        let descriptors = &mut base_descriptors.descriptors;

        // Create fake handle which will be read from, in order to initialize descriptor descriptor
        let value = Value::empty();
        let fake_descriptor_handle = Handle::<Value>::from_fixed_non_heap_ptr(&value).cast();

        // First set up the singleton descriptor descriptor, using an arbitrary vtable
        // (e.g. OrdinaryObject). Can only set self pointer after object initially created.
        let mut descriptor = HeapItemDescriptor::new::<OrdinaryObject>(
            cx,
            fake_descriptor_handle,
            HeapItemKind::Descriptor,
            DescFlags::empty(),
        )?
        .to_handle();
        descriptor.descriptor = *descriptor;
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
        register_descriptor!(HeapItemKind::Proxy, ProxyObject, DescFlags::IS_OBJECT);

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

        register_descriptor!(
            HeapItemKind::MappedArgumentsObject,
            MappedArgumentsObject,
            DescFlags::IS_OBJECT
        );
        ordinary_object_descriptor!(HeapItemKind::UnmappedArgumentsObject);

        register_descriptor!(HeapItemKind::Int8Array, Int8Array, DescFlags::IS_OBJECT);
        register_descriptor!(HeapItemKind::UInt8Array, UInt8Array, DescFlags::IS_OBJECT);
        register_descriptor!(
            HeapItemKind::UInt8ClampedArray,
            UInt8ClampedArray,
            DescFlags::IS_OBJECT
        );
        register_descriptor!(HeapItemKind::Int16Array, Int16Array, DescFlags::IS_OBJECT);
        register_descriptor!(HeapItemKind::UInt16Array, UInt16Array, DescFlags::IS_OBJECT);
        register_descriptor!(HeapItemKind::Int32Array, Int32Array, DescFlags::IS_OBJECT);
        register_descriptor!(HeapItemKind::UInt32Array, UInt32Array, DescFlags::IS_OBJECT);
        register_descriptor!(HeapItemKind::BigInt64Array, BigInt64Array, DescFlags::IS_OBJECT);
        register_descriptor!(HeapItemKind::BigUInt64Array, BigUInt64Array, DescFlags::IS_OBJECT);
        register_descriptor!(HeapItemKind::Float16Array, Float16Array, DescFlags::IS_OBJECT);
        register_descriptor!(HeapItemKind::Float32Array, Float32Array, DescFlags::IS_OBJECT);
        register_descriptor!(HeapItemKind::Float64Array, Float64Array, DescFlags::IS_OBJECT);

        ordinary_object_descriptor!(HeapItemKind::ArrayBufferObject);
        ordinary_object_descriptor!(HeapItemKind::DataViewObject);

        ordinary_object_descriptor!(HeapItemKind::ArrayIterator);
        ordinary_object_descriptor!(HeapItemKind::StringIterator);
        ordinary_object_descriptor!(HeapItemKind::SetIterator);
        ordinary_object_descriptor!(HeapItemKind::MapIterator);
        ordinary_object_descriptor!(HeapItemKind::RegExpStringIterator);
        other_heap_item_descriptor!(HeapItemKind::ForInIterator);
        ordinary_object_descriptor!(HeapItemKind::AsyncFromSyncIterator);
        ordinary_object_descriptor!(HeapItemKind::WrappedValidIterator);
        ordinary_object_descriptor!(HeapItemKind::IteratorHelperObject);

        ordinary_object_descriptor!(HeapItemKind::ObjectPrototype);

        other_heap_item_descriptor!(HeapItemKind::String);
        other_heap_item_descriptor!(HeapItemKind::Symbol);
        other_heap_item_descriptor!(HeapItemKind::BigInt);
        other_heap_item_descriptor!(HeapItemKind::Accessor);

        ordinary_object_descriptor!(HeapItemKind::Promise);
        other_heap_item_descriptor!(HeapItemKind::PromiseReaction);
        other_heap_item_descriptor!(HeapItemKind::PromiseCapability);

        other_heap_item_descriptor!(HeapItemKind::Realm);

        ordinary_object_descriptor!(HeapItemKind::Closure);
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

        ordinary_object_descriptor!(HeapItemKind::Generator);
        ordinary_object_descriptor!(HeapItemKind::AsyncGenerator);
        other_heap_item_descriptor!(HeapItemKind::AsyncGeneratorRequest);

        other_heap_item_descriptor!(HeapItemKind::DenseArrayProperties);
        other_heap_item_descriptor!(HeapItemKind::SparseArrayProperties);

        other_heap_item_descriptor!(HeapItemKind::CompiledRegExpObject);

        other_heap_item_descriptor!(HeapItemKind::BoxedValue);

        other_heap_item_descriptor!(HeapItemKind::ObjectNamedPropertiesMap);
        other_heap_item_descriptor!(HeapItemKind::MapObjectValueMap);
        other_heap_item_descriptor!(HeapItemKind::SetObjectValueSet);
        other_heap_item_descriptor!(HeapItemKind::ExportMap);
        other_heap_item_descriptor!(HeapItemKind::WeakMapObjectWeakValueMap);
        other_heap_item_descriptor!(HeapItemKind::WeakSetObjectWeakValueSet);
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

        other_heap_item_descriptor!(HeapItemKind::ValueVec);

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

impl HeapItem for HeapPtr<HeapItemDescriptor> {
    fn byte_size(&self) -> usize {
        size_of::<HeapItemDescriptor>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
        visitor.visit_rust_vtable_pointer(&mut self.vtable);
    }
}
