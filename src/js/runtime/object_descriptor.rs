use std::mem::size_of;

use bitflags::bitflags;

use crate::{
    js::runtime::{
        arguments_object::MappedArgumentsObject,
        module::module_namespace_object::ModuleNamespaceObject, ordinary_object::OrdinaryObject,
        Value,
    },
    set_uninit,
};

use super::{
    array_object::ArrayObject,
    gc::{Handle, HeapObject, HeapPtr, HeapVisitor},
    intrinsics::typed_array::{
        BigInt64Array, BigUInt64Array, Float32Array, Float64Array, Int16Array, Int32Array,
        Int8Array, UInt16Array, UInt32Array, UInt8Array, UInt8ClampedArray,
    },
    object_value::{extract_object_vtable, VirtualObject, VirtualObjectVtable},
    proxy_object::ProxyObject,
    string_object::StringObject,
    Context,
};

/// An object's "hidden class".
#[repr(C)]
pub struct ObjectDescriptor {
    // Always the singleton descriptor descriptor
    descriptor: HeapPtr<ObjectDescriptor>,
    // Rust VirtualObject vtable, used for dynamic dispatch to some object methods
    vtable: VirtualObjectVtable,
    // Object's type
    kind: ObjectKind,
    // Bitflags for object
    flags: DescFlags,
}

/// Type of an object on the heap. May also represent other non-object data stored on the heap,
/// e.g. descriptors and realms.
#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum ObjectKind {
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
    InternedStringsMap,
    InternedStringsSet,
    LexicalNamesMap,
    ModuleCacheMap,

    // Arrays
    ValueArray,
    ByteArray,
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

impl ObjectKind {
    const fn count() -> usize {
        ObjectKind::Last as usize
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct DescFlags: u8 {
        /// Whether this heap item is an object value
        const IS_OBJECT = 1 << 0;
    }
}

impl ObjectDescriptor {
    pub fn new<T>(
        cx: Context,
        descriptor: Handle<ObjectDescriptor>,
        kind: ObjectKind,
        flags: DescFlags,
    ) -> Handle<ObjectDescriptor>
    where
        Handle<T>: VirtualObject,
    {
        let mut desc = cx.alloc_uninit::<ObjectDescriptor>();

        set_uninit!(desc.descriptor, *descriptor);
        set_uninit!(desc.vtable, extract_object_vtable::<Handle<T>>());
        set_uninit!(desc.kind, kind);
        set_uninit!(desc.flags, flags);

        desc.to_handle()
    }

    #[inline]
    pub const fn kind(&self) -> ObjectKind {
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
    descriptors: Vec<HeapPtr<ObjectDescriptor>>,
}

impl BaseDescriptors {
    pub fn uninit() -> BaseDescriptors {
        BaseDescriptors { descriptors: vec![] }
    }

    pub fn new(cx: Context) -> BaseDescriptors {
        let mut descriptors = vec![];

        descriptors.reserve_exact(ObjectKind::count());
        unsafe { descriptors.set_len(ObjectKind::count()) };

        // Create fake handle which will be read from, in order to initialize descriptor descriptor
        let value = Value::empty();
        let fake_descriptor_handle = Handle::<Value>::from_fixed_non_heap_ptr(&value).cast();

        // First set up the singleton descriptor descriptor, using an arbitrary vtable
        // (e.g. OrdinaryObject). Can only set self pointer after object initially created.
        let mut descriptor = ObjectDescriptor::new::<OrdinaryObject>(
            cx,
            fake_descriptor_handle,
            ObjectKind::Descriptor,
            DescFlags::empty(),
        );
        descriptor.descriptor = *descriptor;
        descriptors[ObjectKind::Descriptor as usize] = *descriptor;

        macro_rules! register_descriptor {
            ($object_kind:expr, $object_ty:ty, $flags:expr) => {
                let desc =
                    ObjectDescriptor::new::<$object_ty>(cx, descriptor, $object_kind, $flags);
                descriptors[$object_kind as usize] = *desc;
            };
        }

        macro_rules! ordinary_object_descriptor {
            ($object_kind:expr) => {
                register_descriptor!($object_kind, OrdinaryObject, DescFlags::IS_OBJECT);
            };
        }

        macro_rules! other_heap_object_descriptor {
            ($object_kind:expr) => {
                register_descriptor!($object_kind, OrdinaryObject, DescFlags::empty());
            };
        }

        ordinary_object_descriptor!(ObjectKind::OrdinaryObject);
        register_descriptor!(ObjectKind::Proxy, ProxyObject, DescFlags::IS_OBJECT);

        ordinary_object_descriptor!(ObjectKind::BooleanObject);
        ordinary_object_descriptor!(ObjectKind::NumberObject);
        register_descriptor!(ObjectKind::StringObject, StringObject, DescFlags::IS_OBJECT);
        ordinary_object_descriptor!(ObjectKind::SymbolObject);
        ordinary_object_descriptor!(ObjectKind::BigIntObject);
        register_descriptor!(ObjectKind::ArrayObject, ArrayObject, DescFlags::IS_OBJECT);
        ordinary_object_descriptor!(ObjectKind::RegExpObject);
        ordinary_object_descriptor!(ObjectKind::ErrorObject);
        ordinary_object_descriptor!(ObjectKind::DateObject);
        ordinary_object_descriptor!(ObjectKind::SetObject);
        ordinary_object_descriptor!(ObjectKind::MapObject);
        ordinary_object_descriptor!(ObjectKind::WeakRefObject);
        ordinary_object_descriptor!(ObjectKind::WeakSetObject);
        ordinary_object_descriptor!(ObjectKind::WeakMapObject);
        ordinary_object_descriptor!(ObjectKind::FinalizationRegistryObject);

        register_descriptor!(
            ObjectKind::MappedArgumentsObject,
            MappedArgumentsObject,
            DescFlags::IS_OBJECT
        );
        ordinary_object_descriptor!(ObjectKind::UnmappedArgumentsObject);

        register_descriptor!(ObjectKind::Int8Array, Int8Array, DescFlags::IS_OBJECT);
        register_descriptor!(ObjectKind::UInt8Array, UInt8Array, DescFlags::IS_OBJECT);
        register_descriptor!(
            ObjectKind::UInt8ClampedArray,
            UInt8ClampedArray,
            DescFlags::IS_OBJECT
        );
        register_descriptor!(ObjectKind::Int16Array, Int16Array, DescFlags::IS_OBJECT);
        register_descriptor!(ObjectKind::UInt16Array, UInt16Array, DescFlags::IS_OBJECT);
        register_descriptor!(ObjectKind::Int32Array, Int32Array, DescFlags::IS_OBJECT);
        register_descriptor!(ObjectKind::UInt32Array, UInt32Array, DescFlags::IS_OBJECT);
        register_descriptor!(ObjectKind::BigInt64Array, BigInt64Array, DescFlags::IS_OBJECT);
        register_descriptor!(ObjectKind::BigUInt64Array, BigUInt64Array, DescFlags::IS_OBJECT);
        register_descriptor!(ObjectKind::Float32Array, Float32Array, DescFlags::IS_OBJECT);
        register_descriptor!(ObjectKind::Float64Array, Float64Array, DescFlags::IS_OBJECT);

        ordinary_object_descriptor!(ObjectKind::ArrayBufferObject);
        ordinary_object_descriptor!(ObjectKind::DataViewObject);

        ordinary_object_descriptor!(ObjectKind::ArrayIterator);
        ordinary_object_descriptor!(ObjectKind::StringIterator);
        ordinary_object_descriptor!(ObjectKind::SetIterator);
        ordinary_object_descriptor!(ObjectKind::MapIterator);
        ordinary_object_descriptor!(ObjectKind::RegExpStringIterator);
        other_heap_object_descriptor!(ObjectKind::ForInIterator);
        ordinary_object_descriptor!(ObjectKind::AsyncFromSyncIterator);

        ordinary_object_descriptor!(ObjectKind::ObjectPrototype);

        other_heap_object_descriptor!(ObjectKind::String);
        other_heap_object_descriptor!(ObjectKind::Symbol);
        other_heap_object_descriptor!(ObjectKind::BigInt);
        other_heap_object_descriptor!(ObjectKind::Accessor);

        ordinary_object_descriptor!(ObjectKind::Promise);
        other_heap_object_descriptor!(ObjectKind::PromiseReaction);
        other_heap_object_descriptor!(ObjectKind::PromiseCapability);

        other_heap_object_descriptor!(ObjectKind::Realm);

        ordinary_object_descriptor!(ObjectKind::Closure);
        other_heap_object_descriptor!(ObjectKind::BytecodeFunction);
        other_heap_object_descriptor!(ObjectKind::ConstantTable);
        other_heap_object_descriptor!(ObjectKind::ExceptionHandlers);
        other_heap_object_descriptor!(ObjectKind::SourceFile);

        other_heap_object_descriptor!(ObjectKind::Scope);
        other_heap_object_descriptor!(ObjectKind::ScopeNames);
        other_heap_object_descriptor!(ObjectKind::GlobalNames);
        other_heap_object_descriptor!(ObjectKind::ClassNames);

        other_heap_object_descriptor!(ObjectKind::SourceTextModule);
        other_heap_object_descriptor!(ObjectKind::SyntheticModule);
        register_descriptor!(
            ObjectKind::ModuleNamespaceObject,
            ModuleNamespaceObject,
            DescFlags::IS_OBJECT
        );
        other_heap_object_descriptor!(ObjectKind::ImportAttributes);

        ordinary_object_descriptor!(ObjectKind::Generator);
        ordinary_object_descriptor!(ObjectKind::AsyncGenerator);
        other_heap_object_descriptor!(ObjectKind::AsyncGeneratorRequest);

        other_heap_object_descriptor!(ObjectKind::DenseArrayProperties);
        other_heap_object_descriptor!(ObjectKind::SparseArrayProperties);

        other_heap_object_descriptor!(ObjectKind::CompiledRegExpObject);

        other_heap_object_descriptor!(ObjectKind::BoxedValue);

        other_heap_object_descriptor!(ObjectKind::ObjectNamedPropertiesMap);
        other_heap_object_descriptor!(ObjectKind::MapObjectValueMap);
        other_heap_object_descriptor!(ObjectKind::SetObjectValueSet);
        other_heap_object_descriptor!(ObjectKind::ExportMap);
        other_heap_object_descriptor!(ObjectKind::WeakMapObjectWeakValueMap);
        other_heap_object_descriptor!(ObjectKind::WeakSetObjectWeakValueSet);
        other_heap_object_descriptor!(ObjectKind::GlobalSymbolRegistryMap);
        other_heap_object_descriptor!(ObjectKind::InternedStringsMap);
        other_heap_object_descriptor!(ObjectKind::InternedStringsSet);
        other_heap_object_descriptor!(ObjectKind::LexicalNamesMap);
        other_heap_object_descriptor!(ObjectKind::ModuleCacheMap);

        other_heap_object_descriptor!(ObjectKind::ValueArray);
        other_heap_object_descriptor!(ObjectKind::ByteArray);
        other_heap_object_descriptor!(ObjectKind::ModuleRequestArray);
        other_heap_object_descriptor!(ObjectKind::ModuleOptionArray);
        other_heap_object_descriptor!(ObjectKind::StackFrameInfoArray);
        other_heap_object_descriptor!(ObjectKind::FinalizationRegistryCells);
        other_heap_object_descriptor!(ObjectKind::GlobalScopes);

        other_heap_object_descriptor!(ObjectKind::ValueVec);

        BaseDescriptors { descriptors }
    }

    pub fn get(&self, kind: ObjectKind) -> HeapPtr<ObjectDescriptor> {
        self.descriptors[kind as usize]
    }
}

impl HeapObject for HeapPtr<ObjectDescriptor> {
    fn byte_size(&self) -> usize {
        size_of::<ObjectDescriptor>()
    }

    fn visit_pointers(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.descriptor);
    }
}
