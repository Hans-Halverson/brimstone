use std::mem::size_of;

use bitflags::bitflags;

use crate::{
    runtime::{
        alloc_error::AllocResult,
        gc::{Handle, HeapItem, HeapPtr, HeapVisitor},
        object_value::{VirtualObject, VirtualObjectVtable},
        ordinary_object::OrdinaryObject,
        rust_vtables::extract_virtual_object_vtable,
        Context, Value,
    },
    set_uninit,
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
    RawJSONObject,

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
    BuiltinGenerator,

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
    WeakVec,

    // Numerical value is the number of kinds in the enum
    Last,
}

impl HeapItemKind {
    pub const fn count() -> usize {
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

    /// Create the singleton descriptor descriptor, which all descriptors have in their
    /// `descriptor` field.
    pub fn new_descriptor_descriptor(cx: Context) -> AllocResult<HeapPtr<HeapItemDescriptor>> {
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
        )?;

        // Descriptor descriptor points to itself, like all other descriptors
        descriptor.descriptor = descriptor;

        Ok(descriptor)
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

    pub fn set_descriptor(&mut self, descriptor: HeapPtr<HeapItemDescriptor>) {
        self.descriptor = descriptor;
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
