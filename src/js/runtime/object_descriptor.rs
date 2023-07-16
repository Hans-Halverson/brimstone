use bitflags::bitflags;

use crate::{
    js::runtime::{ordinary_object::OrdinaryObject, Value},
    set_uninit,
};

use super::{
    arguments_object::MappedArgumentsObject,
    array_object::ArrayObject,
    bound_function_object::BoundFunctionObject,
    builtin_function::BuiltinFunction,
    function::Function,
    gc::{Handle, Heap, HeapPtr, IsHeapObject},
    intrinsics::{
        function_prototype::FunctionPrototype,
        typed_array::{
            BigInt64Array, BigUInt64Array, Float32Array, Float64Array, Int16Array, Int32Array,
            Int8Array, UInt16Array, UInt32Array, UInt8Array, UInt8ClampedArray,
        },
    },
    object_value::{extract_object_vtable, VirtualObject, VirtualObjectVtable},
    proxy_object::ProxyObject,
    string_object::StringObject,
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

impl IsHeapObject for ObjectDescriptor {}

/// Type of an object on the heap. May also represent other non-object data stored on the heap,
/// e.g. descriptors and realms.
#[derive(Clone, Copy, PartialEq)]
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
    ErrorObject,
    SetObject,
    MapObject,

    Function,
    BuiltinFunction,
    BoundFunctionObject,

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

    ObjectPrototype,
    FunctionPrototype,

    // Other heap items
    String,
    Symbol,
    BigInt,
    Accessor,

    ExecutionContext,
    Realm,
    Script,

    DeclarativeEnvironment,
    FunctionEnvironment,
    GlobalEnvironment,
    ModuleEnvironment,
    ObjectEnvironment,
    PrivateEnvironment,

    DenseArrayProperties,
    SparseArrayProperties,

    ArgAccessorClosureEnvironment,
    RevokeProxyClosureEnvironment,

    // Hash maps
    ObjectNamedPropertiesMap,
    MapObjectValueMap,
    SetObjectValueSet,
    DeclarativeEnvironmentBindingsMap,
    RealmTemplateMap,
    PrivateEnvironmentNameMap,
    GlobalEnvironmentNameSet,

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
        heap: &mut Heap,
        descriptor: Handle<ObjectDescriptor>,
        kind: ObjectKind,
        flags: DescFlags,
    ) -> Handle<ObjectDescriptor>
    where
        Handle<T>: VirtualObject,
    {
        let mut desc = heap.alloc_uninit::<ObjectDescriptor>();

        set_uninit!(desc.descriptor, descriptor.get_());
        set_uninit!(desc.vtable, extract_object_vtable::<Handle<T>>());
        set_uninit!(desc.kind, kind);
        set_uninit!(desc.flags, flags);

        desc.to_handle()
    }

    pub const fn kind(&self) -> ObjectKind {
        self.kind
    }

    pub const fn vtable(&self) -> VirtualObjectVtable {
        self.vtable
    }

    pub fn is_object(&self) -> bool {
        self.flags.contains(DescFlags::IS_OBJECT)
    }
}

pub struct BaseDescriptors {
    descriptors: Vec<HeapPtr<ObjectDescriptor>>,
}

impl BaseDescriptors {
    pub fn new(heap: &mut Heap) -> BaseDescriptors {
        let mut descriptors = vec![];

        descriptors.reserve_exact(ObjectKind::count());
        unsafe { descriptors.set_len(ObjectKind::count()) };

        // Create fake handle which will be read from, in order to initialize descriptor descriptor
        let value = Value::empty();
        let fake_descriptor_handle = Handle::<Value>::from_fixed_non_heap_ptr(&value).cast();

        // First set up the singleton descriptor descriptor, using an arbitrary vtable
        // (e.g. OrdinaryObject). Can only set self pointer after object initially created.
        let mut descriptor = ObjectDescriptor::new::<OrdinaryObject>(
            heap,
            fake_descriptor_handle,
            ObjectKind::Descriptor,
            DescFlags::empty(),
        );
        descriptor.descriptor = descriptor.get_();

        macro_rules! register_descriptor {
            ($object_kind:expr, $object_ty:ty, $flags:expr) => {
                let desc =
                    ObjectDescriptor::new::<$object_ty>(heap, descriptor, $object_kind, $flags);
                descriptors[$object_kind as usize] = desc.get_();
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
        ordinary_object_descriptor!(ObjectKind::ErrorObject);
        ordinary_object_descriptor!(ObjectKind::SetObject);
        ordinary_object_descriptor!(ObjectKind::MapObject);

        register_descriptor!(ObjectKind::Function, Function, DescFlags::IS_OBJECT);
        register_descriptor!(ObjectKind::BuiltinFunction, BuiltinFunction, DescFlags::IS_OBJECT);
        register_descriptor!(
            ObjectKind::BoundFunctionObject,
            BoundFunctionObject,
            DescFlags::IS_OBJECT
        );

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

        ordinary_object_descriptor!(ObjectKind::ObjectPrototype);
        register_descriptor!(
            ObjectKind::FunctionPrototype,
            FunctionPrototype,
            DescFlags::IS_OBJECT
        );

        other_heap_object_descriptor!(ObjectKind::String);
        other_heap_object_descriptor!(ObjectKind::Symbol);
        other_heap_object_descriptor!(ObjectKind::BigInt);
        other_heap_object_descriptor!(ObjectKind::Accessor);

        other_heap_object_descriptor!(ObjectKind::ExecutionContext);
        other_heap_object_descriptor!(ObjectKind::Realm);
        other_heap_object_descriptor!(ObjectKind::Script);

        other_heap_object_descriptor!(ObjectKind::DeclarativeEnvironment);
        other_heap_object_descriptor!(ObjectKind::FunctionEnvironment);
        other_heap_object_descriptor!(ObjectKind::GlobalEnvironment);
        other_heap_object_descriptor!(ObjectKind::ModuleEnvironment);
        other_heap_object_descriptor!(ObjectKind::ObjectEnvironment);
        other_heap_object_descriptor!(ObjectKind::PrivateEnvironment);

        other_heap_object_descriptor!(ObjectKind::DenseArrayProperties);
        other_heap_object_descriptor!(ObjectKind::SparseArrayProperties);

        other_heap_object_descriptor!(ObjectKind::ArgAccessorClosureEnvironment);
        other_heap_object_descriptor!(ObjectKind::RevokeProxyClosureEnvironment);

        other_heap_object_descriptor!(ObjectKind::ObjectNamedPropertiesMap);
        other_heap_object_descriptor!(ObjectKind::MapObjectValueMap);
        other_heap_object_descriptor!(ObjectKind::SetObjectValueSet);
        other_heap_object_descriptor!(ObjectKind::DeclarativeEnvironmentBindingsMap);
        other_heap_object_descriptor!(ObjectKind::RealmTemplateMap);
        other_heap_object_descriptor!(ObjectKind::PrivateEnvironmentNameMap);
        other_heap_object_descriptor!(ObjectKind::GlobalEnvironmentNameSet);

        BaseDescriptors { descriptors }
    }

    pub fn get(&self, kind: ObjectKind) -> HeapPtr<ObjectDescriptor> {
        self.descriptors[kind as usize]
    }
}

/// An arbitrary heap item. Only common field between heap items is their descriptor, which can be
/// used to determine the true type of the heap item.
#[repr(C)]
pub struct HeapItem {
    descriptor: HeapPtr<ObjectDescriptor>,
}

impl IsHeapObject for HeapItem {}

impl HeapItem {
    pub fn descriptor(&self) -> HeapPtr<ObjectDescriptor> {
        self.descriptor
    }
}
