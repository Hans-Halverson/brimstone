use super::{
    arguments_object::MappedArgumentsObject,
    array_object::ArrayObject,
    bound_function_object::BoundFunctionObject,
    builtin_function::BuiltinFunction,
    function::Function,
    gc::{GcDeref, Heap},
    intrinsics::{
        function_prototype::FunctionPrototype,
        typed_array::{
            BigInt64Array, BigUInt64Array, Float32Array, Float64Array, Int16Array, Int32Array,
            Int8Array, UInt16Array, UInt32Array, UInt8Array, UInt8ClampedArray,
        },
    },
    object_value::{extract_object_vtable, VirtualObject, VirtualObjectVtable},
    ordinary_object::OrdinaryObject,
    proxy_object::ProxyObject,
    string_object::StringObject,
    Gc,
};

/// An object's "hidden class".
#[repr(C)]
pub struct ObjectDescriptor {
    // Always the singleton descriptor descriptor
    descriptor: Gc<ObjectDescriptor>,
    // Object's type
    kind: ObjectKind,
    // Rust VirtualObject vtable, used for dynamic dispatch to some object methods
    vtable: VirtualObjectVtable,
}

impl GcDeref for ObjectDescriptor {}

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

    // Numerical value is the number of kinds in the enum
    Last,
}

impl ObjectKind {
    const fn count() -> usize {
        ObjectKind::Last as usize
    }
}

impl ObjectDescriptor {
    pub fn new<T: VirtualObject>(
        heap: &mut Heap,
        descriptor: Gc<ObjectDescriptor>,
        kind: ObjectKind,
    ) -> Gc<ObjectDescriptor> {
        let desc = ObjectDescriptor { descriptor, kind, vtable: extract_object_vtable::<T>() };
        heap.alloc(desc)
    }

    pub const fn kind(&self) -> ObjectKind {
        self.kind
    }

    pub const fn vtable(&self) -> VirtualObjectVtable {
        self.vtable
    }
}

pub struct BaseDescriptors {
    descriptors: Vec<Gc<ObjectDescriptor>>,
}

impl BaseDescriptors {
    pub fn new(heap: &mut Heap) -> BaseDescriptors {
        let mut descriptors = vec![];

        descriptors.reserve_exact(ObjectKind::count());
        unsafe { descriptors.set_len(ObjectKind::count()) };

        // First set up the singleton descriptor descriptor, using an arbitrary vtable
        // (e.g. OrdinaryObject). Can only set self pointer after object initially created.
        let mut descriptor =
            ObjectDescriptor::new::<OrdinaryObject>(heap, Gc::uninit(), ObjectKind::Descriptor);
        descriptor.descriptor = descriptor;

        macro_rules! register_descriptor {
            ($object_kind:expr, $object_ty:ty) => {
                let desc = ObjectDescriptor::new::<$object_ty>(heap, descriptor, $object_kind);
                descriptors[$object_kind as usize] = desc;
            };
        }

        macro_rules! ordinary_descriptor {
            ($object_kind:expr) => {
                register_descriptor!($object_kind, OrdinaryObject);
            };
        }

        ordinary_descriptor!(ObjectKind::OrdinaryObject);
        register_descriptor!(ObjectKind::Proxy, ProxyObject);

        ordinary_descriptor!(ObjectKind::BooleanObject);
        ordinary_descriptor!(ObjectKind::NumberObject);
        register_descriptor!(ObjectKind::StringObject, StringObject);
        ordinary_descriptor!(ObjectKind::SymbolObject);
        ordinary_descriptor!(ObjectKind::BigIntObject);
        register_descriptor!(ObjectKind::ArrayObject, ArrayObject);
        ordinary_descriptor!(ObjectKind::ErrorObject);
        ordinary_descriptor!(ObjectKind::SetObject);
        ordinary_descriptor!(ObjectKind::MapObject);

        register_descriptor!(ObjectKind::Function, Function);
        register_descriptor!(ObjectKind::BuiltinFunction, BuiltinFunction);
        register_descriptor!(ObjectKind::BoundFunctionObject, BoundFunctionObject);

        register_descriptor!(ObjectKind::MappedArgumentsObject, MappedArgumentsObject);
        ordinary_descriptor!(ObjectKind::UnmappedArgumentsObject);

        register_descriptor!(ObjectKind::Int8Array, Int8Array);
        register_descriptor!(ObjectKind::UInt8Array, UInt8Array);
        register_descriptor!(ObjectKind::UInt8ClampedArray, UInt8ClampedArray);
        register_descriptor!(ObjectKind::Int16Array, Int16Array);
        register_descriptor!(ObjectKind::UInt16Array, UInt16Array);
        register_descriptor!(ObjectKind::Int32Array, Int32Array);
        register_descriptor!(ObjectKind::UInt32Array, UInt32Array);
        register_descriptor!(ObjectKind::BigInt64Array, BigInt64Array);
        register_descriptor!(ObjectKind::BigUInt64Array, BigUInt64Array);
        register_descriptor!(ObjectKind::Float32Array, Float32Array);
        register_descriptor!(ObjectKind::Float64Array, Float64Array);

        ordinary_descriptor!(ObjectKind::ArrayBufferObject);
        ordinary_descriptor!(ObjectKind::DataViewObject);

        ordinary_descriptor!(ObjectKind::ArrayIterator);
        ordinary_descriptor!(ObjectKind::StringIterator);
        ordinary_descriptor!(ObjectKind::SetIterator);
        ordinary_descriptor!(ObjectKind::MapIterator);

        ordinary_descriptor!(ObjectKind::ObjectPrototype);
        register_descriptor!(ObjectKind::FunctionPrototype, FunctionPrototype);

        BaseDescriptors { descriptors }
    }

    pub fn get(&self, kind: ObjectKind) -> Gc<ObjectDescriptor> {
        self.descriptors[kind as usize]
    }
}
