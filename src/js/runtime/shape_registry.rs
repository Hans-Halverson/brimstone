use crate::runtime::{
    Context, HeapItemKind,
    alloc_error::AllocResult,
    arguments_object::MappedArgumentsObject,
    array_object::ArrayObject,
    gc::{HeapPtr, HeapVisitor},
    intrinsics::typed_array::{
        BigInt64ArrayObject, BigUInt64ArrayObject, Float16ArrayObject, Float32ArrayObject,
        Float64ArrayObject, Int8ArrayObject, Int16ArrayObject, Int32ArrayObject, UInt8ArrayObject,
        UInt8ClampedArrayObject, UInt16ArrayObject, UInt32ArrayObject,
    },
    module::module_namespace_object::ModuleNamespaceObject,
    ordinary_object::OrdinaryObject,
    proxy_object::ProxyObject,
    shape::{Shape, ShapeFlags},
    string_object::StringObject,
};

/// Central registry for all shapes in a context.
pub struct ShapeRegistry {
    shapes: Vec<HeapPtr<Shape>>,
}

impl ShapeRegistry {
    pub fn uninit_empty() -> Self {
        ShapeRegistry { shapes: vec![] }
    }

    pub fn uninit() -> Self {
        let mut shapes = vec![];

        shapes.reserve_exact(HeapItemKind::COUNT);
        unsafe { shapes.set_len(HeapItemKind::COUNT) };

        ShapeRegistry { shapes }
    }

    pub fn new(cx: Context) -> AllocResult<ShapeRegistry> {
        let mut base_shapes = Self::uninit();
        let shapes = &mut base_shapes.shapes;

        // First set up the shape shape since it is needed for all others
        let shape_shape = Shape::new_shape_shape(cx)?.to_handle();
        shapes[HeapItemKind::Shape as usize] = *shape_shape;

        macro_rules! register_shape {
            ($object_kind:expr, $object_ty:ty, $flags:expr) => {
                let shape = Shape::new::<$object_ty>(cx, shape_shape, $object_kind, $flags)?;
                shapes[$object_kind as usize] = shape;
            };
        }

        macro_rules! ordinary_object_shape {
            ($object_kind:expr) => {
                register_shape!($object_kind, OrdinaryObject, ShapeFlags::IS_OBJECT);
            };
        }

        macro_rules! exotic_object_shape {
            ($object_kind:expr, $object_ty:ty) => {
                register_shape!($object_kind, $object_ty, ShapeFlags::IS_OBJECT);
            };
        }

        macro_rules! non_object_heap_item_shape {
            ($object_kind:expr) => {
                register_shape!($object_kind, OrdinaryObject, ShapeFlags::empty());
            };
        }

        // Objects
        ordinary_object_shape!(HeapItemKind::OrdinaryObject);
        exotic_object_shape!(HeapItemKind::ProxyObject, ProxyObject);
        ordinary_object_shape!(HeapItemKind::BooleanObject);
        ordinary_object_shape!(HeapItemKind::NumberObject);
        exotic_object_shape!(HeapItemKind::StringObject, StringObject);
        ordinary_object_shape!(HeapItemKind::SymbolObject);
        ordinary_object_shape!(HeapItemKind::BigIntObject);
        exotic_object_shape!(HeapItemKind::ArrayObject, ArrayObject);
        ordinary_object_shape!(HeapItemKind::RegExpObject);
        ordinary_object_shape!(HeapItemKind::ErrorObject);
        ordinary_object_shape!(HeapItemKind::DateObject);
        ordinary_object_shape!(HeapItemKind::SetObject);
        ordinary_object_shape!(HeapItemKind::MapObject);
        ordinary_object_shape!(HeapItemKind::WeakRefObject);
        ordinary_object_shape!(HeapItemKind::WeakSetObject);
        ordinary_object_shape!(HeapItemKind::WeakMapObject);
        ordinary_object_shape!(HeapItemKind::FinalizationRegistryObject);
        ordinary_object_shape!(HeapItemKind::PromiseObject);
        ordinary_object_shape!(HeapItemKind::ClosureObject);
        ordinary_object_shape!(HeapItemKind::GeneratorObject);
        ordinary_object_shape!(HeapItemKind::AsyncGeneratorObject);
        exotic_object_shape!(HeapItemKind::ModuleNamespaceObject, ModuleNamespaceObject);
        exotic_object_shape!(HeapItemKind::MappedArgumentsObject, MappedArgumentsObject);
        ordinary_object_shape!(HeapItemKind::UnmappedArgumentsObject);
        ordinary_object_shape!(HeapItemKind::ArrayBufferObject);
        ordinary_object_shape!(HeapItemKind::DataViewObject);
        exotic_object_shape!(HeapItemKind::Int8ArrayObject, Int8ArrayObject);
        exotic_object_shape!(HeapItemKind::UInt8ArrayObject, UInt8ArrayObject);
        exotic_object_shape!(HeapItemKind::UInt8ClampedArrayObject, UInt8ClampedArrayObject);
        exotic_object_shape!(HeapItemKind::Int16ArrayObject, Int16ArrayObject);
        exotic_object_shape!(HeapItemKind::UInt16ArrayObject, UInt16ArrayObject);
        exotic_object_shape!(HeapItemKind::Int32ArrayObject, Int32ArrayObject);
        exotic_object_shape!(HeapItemKind::UInt32ArrayObject, UInt32ArrayObject);
        exotic_object_shape!(HeapItemKind::BigInt64ArrayObject, BigInt64ArrayObject);
        exotic_object_shape!(HeapItemKind::BigUInt64ArrayObject, BigUInt64ArrayObject);
        exotic_object_shape!(HeapItemKind::Float16ArrayObject, Float16ArrayObject);
        exotic_object_shape!(HeapItemKind::Float32ArrayObject, Float32ArrayObject);
        exotic_object_shape!(HeapItemKind::Float64ArrayObject, Float64ArrayObject);
        ordinary_object_shape!(HeapItemKind::DurationObject);
        ordinary_object_shape!(HeapItemKind::InstantObject);
        ordinary_object_shape!(HeapItemKind::PlainDateObject);
        ordinary_object_shape!(HeapItemKind::PlainDateTimeObject);
        ordinary_object_shape!(HeapItemKind::PlainMonthDayObject);
        ordinary_object_shape!(HeapItemKind::PlainTimeObject);
        ordinary_object_shape!(HeapItemKind::PlainYearMonthObject);
        ordinary_object_shape!(HeapItemKind::ZonedDateTimeObject);
        ordinary_object_shape!(HeapItemKind::ArrayIteratorObject);
        ordinary_object_shape!(HeapItemKind::StringIteratorObject);
        ordinary_object_shape!(HeapItemKind::SetIteratorObject);
        ordinary_object_shape!(HeapItemKind::MapIteratorObject);
        ordinary_object_shape!(HeapItemKind::RegExpStringIteratorObject);
        ordinary_object_shape!(HeapItemKind::AsyncFromSyncIteratorObject);
        ordinary_object_shape!(HeapItemKind::WrappedValidIteratorObject);
        ordinary_object_shape!(HeapItemKind::IteratorHelperObject);
        ordinary_object_shape!(HeapItemKind::ObjectPrototypeObject);
        ordinary_object_shape!(HeapItemKind::RawJSONObject);

        // Non-objects
        non_object_heap_item_shape!(HeapItemKind::StringValue);
        non_object_heap_item_shape!(HeapItemKind::SymbolValue);
        non_object_heap_item_shape!(HeapItemKind::BigIntValue);
        non_object_heap_item_shape!(HeapItemKind::Accessor);
        non_object_heap_item_shape!(HeapItemKind::PromiseReaction);
        non_object_heap_item_shape!(HeapItemKind::PromiseCapability);
        non_object_heap_item_shape!(HeapItemKind::Realm);
        non_object_heap_item_shape!(HeapItemKind::BytecodeFunction);
        non_object_heap_item_shape!(HeapItemKind::ConstantTable);
        non_object_heap_item_shape!(HeapItemKind::ExceptionHandlers);
        non_object_heap_item_shape!(HeapItemKind::SourceFile);
        non_object_heap_item_shape!(HeapItemKind::Scope);
        non_object_heap_item_shape!(HeapItemKind::ScopeNames);
        non_object_heap_item_shape!(HeapItemKind::GlobalNames);
        non_object_heap_item_shape!(HeapItemKind::ClassNames);
        non_object_heap_item_shape!(HeapItemKind::SourceTextModule);
        non_object_heap_item_shape!(HeapItemKind::SyntheticModule);
        non_object_heap_item_shape!(HeapItemKind::ImportAttributes);
        non_object_heap_item_shape!(HeapItemKind::AsyncGeneratorRequest);
        non_object_heap_item_shape!(HeapItemKind::BuiltinGenerator);
        non_object_heap_item_shape!(HeapItemKind::DenseArrayProperties);
        non_object_heap_item_shape!(HeapItemKind::SparseArrayPropertiesMap);
        non_object_heap_item_shape!(HeapItemKind::CompiledRegExp);
        non_object_heap_item_shape!(HeapItemKind::ForInIterator);
        non_object_heap_item_shape!(HeapItemKind::BoxedValue);
        non_object_heap_item_shape!(HeapItemKind::NamedPropertiesMap);
        non_object_heap_item_shape!(HeapItemKind::ValueIndexMap);
        non_object_heap_item_shape!(HeapItemKind::ValueIndexSet);
        non_object_heap_item_shape!(HeapItemKind::ExportMap);
        non_object_heap_item_shape!(HeapItemKind::WeakValueMap);
        non_object_heap_item_shape!(HeapItemKind::WeakValueSet);
        non_object_heap_item_shape!(HeapItemKind::GlobalSymbolRegistryMap);
        non_object_heap_item_shape!(HeapItemKind::InternedStringsSet);
        non_object_heap_item_shape!(HeapItemKind::LexicalNamesMap);
        non_object_heap_item_shape!(HeapItemKind::ModuleCacheMap);
        non_object_heap_item_shape!(HeapItemKind::ValueArray);
        non_object_heap_item_shape!(HeapItemKind::ByteArray);
        non_object_heap_item_shape!(HeapItemKind::U32Array);
        non_object_heap_item_shape!(HeapItemKind::ModuleRequestArray);
        non_object_heap_item_shape!(HeapItemKind::ModuleOptionArray);
        non_object_heap_item_shape!(HeapItemKind::StackFrameInfoArray);
        non_object_heap_item_shape!(HeapItemKind::StackFrameArray);
        non_object_heap_item_shape!(HeapItemKind::FinalizationRegistryCells);
        non_object_heap_item_shape!(HeapItemKind::GlobalScopes);
        non_object_heap_item_shape!(HeapItemKind::FunctionVec);
        non_object_heap_item_shape!(HeapItemKind::SourceTextModuleVec);
        non_object_heap_item_shape!(HeapItemKind::WeakValueVec);

        Ok(base_shapes)
    }

    pub fn get(&self, kind: HeapItemKind) -> HeapPtr<Shape> {
        self.shapes[kind as usize]
    }

    pub fn visit_roots(&mut self, visitor: &mut impl HeapVisitor) {
        for shape in &mut self.shapes {
            visitor.visit_pointer(shape);
        }
    }
}
