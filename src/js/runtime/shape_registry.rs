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

        macro_rules! other_heap_item_shape {
            ($object_kind:expr) => {
                register_shape!($object_kind, OrdinaryObject, ShapeFlags::empty());
            };
        }

        ordinary_object_shape!(HeapItemKind::OrdinaryObject);
        register_shape!(HeapItemKind::ProxyObject, ProxyObject, ShapeFlags::IS_OBJECT);

        ordinary_object_shape!(HeapItemKind::BooleanObject);
        ordinary_object_shape!(HeapItemKind::NumberObject);
        register_shape!(HeapItemKind::StringObject, StringObject, ShapeFlags::IS_OBJECT);
        ordinary_object_shape!(HeapItemKind::SymbolObject);
        ordinary_object_shape!(HeapItemKind::BigIntObject);
        register_shape!(HeapItemKind::ArrayObject, ArrayObject, ShapeFlags::IS_OBJECT);
        ordinary_object_shape!(HeapItemKind::RegExpObject);
        ordinary_object_shape!(HeapItemKind::ErrorObject);
        ordinary_object_shape!(HeapItemKind::DateObject);
        ordinary_object_shape!(HeapItemKind::SetObject);
        ordinary_object_shape!(HeapItemKind::MapObject);
        ordinary_object_shape!(HeapItemKind::WeakRefObject);
        ordinary_object_shape!(HeapItemKind::WeakSetObject);
        ordinary_object_shape!(HeapItemKind::WeakMapObject);
        ordinary_object_shape!(HeapItemKind::FinalizationRegistryObject);
        ordinary_object_shape!(HeapItemKind::RawJSONObject);

        register_shape!(
            HeapItemKind::MappedArgumentsObject,
            MappedArgumentsObject,
            ShapeFlags::IS_OBJECT
        );
        ordinary_object_shape!(HeapItemKind::UnmappedArgumentsObject);

        register_shape!(HeapItemKind::Int8ArrayObject, Int8ArrayObject, ShapeFlags::IS_OBJECT);
        register_shape!(HeapItemKind::UInt8ArrayObject, UInt8ArrayObject, ShapeFlags::IS_OBJECT);
        register_shape!(
            HeapItemKind::UInt8ClampedArrayObject,
            UInt8ClampedArrayObject,
            ShapeFlags::IS_OBJECT
        );
        register_shape!(HeapItemKind::Int16ArrayObject, Int16ArrayObject, ShapeFlags::IS_OBJECT);
        register_shape!(HeapItemKind::UInt16ArrayObject, UInt16ArrayObject, ShapeFlags::IS_OBJECT);
        register_shape!(HeapItemKind::Int32ArrayObject, Int32ArrayObject, ShapeFlags::IS_OBJECT);
        register_shape!(HeapItemKind::UInt32ArrayObject, UInt32ArrayObject, ShapeFlags::IS_OBJECT);
        register_shape!(
            HeapItemKind::BigInt64ArrayObject,
            BigInt64ArrayObject,
            ShapeFlags::IS_OBJECT
        );
        register_shape!(
            HeapItemKind::BigUInt64ArrayObject,
            BigUInt64ArrayObject,
            ShapeFlags::IS_OBJECT
        );
        register_shape!(
            HeapItemKind::Float16ArrayObject,
            Float16ArrayObject,
            ShapeFlags::IS_OBJECT
        );
        register_shape!(
            HeapItemKind::Float32ArrayObject,
            Float32ArrayObject,
            ShapeFlags::IS_OBJECT
        );
        register_shape!(
            HeapItemKind::Float64ArrayObject,
            Float64ArrayObject,
            ShapeFlags::IS_OBJECT
        );

        ordinary_object_shape!(HeapItemKind::ArrayBufferObject);
        ordinary_object_shape!(HeapItemKind::DataViewObject);

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
        other_heap_item_shape!(HeapItemKind::ForInIterator);
        ordinary_object_shape!(HeapItemKind::AsyncFromSyncIteratorObject);
        ordinary_object_shape!(HeapItemKind::WrappedValidIteratorObject);
        ordinary_object_shape!(HeapItemKind::IteratorHelperObject);

        ordinary_object_shape!(HeapItemKind::ObjectPrototypeObject);

        other_heap_item_shape!(HeapItemKind::StringValue);
        other_heap_item_shape!(HeapItemKind::SymbolValue);
        other_heap_item_shape!(HeapItemKind::BigIntValue);
        other_heap_item_shape!(HeapItemKind::Accessor);

        ordinary_object_shape!(HeapItemKind::PromiseObject);
        other_heap_item_shape!(HeapItemKind::PromiseReaction);
        other_heap_item_shape!(HeapItemKind::PromiseCapability);

        other_heap_item_shape!(HeapItemKind::Realm);

        ordinary_object_shape!(HeapItemKind::ClosureObject);
        other_heap_item_shape!(HeapItemKind::BytecodeFunction);
        other_heap_item_shape!(HeapItemKind::ConstantTable);
        other_heap_item_shape!(HeapItemKind::ExceptionHandlers);
        other_heap_item_shape!(HeapItemKind::SourceFile);

        other_heap_item_shape!(HeapItemKind::Scope);
        other_heap_item_shape!(HeapItemKind::ScopeNames);
        other_heap_item_shape!(HeapItemKind::GlobalNames);
        other_heap_item_shape!(HeapItemKind::ClassNames);

        other_heap_item_shape!(HeapItemKind::SourceTextModule);
        other_heap_item_shape!(HeapItemKind::SyntheticModule);
        register_shape!(
            HeapItemKind::ModuleNamespaceObject,
            ModuleNamespaceObject,
            ShapeFlags::IS_OBJECT
        );
        other_heap_item_shape!(HeapItemKind::ImportAttributes);

        ordinary_object_shape!(HeapItemKind::GeneratorObject);
        ordinary_object_shape!(HeapItemKind::AsyncGeneratorObject);
        other_heap_item_shape!(HeapItemKind::AsyncGeneratorRequest);
        other_heap_item_shape!(HeapItemKind::BuiltinGenerator);

        other_heap_item_shape!(HeapItemKind::DenseArrayProperties);
        other_heap_item_shape!(HeapItemKind::SparseArrayPropertiesMap);

        other_heap_item_shape!(HeapItemKind::CompiledRegExp);

        other_heap_item_shape!(HeapItemKind::BoxedValue);

        other_heap_item_shape!(HeapItemKind::NamedPropertiesMap);
        other_heap_item_shape!(HeapItemKind::ValueIndexMap);
        other_heap_item_shape!(HeapItemKind::ValueIndexSet);
        other_heap_item_shape!(HeapItemKind::ExportMap);
        other_heap_item_shape!(HeapItemKind::WeakValueMap);
        other_heap_item_shape!(HeapItemKind::WeakValueSet);
        other_heap_item_shape!(HeapItemKind::GlobalSymbolRegistryMap);
        other_heap_item_shape!(HeapItemKind::InternedStringsSet);
        other_heap_item_shape!(HeapItemKind::LexicalNamesMap);
        other_heap_item_shape!(HeapItemKind::ModuleCacheMap);

        other_heap_item_shape!(HeapItemKind::ValueArray);
        other_heap_item_shape!(HeapItemKind::ByteArray);
        other_heap_item_shape!(HeapItemKind::U32Array);
        other_heap_item_shape!(HeapItemKind::ModuleRequestArray);
        other_heap_item_shape!(HeapItemKind::ModuleOptionArray);
        other_heap_item_shape!(HeapItemKind::StackFrameInfoArray);
        other_heap_item_shape!(HeapItemKind::StackFrameArray);
        other_heap_item_shape!(HeapItemKind::FinalizationRegistryCells);
        other_heap_item_shape!(HeapItemKind::GlobalScopes);

        other_heap_item_shape!(HeapItemKind::FunctionVec);
        other_heap_item_shape!(HeapItemKind::SourceTextModuleVec);
        other_heap_item_shape!(HeapItemKind::WeakValueVec);

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
