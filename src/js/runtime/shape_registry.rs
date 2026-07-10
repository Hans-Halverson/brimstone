use std::hash::{Hash, Hasher};

use crate::{
    impl_hash_map_instance,
    runtime::{
        Context, Handle, HeapItemKind,
        alloc_error::AllocResult,
        arguments_object::MappedArgumentsObject,
        array_object::ArrayObject,
        collections::{BsHashMapField, HashMapInstance, VecInstance},
        gc::{HeapItem, HeapPtr, HeapVisitor, WithHeapItemKind},
        global_object::GlobalObject,
        intrinsics::typed_array::{
            BigInt64ArrayObject, BigUInt64ArrayObject, Float16ArrayObject, Float32ArrayObject,
            Float64ArrayObject, Int8ArrayObject, Int16ArrayObject, Int32ArrayObject,
            UInt8ArrayObject, UInt8ClampedArrayObject, UInt16ArrayObject, UInt32ArrayObject,
        },
        module::module_namespace_object::ModuleNamespaceObject,
        object_value::ObjectValue,
        ordinary_object::OrdinaryObject,
        proxy_object::ProxyObject,
        shape::{ObjectFlags, PropertyDefinitionVec, Shape},
        string_object::StringObject,
    },
    set_uninit,
};

/// Central registry for all shapes in a context.
pub struct ShapeRegistry {
    /// Canonical shapes for all heap items, indexed by kind.
    /// - For object items these are the initial shape for that kind without a prototype. They are
    ///   not part of the transition tree nor are they used directly.
    /// - For non-object items these are the singleton shape for that kind
    canonical_shapes: Vec<HeapPtr<Shape>>,

    /// Root shapes in the transition tree, keyed by the object's prototype and kind.
    transition_tree_roots: HeapPtr<TransitionTreeRootsMap>,

    /// An empty vector of property definitions to use as the initial value in a shape
    pub default_property_definitions: HeapPtr<PropertyDefinitionVec>,
}

impl ShapeRegistry {
    pub fn uninit_empty() -> Self {
        ShapeRegistry {
            canonical_shapes: vec![],
            transition_tree_roots: HeapPtr::uninit(),
            default_property_definitions: HeapPtr::uninit(),
        }
    }

    pub fn uninit() -> Self {
        let mut canonical_shapes = vec![];

        canonical_shapes.reserve_exact(HeapItemKind::COUNT);
        unsafe { canonical_shapes.set_len(HeapItemKind::COUNT) };

        ShapeRegistry {
            canonical_shapes,
            transition_tree_roots: HeapPtr::uninit(),
            default_property_definitions: HeapPtr::uninit(),
        }
    }

    /// Must be called immediately after the registry has been created with all shapes and assigned
    /// to the context, since allocating looks up shapes in the registry.
    pub fn init(mut cx: Context) -> AllocResult<()> {
        // Default property definitions can now reference a shape
        let mut default_property_definitions = cx.shapes.default_property_definitions;
        default_property_definitions.init(cx, PropertyDefinitionVec::KIND, 0);

        // Transition tree roots map can now be allocated
        let transition_roots_map = TransitionTreeRootsMap::new_initial(cx)?;
        set_uninit!(cx.shapes.transition_tree_roots, transition_roots_map);

        Ok(())
    }

    pub fn new(cx: Context) -> AllocResult<ShapeRegistry> {
        let mut base_shapes = Self::uninit();
        let shapes = &mut base_shapes.canonical_shapes;

        // Empty default property descriptions vec is referenced by all shapes, but also needs to
        // reference a shape. Allocate uninitialized then initialize after shapes are created.
        let default_property_definitions_size = PropertyDefinitionVec::calculate_size_in_bytes(0);
        let default_property_definitions = cx
            .alloc_uninit_with_size::<PropertyDefinitionVec>(default_property_definitions_size)?
            .to_handle();
        base_shapes.default_property_definitions = *default_property_definitions;

        // Shape shape is needed for all other shapes
        let shape_shape = Shape::new_shape_shape(cx, default_property_definitions)?.to_handle();
        shapes[HeapItemKind::Shape as usize] = *shape_shape;

        macro_rules! register_shape {
            ($object_kind:expr, $object_ty:ty, $flags:expr) => {
                let shape = Shape::new::<$object_ty>(
                    cx,
                    shape_shape,
                    $object_kind,
                    $flags,
                    /* is_prototype_object */ false,
                    default_property_definitions,
                )?;
                shapes[$object_kind as usize] = shape;
            };
        }

        macro_rules! ordinary_object_shape {
            ($object_kind:expr) => {
                register_shape!($object_kind, OrdinaryObject, ObjectFlags::new_object());
            };
        }

        macro_rules! exotic_object_shape {
            ($object_kind:expr, $object_ty:ty) => {
                register_shape!($object_kind, $object_ty, ObjectFlags::new_object());
            };
        }

        macro_rules! non_object_heap_item_shape {
            ($object_kind:expr) => {
                register_shape!($object_kind, OrdinaryObject, ObjectFlags::new_non_object());
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
        register_shape!(
            HeapItemKind::ModuleNamespaceObject,
            ModuleNamespaceObject,
            ObjectFlags::new_object() & !ObjectFlags::IS_EXTENSIBLE
        );
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
        exotic_object_shape!(HeapItemKind::GlobalObject, GlobalObject);
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
        non_object_heap_item_shape!(HeapItemKind::GlobalProperty);
        non_object_heap_item_shape!(HeapItemKind::NamedPropertiesMap);
        non_object_heap_item_shape!(HeapItemKind::GlobalPropertiesMap);
        non_object_heap_item_shape!(HeapItemKind::ValueIndexMap);
        non_object_heap_item_shape!(HeapItemKind::ValueIndexSet);
        non_object_heap_item_shape!(HeapItemKind::ExportMap);
        non_object_heap_item_shape!(HeapItemKind::WeakValueMap);
        non_object_heap_item_shape!(HeapItemKind::WeakValueSet);
        non_object_heap_item_shape!(HeapItemKind::TransitionTreeRootsMap);
        non_object_heap_item_shape!(HeapItemKind::GlobalSymbolRegistryMap);
        non_object_heap_item_shape!(HeapItemKind::InternedStringsSet);
        non_object_heap_item_shape!(HeapItemKind::LexicalNamesMap);
        non_object_heap_item_shape!(HeapItemKind::ModuleCacheMap);
        non_object_heap_item_shape!(HeapItemKind::ValueArray);
        non_object_heap_item_shape!(HeapItemKind::ByteArray);
        non_object_heap_item_shape!(HeapItemKind::U32Array);
        non_object_heap_item_shape!(HeapItemKind::ModuleRequestArray);
        non_object_heap_item_shape!(HeapItemKind::ModuleOptionArray);
        non_object_heap_item_shape!(HeapItemKind::CacheArray);
        non_object_heap_item_shape!(HeapItemKind::StackFrameInfoArray);
        non_object_heap_item_shape!(HeapItemKind::StackFrameArray);
        non_object_heap_item_shape!(HeapItemKind::FinalizationRegistryCells);
        non_object_heap_item_shape!(HeapItemKind::GlobalScopes);
        non_object_heap_item_shape!(HeapItemKind::ValueVec);
        non_object_heap_item_shape!(HeapItemKind::FunctionVec);
        non_object_heap_item_shape!(HeapItemKind::SourceTextModuleVec);
        non_object_heap_item_shape!(HeapItemKind::TransitionVec);
        non_object_heap_item_shape!(HeapItemKind::PropertyDefinitionVec);
        non_object_heap_item_shape!(HeapItemKind::PrototypeObjectChildrenShapesVec);

        Ok(base_shapes)
    }

    /// Get a non-object shape by its kind. Must only be called on non-object kinds.
    pub fn get(&self, kind: HeapItemKind) -> HeapPtr<Shape> {
        debug_assert!(!kind.is_object_kind());

        self.canonical_shapes[kind as usize]
    }

    /// Get the root shape in the transition tree for an object with the given prototype and kind.
    /// Must only be called for object kinds.
    pub fn get_root_object_shape(
        cx: Context,
        kind: HeapItemKind,
        prototype: Option<Handle<ObjectValue>>,
    ) -> AllocResult<Handle<Shape>> {
        debug_assert!(kind.is_object_kind());

        // Use the root shape for this prototype and kind, if one already exists
        let key = PrototypeAndKind { prototype: prototype.map(|p| *p), kind };
        if let Some(root_shape) = cx.shapes.transition_tree_roots.get(&key) {
            return Ok(root_shape.to_handle());
        }

        // Otherwise none exists yet, so create a new root shape for this prototype and kind
        let canonical_shape = cx.shapes.canonical_shapes[kind as usize].to_handle();
        let new_root_shape = canonical_shape
            .clone_for_transition_tree_root(cx, prototype)?
            .to_handle();

        TransitionTreeRootsMapField
            .maybe_grow_for_insertion(cx)?
            .insert_without_growing(
                PrototypeAndKind { prototype: prototype.map(|p| *p), kind },
                *new_root_shape,
            );

        Ok(new_root_shape)
    }

    pub fn transition_tree_roots_ptr(&self) -> HeapPtr<TransitionTreeRootsMap> {
        self.transition_tree_roots
    }

    /// Visit all heap roots that should always be visited.
    pub fn visit_common_roots(&mut self, visitor: &mut impl HeapVisitor) {
        visitor.visit_pointer(&mut self.transition_tree_roots);
    }

    /// Visit all heap roots that are guaranteed to point to the permanent semispace.
    pub fn visit_permanent_roots(&mut self, visitor: &mut impl HeapVisitor) {
        for shape in &mut self.canonical_shapes {
            visitor.visit_pointer(shape);
        }

        visitor.visit_pointer(&mut self.default_property_definitions);
    }
}

impl_hash_map_instance!(TransitionTreeRootsMap, PrototypeAndKind, HeapPtr<Shape>);

impl HeapItem for TransitionTreeRootsMap {
    fn byte_size(map: HeapPtr<Self>) -> usize {
        Self::calculate_size_in_bytes(map.capacity())
    }

    fn visit_pointers(mut map: HeapPtr<Self>, visitor: &mut impl HeapVisitor) {
        map.visit_map_pointers(visitor);

        // Weak references to the transition tree (and keys). Will be pruned and fixed up during GC.
        for (key, shape) in map.iter_mut_gc_unsafe() {
            visitor.visit_weak_pointer_opt(&mut key.prototype);
            visitor.visit_weak_pointer(shape);
        }
    }
}

pub struct TransitionTreeRootsMapField;

impl BsHashMapField<TransitionTreeRootsMap> for TransitionTreeRootsMapField {
    fn get(&self, cx: Context) -> HeapPtr<TransitionTreeRootsMap> {
        cx.shapes.transition_tree_roots
    }

    fn set_new(
        &mut self,
        mut cx: Context,
        capacity: usize,
    ) -> AllocResult<HeapPtr<TransitionTreeRootsMap>> {
        let map = TransitionTreeRootsMap::new(cx, capacity)?;
        cx.shapes.transition_tree_roots = map;
        Ok(map)
    }
}

/// Simple key for object shape registry.
#[derive(Clone, Copy)]
pub struct PrototypeAndKind {
    pub prototype: Option<HeapPtr<ObjectValue>>,
    pub kind: HeapItemKind,
}

impl PartialEq for PrototypeAndKind {
    fn eq(&self, other: &Self) -> bool {
        self.prototype.map(|p| p.as_ptr()) == other.prototype.map(|p| p.as_ptr())
            && self.kind == other.kind
    }
}

impl Eq for PrototypeAndKind {}

impl Hash for PrototypeAndKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if let Some(mut prototype) = self.prototype {
            state.write_u32(prototype.hash_code().into());
        } else {
            state.write_u8(0);
        }

        state.write_u8(self.kind as u8);
    }
}
